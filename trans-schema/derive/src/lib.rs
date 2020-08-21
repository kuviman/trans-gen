#![recursion_limit = "256"]
extern crate proc_macro;

use quote::quote;

use proc_macro2::TokenStream;

#[proc_macro_derive(Schematic, attributes(schematic))]
pub fn derive_schematic(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let result: TokenStream = {
        let ast: syn::DeriveInput = syn::parse_str(&input.to_string()).unwrap();
        let input_type = &ast.ident;
        let generic_params: Vec<_> = ast
            .generics
            .type_params()
            .map(|param| &param.ident)
            .collect();
        let generic_params = &generic_params;
        let mut base_name =
            syn::LitStr::new(&ast.ident.to_string(), proc_macro2::Span::call_site());
        let mut magic: Option<syn::Expr> = None;
        let mut generics_in_name = true;
        for attr in &ast.attrs {
            if let Ok(syn::Meta::List(syn::MetaList {
                path: ref meta_path,
                ref nested,
                ..
            })) = attr.parse_meta()
            {
                if meta_path.is_ident("schematic") {
                    for inner in nested {
                        match *inner {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                                path: ref meta_path,
                                lit: syn::Lit::Str(ref lit),
                                ..
                            })) => {
                                if meta_path.is_ident("rename") {
                                    base_name = lit.clone();
                                } else if meta_path.is_ident("magic") {
                                    magic = Some(syn::parse_str(&lit.value()).unwrap());
                                }
                            }
                            syn::NestedMeta::Meta(syn::Meta::Path(ref meta_path)) => {
                                if meta_path.is_ident("no_generics_in_name") {
                                    generics_in_name = false;
                                }
                            }
                            _ => panic!("Unexpected meta"),
                        }
                    }
                }
            }
        }
        let magic = match magic {
            Some(expr) => quote! { Some(#expr) },
            None => quote! { None },
        };
        let final_name = quote! {{
            let mut name = #base_name.to_owned();
            if #generics_in_name {
                #(
                    name += &trans_schema::schema::<#generic_params>().full_name().raw();
                )*
            }
            name
        }};
        match ast.data {
            syn::Data::Struct(syn::DataStruct { ref fields, .. }) => match fields {
                syn::Fields::Named(_) => {
                    let field_tys: Vec<_> = fields.iter().map(|field| &field.ty).collect();
                    let field_tys = &field_tys;
                    let field_names: Vec<_> = fields
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap())
                        .collect();
                    let field_names = &field_names;
                    let mut generics = ast.generics.clone();
                    let extra_where_clauses = quote! {
                        where
                            #(#field_tys: trans_schema::Schematic + 'static,)*
                            #(#generic_params: trans_schema::Schematic,)*
                    };
                    let extra_where_clauses: syn::WhereClause =
                        syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                    generics
                        .make_where_clause()
                        .predicates
                        .extend(extra_where_clauses.predicates);
                    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                    let expanded = quote! {
                        impl #impl_generics trans_schema::Schematic for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans_schema::Schema {
                                let name = #final_name;
                                trans_schema::Schema::Struct(trans_schema::Struct {
                                    name: trans_schema::Name::new(name),
                                    magic: #magic,
                                    fields: vec![
                                        #(trans_schema::Field {
                                            name: trans_schema::Name::new(stringify!(#field_names).to_owned()),
                                            schema: trans_schema::schema::<#field_tys>(),
                                        }),*
                                    ],
                                })
                            }
                        }
                    };
                    expanded.into()
                }
                syn::Fields::Unnamed(_) => {
                    if fields.iter().len() != 1 {
                        panic!("Tuple structs other than newtype not supported");
                    }
                    let inner_ty = fields.iter().next().unwrap();
                    let mut generics = ast.generics.clone();
                    let extra_where_clauses = quote! {
                        where #inner_ty: trans_schema::Schematic + 'static
                    };
                    let extra_where_clauses: syn::WhereClause =
                        syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                    generics
                        .make_where_clause()
                        .predicates
                        .extend(extra_where_clauses.predicates);
                    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                    let expanded = quote! {
                        impl #impl_generics trans_schema::Schematic for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans_schema::Schema {
                                <#inner_ty as trans_schema::Schematic>::create_schema()
                            }
                        }
                    };
                    expanded.into()
                }
                syn::Fields::Unit => panic!("Unit structs not supported"),
            },
            syn::Data::Enum(syn::DataEnum { ref variants, .. }) => {
                let mut generics = ast.generics.clone();
                // let all_field_tys = variants
                //     .iter()
                //     .map(|variant| variant.fields.iter().map(|field| &field.ty))
                //     .flatten();
                // let extra_where_clauses = quote! {
                //     where
                //         #(#all_field_tys: trans_schema::Schematic + 'static,)*
                //         #(#generic_params: trans_schema::Schematic,)*
                // };
                // let extra_where_clauses: syn::WhereClause =
                //     syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                // generics
                //     .make_where_clause()
                //     .predicates
                //     .extend(extra_where_clauses.predicates);
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                if variants.iter().all(|variant| {
                    if let syn::Fields::Unit = variant.fields {
                        true
                    } else {
                        false
                    }
                }) {
                    let variants = variants.iter();
                    let expanded = quote! {
                        impl #impl_generics trans_schema::Schematic for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans_schema::Schema {
                                let base_name = #final_name;
                                trans_schema::Schema::Enum {
                                    base_name: trans_schema::Name::new(base_name),
                                    variants: vec![#(trans_schema::Name::new(stringify!(#variants).to_owned())),*],
                                }
                            }
                        }
                    };
                    expanded.into()
                } else {
                    let variants = variants.iter().map(|variant| {
                        let variant_name = &variant.ident;
                        let field_names = variant
                            .fields
                            .iter()
                            .map(|field| field.ident.as_ref().unwrap());
                        let field_tys = variant.fields.iter().map(|field| &field.ty);
                        quote! {
                            trans_schema::Struct {
                                name: trans_schema::Name::new(stringify!(#variant_name).to_owned()),
                                magic: None,
                                fields: vec![
                                    #(trans_schema::Field {
                                        name: trans_schema::Name::new(stringify!(#field_names).to_owned()),
                                        schema: trans_schema::schema::<#field_tys>(),
                                    }),*
                                ],
                            }
                        }
                    });
                    let expanded = quote! {
                        impl #impl_generics trans_schema::Schematic for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans_schema::Schema {
                                let base_name = #final_name;
                                trans_schema::Schema::OneOf {
                                    base_name: trans_schema::Name::new(base_name),
                                    variants: vec![#(#variants),*],
                                }
                            }
                        }
                    };
                    expanded.into()
                }
            }
            syn::Data::Union(_) => panic!("Unions not supported"),
        }
    };
    result.into()
}
