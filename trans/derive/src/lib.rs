#![recursion_limit = "256"]
extern crate proc_macro;

use quote::quote;

use proc_macro2::TokenStream;

fn field_schema_names<'a>(
    fields: impl Iterator<Item = &'a syn::Field> + 'a,
) -> impl Iterator<Item = syn::Ident> + 'a {
    fields.map(|field| {
        let mut name = field.ident.clone().unwrap();
        for attr in &field.attrs {
            if let Ok(syn::Meta::List(syn::MetaList {
                path: ref meta_path,
                ref nested,
                ..
            })) = attr.parse_meta()
            {
                if meta_path.is_ident("trans") {
                    for inner in nested {
                        match *inner {
                            syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                                path: ref meta_path,
                                lit: syn::Lit::Str(ref lit),
                                ..
                            })) => {
                                if meta_path.is_ident("rename") {
                                    name = syn::Ident::new(&lit.value(), lit.span());
                                }
                            }
                            _ => panic!("Unexpected meta"),
                        }
                    }
                }
            }
        }
        name
    })
}

#[proc_macro_derive(Trans, attributes(trans))]
pub fn derive_trans(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
                if meta_path.is_ident("trans") {
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
        let (magic_write, magic_read) = match &magic {
            Some(magic) => (
                quote! {
                    <i32 as trans::Trans>::write_to(&#magic, writer)?;
                },
                quote! {
                    assert_eq!(<i32 as trans::Trans>::read_from(reader)?, #magic);
                },
            ),
            None => (quote! {}, quote! {}),
        };
        let magic_value = match &magic {
            Some(expr) => quote! { Some(#expr) },
            None => quote! { None },
        };
        let final_name = quote! {{
            let mut name = #base_name.to_owned();
            if #generics_in_name {
                #(
                    name += &trans::Schema::of::<#generic_params>().full_name().raw();
                )*
            }
            name
        }};
        match ast.data {
            syn::Data::Struct(syn::DataStruct { ref fields, .. }) => match fields {
                syn::Fields::Named(_) => {
                    let field_tys: Vec<_> = fields.iter().map(|field| &field.ty).collect();
                    let field_tys = &field_tys;
                    let field_schema_names: Vec<_> = field_schema_names(fields.iter()).collect();
                    let field_names: Vec<_> = fields
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap())
                        .collect();
                    let field_names = &field_names;
                    let mut generics = ast.generics.clone();
                    let extra_where_clauses = quote! {
                        where
                            #(#field_tys: trans::Trans,)*
                            #(#generic_params: trans::Trans,)*
                    };
                    let extra_where_clauses: syn::WhereClause =
                        syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                    generics
                        .make_where_clause()
                        .predicates
                        .extend(extra_where_clauses.predicates);
                    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans::Schema {
                                let name = #final_name;
                                trans::Schema::Struct(trans::Struct {
                                    name: trans::Name::new(name),
                                    magic: #magic_value,
                                    fields: vec![
                                        #(trans::Field {
                                            name: trans::Name::new(stringify!(#field_schema_names).to_owned()),
                                            schema: trans::Schema::of::<#field_tys>(),
                                        }),*
                                    ],
                                })
                            }
                            fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
                                #magic_write
                                #(trans::Trans::write_to(&self.#field_names, writer)?;)*
                                Ok(())
                            }
                            fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
                                #magic_read
                                Ok(Self {
                                    #(#field_names: trans::Trans::read_from(reader)?),*
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
                    if magic.is_some() {
                        panic!("Magic with newtypes not supported");
                    }
                    let inner_ty = fields.iter().next().unwrap();
                    let mut generics = ast.generics.clone();
                    let extra_where_clauses = quote! {
                        where #inner_ty: trans::Trans + 'static
                    };
                    let extra_where_clauses: syn::WhereClause =
                        syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                    generics
                        .make_where_clause()
                        .predicates
                        .extend(extra_where_clauses.predicates);
                    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans::Schema {
                                <#inner_ty as trans::Trans>::create_schema()
                            }
                            fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
                                trans::Trans::write_to(&self.0, writer)?;
                                Ok(())
                            }
                            fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
                                Ok(Self(trans::Trans::read_from(reader)?))
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
                //         #(#all_field_tys: trans::Trans + 'static,)*
                //         #(#generic_params: trans::Trans,)*
                // };
                // let extra_where_clauses: syn::WhereClause =
                //     syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                // generics
                //     .make_where_clause()
                //     .predicates
                //     .extend(extra_where_clauses.predicates);
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                let read_write_impl = {
                    let variant_writes =
                        variants.iter().enumerate().map(|(discriminant, variant)| {
                            let discriminant = discriminant as i32;
                            let variant_name = &variant.ident;
                            let field_names: Vec<_> = variant
                                .fields
                                .iter()
                                .map(|field| {
                                    field
                                        .ident
                                        .as_ref()
                                        .expect("Only named fields are supported")
                                })
                                .collect();
                            let field_names = &field_names;
                            let field_names_copy = field_names;
                            quote! {
                                #input_type::#variant_name { #(#field_names,)* } => {
                                    trans::Trans::write_to(&#discriminant, writer)?;
                                    #(trans::Trans::write_to(#field_names_copy, writer)?;)*
                                }
                            }
                        });
                    let variant_reads =
                        variants.iter().enumerate().map(|(discriminant, variant)| {
                            let discriminant = discriminant as i32;
                            let variant_name = &variant.ident;
                            let field_names = variant
                                .fields
                                .iter()
                                .map(|field| field.ident.as_ref().unwrap());
                            quote! {
                                #discriminant => #input_type::#variant_name {
                                    #(#field_names: trans::Trans::read_from(reader)?,)*
                                },
                            }
                        });
                    quote! {
                        fn write_to(&self, writer: &mut dyn std::io::Write) -> std::io::Result<()> {
                            match self {
                                #(#variant_writes)*
                            }
                            Ok(())
                        }
                        fn read_from(reader: &mut dyn std::io::Read) -> std::io::Result<Self> {
                            Ok(match <i32 as trans::Trans>::read_from(reader)? {
                                #(#variant_reads)*
                                discriminant => {
                                    return Err(std::io::Error::new(
                                        std::io::ErrorKind::Other,
                                        format!("Unexpected discriminant {:?}", discriminant)));
                                }
                            })
                        }
                    }
                };
                if variants.iter().all(|variant| {
                    if let syn::Fields::Unit = variant.fields {
                        true
                    } else {
                        false
                    }
                }) {
                    let variants = variants.iter();
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans::Schema {
                                let base_name = #final_name;
                                trans::Schema::Enum {
                                    base_name: trans::Name::new(base_name),
                                    variants: vec![#(trans::Name::new(stringify!(#variants).to_owned())),*],
                                }
                            }
                            #read_write_impl
                        }
                    };
                    expanded.into()
                } else {
                    let variants = variants.iter().map(|variant| {
                        let variant_name = &variant.ident;
                        let field_schema_names = field_schema_names(variant.fields.iter());
                        let field_tys = variant.fields.iter().map(|field| &field.ty);
                        quote! {
                            trans::Struct {
                                name: trans::Name::new(stringify!(#variant_name).to_owned()),
                                magic: None,
                                fields: vec![
                                    #(trans::Field {
                                        name: trans::Name::new(stringify!(#field_schema_names).to_owned()),
                                        schema: trans::Schema::of::<#field_tys>(),
                                    }),*
                                ],
                            }
                        }
                    });
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema() -> trans::Schema {
                                let base_name = #final_name;
                                trans::Schema::OneOf {
                                    base_name: trans::Name::new(base_name),
                                    variants: vec![#(#variants),*],
                                }
                            }
                            #read_write_impl
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
