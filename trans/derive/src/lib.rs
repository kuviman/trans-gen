#![recursion_limit = "256"]
extern crate proc_macro;

use quote::quote;

use proc_macro2::TokenStream;

fn field_schema_name(field: &syn::Field) -> syn::Ident {
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
}

fn version_req(attrs: &[syn::Attribute]) -> Option<syn::LitStr> {
    let mut version_req = None;
    for attr in attrs {
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
                            if meta_path.is_ident("version") {
                                version_req = Some(lit.clone());
                            }
                        }
                        _ => panic!("Unexpected meta"),
                    }
                }
            }
        }
    }
    version_req
}

fn default_field_value(field: &syn::Field) -> Option<syn::Expr> {
    let mut default: Option<syn::Expr> = None;
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
                            if meta_path.is_ident("default") {
                                default = Some(
                                    syn::parse_str(&lit.value())
                                        .expect("Failed to parse default_value"),
                                );
                            }
                        }
                        _ => panic!("Unexpected meta"),
                    }
                }
            }
        }
    }
    default
}

fn add_version_req(
    version_req: Option<syn::LitStr>,
    ast: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    if let Some(version_req) = version_req {
        quote! {
            if trans::VersionReq::parse(#version_req).unwrap().matches(version) {
                #ast
            }
        }
    } else {
        ast
    }
}

fn get_documentation(attrs: &[syn::Attribute]) -> proc_macro2::TokenStream {
    use std::collections::HashMap;
    let mut language_docs: HashMap<String, String> = HashMap::new();
    for attr in attrs {
        if let Ok(syn::Meta::NameValue(syn::MetaNameValue {
            path,
            lit: syn::Lit::Str(lit),
            ..
        })) = attr.parse_meta()
        {
            let doc = if path.is_ident("doc") {
                Some(("en".to_owned(), lit.value()))
            } else if path.is_ident("trans_doc") {
                let text = lit.value();
                let colon_pos = text
                    .find(':')
                    .expect("trans_doc should be in format \"LANG:TEXT\"");
                Some((
                    text[..colon_pos].to_owned(),
                    text[colon_pos + 1..].to_owned(),
                ))
            } else {
                None
            };
            if let Some((lang, text)) = doc {
                let lang = lang.trim();
                let text = text.trim();
                let current_text = language_docs.entry(lang.to_owned()).or_default();
                if !current_text.is_empty() {
                    current_text.push(' ');
                }
                current_text.push_str(text);
            }
        }
    }
    let language_docs = language_docs.into_iter().map(|(language, text)| {
        // let language = syn::LitStr::new(&language, proc_macro2::Span::call_site());
        // let text = syn::LitStr::new(&text, proc_macro2::Span::call_site());
        quote! {
            trans::LanguageDocumentation {
                language: #language.to_owned(),
                text: #text.to_owned(),
            }
        }
    });
    quote! {
        trans::Documentation {
            languages: {
                let mut result = Vec::new();
                #(result.push(#language_docs);)*
                result
            },
        }
    }
}

fn field_write(
    field: &syn::Field,
    input_type: &syn::Ident,
    ty_generics: &syn::TypeGenerics,
    variant_name: Option<&syn::Ident>,
) -> proc_macro2::TokenStream {
    let field_name = field
        .ident
        .as_ref()
        .expect("Only named fields are supported");
    let error_context = match variant_name {
        Some(variant_name) => quote! {
            trans::error_format::write_variant_field::<#input_type #ty_generics>(stringify!(#variant_name), stringify!(#field_name))
        },
        None => quote! {
            trans::error_format::write_field::<#input_type #ty_generics>(stringify!(#field_name))
        },
    };
    add_version_req(
        version_req(&field.attrs),
        quote! {
            trans::add_error_context(trans::Trans::write_to(#field_name, writer, version), #error_context)?;
        },
    )
}

fn field_read(
    field: &syn::Field,
    input_type: &syn::Ident,
    ty_generics: &syn::TypeGenerics,
    variant_name: Option<&syn::Ident>,
) -> proc_macro2::TokenStream {
    let field_name = field
        .ident
        .as_ref()
        .expect("Only named fields are supported");
    let error_context = match variant_name {
        Some(variant_name) => quote! {
            trans::error_format::read_variant_field::<#input_type #ty_generics>(stringify!(#variant_name), stringify!(#field_name))
        },
        None => quote! {
            trans::error_format::read_field::<#input_type #ty_generics>(stringify!(#field_name))
        },
    };
    let mut field_read = quote! {
        trans::add_error_context(trans::Trans::read_from(reader, version), #error_context)?
    };
    if let Some(version_req) = version_req(&field.attrs) {
        let field_default = default_field_value(field)
            .expect("Fields with version requirements need a default value");
        field_read = quote! {
            if trans::VersionReq::parse(#version_req).unwrap().matches(version) {
                #field_read
            } else {
                #field_default
            }
        };
    }
    quote! {
        #field_name: #field_read
    }
}

#[proc_macro_derive(Trans, attributes(trans, trans_doc))]
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
        let mut generics_in_name = true;
        let mut namespace: Option<syn::Path> = None;
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
                                } else if meta_path.is_ident("namespace") {
                                    namespace = Some(syn::parse_str(&lit.value()).unwrap());
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
        let final_name = quote! {{
            let mut name = #base_name.to_owned();
            if #generics_in_name {
                #(
                    name += &trans::Schema::of::<#generic_params>(version).full_name().raw();
                )*
            }
            name
        }};
        let namespace = match namespace {
            Some(path) => {
                let parts = path.segments.iter().map(|segment| {
                    let ident = &segment.ident;
                    quote! {
                        trans::Name::new(stringify!(#ident).to_owned())
                    }
                });
                quote! {
                    trans::Namespace {
                        parts: vec![#(#parts),*],
                    }
                }
            }
            None => quote! {
                trans::Namespace { parts: Vec::new() }
            },
        };
        match ast.data {
            syn::Data::Struct(syn::DataStruct { ref fields, .. }) => match fields {
                syn::Fields::Named(_) => {
                    let field_tys: Vec<_> = fields.iter().map(|field| &field.ty).collect();
                    let field_tys = &field_tys;
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
                    let schema_fields = fields.iter().map(|field| {
                        let documentation = get_documentation(&field.attrs);
                        let schema_name = field_schema_name(field);
                        let ty = &field.ty;
                        add_version_req(
                            version_req(&field.attrs),
                            quote! {
                                fields.push(trans::Field {
                                    documentation: #documentation,
                                    name: trans::Name::new(stringify!(#schema_name).to_owned()),
                                    schema: trans::Schema::of::<#ty>(version),
                                });
                            },
                        )
                    });
                    let field_names = fields.iter().map(|field| {
                        field
                            .ident
                            .as_ref()
                            .expect("Only named fields are supported")
                    });
                    let field_reads = fields
                        .iter()
                        .map(|field| field_read(field, input_type, &ty_generics, None));
                    let field_writes = fields
                        .iter()
                        .map(|field| field_write(field, input_type, &ty_generics, None));
                    let documentation = get_documentation(&ast.attrs);
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema(version: &trans::Version) -> trans::Schema {
                                let name = #final_name;
                                trans::Schema::Struct {
                                    namespace: #namespace,
                                    definition: trans::Struct {
                                        documentation: #documentation,
                                        name: trans::Name::new(name),
                                        fields: {
                                            let mut fields = Vec::new();
                                            #(#schema_fields)*
                                            fields
                                        },
                                    },
                                }
                            }
                            fn write_to(&self, writer: &mut dyn std::io::Write, version: &trans::Version) -> std::io::Result<()> {
                                let Self { #(#field_names,)* } = self;
                                #(#field_writes)*
                                Ok(())
                            }
                            fn read_from(reader: &mut dyn std::io::Read, version: &trans::Version) -> std::io::Result<Self> {
                                Ok(Self {
                                    #(#field_reads,)*
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
                            fn create_schema(version: &trans::Version) -> trans::Schema {
                                <#inner_ty as trans::Trans>::create_schema(version)
                            }
                            fn write_to(&self, writer: &mut dyn std::io::Write, version: &trans::Version) -> std::io::Result<()> {
                                trans::Trans::write_to(&self.0, writer, version)?;
                                Ok(())
                            }
                            fn read_from(reader: &mut dyn std::io::Read, version: &trans::Version) -> std::io::Result<Self> {
                                Ok(Self(trans::Trans::read_from(reader, version)?))
                            }
                        }
                    };
                    expanded.into()
                }
                syn::Fields::Unit => panic!("Unit structs not supported"),
            },
            syn::Data::Enum(syn::DataEnum { ref variants, .. }) => {
                let generics = ast.generics.clone();
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                let read_write_impl = {
                    let variant_writes = variants.iter().enumerate().map(|(tag, variant)| {
                        let tag = tag as i32;
                        let variant_name = &variant.ident;
                        let field_names = variant
                            .fields
                            .iter()
                            .map(|field| {
                                field
                                    .ident
                                    .as_ref()
                                    .expect("Only named fields are supported")
                            });
                        let field_writes = variant.fields.iter().map(|field| field_write(field, input_type, &ty_generics, Some(variant_name)));
                        quote! {
                            #input_type::#variant_name { #(#field_names,)* } => {
                                trans::add_error_context(
                                    trans::Trans::write_to(&#tag, writer, version),
                                    trans::error_format::write_tag::<#input_type #ty_generics>(stringify!(#variant_name)),
                                )?;
                                #(#field_writes)*
                            }
                        }
                    });
                    let variant_reads = variants.iter().enumerate().map(|(tag, variant)| {
                        let tag = tag as i32;
                        let variant_name = &variant.ident;
                        let field_reads = variant.fields.iter().map(|field| {
                            field_read(field, input_type, &ty_generics, Some(&variant.ident))
                        });
                        quote! {
                            #tag => #input_type::#variant_name {
                                #(#field_reads,)*
                            },
                        }
                    });
                    quote! {
                        fn write_to(&self, writer: &mut dyn std::io::Write, version: &trans::Version) -> std::io::Result<()> {
                            match self {
                                #(#variant_writes)*
                            }
                            Ok(())
                        }
                        fn read_from(reader: &mut dyn std::io::Read, version: &trans::Version) -> std::io::Result<Self> {
                            let tag = trans::add_error_context(
                                <i32 as trans::Trans>::read_from(reader, version),
                                trans::error_format::read_tag::<#input_type #ty_generics>(),
                            )?;
                            Ok(match tag {
                                #(#variant_reads)*
                                _ => {
                                    return Err(std::io::Error::new(
                                        std::io::ErrorKind::Other,
                                        trans::error_format::unexpected_tag::<#input_type #ty_generics>(tag)));
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
                    let variants = variants.iter().map(|variant| {
                        let name = &variant.ident;
                        let documentation = get_documentation(&variant.attrs);
                        add_version_req(
                            version_req(&variant.attrs),
                            quote! {
                                variants.push(trans::EnumVariant {
                                    name: trans::Name::new(stringify!(#name).to_owned()),
                                    documentation: #documentation,
                                });
                            },
                        )
                    });
                    let documentation = get_documentation(&ast.attrs);
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema(version: &trans::Version) -> trans::Schema {
                                let base_name = #final_name;
                                trans::Schema::Enum {
                                    namespace: #namespace,
                                    documentation: #documentation,
                                    base_name: trans::Name::new(base_name),
                                    variants: {
                                        let mut variants = Vec::new();
                                        #(#variants)*
                                        variants
                                    },
                                }
                            }
                            #read_write_impl
                        }
                    };
                    expanded.into()
                } else {
                    let variants = variants.iter().map(|variant| {
                        let documentation = get_documentation(&variant.attrs);
                        let variant_name = &variant.ident;
                        let schema_fields = variant.fields.iter().map(|field| {
                            let documentation = get_documentation(&field.attrs);
                            let schema_name = field_schema_name(field);
                            let ty = &field.ty;
                            add_version_req(
                                version_req(&field.attrs),
                                quote! {
                                    fields.push(trans::Field {
                                        documentation: #documentation,
                                        name: trans::Name::new(stringify!(#schema_name).to_owned()),
                                        schema: trans::Schema::of::<#ty>(version),
                                    });
                                },
                            )
                        });
                        add_version_req(
                            version_req(&variant.attrs),
                            quote! {
                                variants.push(trans::Struct {
                                    documentation: #documentation,
                                    name: trans::Name::new(stringify!(#variant_name).to_owned()),
                                    fields: {
                                        let mut fields = Vec::new();
                                        #(#schema_fields)*
                                        fields
                                    },
                                });
                            },
                        )
                    });
                    let documentation = get_documentation(&ast.attrs);
                    let expanded = quote! {
                        impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                            fn create_schema(version: &trans::Version) -> trans::Schema {
                                let base_name = #final_name;
                                trans::Schema::OneOf {
                                    namespace: #namespace,
                                    documentation: #documentation,
                                    base_name: trans::Name::new(base_name),
                                    variants: {
                                        let mut variants = Vec::new();
                                        #(#variants)*
                                        variants
                                    },
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
