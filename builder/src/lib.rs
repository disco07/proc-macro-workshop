extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput,
    Field, Fields, FieldsNamed, GenericArgument, Ident, Lit, Meta, MetaList, Path, PathArguments,
    PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let b_name = format!("{}Builder", name);
    let b_ident = Ident::new(&b_name, name.span());

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        Some(named)
    } else {
        None
    };
    assert!(fields.is_some());

    let optional = fields.unwrap().iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if inner_type(ty, "Option").is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.unwrap().iter().map(|f| {
        let name = &f.ident.clone().unwrap();
        let mut ty = &f.ty;
        if let Some(inner_ty) = inner_type(ty, "Option") {
            ty = inner_ty;
        }
        let q = create_method(f, name);

        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
            #q
        }
    });

    let builder_method = fields.unwrap().iter().map(|f| {
        let name = &f.ident;
        quote! { #name: None }
    });
    let build = fields.unwrap().iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        let value = if inner_type(ty, "Option").is_some() {
            quote! {
                self.#name.clone()
            }
        } else {
            quote! {
                self.#name.clone().unwrap_or_default()
            }
        };
        quote! { #name: #value }
    });

    let expand = quote! {
        use std::error::Error;

        #[derive(Clone)]
        pub struct #b_ident {
            #(#optional,)*
        }

        impl #b_ident {
            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn Error>> {
                Ok(#name {
                    #(#build,)*
                })
            }
            #(#methods)*
        }

        impl #name {
            fn builder() -> #b_ident {
                #b_ident {
                    #(#builder_method,)*
                }
            }
        }
    };
    expand.into()
}

fn get_attrs<'a>(field: &'a Field, attribute: &str) -> Option<&'a Attribute> {
    while let Some(attr) = field.attrs.first() {
        if let Meta::List(MetaList {
            path: Path { segments, .. },
            ..
        }) = &attr.meta
        {
            if let Some(PathSegment { ident, .. }) = segments.iter().next() {
                if ident != attribute {
                    return None;
                }
                return Some(attr);
            }
        }
    }

    None
}

fn create_method(field: &Field, i: &Ident) -> proc_macro2::TokenStream {
    if let Some(Attribute { meta, .. }) = get_attrs(field, "builder") {
        if let Meta::List(MetaList { tokens, .. }) = meta {
            if let Some(TokenTree::Ident(i)) = tokens.clone().into_iter().next() {
                if i != "each" {
                    return syn::Error::new_spanned(&meta, "expected `builder(each = \"...\")`")
                        .to_compile_error();
                }
            }
            match tokens.clone().into_iter().nth(1).unwrap() {
                TokenTree::Punct(p) => assert_eq!(p.as_char(), '='),
                tt => panic!("expected '=' found {}", tt),
            }
            let literal = match tokens.clone().into_iter().nth(2).unwrap() {
                TokenTree::Literal(l) => Lit::new(l),
                tt => panic!("found {}", tt),
            };

            match literal {
                Lit::Str(s) => {
                    let ident = Ident::new(&s.value(), field.span());
                    if *i != ident {
                        let name = &field.ident.clone().unwrap();
                        let ty = inner_type(&field.ty, "Vec").unwrap();
                        return quote! {
                            pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                                if let std::option::Option::Some(ref mut value) = self.#name {
                                    value.push(#ident);
                                } else {
                                    self.#name = std::option::Option::Some(vec![#ident]);
                                }
                                self
                            }
                        };
                    }
                }
                _ => unimplemented!(),
            }
        }
    }

    proc_macro2::TokenStream::new()
}

fn inner_type<'a>(ty: &'a Type, wrapper: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if let Some(PathSegment { ident, arguments }) = segments.iter().next() {
            if ident != wrapper {
                return None;
            }
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                arguments
            {
                if let Some(GenericArgument::Type(ref inner_type)) = args.iter().next() {
                    return Some(inner_type);
                }
            }
        }
    }

    None
}
