extern crate proc_macro;

use proc_macro::{TokenStream};
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Fields, FieldsNamed, GenericArgument, Ident, Path, PathArguments, PathSegment, Type, TypePath, Attribute, Field, Meta, MetaList, Lit};
use syn::spanned::Spanned;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    eprintln!("{:#?}", ast);
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
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });

    let extract_methods = fields.unwrap().iter().map(|f| {
        create_method(f)
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
            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                Ok(#name {
                    #(#build,)*
                })
            }
            #(#methods)*

            #(#extract_methods)*
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
    while let Some(attr) = field.attrs.iter().next() {
        if let Meta::List(MetaList {path: Path{segments,..}, .. }) = &attr.meta {
            if let Some(PathSegment{ident, ..}) = segments.iter().next() {
                if ident != attribute {
                    return None;
                }
                return Some(attr)
            }
        }
    }

    None
}

fn create_method(field: &Field) -> proc_macro2::TokenStream {
    if let Some(Attribute {meta: Meta::List(MetaList { tokens, .. }), ..}) = get_attrs(field, "builder") {
        match tokens.clone().into_iter().nth(0).unwrap() {
            TokenTree::Ident(i) => assert_eq!(i, "each"),
            tt=> panic!("expected each found {}", tt)
        }
        match tokens.clone().into_iter().nth(1).unwrap() {
            TokenTree::Punct(p) => assert_eq!(p.as_char(), '='),
            tt=> panic!("expected '=' found {}", tt)
        }
        let literal = match tokens.clone().into_iter().nth(2).unwrap() {
            TokenTree::Literal(l) => Lit::new(l),
            tt => panic!("found {}", tt)
        };

        match literal {
            Lit::Str(s) => {
                let ident = Ident::new(&s.value(), field.span());
                let ty = &field.ty;
                return quote! {
                    pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self
                    }
                };
            }
            _ => unimplemented!()
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
