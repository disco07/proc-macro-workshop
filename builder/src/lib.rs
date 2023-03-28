extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, FieldsNamed, Ident, Type, TypePath, Path};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    // eprintln!("{:#?}", ast);
    let b_name = format!("{}Builder", name);
    let b_ident = Ident::new(&b_name, name.span());

    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };

    let optional = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let ident = identifier(ty);
        let value = if format!("{}", ident) == "Option" {
            quote! {
                #ty
            }
        } else {
            quote! {
                std::option::Option<#ty>
            }
        };
        quote! { #name: #value }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident.clone().unwrap();
        let ty = &f.ty;
        let ident = identifier(ty);
        let value = if format!("{}", ident) == "Option" {
            quote! {
                String
            }
        } else {
            quote! {
                #ty
            }
        };
        quote! {
            pub fn #name(&mut self, #name: #value) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    });
    let builder_method = fields.iter().map(|f|{
        let name = &f.ident;
        quote! { #name: None }
    });
    let build = fields.iter().map(|f|{
        let name = &f.ident;
        let ty = &f.ty;
        let ident = identifier(ty);

        let value = if format!("{}", ident) == "Option" {
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
        }

        impl #name {
            fn builder() -> #b_ident {
                #b_ident {
                    #(#builder_method,)*
                }
            }
        }
    };
    TokenStream::from(expand)
}

fn identifier(ty: &Type) -> &Ident {
    if let Type::Path(TypePath {path: Path{segments, ..}, ..}) = ty {
        &segments.iter().next().unwrap().ident
    } else {
        unimplemented!()
    }
}