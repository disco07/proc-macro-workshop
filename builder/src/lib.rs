use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Ident, parse_macro_input};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let b_name = format!("{}Builder", name);
    let b_ident = Ident::new(&b_name, name.span());

    let expand = quote!{
        use std::error::Error;

        #[derive(Clone)]
        pub struct #b_ident {
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>,
        }

        impl #b_ident {
            pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
                Ok(Command {
                    executable: self.executable.clone().unwrap_or_default(),
                    args: self.args.clone().unwrap_or_default(),
                    env: self.env.clone().unwrap_or_default(),
                    current_dir: self.current_dir.clone().unwrap_or_default(),
                })
            }
            fn executable(&mut self, str: String) -> &mut Self {
                self.executable = Some(str);
                self
            }

            fn args(&mut self, vec: Vec<String>) -> &mut Self {
                self.args = Some(vec);
                self
            }

            fn env(&mut self, vec: Vec<String>) -> &mut Self {
                self.env = Some(vec);
                self
            }

            fn current_dir(&mut self, str: String) -> &mut Self {
                self.current_dir = Some(str);
                self
            }
        }

        impl #name {
            fn builder() -> #b_ident {
                #b_ident {
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }
    };
    TokenStream::from(expand)
}

/*

02-create-builder.rs

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;

    let expand = quote!{
        impl #name {
            fn builder() {}
        }
    };
    TokenStream::from(expand)
}
*/

/*

01-parse.rs

use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("{:?}", ast);

    TokenStream::new()
}

*/