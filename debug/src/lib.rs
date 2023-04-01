use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed, Meta, MetaNameValue, parse_macro_input, Path, PathSegment};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);
    let name = ast.ident;
    let name_string = format!("{}", name);
    let fields = if let Data::Struct(DataStruct{fields: Fields::Named(FieldsNamed {named, ..}), ..}) = ast.data {
        Some(named)
    } else {
        None
    };
    assert!(fields.is_some());

    let binding = fields.unwrap();
    let method = binding.iter().map(|f| {
        let name = &f.ident.clone().unwrap();
        let name_string = format!("{}", name);
        let _ty = &f.ty;

        quote! {
            field(#name_string, &format_args!("{}", self.#name))
        }
    });

    let expanded = quote!{
        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_string).
                #(#method.)*
                finish()
            }
        }
    };

    TokenStream::from(expanded)
}

// fn get_attrs<'a>(field: &'a Field, attrs: &str) -> Option<&'a Attribute> {
//     while let Some(attr) = field.attrs.first() {
//         if let Meta::NameValue(MetaNameValue {path: Path {segments, ..}, ..}) = &attr.meta {
//             if let Some(PathSegment{ident, .. }) = segments.first() {
//                 if ident != attrs {
//                     return None;
//                 }
//
//                 return Some(attr)
//             }
//         }
//     }
//     None
// }
