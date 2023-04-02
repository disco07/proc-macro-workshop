use proc_macro::{TokenStream};
use quote::quote;
use syn::{Attribute, Ident, Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Fields, FieldsNamed, Lit, Meta, MetaNameValue, parse_macro_input, Path, PathSegment, Generics, GenericParam, parse_quote};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    eprintln!("{:#?}", ast);
    let name = ast.ident;
    let name_string = format!("{}", name);
    let fields = if let Data::Struct(DataStruct{fields: Fields::Named(FieldsNamed {named, ..}), ..}) = ast.data {
        Some(named)
    } else {
        None
    };
    assert!(fields.is_some());

    // Add a bound `T: HeapSize` to every type parameter T.
    let generics = add_trait_bounds(ast.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let binding = fields.unwrap();
    let method = binding.iter().map(|f| {
        let name = &f.ident.clone().unwrap();
        let _ty = &f.ty;

        create_field(f, name)
    });

    let expanded = quote!{
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_string).
                #(#method.)*
                finish()
            }
        }
    };

    TokenStream::from(expanded)
}

fn get_attrs<'a>(field: &'a Field, attrs: &str) -> Option<&'a Attribute> {
    while let Some(attr) = field.attrs.first() {
        if let Meta::NameValue(MetaNameValue {path: Path {segments, ..}, ..}) = &attr.meta {
            if let Some(PathSegment{ident, .. }) = segments.first() {
                if ident != attrs {
                    return None;
                }

                return Some(attr)
            }
        }
    }
    None
}

fn create_field(field: &Field, ident: &Ident) -> proc_macro2::TokenStream {
    let attr = get_attrs(field, "debug");
    let name_string = format!("{}", ident);
    if let Some(Attribute{meta, ..}) = attr {
        if let Meta::NameValue(MetaNameValue { value: Expr::Lit(ExprLit { lit, .. }), .. }) = meta {
            match lit {
                Lit::Str(s) => {
                    let t = s.token();
                    return quote! {
                        field(#name_string, &format_args!(#t, self.#ident))
                    }
                }
                _ => unimplemented!()
            }
        }
    }
    quote! {
        field(#name_string, &self.#ident)
    }
}

// Add a bound `T: HeapSize` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}
