use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, parse_quote, AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Error, Expr, ExprLit, Field, Fields, FieldsNamed, GenericArgument, GenericParam, Generics, Ident, Lit, Meta, MetaList, MetaNameValue, Path, PathArguments, PathSegment, Type, TypePath, WherePredicate};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // eprintln!("{:#?}", ast);
    let name = ast.ident;
    let fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named, .. }),
        ..
    }) = ast.data
    {
        Some(named)
    } else {
        None
    };
    assert!(fields.is_some());
    let attrs = ast.attrs;
    let bound_res = get_bound_attrs(attrs);
    if let Err(err) = bound_res {
        return err.to_compile_error().into();
    }
    let bound_attr = bound_res.unwrap();

    let binding = fields.unwrap();

    let generic_types = ast
        .generics
        .type_params()
        .map(|t| &t.ident)
        .collect::<Vec<_>>();

    let phantom_ident = binding
        .iter()
        .filter_map(|field| {
            let ty = &field.ty;
            let inner_ty = ty_inner(ty, "PhantomData")?;
            if let Type::Path(type_path) = inner_ty {
                let type_ident = &type_path.path.segments.first()?.ident;
                if generic_types.contains(&type_ident) {
                    return Some(type_ident);
                }
            }
            None
        })
        .collect::<Vec<_>>();

    // List of associated types e.g `T::Value`
    let associated_types = binding
        .iter()
        .filter_map(|f| get_associated_types(&f.ty, &generic_types))
        .collect::<Vec<_>>();

    // Add a bound `T: std::fmt::Debug` to every type parameter T.
    let generics = add_trait_bounds(ast.generics, phantom_ident, associated_types, bound_attr);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let field = binding.iter().map(|f| {
        let name = &f.ident.clone().unwrap();
        let _ty = &f.ty;

        create_field(f, name)
    });

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!(#name)).
                #(#field.)*
                finish()
            }
        }
    };
    TokenStream::from(expanded)
}

fn get_attrs<'a>(field: &'a Field, attrs: &str) -> Option<&'a Attribute> {
    while let Some(attr) = field.attrs.first() {
        if let Meta::NameValue(MetaNameValue {
            path: Path { segments, .. },
            ..
        }) = &attr.meta
        {
            if let Some(PathSegment { ident, .. }) = segments.first() {
                if ident != attrs {
                    return None;
                }

                return Some(attr);
            }
        }
    }
    None
}

fn error_attr<T: ToTokens>(tokens: &T) -> Error {
    Error::new_spanned(&tokens, "expected `builder(bound = \"...\")`")
}

fn get_bound_attrs(attrs: Vec<Attribute>) -> Result<Option<syn::WherePredicate>, Error> {
    for attr in attrs {
            if let Meta::List(MetaList { ref tokens, ref path, .. }) = attr.meta {
                if let Some(PathSegment { ident, .. }) = path.segments.first() {
                    if ident != "debug" {
                        return Err(error_attr(ident));
                    }
                }

                if let Some(TokenTree::Ident(i)) = tokens.clone().into_iter().nth(0) {
                    if i != "bound" {
                        return Err(error_attr(&attr.meta));
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
                        let value = s.value();
                        return match syn::parse_str::<syn::WherePredicate>(&value) {
                            Ok(where_predicate) => Ok(Some(where_predicate)),
                            Err(e) => Err(Error::new_spanned(s, e)),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
    }

    Ok(None)
}

fn create_field(field: &Field, ident: &Ident) -> proc_macro2::TokenStream {
    let attr = get_attrs(field, "debug");

    if let Some(Attribute { meta, .. }) = attr {
        if let Meta::NameValue(MetaNameValue {
            value: Expr::Lit(ExprLit { lit, .. }),
            ..
        }) = meta
        {
            match lit {
                Lit::Str(s) => {
                    let t = s.token();
                    return quote! {
                        field(stringify!(#ident), &format_args!(#t, self.#ident))
                    };
                }
                _ => unimplemented!(),
            }
        }
    }

    quote! {
        field(stringify!(#ident), &self.#ident)
    }
}

fn get_associated_types<'a>(ty: &'a Type, generic_types: &[&Ident]) -> Option<&'a TypePath> {
    if let Some(inner_ty) = ty_inner(ty, "Vec") {
        return get_associated_types(inner_ty, generic_types);
    }

    if let Type::Path(type_path) = ty {
        if type_path.path.segments.len() < 2 {
            return None;
        }

        let type_ident = &type_path.path.segments[0].ident;
        if generic_types.contains(&type_ident) {
            return Some(type_path);
        }
    }

    None
}

// Add a bound `T:Debug` to every type parameter T.
fn add_trait_bounds(
    mut generics: Generics,
    phantom_ident: Vec<&Ident>,
    associated_types: Vec<&TypePath>,
    bound_attr: Option<WherePredicate>,
) -> Generics {
    if let Some(where_predicate) = bound_attr {
        generics
            .make_where_clause()
            .predicates
            .push(where_predicate)
    } else {
        let associated_types_ident = associated_types
            .iter()
            .map(|ty| &ty.path.segments[0].ident)
            .collect::<Vec<_>>();
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                // Do not add bound for Phantom types
                if phantom_ident.contains(&&type_param.ident) {
                    continue;
                }
                // Do not add bound for associated types
                if associated_types_ident.contains(&&type_param.ident) {
                    continue;
                }
                type_param.bounds.push(parse_quote!(::std::fmt::Debug));
            }
        }

        // Add trait Debug to clause where
        for associated_type in associated_types {
            generics
                .make_where_clause()
                .predicates
                .push(syn::parse2(quote! { #associated_type: std::fmt::Debug }).unwrap());
        }
    }
    generics
}

fn ty_inner<'a>(ty: &'a Type, wrapper: &str) -> Option<&'a Type> {
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
