use proc_macro::TokenStream;
// use proc_macro2::{Literal, Span};
// use proc_macro2::Literal;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput}; //, Ident};

// returns Option<InnerType> if Type is Option<InnerType>,
// otherwise None.
fn is_option(field: &syn::Field) -> Option<syn::Type> {
    let syn::Type::Path(path) = &field.ty else {
        return None;
    };
    let Some(segment) = path.path.segments.iter().next() else {
        return None;
    };
    if segment.ident.to_string() != "Option" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };
    let Some(syn::GenericArgument::Type(inner_ty)) = arguments.args.iter().next() else {
        return None;
    };
    Some(inner_ty.clone())
}

fn is_builder(meta: &syn::Meta) -> Option<proc_macro2::Ident> {
    use proc_macro2::TokenTree;
    let syn::Meta::List(list) = meta else {
        return None;
    };
    let Some(segment) = list.path.segments.iter().next() else {
        return None;
    };
    if segment.ident.to_string() != "builder" {
        return None;
    }
    // let mut literal: Option<Literal> = None;
    let mut it = list.tokens.clone().into_iter();
    let Some(TokenTree::Ident(ident)) = it.next() else {
        return None;
    };
    if ident.to_string() != "each" {
        return None;
    }
    let Some(TokenTree::Punct(punct)) = it.next() else {
        return None;
    };
    if punct.to_string() != "=" {
        return None;
    }
    let Some(TokenTree::Literal(literal)) = it.next() else {
        return None;
    };
    if it.next().is_some() {
        // if let Some(token) = it.next() {//it.next().is_some() {
        // compile_error!("Only one `each = \"value\"` allowed!");
        return None;
    }

    let span = literal.span();
    let name = literal.to_string();
    let name = name.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
    let ident = proc_macro2::Ident::new(name, span);
    Some(ident)
}

fn is_vector(field: &syn::Field) -> Option<(syn::Type, proc_macro2::Ident)> {
    // check if the field is annotated with `#[builder(each = "<arg>")]`
    let eaches: Vec<_> = field
        .attrs
        .iter()
        .filter_map(|attr| is_builder(&attr.meta))
        .collect();

    let mut eaches_it = eaches.into_iter();
    let Some(each) = eaches_it.next() else {
        return None;
    };
    if eaches_it.next().is_some() {
        // compile_error!("Only one `each = \"value\"` allowed!");
        return None;
    }

    // unwrap the type in the Vec
    let syn::Type::Path(path) = &field.ty else {
        return None;
    };
    let Some(segment) = path.path.segments.iter().next() else {
        return None;
    };
    if segment.ident.to_string() != "Vec" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return None;
    };
    let Some(syn::GenericArgument::Type(inner_ty)) = arguments.args.iter().next() else {
        return None;
    };
    Some((inner_ty.clone(), each))
}

enum Modifier {
    Option(syn::Type),
    Vector((syn::Type, proc_macro2::Ident)),
    None,
}

fn check_no_unexpected(fields: &syn::FieldsNamed) -> Result<(), syn::Error> {
    for field in fields.named.iter() {
        // eprintln!("FIELD: {field:#?}");
        for attr in field.attrs.iter() {
            // eprintln!("ATTR: {attr:#?}");

            // Cover case #[builder] with no each
            if let syn::Meta::Path(path) = &attr.meta {
                let Some(segment) = path.segments.iter().next() else {
                    continue;
                };
                eprintln!("3");
                if segment.ident != "builder" {
                    continue;
                }
                return Err(syn::Error::new(
                    segment.ident.span(),
                    "Missing token `each`",
                ));
            }

            // Cover case #[builder(...)] with first token not each
            let syn::Meta::List(list) = &attr.meta else {
                continue;
            };
            let Some(segment) = list.path.segments.iter().next() else {
                continue;
            };
            if segment.ident != "builder" {
                continue;
            }

            let tt = list.tokens.clone().into_iter().next();
            use syn::spanned::Spanned;
            match tt {
                // Some(proc_macro2::TokenTree::Ident(ident)) => {
                //     if ident != "each" {
                //         return Err(syn::Error::new(ident.span(), "Incorrect token, expected `each`"));
                //     } else {
                //         // TODO: expect this is followed by Punct(=), then Literal(name),
                //         // then nothing further.
                //         continue;
                //     }
                // }
                // Some(tt) => return Err(syn::Error::new(tt.span(), "Unexpected tokens")),
                // Some(tt) => return Err(syn::Error::new(attr.meta.span(), "expected `builder(each = \"...\")`")),
                Some(tt) => {
                    if let proc_macro2::TokenTree::Ident(ident) = &tt {
                        if ident == "each" {
                            continue;
                        }
                    }

                    // NB: On stable 1.69.0, span1 is only
                    // #[builder(eac = "arg")]
                    //   ^^^^^^^
                    // while span2 is
                    // #[builder(eac = "arg")]
                    //          ^^^^^^^^^^^^^
                    // And span1.join(span2).is_none() (cf. docs)
                    //
                    // On 1.72.0-nightly, span1 itself is
                    // #[builder(eac = "arg")]
                    //   ^^^^^^^^^^^^^^^^^^^^
                    // which test 08 expects.
                    //
                    // let span1 = attr.meta.span();
                    // let span2 = list.delimiter.span().join();
                    // let span = span1.resolved_at(span2);

                    // works on nightly
                    let span = attr.meta.span();
                    return Err(syn::Error::new(span, "expected `builder(each = \"...\")`"));
                }
                // This doesn't really seem to occur!
                None => {
                    return Err(syn::Error::new(
                        segment.ident.span(),
                        "Missing token `each`",
                    ))
                }
            }
        }
    }
    Ok(())
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    // eprintln!("INPUT: {:#?}", input);

    let ident = input.ident;

    let builder = format_ident!("{ident}Builder");

    let syn::Data::Struct(data) = input.data else {
        panic!("not a struct");
    };
    let syn::Fields::Named(fields) = data.fields else {
        panic!("fields not named");
    };

    // eprintln!("{fields:#?}");

    if let Err(error) = check_no_unexpected(&fields) {
        return error.into_compile_error().into();
    }

    let fields = fields.named;

    let init: Vec<_> = fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            quote! { #ident: core::default::Default::default(), }
        })
        .collect();

    let options: Vec<_> = fields.iter().map(|field| is_option(field)).collect();
    // eprintln!("{options:#?}");

    let vectors: Vec<_> = fields.iter().map(|field| is_vector(field)).collect();
    // eprintln!("{vectors:#?}");

    let modifiers: Vec<_> = options
        .into_iter()
        .zip(vectors.into_iter())
        .map(
            |(maybe_option, maybe_vector)| match (maybe_option, maybe_vector) {
                (Some(_), Some(_)) => unreachable!(),
                (Some(option), _) => Modifier::Option(option),
                (_, Some(vector)) => Modifier::Vector(vector),
                _ => Modifier::None,
            },
        )
        .collect();

    let builder_fields: Vec<_> = fields
        .iter()
        .zip(modifiers.iter())
        .map(|(field, modifier)| {
            let ident = &field.ident;
            let ty = &field.ty;
            match modifier {
                Modifier::None => quote! { #ident: core::option::Option<#ty>, },
                _ => quote! { #ident: #ty, },
            }
        })
        .collect();

    let unwrappers: Vec<_> = fields
        .iter()
        .zip(modifiers.iter())
        .map(|(field, modifier)| {
            let ident = field.ident.as_ref().expect("fields are named");
            let error = format!("{ident} not set");
            match modifier {
                Modifier::None => quote! {
                    #ident: self.#ident.clone().ok_or_else(|| #error.to_string())?,
                },
                _ => quote! {
                    #ident: self.#ident.clone(),
                },
            }
        })
        .collect();

    let builder_setters: Vec<_> = fields
        .iter()
        .zip(modifiers.iter())
        .map(|(field, modifier)| {
            let ident = &field.ident;
            let ty = &field.ty;
            match modifier {
                Modifier::Option(inner_ty) => quote! {
                    fn #ident(&mut self, #ident: #inner_ty) -> &mut Self {
                        self.#ident = core::option::Option::Some(#ident);
                        self
                    }
                },
                Modifier::Vector((inner_ty, literal)) => quote! {
                    fn #literal(&mut self, #literal: #inner_ty) -> &mut Self {
                        self.#ident.push(#literal);
                        self
                    }
                },
                Modifier::None => quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = core::option::Option::Some(#ident);
                        self
                    }
                },
            }
        })
        .collect();

    quote! {
        #[derive(Debug)]
        pub struct #builder {
            #(#builder_fields)*
        }

        impl #ident {
            fn builder() -> #builder {
                #builder {
                    #(#init)*
                }
            }
        }

        impl #builder {
            #(#builder_setters)*

            pub fn build(&mut self) -> core::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                core::result::Result::Ok(#ident {
                    #(#unwrappers)*
                })
            }
        }
    }.into()
}
