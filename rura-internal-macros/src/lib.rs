use proc_macro::TokenStream as TStream;
use proc_macro2::{Ident, Literal, TokenStream};

fn generate_marked_tuple<'a, I>(x: I, ready_cnt: usize) -> TokenStream
where
    I: Iterator<Item = &'a Ident>,
{
    let iter = x.enumerate().map(|(idx, x)| {
        if idx < ready_cnt {
            quote::quote! { Ready<#x> }
        } else {
            quote::quote! { Hole<#x> }
        }
    });
    quote::quote! { (#(#iter,)*) }
}

fn generate_pending_tuple<'a, I>(x: I, ready_cnt: usize) -> TokenStream
where
    I: Iterator<Item = &'a Ident>,
{
    let iter = x.skip(ready_cnt).map(|x| {
        quote::quote! { #x }
    });
    quote::quote! { (#(#iter,)*) }
}

fn generate_transmute_expression(total_cnt: usize, ready_cnt: usize) -> TokenStream {
    let iter = (0..total_cnt).map(|idx| {
        let literal = Literal::usize_unsuffixed(idx);
        if idx < ready_cnt {
            quote::quote! { self.#literal.0 }
        } else {
            quote::quote! { self.#literal.0.assume_init() }
        }
    });
    quote::quote! { (#(#iter,)*) }
}

fn generate_from_impl(total_cnt: usize) -> TokenStream {
    let full_ident = generate_type_idents(total_cnt).collect::<Vec<_>>();
    let uninit = (0..total_cnt).map(|_| {
        quote::quote! { Hole(MaybeUninit::uninit()) }
    });

    quote::quote! {
        impl<#(#full_ident : Clone + 'static,)*R : 'static, F> From<F> for Closure<(#(#full_ident,)*), R>
            where F : FnOnce((#(#full_ident,)*)) -> R + Clone + 'static
        {
            fn from(code: F) -> Self {
                Closure (Rc::new(Thunk {
                    code,
                    params: (#(#uninit,)*),
                }))
            }
        }
    }
}

fn generate_from_impls2() -> TokenStream {
    let mut tokens = quote::quote! {};
    for i in 0..=16 {
        tokens.extend(generate_from_impl(i));
    }
    tokens
}

fn generate_partial_param_impl(total_cnt: usize, ready_cnt: usize) -> TokenStream {
    let full_ident = generate_type_idents(total_cnt).collect::<Vec<_>>();
    let pending_tuple = generate_pending_tuple(full_ident.iter(), ready_cnt);
    let marked_tuple = generate_marked_tuple(full_ident.iter(), ready_cnt);
    let progress_tuple = generate_marked_tuple(full_ident.iter(), ready_cnt + 1);
    let transmute_expression = generate_transmute_expression(total_cnt, ready_cnt);
    let write = if ready_cnt == total_cnt {
        quote::quote! {}
    } else {
        let ready_cnt = Literal::usize_unsuffixed(ready_cnt);
        quote::quote! { self.#ready_cnt.0.write(next); }
    };

    quote::quote! {
        impl<#(#full_ident : Clone),*> PartialParams for #marked_tuple {
            type Pending = #pending_tuple;
            type Progress = #progress_tuple;
            type Full = (#(#full_ident,)*);
            fn apply(&mut self, next: <Self::Pending as Params>::Head) {
                #write
            }
            unsafe fn transmute_full(self) -> Self::Full {
                #transmute_expression
            }
        }
    }
}

fn generate_all_partial_param_impls2() -> TokenStream {
    let mut tokens = quote::quote! {
        impl PartialParams for () {
            type Progress = ();
            type Full = ();
            type Pending = ();
            fn apply(&mut self, _: ()) {}
            unsafe fn transmute_full(self) -> Self::Full {}
        }
    };
    for i in 1..=16 {
        for j in 0..=i {
            tokens.extend(generate_partial_param_impl(i, j));
        }
    }
    tokens
}

#[proc_macro]
pub fn generate_all_partial_param_impls(_item: TStream) -> TStream {
    generate_all_partial_param_impls2().into()
}

fn generate_type_idents(x: usize) -> impl Iterator<Item = Ident> {
    (0..x).map(|i| {
        let ident = format!("T{}", i);
        proc_macro2::Ident::new(&ident, proc_macro2::Span::call_site())
    })
}

fn split_type_idents(x: usize) -> (Ident, Vec<Ident>) {
    let mut idents = generate_type_idents(x);
    let ident = idents.next().unwrap();
    (ident, idents.collect())
}

fn impl_parameters2() -> TokenStream {
    let mut tokens = quote::quote! {
        impl Params for () {
            type Head = ();
            type Tail = ();
    }};

    for i in 1..=16 {
        let (first, idents) = split_type_idents(i);
        tokens.extend(quote::quote! {
            impl<#first, #(#idents),*> Params for (#first, #(#idents,)*) {
                type Head = #first;
                type Tail = (#(#idents,)*);
            }
        });
    }
    tokens
}

#[proc_macro]
pub fn impl_parameters(_item: TStream) -> TStream {
    impl_parameters2().into()
}

#[proc_macro]
pub fn generate_from_impls(_item: TStream) -> TStream {
    generate_from_impls2().into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_type_idents() {
        let (fst, idents) = split_type_idents(3);
        assert_eq!(idents.len(), 2);
        assert_eq!(fst.to_string(), "T0");
        assert_eq!(idents[0].to_string(), "T1");
        assert_eq!(idents[1].to_string(), "T2");
    }

    #[test]
    fn impl_parameters_works() {
        let file = syn::parse_file(&impl_parameters2().to_string()).unwrap();
        println!("{}", prettyplease::unparse(&file))
    }

    #[test]
    fn generate_all_partial_param_impls_works() {
        let file = syn::parse_file(&generate_all_partial_param_impls2().to_string()).unwrap();
        println!("{}", prettyplease::unparse(&file))
    }

    #[test]
    fn generate_from_impls_works() {
        let file = syn::parse_file(&generate_from_impls2().to_string()).unwrap();
        println!("{}", prettyplease::unparse(&file))
    }
}
