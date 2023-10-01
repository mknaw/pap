// In your macro crate, e.g., "tests_macro/src/lib.rs"

extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;

#[proc_macro]
pub fn generate_disassembler_tests(_input: TokenStream) -> TokenStream {
    let tests = std::fs::read_dir("./pap/tests/cases/")
        .unwrap()
        .map(|p| {
            let path = p.unwrap().path();
            let test_name = Ident::new(
                &format!(
                    "disassemble_{}",
                    &path.file_stem().unwrap().to_str().unwrap().to_lowercase()
                ),
                Span::call_site(),
            );
            let path_str = path.to_str().unwrap().replace("/pap", "");
            quote! {
                #[test]
                fn #test_name() {
                    test_disassemble(&Path::new(#path_str).to_path_buf());
                }
            }
        })
        .collect::<Vec<_>>();

    let output = quote! {
        #(#tests)*
    };

    output.into()
}
