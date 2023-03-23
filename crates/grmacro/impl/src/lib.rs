extern crate proc_macro;

use proc_macro::TokenStream;

mod macrogen;

#[proc_macro_derive(Instruction)]
pub fn derive(input: TokenStream) -> TokenStream {
    macrogen::derive(input)
}
