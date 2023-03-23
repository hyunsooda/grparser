use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{parse_macro_input, Data, DataEnum, DeriveInput, Variant};

type EnumVariants = syn::punctuated::Punctuated<Variant, syn::token::Comma>;

fn get_enum_variants(data: Data) -> EnumVariants {
    if let Data::Enum(DataEnum { variants, .. }) = data {
        variants
    } else {
        unimplemented!()
    }
}

fn get_enum_ident_pairs(variants: EnumVariants) -> Vec<Ident> {
    variants
        .into_iter()
        .map(|v| {
            let Variant { ident, .. } = v;
            ident
        })
        .collect::<Vec<_>>()
}

pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let input_ident = &ast.ident;
    let variants = get_enum_variants(ast.data);

    let enum_idents = get_enum_ident_pairs(variants);
    let update_metadata_arms = enum_idents
        .iter()
        .map(|ident| {
            quote! {
                Self::#ident(instr) => instr.metadata.push(metadata)
            }
        })
        .collect::<Vec<_>>();
    let get_func_sig_arms = enum_idents
        .iter()
        .map(|ident| {
            quote! {
                Self::#ident(instr) => instr.instr_scope.func_info.func_sig.clone()
            }
        })
        .collect::<Vec<_>>();
    let get_associated_func_name_arms = enum_idents
        .iter()
        .map(|ident| {
            quote! {
                Self::#ident(instr) => {
                    (instr.instr_scope.func_info.func_sig.package.to_string(),
                    instr.instr_scope.func_info.func_sig.func_ident.to_string())
                }
            }
        })
        .collect::<Vec<_>>();
    let get_raw_ir_arms = enum_idents.iter().map(|ident| {
        quote! {
            Self::#ident(instr) => &instr.raw_ir
        }
    });
    let get_metadata_arms = enum_idents.iter().map(|ident| {
        quote! {
            Self::#ident(instr) => {
                instr.metadata.
                    iter().
                    filter(|m| m.is_some())
                    .map(|m| m.clone().unwrap()).collect::<Vec<_>>()
            }
        }
    });
    let get_var_metadata_ident_arms = enum_idents.iter().map(|ident| {
        quote! {
            Self::#ident(instr) => {
                if instr.metadata.len() > 0 {
                    if let Some(metadata) = &instr.metadata[0] {
                        if let Metadata::VarMetadata(var) = metadata {
                            return Some(var.ident.to_string())
                        }
                    }
                }
                None
            }
        }
    });
    let get_instr_typ = enum_idents.iter().map(|ident| {
        quote! {
               Self::#ident(instr) => &instr.instr_typ
        }
    });

    quote! {
        impl #input_ident {
            pub fn update_metadata(&mut self, metadata: Option<Metadata>) {
                match self {
                    #(#update_metadata_arms,)*
                }
            }
            pub fn get_func_sig(&self) -> FuncSig {
                match self {
                    #(#get_func_sig_arms,)*
                }
            }
            pub fn get_associated_func_name(&self) -> (String, String) {
                match self {
                    #(#get_associated_func_name_arms,)*
                }
            }
            pub fn get_raw_ir(&self) -> &str {
                match self {
                    #(#get_raw_ir_arms,)*
                }
            }
            pub fn get_var_metadata_ident(&self) -> Option<String> {
                match self {
                    #(#get_var_metadata_ident_arms,)*
                }
            }
            pub fn get_metadata(&self) -> Vec<Metadata> {
                match self {
                    #(#get_metadata_arms,)*
                }
            }
            pub fn get_instr_typ(&self) -> &InstrTyp {
                match self {
                    #(#get_instr_typ,)*
                }
            }
        }
    }
    .into()
}
