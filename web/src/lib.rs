use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn print_syntax_tree(text: &str) -> String {
  let tree = resilient_ll_parsing::parse(text);
  format!("{tree:?}")
}
