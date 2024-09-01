mod utils;
use core::vm::VM;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn evaluate(input: &str) -> String {
    match VM::new().eval(input) {
        Ok(result) => result.to_string(),
        Err(error) => error.message
    }
}