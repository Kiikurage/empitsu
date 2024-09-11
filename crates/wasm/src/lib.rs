mod utils;
use core::ast::get_range::GetRange;
use core::vm::{EMObject, Value, VM};
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
pub fn evaluate(input: &str) -> ValueView {
    let mut vm = VM::new();

    let result = match vm.eval(input) {
        Ok(result) => result.clone(),
        Err(error) => return ValueView {
            type_: "Error".to_string(),
            value: format!("Error({}) {}", error.start(), error.message),
            properties: vec![],
        }
    };

    ValueView::from_value(&vm, &result)
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Debug, PartialEq, Clone)]
pub struct ValueView {
    pub type_: String,
    pub value: String,
    pub properties: Vec<PropertyView>,
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Debug, PartialEq, Clone)]
pub struct PropertyView {
    pub name: String,
    pub value: ValueView,
}

#[wasm_bindgen]
impl ValueView {
    fn from_value(vm: &VM, value: &Value) -> Self {
        match value {
            Value::Number(value) => ValueView {
                type_: "number".to_string(),
                value: value.to_string(),
                properties: vec![],
            },
            Value::Bool(value) => ValueView {
                type_: "bool".to_string(),
                value: value.to_string(),
                properties: vec![],
            },
            Value::Ref(address) => {
                let object = vm.heap.get(address).unwrap();
                ValueView::from_object(vm, object)
            }
        }
    }

    fn from_object(vm: &VM, object: &EMObject) -> Self {
        match object {
            EMObject::Box(boxed) => ValueView::from_value(vm, &boxed.value),
            EMObject::Struct(struct_) => {
                let definition = match vm.heap.get(&struct_.definition) {
                    Some(EMObject::StructDefinition(definition)) => definition,
                    _ => panic!("Invalid struct definition"),
                };

                let mut properties = vec![];
                for (name, value) in definition.properties.iter().zip(struct_.properties.iter()) {
                    properties.push(PropertyView {
                        name: name.clone(),
                        value: ValueView::from_value(vm, value),
                    });
                }

                ValueView {
                    type_: definition.name.clone(),
                    value: "".to_string(),
                    properties,
                }
            }
            EMObject::StructDefinition(struct_) => ValueView {
                type_: "struct".to_string(),
                value: struct_.name.clone(),
                properties: vec![],
            },
            EMObject::UserFunction(function) => ValueView {
                type_: "fn".to_string(),
                value: function.name.clone(),
                properties: vec![],
            },
        }
    }
}