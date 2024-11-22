use std::collections::HashMap;
use crate::tokenizer::Location;
use crate::value::{Declaration, FunctionRef, Value};

type MemoryScope = HashMap<String, ScopeValue>;
#[derive(Default)]
pub(crate) struct Memory {
    scopes: Vec<MemoryScope>,
}
pub(crate) enum ScopeValue {
    Value(Value),
    Declaration(Declaration),
}
impl Memory {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    pub(crate) fn enter_scope(&mut self) {
        self.scopes.push(Default::default());
    }
    pub(crate) fn leave_scope(&mut self) {
        self.scopes.pop();
    }
    pub(crate) fn declare_user_function(&mut self, value: FunctionRef, loc: Location) -> Option<()> {
        let scope = self.scopes.last_mut().expect("No scopes left in memory!");
        let name = &value.inner.name;
        if scope.contains_key(name) {
            eprintln!("Can not declare function with name {name} at {loc}, name is already used in this scope");
            return None;
        }
        scope.insert(name.to_string(), ScopeValue::Declaration(Declaration::UserFunction(value)));
        Some(())
    }
    pub(crate) fn declare_native_function(&mut self, name: &'static str) -> Option<()> {
        let scope = self.scopes.last_mut().expect("No scopes left in memory!");
        if scope.contains_key(name) {
            eprintln!("Can not declare native function with name {name}, name is already used in this scope");
            return None;
        }
        scope.insert(name.to_string(), ScopeValue::Declaration(Declaration::NativeFunction(name)));
        Some(())
    }
    pub(crate) fn declare_variable(&mut self, name: &str, value: Value, loc: Location) -> Option<()> {
        let scope = self.scopes.last_mut().expect("No scopes left in memory!");
        if let Some(val_ref) = scope.get_mut(name) {
            Self::do_assign(val_ref, value, name, loc)?;
        } else {
            scope.insert(name.to_string(), ScopeValue::Value(value));
        }
        Some(())
    }
    pub(crate) fn assign(&mut self, name: &str, value: Value, loc: Location) -> Option<()> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val_ref) = scope.get_mut(name) {
                return Self::do_assign(val_ref, value, name, loc);
            }
        }
        eprintln!("Can not assign to an undefined variable {name} at {loc}");
        None
    }
    fn do_assign(val_ref: &mut ScopeValue, value: Value, name: &str, loc: Location) -> Option<()> {
        match val_ref {
            ScopeValue::Value(val_ref) => { 
                *val_ref = value;
                Some(())
            },
            ScopeValue::Declaration(x) => {
                eprintln!("Can't overwrite the declaration of {name} at {loc} {}", x.print_location());
                None
            }
        }
    }
    pub(crate) fn get(&self, name: &str, loc: Location) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return match value {
                    ScopeValue::Value(x) => Some(x.clone()),
                    ScopeValue::Declaration(x) => Some(x.clone().into()),
                };
            }
        }
        eprintln!("Can not read an undefined variable {name} at {loc}");
        None
    }
}