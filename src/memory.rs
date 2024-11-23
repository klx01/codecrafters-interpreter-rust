use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use crate::tokenizer::Location;
use crate::value::{Declaration, FunctionRef, Value};

pub(crate) type MemoryScope = HashMap<String, ScopeValue>;
#[derive(Default)]
pub(crate) struct Memory {
    globals: MemoryScope,
    closure_captures: Option<Rc<RefCell<MemoryScope>>>,
    scopes: Vec<MemoryScope>,
    call_scopes: Vec<Vec<MemoryScope>>,
}
#[derive(Debug, Clone)]
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
        self.scopes.pop().expect("unbalanced enter/leave scope");
    }
    pub(crate) fn enter_call(&mut self) {
        let scopes = mem::replace(&mut self.scopes, Default::default());
        self.call_scopes.push(scopes);
        self.enter_scope();
    }
    pub(crate) fn leave_call(&mut self) {
        let scopes = self.call_scopes.pop().expect("unbalanced enter/leave call");
        self.scopes = scopes;
        self.closure_captures = None;
    }
    pub(crate) fn declare_user_function(&mut self, orig: &FunctionRef, loc: Location) -> Option<()> {
        let captures = self.get_captures();
        let scope = self.get_last_scope();
        let name = &orig.inner.name;
        if scope.contains_key(name) {
            eprintln!("Can not declare function with name {name} at {loc}, name is already used in this scope");
            return None;
        }
        let value = orig.instance_with_captures(captures);
        // the function that is being declared is not captured, so recursion is not possible for local functions just with captures
        // capturing it would lead to reference cycles
        // instead we can add callee to the scope during the call
        scope.insert(name.to_string(), ScopeValue::Declaration(Declaration::UserFunction(value)));
        Some(())
    }
    fn get_last_scope(&mut self) -> &mut MemoryScope {
        self.scopes.last_mut().unwrap_or(&mut self.globals)
    }
    fn get_captures(&self) -> Option<Rc<RefCell<MemoryScope>>> {
        if self.scopes.len() == 0 {
            return None;
        }
        let mut captures = MemoryScope::default();
        for scope in &self.scopes {
            for (name, value) in scope {
                // todo: can this be optimized?
                captures.insert(name.clone(), value.clone());
            }
        }
        Some(Rc::new(RefCell::new(captures)))
    }
    pub(crate) fn init_closure(&mut self, orig: &FunctionRef, loc: Location) -> Option<()> {
        let name = &orig.inner.name;
        let Some(captures) = orig.captures.as_ref() else {
            eprintln!("init_closure was called on a function without captures {name} at {loc}");
            return None;
        };
        self.closure_captures = Some(Rc::clone(captures));

        let scope = self.get_last_scope();
        if scope.contains_key(name) {
            eprintln!("Can not add callee to the scope when calling {name} at {loc}, name is already used in this scope");
            return None;
        }
        let value = orig.clone();
        scope.insert(name.to_string(), ScopeValue::Declaration(Declaration::UserFunction(value)));

        Some(())
    }
    pub(crate) fn declare_native_function(&mut self, name: &'static str) -> bool {
        let scope = &mut self.globals;
        if scope.contains_key(name) {
            eprintln!("Can not declare native function with name {name}, name is already used in the global scope");
            return false;
        }
        scope.insert(name.to_string(), ScopeValue::Declaration(Declaration::NativeFunction(name)));
        true
    }
    pub(crate) fn declare_variable(&mut self, name: &str, value: Value, loc: Location) -> Option<()> {
        let scope = self.get_last_scope();
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
        if let Some(captures) = self.closure_captures.as_mut() {
            if let Some(val_ref) = captures.borrow_mut().get_mut(name) {
                return Self::do_assign(val_ref, value, name, loc);
            }
        }
        if let Some(val_ref) = self.globals.get_mut(name) {
            return Self::do_assign(val_ref, value, name, loc);
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
                return Self::process_get_value(value);
            }
        }
        if let Some(captures) = self.closure_captures.as_ref() {
            if let Some(value) = captures.borrow().get(name) {
                return Self::process_get_value(value);
            }
        }
        if let Some(value) = self.globals.get(name) {
            return Self::process_get_value(value);
        }
        eprintln!("Can not read an undefined variable {name} at {loc}");
        None
    }
    fn process_get_value(value: &ScopeValue) -> Option<Value> {
        let value = match value {
            ScopeValue::Value(x) => x.clone(),
            ScopeValue::Declaration(x) => x.clone().into(),
        };
        Some(value)
    }
}