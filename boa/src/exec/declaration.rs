//! Declaration execution.

use super::{Executable, Interpreter};
use crate::{
    builtins::{
        function::{Function as FunctionObject, FunctionBody, ThisMode},
        object::Object,
        value::{ResultValue, Value},
    },
    environment::lexical_environment::VariableScope,
    syntax::ast::node::{FunctionDecl, FunctionExpr, VarDeclList},
};

impl Executable for FunctionDecl {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        // Todo: Function.prototype doesn't exist yet, so the prototype right now is the Object.prototype
        // let proto = &self
        //     .realm
        //     .environment
        //     .get_global_object()
        //     .expect("Could not get the global object")
        //     .get_field_slice("Object")
        //     .get_field_slice("Prototype");

        let func = FunctionObject::create_ordinary(
            self.parameters().to_vec(),
            interpreter
                .realm_mut()
                .environment
                .get_current_environment()
                .clone(),
            FunctionBody::Ordinary(self.body().to_vec().into()),
            ThisMode::NonLexical,
        );

        let mut new_func = Object::function();
        new_func.set_call(func);
        let val = Value::from(new_func);
        val.set_field_slice("length", Value::from(self.parameters().len()));

        // Set the name and assign it in the current environment
        if let Some(name) = self.name() {
            val.set_field_slice("name", Value::from(self.name()));
            interpreter.realm_mut().environment.create_mutable_binding(
                name.to_owned(),
                false,
                VariableScope::Function,
            );

            interpreter
                .realm_mut()
                .environment
                .initialize_binding(name, val.clone());
        }

        Ok(val)
    }
}

impl Executable for FunctionExpr {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        // Todo: Function.prototype doesn't exist yet, so the prototype right now is the Object.prototype
        // let proto = &self
        //     .realm
        //     .environment
        //     .get_global_object()
        //     .expect("Could not get the global object")
        //     .get_field_slice("Object")
        //     .get_field_slice("Prototype");

        let func = FunctionObject::create_ordinary(
            self.parameters().to_vec(),
            interpreter
                .realm_mut()
                .environment
                .get_current_environment()
                .clone(),
            FunctionBody::Ordinary(self.body().to_vec().into()),
            ThisMode::NonLexical,
        );

        let mut new_func = Object::function();
        new_func.set_call(func);
        let val = Value::from(new_func);
        val.set_field_slice("length", Value::from(self.parameters().len()));

        if let Some(name) = self.name() {
            val.set_field_slice("name", Value::string(name));
        }

        Ok(val)
    }
}

impl Executable for VarDeclList {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        for var in self.as_ref() {
            let val = match var.init() {
                Some(v) => v.run(interpreter)?,
                None => Value::undefined(),
            };
            let environment = &mut interpreter.realm_mut().environment;

            if environment.has_binding(var.name()) {
                if var.init().is_some() {
                    environment.set_mutable_binding(var.name(), val, true);
                }
            } else {
                environment.create_mutable_binding(
                    var.name().to_owned(),
                    false,
                    VariableScope::Function,
                );
                environment.initialize_binding(var.name(), val);
            }
        }
        Ok(Value::undefined())
    }
}
