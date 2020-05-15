//! Arrow function execution.

use super::{Executable, Interpreter};
use crate::{
    builtins::{
        function::{Function as FunctionObject, FunctionBody, ThisMode},
        object::Object,
        value::{ResultValue, Value},
    },
    syntax::ast::node::ArrowFunctionDecl,
};

impl Executable for ArrowFunctionDecl {
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
            self.params().to_vec(), // TODO: args shouldn't need to be a reference it should be passed by value
            interpreter
                .realm_mut()
                .environment
                .get_current_environment()
                .clone(),
            FunctionBody::Ordinary(self.body().to_vec().into()),
            ThisMode::Lexical,
        );

        let mut new_func = Object::function();
        new_func.set_call(func);
        let val = Value::from(new_func);
        val.set_field_slice("length", Value::from(self.params().len()));

        Ok(val)
    }
}
