//! Statement list execution.

use super::{Executable, Interpreter};
use crate::{
    builtins::value::{ResultValue, Value},
    environment::lexical_environment::new_declarative_environment,
    syntax::ast::node::StatementList,
};

impl Executable for StatementList {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        {
            let env = &mut interpreter.realm_mut().environment;
            env.push(new_declarative_environment(Some(
                env.get_current_environment_ref().clone(),
            )));
        }

        let mut obj = Value::null();
        for (i, item) in self.statements().iter().enumerate() {
            let val = interpreter.exec(item)?;
            // early return
            if interpreter.is_return {
                obj = val;
                break;
            }
            if i + 1 == self.statements().len() {
                obj = val;
            }
        }

        // pop the block env
        let _ = interpreter.realm_mut().environment.pop();

        Ok(obj)
    }
}
