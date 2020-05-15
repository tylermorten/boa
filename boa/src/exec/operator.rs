//! Operator execution.

use super::{Executable, Interpreter};
use crate::{
    builtins::value::{ResultValue, Value},
    environment::lexical_environment::VariableScope,
    syntax::ast::{
        node::{Assign, BinOp, Node},
        op::{self, AssignOp, BitOp, CompOp, LogOp, NumOp},
    },
};
use std::borrow::BorrowMut;

impl Executable for Assign {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        let val = self.rhs().run(interpreter)?;
        match self.lhs() {
            Node::Identifier(ref name) => {
                let environment = &mut interpreter.realm_mut().environment;

                if environment.has_binding(name.as_ref()) {
                    // Binding already exists
                    environment.set_mutable_binding(name.as_ref(), val.clone(), true);
                } else {
                    environment.create_mutable_binding(
                        name.as_ref().to_owned(),
                        true,
                        VariableScope::Function,
                    );
                    environment.initialize_binding(name.as_ref(), val.clone());
                }
            }
            Node::GetConstField(ref obj, ref field) => {
                let val_obj = obj.run(interpreter)?;
                val_obj.set_field_slice(field, val.clone());
            }
            Node::GetField(ref obj, ref field) => {
                let val_obj = obj.run(interpreter)?;
                let val_field = field.run(interpreter)?;
                val_obj.set_field(val_field, val.clone());
            }
            _ => (),
        }
        Ok(val)
    }
}

impl Executable for BinOp {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        match self.op() {
            op::BinOp::Num(op) => {
                let v_a = self.lhs().run(interpreter)?;
                let v_b = self.rhs().run(interpreter)?;
                Ok(match op {
                    NumOp::Add => v_a + v_b,
                    NumOp::Sub => v_a - v_b,
                    NumOp::Mul => v_a * v_b,
                    NumOp::Exp => v_a.as_num_to_power(v_b),
                    NumOp::Div => v_a / v_b,
                    NumOp::Mod => v_a % v_b,
                })
            }
            op::BinOp::Bit(op) => {
                let v_a = self.lhs().run(interpreter)?;
                let v_b = self.rhs().run(interpreter)?;
                Ok(match op {
                    BitOp::And => v_a & v_b,
                    BitOp::Or => v_a | v_b,
                    BitOp::Xor => v_a ^ v_b,
                    BitOp::Shl => v_a << v_b,
                    BitOp::Shr => v_a >> v_b,
                    // TODO Fix
                    BitOp::UShr => v_a >> v_b,
                })
            }
            op::BinOp::Comp(op) => {
                let mut v_a = self.lhs().run(interpreter)?;
                let mut v_b = self.rhs().run(interpreter)?;
                Ok(Value::from(match op {
                    CompOp::Equal => v_a.equals(v_b.borrow_mut(), interpreter),
                    CompOp::NotEqual => !v_a.equals(v_b.borrow_mut(), interpreter),
                    CompOp::StrictEqual => v_a.strict_equals(&v_b),
                    CompOp::StrictNotEqual => !v_a.strict_equals(&v_b),
                    CompOp::GreaterThan => v_a.to_number() > v_b.to_number(),
                    CompOp::GreaterThanOrEqual => v_a.to_number() >= v_b.to_number(),
                    CompOp::LessThan => v_a.to_number() < v_b.to_number(),
                    CompOp::LessThanOrEqual => v_a.to_number() <= v_b.to_number(),
                    CompOp::In => {
                        if !v_b.is_object() {
                            panic!("TypeError: {} is not an Object.", v_b);
                        }
                        let key = interpreter.to_property_key(&mut v_a);
                        interpreter.has_property(&mut v_b, &key)
                    }
                }))
            }
            op::BinOp::Log(op) => {
                // turn a `Value` into a `bool`
                let to_bool = |value| bool::from(&value);
                Ok(match op {
                    LogOp::And => Value::from(
                        to_bool(self.lhs().run(interpreter)?)
                            && to_bool(self.rhs().run(interpreter)?),
                    ),
                    LogOp::Or => Value::from(
                        to_bool(self.lhs().run(interpreter)?)
                            || to_bool(self.rhs().run(interpreter)?),
                    ),
                })
            }
            op::BinOp::Assign(op) => match self.lhs() {
                Node::Identifier(ref name) => {
                    let v_a = interpreter
                        .realm()
                        .environment
                        .get_binding_value(name.as_ref());
                    let v_b = self.rhs().run(interpreter)?;
                    let value = Self::run_assign(op, v_a, v_b);
                    interpreter.realm.environment.set_mutable_binding(
                        name.as_ref(),
                        value.clone(),
                        true,
                    );
                    Ok(value)
                }
                Node::GetConstField(ref obj, ref field) => {
                    let v_r_a = obj.run(interpreter)?;
                    let v_a = v_r_a.get_field_slice(field);
                    let v_b = self.rhs().run(interpreter)?;
                    let value = Self::run_assign(op, v_a, v_b);
                    v_r_a.set_field_slice(&field.clone(), value.clone());
                    Ok(value)
                }
                _ => Ok(Value::undefined()),
            },
        }
    }
}

impl BinOp {
    fn run_assign(op: AssignOp, v_a: Value, v_b: Value) -> Value {
        match op {
            AssignOp::Add => v_a + v_b,
            AssignOp::Sub => v_a - v_b,
            AssignOp::Mul => v_a * v_b,
            AssignOp::Exp => v_a.as_num_to_power(v_b),
            AssignOp::Div => v_a / v_b,
            AssignOp::Mod => v_a % v_b,
            AssignOp::And => v_a & v_b,
            AssignOp::Or => v_a | v_b,
            AssignOp::Xor => v_a ^ v_b,
            AssignOp::Shl => v_a << v_b,
            AssignOp::Shr => v_a << v_b,
        }
    }
}
