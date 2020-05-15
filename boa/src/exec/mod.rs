//! Execution of the AST, this is where the interpreter actually runs

mod array;
mod arrow_function;
mod block;
mod declaration;
mod operator;
mod statement_list;
#[cfg(test)]
mod tests;

use crate::{
    builtins::{
        object::{
            internal_methods_trait::ObjectInternalMethods, ObjectKind, INSTANCE_PROTOTYPE,
            PROTOTYPE,
        },
        property::Property,
        value::{ResultValue, Value, ValueData},
    },
    environment::lexical_environment::VariableScope,
    realm::Realm,
    syntax::ast::{
        node::{MethodDefinitionKind, Node, PropertyDefinition},
        Const,
    },
};
use std::{borrow::Borrow, ops::Deref};

pub trait Executable {
    /// Runs this executable in the given executor.
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue;
}

/// A Javascript intepreter
#[derive(Debug)]
pub struct Interpreter {
    /// Wether it's running a return statement.
    is_return: bool,
    /// realm holds both the global object and the environment
    pub realm: Realm,
}

impl Interpreter {
    /// Creates a new interpreter.
    pub fn new(realm: Realm) -> Self {
        Self {
            realm,
            is_return: false,
        }
    }

    /// Retrieves the `Realm` of this executor.
    pub(crate) fn realm(&self) -> &Realm {
        &self.realm
    }

    /// Retrieves the `Realm` of this executor as a mutable reference.
    pub(crate) fn realm_mut(&mut self) -> &mut Realm {
        &mut self.realm
    }

    /// Run an expression.
    pub(crate) fn exec(&mut self, node: &Node) -> ResultValue {
        node.run(self)
    }

    /// <https://tc39.es/ecma262/#sec-call>
    pub(crate) fn call(
        &mut self,
        f: &Value,
        this: &mut Value,
        arguments_list: &[Value],
    ) -> ResultValue {
        // All functions should be objects, and eventually will be.
        // During this transition call will support both native functions and function objects
        match (*f).deref() {
            ValueData::Object(ref obj) => match (*obj).deref().borrow().call {
                Some(ref func) => func.call(&mut f.clone(), arguments_list, self, this),
                None => panic!("Expected function"),
            },
            _ => Err(Value::undefined()),
        }
    }

    /// Converts a value into a rust heap allocated string.
    pub(crate) fn value_to_rust_string(&mut self, value: &Value) -> String {
        match *value.deref().borrow() {
            ValueData::Null => String::from("null"),
            ValueData::Boolean(ref boolean) => boolean.to_string(),
            ValueData::Rational(ref num) => num.to_string(),
            ValueData::Integer(ref num) => num.to_string(),
            ValueData::String(ref string) => string.clone(),
            ValueData::Object(_) => {
                let prim_value = self.to_primitive(&mut (value.clone()), Some("string"));
                self.to_string(&prim_value).to_string()
            }
            _ => String::from("undefined"),
        }
    }

    /// Converts an array object into a rust vector of values.
    ///
    /// This is useful for the spread operator, for any other object an `Err` is returned
    pub(crate) fn extract_array_properties(&mut self, value: &Value) -> Result<Vec<Value>, ()> {
        if let ValueData::Object(ref x) = *value.deref().borrow() {
            // Check if object is array
            if x.deref().borrow().kind == ObjectKind::Array {
                let length: i32 =
                    self.value_to_rust_number(&value.get_field_slice("length")) as i32;
                let values: Vec<Value> = (0..length)
                    .map(|idx| value.get_field_slice(&idx.to_string()))
                    .collect();
                return Ok(values);
            }

            return Err(());
        }

        Err(())
    }

    /// <https://tc39.es/ecma262/#sec-ordinarytoprimitive>
    pub(crate) fn ordinary_to_primitive(&mut self, o: &mut Value, hint: &str) -> Value {
        debug_assert!(o.get_type() == "object");
        debug_assert!(hint == "string" || hint == "number");
        let method_names: Vec<&str> = if hint == "string" {
            vec!["toString", "valueOf"]
        } else {
            vec!["valueOf", "toString"]
        };
        for name in method_names.iter() {
            let method: Value = o.get_field_slice(name);
            if method.is_function() {
                let result = self.call(&method, o, &[]);
                match result {
                    Ok(val) => {
                        if val.is_object() {
                            // TODO: throw exception
                            continue;
                        } else {
                            return val;
                        }
                    }
                    Err(_) => continue,
                }
            }
        }

        Value::undefined()
    }

    /// The abstract operation ToPrimitive takes an input argument and an optional argument PreferredType.
    /// <https://tc39.es/ecma262/#sec-toprimitive>
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn to_primitive(
        &mut self,
        input: &mut Value,
        preferred_type: Option<&str>,
    ) -> Value {
        let mut hint: &str;
        match (*input).deref() {
            ValueData::Object(_) => {
                hint = match preferred_type {
                    None => "default",
                    Some(pt) => match pt {
                        "string" => "string",
                        "number" => "number",
                        _ => "default",
                    },
                };

                // Skip d, e we don't support Symbols yet
                // TODO: add when symbols are supported
                if hint == "default" {
                    hint = "number";
                };

                self.ordinary_to_primitive(input, hint)
            }
            _ => input.clone(),
        }
    }
    /// to_string() converts a value into a String
    /// https://tc39.es/ecma262/#sec-tostring
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn to_string(&mut self, value: &Value) -> Value {
        match *value.deref().borrow() {
            ValueData::Undefined => Value::from("undefined"),
            ValueData::Null => Value::from("null"),
            ValueData::Boolean(ref boolean) => Value::from(boolean.to_string()),
            ValueData::Rational(ref num) => Value::from(num.to_string()),
            ValueData::Integer(ref num) => Value::from(num.to_string()),
            ValueData::String(ref string) => Value::from(string.clone()),
            ValueData::Object(_) => {
                let prim_value = self.to_primitive(&mut (value.clone()), Some("string"));
                self.to_string(&prim_value)
            }
            _ => Value::from("function(){...}"),
        }
    }

    /// The abstract operation ToPropertyKey takes argument argument. It converts argument to a value that can be used as a property key.
    /// https://tc39.es/ecma262/#sec-topropertykey
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn to_property_key(&mut self, value: &mut Value) -> Value {
        let key = self.to_primitive(value, Some("string"));
        if key.is_symbol() {
            key
        } else {
            self.to_string(&key)
        }
    }

    /// https://tc39.es/ecma262/#sec-hasproperty
    pub(crate) fn has_property(&self, obj: &mut Value, key: &Value) -> bool {
        if let Some(obj) = obj.as_object() {
            if !Property::is_property_key(key) {
                false
            } else {
                obj.has_property(key)
            }
        } else {
            false
        }
    }

    /// The abstract operation ToObject converts argument to a value of type Object
    /// https://tc39.es/ecma262/#sec-toobject
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn to_object(&mut self, value: &Value) -> ResultValue {
        match *value.deref().borrow() {
            ValueData::Undefined | ValueData::Integer(_) | ValueData::Null => {
                Err(Value::undefined())
            }
            ValueData::Boolean(_) => {
                let proto = self
                    .realm
                    .environment
                    .get_binding_value("Boolean")
                    .get_field_slice(PROTOTYPE);

                let bool_obj = Value::new_object_from_prototype(proto, ObjectKind::Boolean);
                bool_obj.set_internal_slot("BooleanData", value.clone());
                Ok(bool_obj)
            }
            ValueData::Rational(_) => {
                let proto = self
                    .realm
                    .environment
                    .get_binding_value("Number")
                    .get_field_slice(PROTOTYPE);
                let number_obj = Value::new_object_from_prototype(proto, ObjectKind::Number);
                number_obj.set_internal_slot("NumberData", value.clone());
                Ok(number_obj)
            }
            ValueData::String(_) => {
                let proto = self
                    .realm
                    .environment
                    .get_binding_value("String")
                    .get_field_slice(PROTOTYPE);
                let string_obj = Value::new_object_from_prototype(proto, ObjectKind::String);
                string_obj.set_internal_slot("StringData", value.clone());
                Ok(string_obj)
            }
            ValueData::Object(_) | ValueData::Symbol(_) => Ok(value.clone()),
        }
    }

    pub(crate) fn value_to_rust_number(&mut self, value: &Value) -> f64 {
        match *value.deref().borrow() {
            ValueData::Null => f64::from(0),
            ValueData::Boolean(boolean) => {
                if boolean {
                    f64::from(1)
                } else {
                    f64::from(0)
                }
            }
            ValueData::Rational(num) => num,
            ValueData::Integer(num) => f64::from(num),
            ValueData::String(ref string) => string.parse::<f64>().unwrap(),
            ValueData::Object(_) => {
                let prim_value = self.to_primitive(&mut (value.clone()), Some("number"));
                self.to_string(&prim_value)
                    .to_string()
                    .parse::<f64>()
                    .expect("cannot parse valur to x64")
            }
            _ => {
                // TODO: Make undefined?
                f64::from(0)
            }
        }
    }

    fn set_value(&mut self, node: &Node, value: Value) -> ResultValue {
        match node {
            Node::Identifier(ref name) => {
                self.realm
                    .environment
                    .set_mutable_binding(name.as_ref(), value.clone(), true);
                Ok(value)
            }
            Node::GetConstField(ref obj, ref field) => {
                Ok(self.exec(obj)?.set_field_slice(field, value))
            }
            Node::GetField(ref obj, ref field) => {
                Ok(self.exec(obj)?.set_field(self.exec(field)?, value))
            }
            _ => panic!("TypeError: invalid assignment to {}", node),
        }
    }
}

impl Executable for Node {
    fn run(&self, interpreter: &mut Interpreter) -> ResultValue {
        match *self {
            Node::Const(Const::Null) => Ok(Value::null()),
            Node::Const(Const::Undefined) => Ok(Value::undefined()),
            Node::Const(Const::Num(num)) => Ok(Value::rational(num)),
            Node::Const(Const::Int(num)) => Ok(Value::integer(num)),
            // we can't move String from Const into value, because const is a garbage collected value
            // Which means Drop() get's called on Const, but str will be gone at that point.
            // Do Const values need to be garbage collected? We no longer need them once we've generated Values
            Node::Const(Const::String(ref value)) => Ok(Value::string(value.to_string())),
            Node::Const(Const::Bool(value)) => Ok(Value::boolean(value)),
            Node::Block(ref block) => block.run(interpreter),
            Node::Identifier(ref name) => {
                let val = interpreter
                    .realm()
                    .environment
                    .get_binding_value(name.as_ref());
                Ok(val)
            }
            Node::GetConstField(ref obj, ref field) => {
                let val_obj = interpreter.exec(obj)?;
                Ok(val_obj.borrow().get_field_slice(field))
            }
            Node::GetField(ref obj, ref field) => {
                let val_obj = interpreter.exec(obj)?;
                let val_field = interpreter.exec(field)?;
                Ok(val_obj
                    .borrow()
                    .get_field_slice(&val_field.borrow().to_string()))
            }
            Node::Call(ref callee, ref args) => {
                let (mut this, func) = match callee.deref() {
                    Node::GetConstField(ref obj, ref field) => {
                        let mut obj = interpreter.exec(obj)?;
                        if obj.get_type() != "object" || obj.get_type() != "symbol" {
                            obj = interpreter
                                .to_object(&obj)
                                .expect("failed to convert to object");
                        }
                        (obj.clone(), obj.borrow().get_field_slice(field))
                    }
                    Node::GetField(ref obj, ref field) => {
                        let obj = interpreter.exec(obj)?;
                        let field = interpreter.exec(field)?;
                        (
                            obj.clone(),
                            obj.borrow().get_field_slice(&field.borrow().to_string()),
                        )
                    }
                    _ => (
                        interpreter.realm().global_obj.clone(),
                        interpreter.exec(&callee.clone())?,
                    ), // 'this' binding should come from the function's self-contained environment
                };
                let mut v_args = Vec::with_capacity(args.len());
                for arg in args.iter() {
                    if let Node::Spread(ref x) = arg.deref() {
                        let val = interpreter.exec(x)?;
                        let mut vals = interpreter.extract_array_properties(&val).unwrap();
                        v_args.append(&mut vals);
                        break; // after spread we don't accept any new arguments
                    }
                    v_args.push(interpreter.exec(arg)?);
                }

                // execute the function call itself
                let fnct_result = interpreter.call(&func, &mut this, &v_args);

                // unset the early return flag
                interpreter.is_return = false;

                fnct_result
            }
            Node::WhileLoop(ref cond, ref expr) => {
                let mut result = Value::undefined();
                while interpreter.exec(cond)?.borrow().is_true() {
                    result = interpreter.exec(expr)?;
                }
                Ok(result)
            }
            Node::DoWhileLoop(ref body, ref cond) => {
                let mut result = interpreter.exec(body)?;
                while interpreter.exec(cond)?.borrow().is_true() {
                    result = interpreter.exec(body)?;
                }
                Ok(result)
            }
            Node::ForLoop(ref init, ref cond, ref step, ref body) => {
                if let Some(init) = init {
                    interpreter.exec(init)?;
                }

                while match cond {
                    Some(cond) => interpreter.exec(cond)?.borrow().is_true(),
                    None => true,
                } {
                    interpreter.exec(body)?;

                    if let Some(step) = step {
                        interpreter.exec(step)?;
                    }
                }

                Ok(Value::undefined())
            }
            Node::If(ref cond, ref expr, None) => {
                Ok(if interpreter.exec(cond)?.borrow().is_true() {
                    interpreter.exec(expr)?
                } else {
                    Value::undefined()
                })
            }
            Node::If(ref cond, ref expr, Some(ref else_e)) => {
                Ok(if interpreter.exec(cond)?.borrow().is_true() {
                    interpreter.exec(expr)?
                } else {
                    interpreter.exec(else_e)?
                })
            }
            Node::Switch(ref val_e, ref vals, ref default) => {
                let val = interpreter.exec(val_e)?;
                let mut result = Value::null();
                let mut matched = false;
                for tup in vals.iter() {
                    let cond = &tup.0;
                    let block = &tup.1;
                    if val.strict_equals(&cond.run(interpreter)?) {
                        matched = true;
                        let last_expr = block.last().expect("Block has no expressions");
                        for expr in block.iter() {
                            let e_result = interpreter.exec(expr)?;
                            if expr == last_expr {
                                result = e_result;
                            }
                        }
                    }
                }
                if !matched && default.is_some() {
                    result = interpreter.exec(
                        default
                            .as_ref()
                            .expect("Could not get default as reference"),
                    )?;
                }
                Ok(result)
            }
            Node::Object(ref properties) => {
                let global_val = &interpreter
                    .realm()
                    .environment
                    .get_global_object()
                    .expect("Could not get the global object");
                let obj = Value::new_object(Some(global_val));

                // TODO: Implement the rest of the property types.
                for property in properties.iter() {
                    match property {
                        PropertyDefinition::Property(key, value) => {
                            obj.borrow()
                                .set_field_slice(&key.clone(), interpreter.exec(value)?);
                        }
                        PropertyDefinition::MethodDefinition(kind, name, func) => {
                            if let MethodDefinitionKind::Ordinary = kind {
                                obj.borrow()
                                    .set_field_slice(&name.clone(), func.run(interpreter)?);
                            } else {
                                // TODO: Implement other types of MethodDefinitionKinds.
                                unimplemented!("other types of property method definitions.");
                            }
                        }
                        i => unimplemented!("{:?} type of property", i),
                    }
                }

                Ok(obj)
            }
            Node::ArrayDecl(ref arr) => arr.run(interpreter),
            // <https://tc39.es/ecma262/#sec-createdynamicfunction>
            Node::FunctionDecl(ref decl) => decl.run(interpreter),
            // <https://tc39.es/ecma262/#sec-createdynamicfunction>
            Node::FunctionExpr(ref expr) => expr.run(interpreter),
            Node::ArrowFunctionDecl(ref decl) => decl.run(interpreter),
            Node::BinOp(ref op) => op.run(interpreter),
            Node::UnaryOp(ref op) => op.run(interpreter),
            Node::New(ref call) => {
                let (callee, args) = match call.as_ref() {
                    Node::Call(callee, args) => (callee, args),
                    _ => unreachable!("Node::New(ref call): 'call' must only be Node::Call type."),
                };

                let func_object = interpreter.exec(callee)?;
                let mut v_args = Vec::with_capacity(args.len());
                for arg in args.iter() {
                    v_args.push(interpreter.exec(arg)?);
                }
                let mut this = Value::new_object(None);
                // Create a blank object, then set its __proto__ property to the [Constructor].prototype
                this.borrow().set_internal_slot(
                    INSTANCE_PROTOTYPE,
                    func_object.borrow().get_field_slice(PROTOTYPE),
                );

                match *(func_object.borrow()).deref() {
                    ValueData::Object(ref o) => (*o.deref().clone().borrow_mut())
                        .construct
                        .as_ref()
                        .unwrap()
                        .construct(&mut func_object.clone(), &v_args, interpreter, &mut this),
                    _ => Ok(Value::undefined()),
                }
            }
            Node::Return(ref ret) => {
                let result = match *ret {
                    Some(ref v) => interpreter.exec(v),
                    None => Ok(Value::undefined()),
                };
                // Set flag for return
                interpreter.is_return = true;
                result
            }
            Node::Throw(ref ex) => Err(interpreter.exec(ex)?),
            Node::Assign(ref op) => op.run(interpreter),
            Node::VarDeclList(ref decl) => decl.run(interpreter),
            Node::LetDecl(ref vars) => {
                for var in vars.iter() {
                    let (name, value) = var.clone();
                    let val = match value {
                        Some(v) => interpreter.exec(&v)?,
                        None => Value::undefined(),
                    };
                    interpreter.realm_mut().environment.create_mutable_binding(
                        name.as_ref().to_owned(),
                        false,
                        VariableScope::Block,
                    );
                    interpreter
                        .realm_mut()
                        .environment
                        .initialize_binding(&name, val);
                }
                Ok(Value::undefined())
            }
            Node::ConstDeclList(ref decl) => decl.run(interpreter),
            Node::TypeOf(ref val_e) => {
                let val = interpreter.exec(val_e)?;
                Ok(Value::from(match *val {
                    ValueData::Undefined => "undefined",
                    ValueData::Symbol(_) => "symbol",
                    ValueData::Null => "object",
                    ValueData::Boolean(_) => "boolean",
                    ValueData::Rational(_) | ValueData::Integer(_) => "number",
                    ValueData::String(_) => "string",
                    ValueData::Object(ref o) => {
                        if o.deref().borrow().is_callable() {
                            "function"
                        } else {
                            "object"
                        }
                    }
                }))
            }
            Node::Spread(ref node) => {
                // TODO: for now we can do nothing but return the value as-is
                interpreter.exec(node)
            }
            ref i => unimplemented!("{:?}", i),
        }
    }
}
