use super::low_loader::Value as LowValue;
use syntax::TypeRef;

/// Value Struct
///
/// Represents a typed value that is being used during JIT
/// compilation.
#[derive(Debug,PartialEq)]
pub struct Value {
    typ: TypeRef,
    value: Option<LowValue>
}

impl Value {
    /// Create a Unit Value
    ///
    /// The unit value can be used to represent absence of an
    /// underlying value, such as an if statement without an else
    /// branch, or an empty sequence. Unit values aren't usually
    /// manipulated in the underlying compilation. They're mainly
    /// used to pass metadata around and handle the case that
    /// _all_ statements _must_ have a value of some kind.
    ///
    /// # Returns
    ///
    /// A new `Value` instatnce with no value.
    pub fn unit() -> Self {
        Value { typ: TypeRef::unit(), value: None }
    }

    /// Create a Value
    ///
    /// Wraps a given LLVM value to adorn it with some lovely type
    /// information.
    ///
    /// # Arguments
    ///
    ///  * `typ` - The tpe of the value. This is our higher level
    ///  knowledge of its type rather than the underlying LLVM
    ///  type.
    ///  * `val` - The low level value that this instance wraps.
    ///
    /// # Returns
    ///
    /// The new `Value` instance.
    pub fn new(typ: TypeRef, val: LowValue) -> Self {
        Value { typ: typ, value: Some(val) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use eval::jit::low_loader::Context;
    use syntax::TypeRef;

    #[test]
    fn create_unit_value() {
        let unit = Value::unit();
        assert_eq!(TypeRef::unit(), unit.typ);
        assert!(unit.value.is_none());
    }

    #[test]
    fn all_units_are_equal() {
        let one = Value::unit();
        let two = Value::unit();
        assert_eq!(one, two);
    }

    #[test]
    fn create_simple_value() {
        let ctx = Context::new();
        let raw_val = ctx.const_int(1337);
        let val = Value::new(TypeRef::simple("Number"), raw_val);
        assert_eq!(TypeRef::simple("Number"), val.typ);
        assert!(val.value.is_some());
    }
}
