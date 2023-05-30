use crate::Object;
use std::fmt::{Display, Formatter};

/// The Parameter enum is used as input to function and method calls within the proc macro environment.
/// It defines the type and provides the value.
#[derive(Clone)]
pub enum Parameter {
    /// The String type
    String(String),
    /// An unsigned integer
    UInt(u128),
    /// A signed integer
    Int(i128),
    /// A floating point value
    Float(f64),
    /// An object (created via the `SealedObjectType::new_instance` method)
    Object(Object),
    /// An array of Parameter enums
    Array(Box<[Parameter]>),
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Parameter::String(_) => write!(f, "string"),
            Parameter::UInt(_) => write!(f, "uint"),
            Parameter::Int(_) => write!(f, "int"),
            Parameter::Float(_) => write!(f, "float"),
            Parameter::Object(_) => write!(f, "object"),
            Parameter::Array(_) => write!(f, "array"),
        }
    }
}

impl From<i8> for Parameter {
    fn from(value: i8) -> Self {
        Self::Int(value as i128)
    }
}

impl TryFrom<Parameter> for i8 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for i8 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => i8::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => i8::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an i8",
                value
            )),
        }
    }
}

impl From<i16> for Parameter {
    fn from(value: i16) -> Self {
        Self::Int(value as i128)
    }
}

impl TryFrom<Parameter> for i16 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for i16 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => i16::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => i16::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an i16",
                value
            )),
        }
    }
}

impl From<i32> for Parameter {
    fn from(value: i32) -> Self {
        Self::Int(value as i128)
    }
}

impl TryFrom<Parameter> for i32 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for i32 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => i32::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => i32::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an i32",
                value
            )),
        }
    }
}

impl From<i64> for Parameter {
    fn from(value: i64) -> Self {
        Self::Int(value as i128)
    }
}

impl TryFrom<Parameter> for i64 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for i64 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => i64::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => i64::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an i64",
                value
            )),
        }
    }
}

impl From<i128> for Parameter {
    fn from(value: i128) -> Self {
        Self::Int(value)
    }
}

impl TryFrom<Parameter> for i128 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for i128 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => i128::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => Ok(int),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an i128",
                value
            )),
        }
    }
}

impl From<isize> for Parameter {
    fn from(value: isize) -> Self {
        Self::Int(value as i128)
    }
}

impl TryFrom<Parameter> for isize {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for isize {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => isize::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => isize::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an isize",
                value
            )),
        }
    }
}

impl From<u8> for Parameter {
    fn from(value: u8) -> Self {
        Self::UInt(value as u128)
    }
}

impl TryFrom<Parameter> for u8 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for u8 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => u8::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => u8::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an u8",
                value
            )),
        }
    }
}

impl From<u16> for Parameter {
    fn from(value: u16) -> Self {
        Self::UInt(value as u128)
    }
}

impl TryFrom<Parameter> for u16 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for u16 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => u16::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => u16::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an u16",
                value
            )),
        }
    }
}

impl From<u32> for Parameter {
    fn from(value: u32) -> Self {
        Self::UInt(value as u128)
    }
}

impl TryFrom<Parameter> for u32 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for u32 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => u32::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => u32::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to a u32",
                value
            )),
        }
    }
}

impl From<u64> for Parameter {
    fn from(value: u64) -> Self {
        Self::UInt(value as u128)
    }
}

impl TryFrom<Parameter> for u64 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for u64 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => u64::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => u64::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to a u64",
                value
            )),
        }
    }
}

impl From<u128> for Parameter {
    fn from(value: u128) -> Self {
        Self::UInt(value)
    }
}

impl TryFrom<Parameter> for u128 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for u128 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => Ok(uint),
            Parameter::Int(int) => u128::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to a u128",
                value
            )),
        }
    }
}

impl From<usize> for Parameter {
    fn from(value: usize) -> Self {
        Self::UInt(value as u128)
    }
}

impl TryFrom<Parameter> for usize {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for usize {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::UInt(uint) => usize::try_from(uint).map_err(|x| x.to_string()),
            Parameter::Int(int) => usize::try_from(int).map_err(|x| x.to_string()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to a usize",
                value
            )),
        }
    }
}

impl From<f32> for Parameter {
    fn from(value: f32) -> Self {
        Self::Float(value as f64)
    }
}

impl TryFrom<Parameter> for f32 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for f32 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::Float(float) => Ok(float as f32),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an f32",
                value
            )),
        }
    }
}

impl From<f64> for Parameter {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl TryFrom<Parameter> for f64 {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for f64 {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match *value {
            Parameter::Float(float) => Ok(float),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an f64",
                value
            )),
        }
    }
}

impl From<Object> for Parameter {
    fn from(value: Object) -> Self {
        Parameter::Object(value)
    }
}

impl TryFrom<Parameter> for Object {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for Object {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match value {
            Parameter::Object(object) => Ok(object.clone()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to an object",
                value
            )),
        }
    }
}

impl From<String> for Parameter {
    fn from(value: String) -> Self {
        Parameter::String(value)
    }
}

impl TryFrom<Parameter> for String {
    type Error = String;

    fn try_from(value: Parameter) -> Result<Self, Self::Error> {
        Self::try_from(&value)
    }
}

impl TryFrom<&Parameter> for String {
    type Error = String;

    fn try_from(value: &Parameter) -> Result<Self, Self::Error> {
        match value {
            Parameter::String(string) => Ok(string.clone()),
            _ => Err(format!(
                "can't convert parameter of type '{}' to a String",
                value
            )),
        }
    }
}
