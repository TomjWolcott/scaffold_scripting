use ron::{from_str, Map, Value};
use glam::{bool, f32, Mat4, Vec4};
use std::fmt::{Display, Formatter};
use std::fmt;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
pub enum FromRonError {
    NotMap(Value),
    StructNameNotFound(Map),
    FieldNameNotString(Value),
    BadSeq(Vec<Value>),
    BadFieldValue(Value),
    NotSeq(Value),
    DynamicIsMissingFields(Map)
}

pub trait TryFromRonValue where Self: Sized {
    fn try_from_ron_value(value: Value) -> ron::Result<Self, FromRonError>;
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub name: String,
    pub fields: Vec<(String, Field)>
}

impl Structure {
    pub fn get_field(&self, name: impl AsRef<str>) -> Option<&Field> {
        self.fields.iter()
            .find(|(field_name, _)| field_name == name.as_ref())
            .map(|(_, field)| field)
    }

    pub fn from_ron_string(string: &str) -> ron::Result<Self, FromRonError> {
        let value: Value = from_str(ron_preprocess(string.to_string()).as_str()).unwrap();
        Self::try_from_ron_value(value)
    }
}

impl TryFromRonValue for Structure {
    fn try_from_ron_value(value: Value) -> ron::Result<Self, FromRonError> {
        let Value::Map(mut map) = value else { return Err(FromRonError::NotMap(value)) };
        let Some(Value::String(name)) = map.remove(
            &Value::String("__struct_name".to_string())
        ) else { return Err(FromRonError::StructNameNotFound(map)) };

        let mut fields = Vec::new();

        for (key, val) in map {
            let Value::String(field_name) = key else {
                return Err(FromRonError::FieldNameNotString(key))
            };

            fields.push((field_name, Field::try_from_ron_value(val)?));
        }

        Ok(Self { name, fields })
    }
}

impl Display for Structure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} {{ ", self.name)?;

        for (name, field) in self.fields.iter() {
            write!(f, "{}: {}, ", name, field)?;
        }

        write!(f, " }}")
    }
}

#[derive(Debug, Clone)]
pub enum Field {
    F32(f32),
    Bool(bool),
    Vec4(Vec4),
    Mat4x4(Mat4),
    Dynamic(String, Box<Field>),
    Structure(Box<Structure>)
}

impl TryFromRonValue for Field {
    fn try_from_ron_value(value: Value) -> ron::Result<Self, FromRonError> {
        match value {
            Value::Number(num) => Ok(Self::F32(num.into_f64() as f32)),
            Value::Bool(b) => Ok(Self::Bool(b)),
            Value::Seq(v) => {
                if let Ok(mat4) = Mat4::try_from_ron_value(Value::Seq(v.clone())) {
                    Ok(Self::Mat4x4(mat4))
                } else if let Ok(vec4) = Vec4::try_from_ron_value(Value::Seq(v.clone())) {
                    Ok(Self::Vec4(vec4))
                } else {
                    Err(FromRonError::BadSeq(v))
                }
            },
            Value::Map(m) => {
                if Some(&Value::String("Dynamic".to_string())) != m.get(&Value::String("__struct_name".to_string())) {
                    Ok(Self::Structure(Box::new(Structure::try_from_ron_value(Value::Map(m))?)))
                } else if let (
                    Some(Value::String(id)),
                    Some(value)
                ) = (
                    m.get(&Value::String("id".to_string())),
                    m.get(&Value::String("default".to_string())),
                ) {
                    Ok(Self::Dynamic(
                        id.clone(),
                        Box::new(Field::try_from_ron_value(value.clone())?)
                    ))
                } else {
                    Err(FromRonError::DynamicIsMissingFields(m))
                }
            },
            value => Err(FromRonError::BadFieldValue(value))
        }
    }

}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::F32(num) => write!(f, "{}", num),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Vec4(v) => write!(f, "{}", v),
            Self::Mat4x4(m) => write!(f, "{}", m),
            Self::Dynamic(id, default) => write!(f, "Dynamic({:?}, {})", id, default),
            Self::Structure(s) => write!(f, "{}", s)
        }
    }
}

impl TryFromRonValue for Vec4 {
    fn try_from_ron_value(value: Value) -> ron::Result<Self, FromRonError> {
        let Value::Seq(seq) = value else { return Err(FromRonError::NotSeq(value)) };

        let [
            Value::Number(x),
            Value::Number(y),
            Value::Number(z),
            Value::Number(w),
            ..
        ] = seq.as_slice() else { return Err(FromRonError::BadSeq(seq)) };

        Ok(Self::new(
            x.into_f64() as f32,
            y.into_f64() as f32,
            z.into_f64() as f32,
            w.into_f64() as f32
        ))
    }
}

impl TryFromRonValue for Mat4 {
    fn try_from_ron_value(value: Value) -> ron::Result<Self, FromRonError> {
        let Value::Seq(seq) = value else { return Err(FromRonError::NotSeq(value)) };

        if let [
            Value::Seq(x),
            Value::Seq(y),
            Value::Seq(z),
            Value::Seq(w)
        ] = seq.as_slice() {
            Ok(Mat4::from_cols(
                Vec4::try_from_ron_value(Value::Seq(x.to_vec()))?,
                Vec4::try_from_ron_value(Value::Seq(y.to_vec()))?,
                Vec4::try_from_ron_value(Value::Seq(z.to_vec()))?,
                Vec4::try_from_ron_value(Value::Seq(w.to_vec()))?
            ))
        } else if let [
            Value::Number(m00),
            Value::Number(m01),
            Value::Number(m02),
            Value::Number(m03),
            Value::Number(m10),
            Value::Number(m11),
            Value::Number(m12),
            Value::Number(m13),
            Value::Number(m20),
            Value::Number(m21),
            Value::Number(m22),
            Value::Number(m23),
            Value::Number(m30),
            Value::Number(m31),
            Value::Number(m32),
            Value::Number(m33)
        ] = seq.as_slice() {
            Ok(Mat4::from_cols_array(&[
                m00.into_f64() as f32, m01.into_f64() as f32, m02.into_f64() as f32, m03.into_f64() as f32,
                m10.into_f64() as f32, m11.into_f64() as f32, m12.into_f64() as f32, m13.into_f64() as f32,
                m20.into_f64() as f32, m21.into_f64() as f32, m22.into_f64() as f32, m23.into_f64() as f32,
                m30.into_f64() as f32, m31.into_f64() as f32, m32.into_f64() as f32, m33.into_f64() as f32
            ]))
        } else {
            Err(FromRonError::BadSeq(seq))
        }
    }
}


pub fn ron_preprocess(string: String) -> String {
    lazy_static! {
        static ref STRUCT_NAME_MATCH: Regex = Regex::new(r"(\p{L}[\p{L}0-9]*)\(").unwrap();
    }

    STRUCT_NAME_MATCH.replace_all(string.as_str(), "(__struct_name: \"$1\",").to_string()
}

#[test]
fn test_ronny() {
    let ron = std::fs::read_to_string("assets/definitions.ron").unwrap();

    let val: Value = from_str(ron_preprocess(ron).as_str()).unwrap();

    let structure = Structure::try_from_ron_value(val).unwrap();

    println!("Structure:\n{}", structure);
}

#[test]
fn test_preprocess() {
    let input = r#"
        Container(
            manifold: Shell(
                offset: 0.0,
                shape: Plane4D( normal: [1, 0, 0, 0] )
            )
        )
    "#;

    let expected = r#"
        (__struct_name: "Container",
            manifold: (__struct_name: "Shell",
                offset: 0.0,
                shape: (__struct_name: "Plane4D", normal: [1, 0, 0, 0] )
            )
        )
    "#;

    assert_eq!(ron_preprocess(input.to_string()), expected.to_string());
}
