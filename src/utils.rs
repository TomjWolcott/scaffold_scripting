use ron::{Map, Value};

pub trait GetOnMap {
    fn get(&self, key: &(impl AsRef<str> + ?Sized)) -> Option<&Value>;
}

impl GetOnMap for Map {
    fn get(&self, key: &(impl AsRef<str> + ?Sized)) -> Option<&Value> {
        self.iter().find(|(key_value, value)| {
            match key_value {
                Value::String(other_key) => other_key.as_str() == key.as_ref(),
                _ => false
            }
        }).map(|(_, value)| value)
    }
}