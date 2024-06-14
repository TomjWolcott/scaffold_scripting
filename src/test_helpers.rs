use lazy_static::lazy_static;
use regex::Regex;
use crate::parser::{Document, parse_document};
use crate::structure::Structure;

pub fn get_test_stuff(opt1: usize, opt2: usize) -> (Document, Structure) {
    let doc_str = match opt1 {
        0 => r#"
        interface Proj {
            proj(vector: vec4) -> vec4
        }

        interface Sdf {
            sdf(vector: vec4) -> f32
        }

        class Sphere4D {
            radius: f32,
            Proj::proj(vector: vec4) -> vec4 {
                radius * normalize(vector)
            },
            Sdf::sdf(vector: vec4) -> f32 {
                length(vector) - radius
            }
        }

        class Plane4D {
            normal: vec4,
            Proj::proj(vector: vec4) -> vec4 {
                vector - dot(vector, normal) * normal
            },
            Sdf::sdf(vector: vec4) -> f32 {
                dot(vector, normal)
            }
        }

        class Shift {
            shift: vec4,
            shape: Class,
            Proj::proj<shape: Proj>(vector: vec4) -> vec4 {
                shape.proj(vector - shift) + shift
            },
            Sdf::sdf<shape: Sdf>(vector: vec4) -> f32 {
                shape.sdf(vector - shift)
            }
        }

        class Union {
            shape1: Class,
            shape2: Class,
            Proj::proj<shape1: Proj + Sdf, shape2: Proj + Sdf>(vector: vec4) -> vec4 {
                shape1.proj((5 + shape2.sdf(vector)) * shape2.proj(vector)) * shape1.sdf(vector)
            }
        }

        class Combo {
            shape: Class,
            radius: f32
        } => Union {
            shape1: shape,
            shape2: Shift {
                shift: radius * vec4(1, 0, 0, 0),
                shape: Sphere4D { radius }
            }
        }
    "#,
        _ => ""
    }.to_string();

    let document = parse_document(&doc_str).unwrap();

    let structure_str = match opt2 {
        0 => r#"
            Union(
                shape1: Sphere4D( radius: 4 ),
                shape2: Shift(
                    shift: [4, 0, 0, 0],
                    shape: Plane4D( normal: [1, 0, 0, 0] )
                )
            )
        "#,
        1 => r#"
            Shift(
                shift: [4, 0, 0, 0],
                shape: Sphere4D( radius: 2 )
            )
        "#,
        2 => r#"
            Combo(
                shape: Plane4D( normal: [1, 2, 3, 4] ),
                radius: Expr("abc * 17 + 4")
            )
        "#,
        _ => ""
    };

    let structure = Structure::from_ron_string(structure_str).unwrap();

    (document, structure)
}

pub fn prettify_string(mut string: String) -> String {
    let mut brace_depth = 0;
    let mut paren_depth = 0;
    let mut i = 0;
    let mut insert_newline = false;
    const TAB: &'static str = "    ";

    while i < string.len() {
        let owned = string[i..i+1].to_string();
        let slice = &owned[..];

        match slice {
            "}" => {
                brace_depth -= 1;
                insert_newline = true;
            },
            "," | "=" => insert_newline = false,
            _ => {}
        };

        if paren_depth < 1 && insert_newline && slice != " " {
            if brace_depth == 0 && slice == "c" {
                string.insert_str(i, "\n");
                i += 1;
            }
            let inserted_str = format!("\n{}", TAB.repeat(brace_depth));
            string.insert_str(i, inserted_str.as_str());
            i += inserted_str.len();
        }

        match slice {
            "{" => {brace_depth += 1; insert_newline = true},
            "," => insert_newline = !insert_newline,
            ";" => insert_newline = true,
            "(" => {paren_depth += 1; insert_newline = false},
            ")" => {paren_depth -= 1; insert_newline = false},
            " " => {},
            _ => insert_newline = false
        };

        i += 1;
    }

    string
}

pub fn better_prettify(mut string: String) -> String {
    const TAB: &'static str = "    ";

    lazy_static! {
        static ref END_LINE_CHAR_REGEX: Regex = Regex::new(r"^(,|;|\{)[ \n\r\t]*").unwrap();
    }

    let mut delimiter_stack = Vec::new();
    let mut i = 0;
    let mut tabs = 0;

    while i < string.len() {
        match &string[i..i+1] {
            "(" => delimiter_stack.push('('),
            "[" => delimiter_stack.push('['),
            "{" => {
                tabs += 1;
                delimiter_stack.push('{')
            },
            ")" | "]" => { delimiter_stack.pop(); },
            "}" => {
                delimiter_stack.pop();
                tabs -= 1;
            }
            _ => {}
        };

        match delimiter_stack.last() {
            Some('{') | None => {
                if let Some(cap) = END_LINE_CHAR_REGEX.captures(&string[i..]) {
                    println!("{:?} -- rest: {:?} -- {}", &string[i..i+cap.len()], &string[i+cap.len()..i+cap.len()+8], cap.len());

                    if &string[i+cap.len()..i+cap.len()+1] != "}" {
                        let string2 = format!("\n{}", TAB.repeat(tabs));

                        string.replace_range(i+1..i+cap.len(), string2.as_str());

                        i += string2.len();
                    } else if &string[i..i+1] == "{" {
                        i += cap.len();
                    }
                }

                if string[i..].starts_with("}") {
                    let string2 = format!("\n{}", TAB.repeat(tabs));
                    string.insert_str(i, string2.as_str());
                    i += string2.len()+1;
                }
            },
            _ => {}
        };

        i += 1;
    }

    string
}