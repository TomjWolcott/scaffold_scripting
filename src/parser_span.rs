use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;
use pest::Span;
use std::path::PathBuf;

#[macro_export]
macro_rules! define_span_wrapper {
    ($outer:ident, $inner:ident) => {

        #[derive(Debug, Clone)]
        pub struct $outer(pub $inner, pub SslSpan);

        impl $outer {
            pub fn span(&self) -> SslSpan {
                self.1.clone()
            }
        }

        impl std::ops::Deref for $outer {
            type Target = $inner;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl std::ops::DerefMut for $outer {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl std::fmt::Display for $outer {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        impl std::cmp::PartialEq for $outer {
            fn eq(&self, other: &Self) -> bool {
                self.0 == other.0
            }
        }

        impl From<$inner> for $outer {
            fn from(value: $inner) -> Self {
                $outer( value, SslSpan::empty() )
            }
        }
    }
}

#[derive(Debug)]
pub struct SslScript {
    pub source: String,
    pub path_opt: Option<PathBuf>
}

impl SslScript {
    pub fn new(source: String, path_opt: Option<PathBuf>) -> Self {
        SslScript {
            source,
            path_opt
        }
    }
}

#[derive(Clone)]
pub struct SslSpan {
    start_line_col: (usize, usize),
    start: usize,
    end_line_col: (usize, usize),
    end: usize,
    script: Arc<SslScript>
}
impl Debug for SslSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SslSpan")
            .field("start_line_col", &self.start_line_col)
            .field("start", &self.start)
            .field("end_line_col", &self.end_line_col)
            .field("end", &self.end)
            .field("script", &"...")
            .finish()
    }
}

impl SslSpan {
    pub fn empty() -> Self {
        SslSpan {
            start_line_col: (0, 0),
            start: 0,
            end_line_col: (0, 0),
            end: 0,
            script: Arc::new(SslScript::new("".to_string(), None))
        }
    }

    pub fn from_span(span: Span<'_>, script: Arc<SslScript>) -> Self {
        SslSpan {
            start_line_col: span.start_pos().line_col(),
            start: span.start(),
            end_line_col: span.end_pos().line_col(),
            end: span.end(),
            script
        }
    }

    pub fn context(&self) -> String {
        if self.script.source.len() == 0 {
            return "Err in compiled segment".to_string();
        }

        let path_str = if let Some(path) = &self.script.path_opt {
            format!("Err in file {} at", path.as_os_str().to_str().unwrap())
        } else {
            "Err at".to_string()
        };

        format!(
            "{path_str} {}:{} \"{}\"",
            self.start_line_col.0,
            self.start_line_col.1,
            &self.script.source[self.start..self.end]
        )
    }
}

impl Display for SslSpan {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.context())
    }
}