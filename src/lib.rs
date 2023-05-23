mod _toml_parser;

use _toml_parser::{parse_toml, TomlValue};
use pyo3::create_exception;
use pyo3::prelude::*;
use std::collections::HashMap;

impl IntoPy<PyObject> for TomlValue {
    fn into_py(self, py: Python) -> PyObject {
        match self {
            TomlValue::String(s) => s.into_py(py),
            TomlValue::Integer(i) => i.into_py(py),
            TomlValue::Float(f) => f.into_py(py),
            TomlValue::Boolean(b) => b.into_py(py),
            TomlValue::OffsetDateTime(dt) => dt.into_py(py),
            TomlValue::LocalDateTime(dt) => dt.into_py(py),
            TomlValue::LocalDate(d) => d.into_py(py),
            TomlValue::LocalTime(t) => t.into_py(py),
            TomlValue::Array(a) => a.into_py(py),
            TomlValue::InlineTable(t) => t.into_py(py),
            TomlValue::Table(t) => t.into_py(py),
            TomlValue::ArrayTable(t) => t.into_py(py),
        }
    }
}

create_exception!(
    pyo3_toml_parser,
    TomlParserError,
    pyo3::exceptions::PyException
);

#[pyfunction]
fn parse_str(toml: &str) -> PyResult<HashMap<String, TomlValue>> {
    match parse_toml(toml) {
        Ok((_, map)) => Ok(map),
        Err(e) => Err(TomlParserError::new_err(e.to_string())),
    }
}

#[pyfunction]
fn parse(toml_path: &str) -> PyResult<HashMap<String, TomlValue>> {
    let toml = std::fs::read_to_string(toml_path)?;
    parse_str(&toml)
}

#[pymodule]
fn toml_parser(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse_str, m)?)?;
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    m.add("TomlParserError", py.get_type::<TomlParserError>())?;
    Ok(())
}
