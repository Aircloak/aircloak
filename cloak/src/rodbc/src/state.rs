use std::mem::transmute;
use std::str::from_utf8;
use std::error::Error;
use std::result;

use c_vec::*;

use odbc::*;
use odbc::ffi::SqlDataType::*;

use simple_error::*;

type GenError = result::Result<(), Box<Error>>;

const TYPE_NULL: u8 = 0;
const TYPE_I32: u8 = 1;
const TYPE_I64: u8 = 2;
const TYPE_F32: u8 = 3;
const TYPE_F64: u8 = 4;
const TYPE_BIN: u8 = 5;

pub struct State<'a> {
    stmt: Option<Statement<'a, 'a, Allocated, HasResult>>,
    conn: Option<Connection<'a>>,
    env: Environment<Version3>,
    field_types: Vec<u8>,
}

fn error(message: &str) -> GenError {
    Err(Box::new(SimpleError::new(message)))
}

impl<'a> State<'a> {
    pub fn new() -> result::Result<State<'a>, Option<DiagnosticRecord>> {
        let env = create_environment_v3()?;
        Ok(State {
            env: env,
            conn: None,
            stmt: None,
            field_types: Vec::new(),
        })
    }

    pub fn connect(&'a mut self, connection_string: &CVec<u8>) -> GenError {
        self.stmt = None;
        self.conn = None;
        let connection_string = from_utf8(connection_string.as_ref())?;
        let conn = self.env.connect_with_connection_string(connection_string)?;
        self.conn = Some(conn);
        Ok(())
    }

    fn field_type(t: ffi::SqlDataType) -> u8 {
        match t {
            SQL_INTEGER | SQL_SMALLINT | SQL_EXT_TINYINT | SQL_EXT_BIT => TYPE_I32,
            SQL_EXT_BIGINT => TYPE_I64,
            SQL_FLOAT | SQL_REAL => TYPE_F32,
            SQL_DOUBLE => TYPE_F64,
            _ => TYPE_BIN,
        }
    }

    pub fn execute(&'a mut self, statement_text: &CVec<u8>) -> GenError {
        self.stmt = None;
        self.field_types.clear();
        let statement_text = from_utf8(statement_text.as_ref())?;
        let conn = match self.conn {
            None => return error("Port not connected!"),
            Some(ref conn) => conn,
        };
        if let Data(stmt) = Statement::with_parent(conn)?.exec_direct(statement_text)? {
            let cols = stmt.num_result_cols()? as u16;
            for i in 1..(cols + 1) {
                let field_type = State::field_type(stmt.describe_col(i)?.data_type);
                self.field_types.push(field_type);
            }
            self.stmt = Some(stmt);
        }
        Ok(())
    }

    pub fn fetch(&'a mut self, buf: &mut Vec<u8>) -> GenError {
        let stmt = match self.stmt {
            None => return error("No statement executed!"),
            Some(ref mut stmt) => stmt,
        };
        if let Some(mut cursor) = stmt.fetch()? {
            buf.push(::STATUS_OK);
            for (i, field_type) in self.field_types.iter().enumerate() {
                let i = (i + 1) as u16;
                match *field_type {
                    TYPE_I32 => {
                        if let Some(val) = cursor.get_data::<i32>(i)? {
                            let raw_bytes: [u8; 4] = unsafe { transmute(val) };
                            buf.push(TYPE_I32);
                            buf.extend_from_slice(&raw_bytes);
                        } else {
                            buf.push(TYPE_NULL);
                        }
                    }
                    TYPE_I64 => {
                        if let Some(val) = cursor.get_data::<i64>(i)? {
                            let raw_bytes: [u8; 8] = unsafe { transmute(val) };
                            buf.push(TYPE_I64);
                            buf.extend_from_slice(&raw_bytes);
                        } else {
                            buf.push(TYPE_NULL);
                        }
                    }
                    TYPE_F32 => {
                        if let Some(val) = cursor.get_data::<f32>(i)? {
                            let raw_bytes: [u8; 4] = unsafe { transmute(val) };
                            buf.push(TYPE_F32);
                            buf.extend_from_slice(&raw_bytes);
                        } else {
                            buf.push(TYPE_NULL);
                        }
                    }
                    TYPE_F64 => {
                        if let Some(val) = cursor.get_data::<f64>(i)? {
                            let raw_bytes: [u8; 8] = unsafe { transmute(val) };
                            buf.push(TYPE_F64);
                            buf.extend_from_slice(&raw_bytes);
                        } else {
                            buf.push(TYPE_NULL);
                        }
                    }
                    TYPE_BIN => {
                        if let Some(val) = cursor.get_data::<&str>(i)? {
                            buf.push(TYPE_BIN);
                            let val_bytes = val.as_bytes();
                            let size_bytes: [u8; 4] = unsafe { transmute(val_bytes.len() as u32) };
                            buf.extend_from_slice(&size_bytes);
                            buf.extend_from_slice(val_bytes);
                        } else {
                            buf.push(TYPE_NULL);
                        }
                    }
                    _ => (), // unreachable
                };
            }
        }
        Ok(())
    }
}
