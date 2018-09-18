use std::error::Error;
use std::io::{self, Read, Write, ErrorKind};
use std::mem::transmute;
use std::result;
use std::str::from_utf8;

extern crate odbc;
extern crate simple_error;

use odbc::ffi::SqlDataType::*;
use odbc::*;

use simple_error::SimpleError;

// -------------------------------------------------------------------
//  Port IO data types
// -------------------------------------------------------------------

// Port communication mode is stream.
// Commands come in with the forms:
//      (type-u32, value-u32)
//      (type-u32, size-u32, data-bytes)

const COMMAND_CONNECT: u32 = 0;
const COMMAND_EXECUTE: u32 = 1;
const COMMAND_FETCH_ROWS: u32 = 2;
const COMMAND_SET_FLAG: u32 = 3;
const COMMAND_GET_COLUMNS: u32 = 4;
const COMMAND_STOP: u32 = 5;

const FLAG_WSTR_AS_BIN: u32 = 0;

// Replies go out under the forms:
//      (size-u32, STATUS_OK-u8)
//      (size-u32, STATUS_ERROR-u8, message-bytes)
//      (size-u32, STATUS_BIN-u8, data-bytes)
//      (size-u32, STATUS_ROW-u8, [(type-u8, value-bytes)])
//      (size-u32, STATUS_TABLE-u8, columns_count-u32, [(type-u8, value-bytes)])

const STATUS_OK: u8 = b'K';
const _STATUS_BIN: u8 = b'B';
const _STATUS_ROW: u8 = b'R';
const STATUS_TABLE: u8 = b'T';
const STATUS_ERROR: u8 = b'E';

const TYPE_NULL: u8 = 0;
const TYPE_I32: u8 = 1;
const TYPE_I64: u8 = 2;
const TYPE_F32: u8 = 3;
const TYPE_F64: u8 = 4;
const TYPE_STR: u8 = 5;
const TYPE_BIN: u8 = 6;

// -------------------------------------------------------------------
// Port IO functions
// -------------------------------------------------------------------

fn read_command(in_stream: &mut Read) -> io::Result<(u32, u32)> {
    let mut input = [0; 8];
    match in_stream.read_exact(&mut input) {
        Ok(()) => {
            let command = unsafe { transmute(input) };
            Ok(command)
        }
        Err(error) => {
            match error.kind() {
                ErrorKind::UnexpectedEof | ErrorKind::BrokenPipe => Ok((COMMAND_STOP, 0)),
                _ => Err(error),
            }
        },
    }
}

fn read_binary(in_stream: &mut Read, size: u32) -> io::Result<Vec<u8>> {
    let mut input = Vec::new();
    input.resize(size as usize, 0);
    in_stream.read_exact(&mut input)?;
    Ok(input)
}

fn write_error(output: &mut Vec<u8>, error: Box<Error>) {
    output.clear();
    output.push(STATUS_ERROR);
    output.extend_from_slice(error.to_string().as_bytes());
}

fn send_message(out_stream: &mut Write, message: &Vec<u8>) -> io::Result<()> {
    let message_size: [u8; 4] = unsafe { transmute(message.len() as u32) };
    out_stream.write_all(&message_size)?;
    out_stream.write_all(&message)?;
    out_stream.flush()?;
    Ok(())
}

fn send_ok(out_stream: &mut Write) -> io::Result<()> {
    out_stream.write_all(&[1, 0, 0, 0, STATUS_OK])?;
    out_stream.flush()?;
    Ok(())
}

// -------------------------------------------------------------------
// DB interaction functions
// -------------------------------------------------------------------

type GenError = result::Result<(), Box<Error>>;

fn error(message: &str) -> GenError {
    Err(Box::new(SimpleError::new(message)))
}

fn connect<'env>(
    env: &'env Environment<Version3>,
    connection_string: &Vec<u8>,
) -> result::Result<Connection<'env>, Box<Error>> {
    let connection_string = from_utf8(connection_string.as_ref())?;
    Ok(env.connect_with_connection_string(connection_string)?)
}

fn field_type(t: ffi::SqlDataType, wstr_as_bin: bool) -> u8 {
    match t {
        SQL_INTEGER | SQL_SMALLINT | SQL_EXT_TINYINT | SQL_EXT_BIT => TYPE_I32,
        SQL_EXT_BIGINT => TYPE_I64,
        SQL_REAL => TYPE_F32,
        SQL_FLOAT | SQL_DOUBLE => TYPE_F64,
        SQL_EXT_BINARY | SQL_EXT_VARBINARY | SQL_EXT_LONGVARBINARY => TYPE_BIN,
        SQL_EXT_WCHAR | SQL_EXT_WVARCHAR | SQL_EXT_WLONGVARCHAR =>
            if wstr_as_bin { TYPE_BIN } else { TYPE_STR},
        _ => TYPE_STR,
    }
}

struct ConnectionState<'conn> {
    stmt: Statement<'conn, 'conn, Allocated, HasResult>,
    field_types: Vec<u8>,
}

fn execute<'conn, 'env>(
    conn: &'conn Connection<'env>,
    state: &mut Option<ConnectionState<'conn>>,
    wstr_as_bin: bool,
    statement_text: &Vec<u8>,
) -> GenError {
    *state = None;
    let statement_text = from_utf8(statement_text.as_ref())?;
    if let Data(stmt) = Statement::with_parent(conn)?.exec_direct(statement_text)? {
        let cols = stmt.num_result_cols()? as u16;
        let mut field_types = Vec::with_capacity(cols as usize);
        for i in 1..(cols + 1) {
            let field_type = field_type(stmt.describe_col(i)?.data_type, wstr_as_bin);
            field_types.push(field_type);
        }
        *state = Some(ConnectionState { stmt, field_types });
    }
    Ok(())
}

fn push_binary(buf: &mut Vec<u8>, bytes: &[u8]) {
    let size_bytes: [u8; 4] = unsafe { transmute(bytes.len() as u32) };
    buf.extend_from_slice(&size_bytes);
    buf.extend_from_slice(&bytes);
}

fn write_field<'a, 'b, 'c, S>(
    field_type: u8,
    index: u16,
    cursor: &mut Cursor<'a, 'b, 'c, S>,
    buf: &mut Vec<u8>,
) -> GenError {
    match field_type {
        TYPE_I32 => if let Some(val) = cursor.get_data::<i32>(index)? {
            let raw_bytes: [u8; 4] = unsafe { transmute(val) };
            buf.push(TYPE_I32);
            buf.extend_from_slice(&raw_bytes);
        } else {
            buf.push(TYPE_NULL);
        },

        TYPE_I64 => if let Some(val) = cursor.get_data::<i64>(index)? {
            let raw_bytes: [u8; 8] = unsafe { transmute(val) };
            buf.push(TYPE_I64);
            buf.extend_from_slice(&raw_bytes);
        } else {
            buf.push(TYPE_NULL);
        },

        TYPE_F32 => if let Some(val) = cursor.get_data::<f32>(index)? {
            let raw_bytes: [u8; 4] = unsafe { transmute(val) };
            buf.push(TYPE_F32);
            buf.extend_from_slice(&raw_bytes);
        } else {
            buf.push(TYPE_NULL);
        },

        TYPE_F64 => if let Some(val) = cursor.get_data::<f64>(index)? {
            let raw_bytes: [u8; 8] = unsafe { transmute(val) };
            buf.push(TYPE_F64);
            buf.extend_from_slice(&raw_bytes);
        } else {
            buf.push(TYPE_NULL);
        },

        TYPE_STR => if let Some(val) = cursor.get_data::<&str>(index)? {
            buf.push(TYPE_STR);
            push_binary(buf, val.as_bytes());
        } else {
            buf.push(TYPE_NULL);
        },

        TYPE_BIN => if let Some(val) = cursor.get_data::<&[u8]>(index)? {
            buf.push(TYPE_BIN);
            push_binary(buf, val);
        } else {
            buf.push(TYPE_NULL);
        },

        _ => panic!("Unexpected field type!"), // unreachable
    };

    Ok(())
}

fn fetch_rows<'conn>(
    state: &mut Option<ConnectionState<'conn>>,
    batch_size: u32,
    buf: &mut Vec<u8>,
) -> GenError {
    let state = match *state {
        None => return error("No statement executed!"),
        Some(ref mut state) => state,
    };

    buf.push(STATUS_TABLE);
    let columns_count: [u8; 4] = unsafe { transmute(state.field_types.len() as u32) };
    buf.extend_from_slice(&columns_count);

    for _ in 0..batch_size {
        match state.stmt.fetch()? {
            None => break,
            Some(mut cursor) => {
                for (index, field_type) in state.field_types.iter().enumerate() {
                    write_field(*field_type, (index + 1) as u16, &mut cursor, buf)?;
                }
            }
        }
    }
    Ok(())
}

fn sql_type_to_string(t: ffi::SqlDataType) -> &'static str {
    match t {
        SQL_INTEGER | SQL_SMALLINT | SQL_EXT_TINYINT => "integer",
        SQL_EXT_BIGINT => "bigint",
        SQL_EXT_BIT => "bit",
        SQL_NUMERIC | SQL_DECIMAL => "numeric",
        SQL_REAL | SQL_FLOAT | SQL_DOUBLE => "float",
        SQL_CHAR | SQL_VARCHAR | SQL_EXT_LONGVARCHAR => "varchar",
        SQL_EXT_BINARY | SQL_EXT_VARBINARY | SQL_EXT_LONGVARBINARY => "binary",
        SQL_EXT_WCHAR | SQL_EXT_WVARCHAR | SQL_EXT_WLONGVARCHAR => "wvarchar",
        SQL_DATETIME => "datetime",
        SQL_TIMESTAMP => "timestamp",
        SQL_DATE => "date",
        SQL_TIME => "time",
        SQL_TIME_WITH_TIMEZONE | SQL_SS_TIME2 => "time",
        SQL_TIMESTAMP_WITH_TIMEZONE | SQL_SS_TIMESTAMPOFFSET => "timestamp",
        SQL_EXT_GUID => "guid",
        _ => "unknown",
    }
}

fn get_columns<'conn>(state: &mut Option<ConnectionState<'conn>>, buf: &mut Vec<u8>) -> GenError {
    let state = match state {
        None => return error("No statement executed!"),
        Some(ref mut state) => state,
    };

    buf.push(STATUS_TABLE);
    let columns_count = &[2, 0, 0, 0];
    buf.extend_from_slice(columns_count);

    let num_cols = state.stmt.num_result_cols()?;
    for i in 1..(num_cols + 1) {
        let column = state.stmt.describe_col(i as u16)?;

        buf.push(TYPE_STR);
        push_binary(buf, column.name.as_bytes());

        let column_type = sql_type_to_string(column.data_type);
        buf.push(TYPE_STR);
        push_binary(buf, column_type.as_bytes());
    }

    Ok(())
}

// -------------------------------------------------------------------
// Command execution loop
// -------------------------------------------------------------------

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdin_handle = stdin.lock();
    let stdout = io::stdout();
    let mut stdout_handle = stdout.lock();

    let env = odbc::create_environment_v3().expect("Couldn't create the ODBC environment!");

    let mut output = Vec::with_capacity(1024);

    let mut wstr_as_bin = false;

    loop {
        match read_command(&mut stdin_handle)? {
            (COMMAND_STOP, _) => break,
            (COMMAND_SET_FLAG, FLAG_WSTR_AS_BIN) => {
                wstr_as_bin = true;
                send_ok(&mut stdout_handle)?;
            }
            (COMMAND_CONNECT, size) => {
                let parameter = read_binary(&mut stdin_handle, size)?;
                match connect(&env, &parameter) {
                    Err(error) => {
                        write_error(&mut output, error);
                        send_message(&mut stdout_handle, &output)?;
                    }
                    Ok(conn) => {
                        send_ok(&mut stdout_handle)?;

                        let mut state = None;

                        loop {
                            output.clear();

                            match read_command(&mut stdin_handle)? {
                                (COMMAND_STOP, _) => break,
                                (COMMAND_EXECUTE, size) => {
                                    let parameter = read_binary(&mut stdin_handle, size)?;
                                    output.push(STATUS_OK);
                                    if let Err(error) = execute(&conn, &mut state, wstr_as_bin, &parameter) {
                                        write_error(&mut output, error);
                                    }
                                }
                                (COMMAND_GET_COLUMNS, _size) => {
                                    if let Err(error) = get_columns(&mut state, &mut output) {
                                        write_error(&mut output, error);
                                    }
                                }
                                (COMMAND_FETCH_ROWS, size) => {
                                    if let Err(error) = fetch_rows(&mut state, size, &mut output) {
                                        write_error(&mut output, error);
                                    }
                                }
                                (command, param) =>
                                    panic!(format!("Unexpected command while connected: {} {}!", command, param)),
                            }

                            send_message(&mut stdout_handle, &output)?;
                        }
                        break;
                    }
                }
            }
            (command, param) =>
                panic!(format!("Unexpected command while not connected: {} {}!", command, param)),
        }
    }

    Ok(())
}
