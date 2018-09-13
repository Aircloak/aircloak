use std::os::raw::c_void;

extern crate c_vec;
extern crate libc;
extern crate odbc;
extern crate simple_error;

use c_vec::CVec;
use libc::{c_char, c_int, c_schar, c_uint};

mod erl_driver;
use erl_driver::*;

const ERL_DRV_ERROR_GENERAL: i32 = -1;

const COMMAND_CONNECT: u32 = 0;
const COMMAND_EXECUTE: u32 = 1;
const COMMAND_FETCH: u32 = 2;
const COMMAND_SET_FLAG: u32 = 3;
const COMMAND_GET_COLUMNS: u32 = 4;

const FLAG_WSTR_AS_BIN: u8 = 0;

const STATUS_OK: u8 = b'K';
const STATUS_ERROR: u8 = b'E';

mod connection_state;
use connection_state::*;

struct PortState<'a> {
    conn_state: ConnectionState<'a>,
    port: ErlDrvPort,
    // We keep call state globally per-port, to avoid extra memory allocations.
    // This means that we require to have sequential `port_control` / `receive` operations
    // in the parent Erlang process.
    command: u32,
    input: Vec<u8>,
    output: Vec<u8>,
}

extern "C" fn start(port: ErlDrvPort, _command: *mut c_char) -> ErlDrvData {
    unsafe { set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY as c_int) };
    match ConnectionState::new() {
        Ok(conn_state) => {
            let port_state = PortState {
                conn_state,
                port,
                command: 0,
                input: Vec::<u8>::new(),
                output: Vec::<u8>::new(),
            };
            Box::into_raw(Box::new(port_state)) as ErlDrvData
        }
        Err(Some(error)) => {
            println!("Error during port state initialization: {}", error);
            ERL_DRV_ERROR_GENERAL as ErlDrvData
        }
        Err(None) => {
            println!("Unknown error during port state initialization!");
            ERL_DRV_ERROR_GENERAL as ErlDrvData
        }
    }
}

extern "C" fn stop(drv_data: ErlDrvData) {
    unsafe { Box::from_raw(drv_data as *mut PortState) };
}

unsafe extern "C" fn ready_async(drv_data: ErlDrvData, _async_data: ErlDrvThreadData) {
    let port_state = &mut *(drv_data as *mut PortState);
    let output = &mut port_state.output;
    if driver_output(port_state.port, output.as_ptr() as *mut i8, output.len()) == -1 {
        panic!("Driver output failed!");
    }
}

extern "C" fn async_free(_async_data: *mut c_void) {}

extern "C" fn async_invoke(async_data: *mut c_void) {
    let port_state = unsafe { &mut *(async_data as *mut PortState) };
    let mut output = &mut port_state.output;
    let input = &port_state.input;
    let command = port_state.command;
    let conn_state = &mut port_state.conn_state;

    match command {
        COMMAND_CONNECT => if let Err(error) = conn_state.connect(&input) {
            output.push(STATUS_ERROR);
            output.extend_from_slice(error.to_string().as_bytes());
        },

        COMMAND_EXECUTE => if let Err(error) = conn_state.execute(&input) {
            output.push(STATUS_ERROR);
            output.extend_from_slice(error.to_string().as_bytes());
        },

        COMMAND_FETCH => if let Err(error) = conn_state.fetch(&mut output) {
            output.clear();
            output.push(STATUS_ERROR);
            output.extend_from_slice(error.to_string().as_bytes());
        },

        COMMAND_SET_FLAG => {
            let flag = input[0];
            match flag {
                FLAG_WSTR_AS_BIN => {
                    conn_state.wstr_as_bin = true;
                }
                _ => {
                    let error = format!("Invalid flag set: {}", flag);
                    output.push(STATUS_ERROR);
                    output.extend_from_slice(error.as_bytes());
                }
            }
        }

        COMMAND_GET_COLUMNS => if let Err(error) = conn_state.get_columns(&mut output) {
            output.clear();
            output.push(STATUS_ERROR);
            output.extend_from_slice(error.to_string().as_bytes());
        },

        _ => {
            let error = format!("Invalid command received: {}", command);
            output.push(STATUS_ERROR);
            output.extend_from_slice(error.as_bytes());
        }
    };
}

extern "C" fn control(
    drv_data: ErlDrvData,
    command: c_uint,
    buffer: *mut c_char,
    length: ErlDrvSizeT,
    _reply_buffer: *mut *mut c_char,
    _reply_length: ErlDrvSizeT,
) -> ErlDrvSSizeT {
    let port_state = unsafe { &mut *(drv_data as *mut PortState) };
    let buffer = unsafe { CVec::new(buffer as *mut u8, length) };

    port_state.output.clear();
    port_state.command = command;
    port_state.input.clear();
    port_state.input.extend_from_slice(buffer.as_ref());

    let async_result = unsafe { driver_async(port_state.port, 0 as *mut u32,
            Some(async_invoke), drv_data as *mut c_void, Some(async_free)) };
    if async_result == -1 {
        panic!("Driver async failed!");
    }

    0
}

static mut DRIVER_ENTRY: ErlDrvEntry = ErlDrvEntry {
    init: None,
    start: Some(start),
    stop: Some(stop),
    control: Some(control),
    output: None,
    ready_input: None,
    ready_output: None,
    driver_name: "librodbc\0" as *const str as *mut c_schar,
    finish: None,
    handle: 0 as *mut c_void,
    timeout: None,
    outputv: None,
    ready_async: Some(ready_async),
    flush: None,
    call: None,
    event: None,
    extended_marker: ERL_DRV_EXTENDED_MARKER as c_int,
    major_version: ERL_DRV_EXTENDED_MAJOR_VERSION as c_int,
    minor_version: ERL_DRV_EXTENDED_MINOR_VERSION as c_int,
    driver_flags: ERL_DRV_FLAG_USE_PORT_LOCKING as c_int,
    handle2: 0 as *mut c_void,
    process_exit: None,
    stop_select: None,
    emergency_close: None,
};

#[no_mangle]
pub unsafe extern "C" fn driver_init() -> *mut ErlDrvEntry {
    &mut DRIVER_ENTRY as *mut ErlDrvEntry
}
