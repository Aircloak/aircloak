use std::mem::transmute;
use std::os::raw::c_void;
use std::ptr;

extern crate c_vec;
extern crate libc;
extern crate odbc;
extern crate simple_error;

use libc::*;
use c_vec::*;

mod erl_driver;
use erl_driver::*;

const ERL_DRV_ERROR_GENERAL: i32 = -1;

const COMMAND_CONNECT: u32 = 0;
const COMMAND_EXECUTE: u32 = 1;
const COMMAND_FETCH: u32 = 2;

const STATUS_OK: u8 = b'K';
const STATUS_ERROR: u8 = b'E';

mod state;
use state::*;

extern "C" fn start(port: ErlDrvPort, _command: *mut c_char) -> ErlDrvData {
    unsafe { set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY as c_int) };
    match State::new() {
        Ok(state) => Box::into_raw(Box::new(state)) as ErlDrvData,
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
    unsafe { Box::from_raw(drv_data as *mut State) };
}

unsafe fn reply(message: &[u8], rbuf: *mut *mut c_char, rlen: ErlDrvSizeT) -> ErlDrvSSizeT {
    if message.len() < rlen {
        ptr::copy(message.as_ptr(), *rbuf as *mut u8, message.len());
    } else {
        let binary = driver_alloc_binary(message.len());
        if binary as usize == 0 {
            println!(
                "Error when allocating binary of size {}: Out of memory!",
                message.len()
            );
            return 0;
        }
        ptr::copy(
            message.as_ptr() as *mut i8,
            &mut (*binary).orig_bytes[0],
            message.len(),
        );
        *rbuf = binary as *mut c_char;
    }
    message.len() as isize
}

extern "C" fn control(
    drv_data: ErlDrvData,
    command: c_uint,
    buf: *mut c_char,
    len: ErlDrvSizeT,
    rbuf: *mut *mut c_char,
    rlen: ErlDrvSizeT,
) -> ErlDrvSSizeT {
    let state = unsafe { &mut *(drv_data as *mut State) };
    let mut message = Vec::<u8>::new();
    match command {
        COMMAND_CONNECT => {
            let buf = unsafe { CVec::new(buf as *mut u8, len) };
            if let Err(error) = state.connect(&buf) {
                message.push(STATUS_ERROR);
                message.extend_from_slice(error.to_string().as_bytes());
            }
        }
        COMMAND_EXECUTE => {
            let buf = unsafe { CVec::new(buf as *mut u8, len) };
            if let Err(error) = state.execute(&buf) {
                message.push(STATUS_ERROR);
                message.extend_from_slice(error.to_string().as_bytes());
            }
        }
        COMMAND_FETCH => {
            if let Err(error) = state.fetch(&mut message) {
                message.push(STATUS_ERROR);
                message.extend_from_slice(error.to_string().as_bytes());
            }
        }
        _ => {
            let error = format!("Invalid command received: {}", command);
            message.push(STATUS_ERROR);
            message.extend_from_slice(error.as_bytes());
        }
    };
    unsafe { reply(&message, rbuf, rlen) }
}

static DRIVER_NAME: &'static [u8] = b"librodbc\0";

static mut DRIVER_ENTRY: ErlDrvEntry = erl_drv_entry {
    init: None,
    start: Some(start),
    stop: Some(stop),
    control: Some(control),
    output: None,
    ready_input: None,
    ready_output: None,
    driver_name: 0 as *mut c_schar, // set latter
    finish: None,
    handle: 0 as *mut c_void,
    timeout: None,
    outputv: None,
    ready_async: None,
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
pub extern "C" fn driver_init() -> *mut ErlDrvEntry {
    unsafe {
        DRIVER_ENTRY.driver_name = transmute(&DRIVER_NAME[0]);
        transmute::<&ErlDrvEntry, *mut ErlDrvEntry>(&DRIVER_ENTRY)
    }
}
