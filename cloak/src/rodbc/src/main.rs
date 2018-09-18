use std::io;

extern crate odbc;
extern crate simple_error;

mod lib;
use lib::*;

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
