## Auditor Guidelines

The Lua sandbox is the code that executes Lua tasks in a sandbox per user.  The auditor may carefully inspect this code to ensure that the Lua tasks are indeed adequately sandboxed, and that the only communications is with the parent cloak-core process.

The sandbox is implemented as a set of C processes that are launched by cloak-core.  `job_communicator.c` contains the read call that pulls in the lua task (in subroutine `read_min_size`), and writes back to cloak-core (in subroutine `write_fixed_size`).  `lua_sandbox.c` contains the main loop, which calls `run_next_job()`, the main run cycle (reads command, sets up lua, runs lua, writes result, releases resources).
