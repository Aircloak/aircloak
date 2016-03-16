%% Place for debugging helpers

% Helper macro for term inspection. Evaluates the given term, prints the result, and returns it.
% This can be useful, when you need to print a result of some statement, without disturbing
% the program flow. For example, consider following code:
%   A = some_fun(...),
%   ...
%
% If you need to inspect the result of some_fun, you can use:
%   A = ?DBG_INSPECT(some_fun(...)),
%   ...
%
% The output contains the module, line number, and literal passed to DBG_INSPECT.
% The output is color coded, for better distinction.
% Macro returns the result of the input term, which keeps the program correct.
%
% Notice that there are two versions of the macro. The one arguments version just uses the default
% group leader. The two arguments version accepts the group leader. This can be useful in tests
% that normally capture I/O. From tests, you can call ?DBG_INSPECT(user, ...) to see the result printed.
-define(DBG_INSPECT(Term), ?DBG_INSPECT(group_leader(), Term)).
-define(DBG_INSPECT(IoDevice, Term),
      (
        fun() ->
          DBG_INSPECT_Res = Term,
          io:format(IoDevice,
                "~n\e[32;mINSPECT ~p (line ~p):~n  ~s~n\e[36;m  ~p~n~n\e[0;m",
                [?MODULE, ?LINE, ??Term, DBG_INSPECT_Res]
              ),
          DBG_INSPECT_Res
        end
      )()
    ).


% Helper for profiling some function call. This can be useful for development and testing.
% It runs a function through fprof, and prints the result on console. In addition, profiled
% results are stored to /tmp/profile.analyse so they can be inspected in a text editor.
% The result of the macro is the result of function, so it doesn't break the flow of the
% program.
%
% Usage:
%   ?DBG_PROFILE(some_fun(...))
%
% If you need to use another iodevice (e.g. from tests), pass it as the first argument:
%   ?DBG_PROFILE(user, some_fun(...)).
%
% Limitations: you can't run it concurrently - only one instance of fprof can be running
% at any point in time.
-define(DBG_PROFILE(Term), ?DBG_PROFILE(group_leader(), Term)).
-define(DBG_PROFILE(IoDevice, Term), (fun() ->
      filelib:ensure_dir("/tmp/profile/"),
      DBG_PROFILE_TraceFileName = "/tmp/profile.trace",
      DBG_PROFILE_AnalyseFileName = "/tmp/profile.analyse",
      DBG_PROFILE_Result = fprof:apply(fun() -> Term end, [], [{file, DBG_PROFILE_TraceFileName}]),
      fprof:profile({file, DBG_PROFILE_TraceFileName}),
      fprof:analyse([{dest, DBG_PROFILE_AnalyseFileName}]),
      file:delete(DBG_PROFILE_TraceFileName),
      {ok, Contents} = file:read_file(DBG_PROFILE_AnalyseFileName),
      io:format(IoDevice, "~n~n~s", [Contents]),
      io:format(IoDevice, "Profile results are stored in ~s~n~n", [DBG_PROFILE_AnalyseFileName]),
      DBG_PROFILE_Result
    end)()).