-module(bank_sup).
-export([start/0, stop/0, init/0]).

start() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! terminate.

init() ->
    process_flag(trap_exit, true),
    {ok, SupervisedPid} = bank_atm:start_link(),
    main_loop(SupervisedPid).

main_loop(SupervisedPid) ->
    receive
        {'EXIT', SupervisedPid, Reason} ->
            error_logger:error_msg("Mybank process died for reason: ~s", [Reason]),
            {ok, SupervisedPidNEW} = bank_atm:start_link(),
            main_loop(SupervisedPidNEW);

        terminate ->
            bank_atm:stop()
    end.