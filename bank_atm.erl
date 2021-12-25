-module(bank_atm).
-export([start_link/0, init/0, stop/0, deposit/2, balance/1, withdraw/2]).

-record(state, {accounts}).

%% API
start_link() ->
    io:format("Welcome to the bank!~n"),
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

stop() ->
    ?MODULE ! terminate.

deposit(AccountId, Amount) ->
    ?MODULE ! {deposit, self(), AccountId, Amount},
    receive
        Reply -> Reply
    after 5000 ->
        {error, timeout}
    end.

withdraw(AccountId, Amount) ->
    ?MODULE ! {withdraw, self(), AccountId, Amount},
    receive
        Reply -> Reply
    after 5000 ->
        {error, timeout}
    end.

balance(AccountId) ->
    ?MODULE ! {balance, self(), AccountId},
    receive
        Reply -> Reply
    after 5000 ->
        {error, timeout}
    end.

%% INTERNAL
init() ->
    Accounts = dict:new(),
    State = #state{accounts = Accounts},
    main_loop(State).

main_loop(#state{
    accounts = Accounts
} = State) -> 

    receive
        
        {deposit, CallerPid ,AccountId, Amount} ->
            CurrentBalance = get_current_balance(AccountId, Accounts),
            Accounts1 = dict:store(AccountId, CurrentBalance + Amount, Accounts),
            io:format("Account ~p: ~p~n", [AccountId, CurrentBalance + Amount]),
            CallerPid ! ok,
            main_loop(State#state{ accounts = Accounts1});

        {withdraw, CallerPid, AccountId, Amount} ->
            case get_current_balance(AccountId, Accounts) of
                CurrentBalance when Amount =< CurrentBalance ->
                    Accounts1 = dict:store(AccountId, CurrentBalance - Amount, Accounts),
                    io:format("Account after withdrawn ~p: ~p~n", [AccountId, CurrentBalance - Amount]),
                    CallerPid ! get_current_balance(AccountId, Accounts1),
                    main_loop(State#state{ accounts = Accounts1});
                _ -> io:format("Insufficient funds~n"),
                    main_loop(State)
            end;
            
        
        {balance, CallerPid, AccountId} ->
            CurrentBalance = get_current_balance(AccountId, Accounts),
            io:format("Account balance ~p: ~p~n", [AccountId, CurrentBalance]),
            CallerPid ! CurrentBalance,
            main_loop(State);

        terminate ->
            io:format("Bank closed!~n")
    end.

get_current_balance(AccountId, Accounts) ->
    case dict:find(AccountId, Accounts) of
        error -> 0;
        {ok, Amount0} -> Amount0
    end.