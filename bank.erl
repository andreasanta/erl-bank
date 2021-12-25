-module(bank).

-export([start/0, stop/0, balance/1, deposit/2, withdraw/2]).

start() ->
    bank_sup:start().

stop() ->
    bank_sup:stop().

deposit(AccountId, Amount) ->
    bank_atm:deposit(AccountId, Amount).

balance(AccountId) ->
    bank_atm:balance(AccountId).

withdraw(AccountId, Amount) ->
    bank_atm:withdraw(AccountId, Amount).