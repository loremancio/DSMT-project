%%%-------------------------------------------------------------------
%% @doc erlang_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_backend_sup:start_link().

stop(_State) ->
    ok.
