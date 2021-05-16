%%%-------------------------------------------------------------------
%% @doc dslr public API
%% @end
%%%-------------------------------------------------------------------

-module(dslr_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dslr_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
