-module(chat_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    sync:go(),
    application:start(chat).

start(_StartType, _StartArgs) ->
    chat_sup:start_link().

stop(_State) ->
    ok.
