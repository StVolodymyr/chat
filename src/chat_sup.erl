
-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_session/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_session(Name, Options) ->
    supervisor:start_child(user_session_sup, [Name, Options]).
%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([user_session]) ->
  {ok, {{simple_one_for_one, 1000, 1000}, [
    {   undefined,                               % Id       = internal id
    {user_session,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};
  
init([]) ->
    Children = [
  {   chat_tracker_sup,
      {chat_tracker,start_link,[]},
      permanent,                           % Restart  = permanent | transient | temporary
      1000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      worker,                              % Type     = worker | supervisor
      [chat_tracker]                                       % Modules  = [Module] | dynamic
  },
  {user_session_sup,
      {supervisor,start_link,[{local, user_session_sup}, ?MODULE, [user_session]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  }
  ],
    {ok, { {one_for_one, 5, 10}, Children} }.

