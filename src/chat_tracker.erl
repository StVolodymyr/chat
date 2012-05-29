-module(chat_tracker).

-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-export([start_link/0, login/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-record(entry, {
  name,
  ref,
  pid
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Name) ->
    {ok,Pid_} = case ets:lookup(?MODULE, Name) of
    [] -> gen_server:call(?MODULE, {login, Name});
    [#entry{pid = Pid}] -> {ok, Pid}
  end,
  {ok, Pid_}.

init([]) ->
    ets:new(?MODULE, [public,named_table,{keypos,#entry.name}]),
    {ok, #state{}}.

handle_call({login, Name}, _From, #state{} = State) ->
	Pid = case ets:lookup(?MODULE, Name) of
	[] ->
		{ok, Pid_} = chat_sup:start_session(Name, []),
		Ref = erlang:monitor(process, Pid_),
		ets:insert(?MODULE, #entry{name = Name, ref = Ref, pid = Pid_}),
		Pid_;
	[#entry{pid = Pid_}] ->
		Pid_
	end,
	{reply, {ok,Pid}, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
    
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Game, _Reason}, #state{} = State) ->
  ets:select_delete(?MODULE, ets:fun2ms(fun(#entry{pid = Pid}) when Game == Pid -> true end)),
  {noreply, State};

handle_info(_Info, State) ->
    {stop, {unknown_message, _Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
