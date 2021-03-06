%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created :  2 Oct 2013 by Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(supavizah).

-behaviour(supervisor).

%% API
-export([start_link/0,stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
stop() ->
    case whereis(?MODULE) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    SchedulerChild = {'scheduler_bridge', {scheduler_bridge, start_link, [scheduler]},
	      Restart, Shutdown, Type, ['scheduler_bridge']},
    TesterChild = {'tester_bridge', {tester_bridge, start_link, [tester]},
     	      Restart, Shutdown, Type, ['tester_bridge']},
    SymbolChild = {'symbols_bridge', {symbols_bridge, start_link, [symbols]},
    	      Restart, Shutdown, Type, ['symbols_bridge']},

    {ok, {SupFlags, [SchedulerChild,TesterChild,SymbolChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

