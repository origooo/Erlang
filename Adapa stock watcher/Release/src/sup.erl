%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-195017.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc sup is the main supervisor responsible for starting and taking care
%%% of all the servers that the backend requires. They are:
%%% lse_server, nasdaq_server, dax_server, batcher_server, logger_server, symbol_server
%%% it also starts inets, crypto and ssl for use in the other servers / modules.
%%% @end
%%% Created :  5 Nov 2013 by Patrik Bäckström <origooo@dhcp-195017.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(sup).
-include("definitions.hrl").
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
	    exit(P,shutdown);
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
    inets:start(),
    crypto:start(),
    ssl:start(),
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

    FavouriteChild = {'nasdaq_server', {nasdaq_server, start_link, []},
	      Restart, Shutdown, Type, ['nasdaq_server']},
    SecondBestChild = {'lse_server', {lse_server, start_link, []},
     	      Restart, Shutdown, Type, ['lse_server']},
    PunchbagChild = {'dax_server', {dax_server, start_link, []},
    	      Restart, Shutdown, Type, ['dax_server']},
    LoggerChild = {'logger_server', {logger_server, start_link, []},
	      Restart, Shutdown, Type, ['logger_server']},
    BatcherChild = {'batcher_server', {batcher_server, start_link, []},
	      Restart, Shutdown, Type, ['batcher_server']},
    SymbolChild = {'symbol_server', {symbol_server, start_link, []},
	      Restart, Shutdown, Type, ['symbol_server']},

    {ok, {SupFlags, [FavouriteChild,SecondBestChild,PunchbagChild,LoggerChild,BatcherChild,SymbolChild]}}.
