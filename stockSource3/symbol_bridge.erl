%%%-------------------------------------------------------------------
%%% @author Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%% @copyright (C) 2013, Patrik Bäckström
%%% @doc
%%%
%%% @end
%%% Created :  2 Oct 2013 by Patrik Bäckström <origooo@dhcp-203053.eduroam.chalmers.se>
%%%-------------------------------------------------------------------
-module(symbol_bridge).

-behaviour(supervisor_bridge).

%% API
-export([start_link/1,stop/0]).

%% supervisor_bridge callbacks
-export([init/1, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {mod,pid}).

%%%===================================================================
%%% API
%%%===================================================================
stop() ->
    case whereis(?MODULE) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ -> ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor bridge
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Mod) ->
    supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, Mod).

%%%===================================================================
%%% supervisor_bridge callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a supervisor_bridge process, linked to the calling process,
%% which calls Module:init/1 to start the subsystem. To ensure a
%% synchronized start-up procedure, this function does not return
%% until Module:init/1 has returned.
%%
%% @spec init(Args) -> {ok, Pid, State} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Mod) ->
    process_flag(trap_exit,true),
    case Mod:start_link() of
	{ok, Pid} ->
	    {ok, Pid, #state{mod=Mod,pid=Pid}};
	Error ->
	    Error
		end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by the supervisor_bridge when it is about
%% to terminate. It should be the opposite of Module:init/1 and stop
%% the subsystem and do any necessary cleaning up.The return value is
%% ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{mod=Mod,pid=_Pid}) ->
    Mod:stop(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
