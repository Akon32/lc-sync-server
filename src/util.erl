-module(util).
-compile(export_all).
-export([start/0,stop/0,create_db/0,status/0]).

%% start() -> boolean()
start()->
	Apps=apps_list(),
	error_logger:info_msg("Util: starting applications: ~p~n",[Apps]),
	lists:foreach(fun ensure_running/1, Apps),
	status().

%% stop() -> void()
stop()->
	error_logger:info_msg("Util: stopping node~n",[]),
	init:stop().

%% status() -> boolean()
status()->
	R=is_running(syncserver),
	error_logger:info_msg("Util: status: ~p~n",[R]),
	R.

%% create_db() -> ok | already_exists | {error,Reason}
create_db()->
	R=db_impl_mnesia:init(),
	error_logger:info_msg("Util: create_db: ~p~n",[R]),
	case R of 
		{error,{_,{already_exists,_}}} -> already_exists;
		A -> A
	end.

ensure_running(Application)->
	Running=is_running(Application),
	if	Running -> 
			error_logger:info_msg(
				"Util: ensure_running: ~p is running~n",[Application]),
			ok;
		true ->
			R=application:start(Application),
			error_logger:info_msg(
				"Util: ensure_running: start ~p : ~p~n",[Application,R]),
			R
	end.

apps_list()->
	application:load(syncserver),
	{ok,Deps}=application:get_key(syncserver,applications),
	Deps++[syncserver].

is_running(Application)->
	lists:member(Application,
		lists:map(fun({A,_,_}) -> A end,application:which_applications())).


