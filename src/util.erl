-module(util).
-export([start/0,stop/0,create_db/0,status/0]).
-export([list2int/1,int2list/1]).
-export([marshal/1,unmarshal/1]).
-export([msg_head/1]).
-define(msg_head_maxlistcount,1).
-define(msg_head_maxlistlength,256).
-import(lists,[map/2,flatten/1,sublist/3,sublist/2,split/2,nthtail/2]).

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
		map(fun({A,_,_}) -> A end,application:which_applications())).

msg_head(Msg)->
	map(fun(X)->
		sublist(X,?msg_head_maxlistlength)
	end,sublist(Msg,?msg_head_maxlistcount)).

marshal(List) ->
	L = int2list(length(List)),
	LS = map(fun (X)->int2list(length(X)) end,List),
	L ++ flatten(LS) ++ flatten(List).

unmarshal(List)->
	L = list2int(sublist(List,1,4)),
	LS = lengths(sublist(List,1+4,4*L)),
	extractLists(LS,nthtail(4*(L+1),List)).

extractLists([],[])->[];
extractLists([L|LS],XS)->
	{X,Rest}=split(L,XS),
	[X|extractLists(LS,Rest)].

lengths([])->[];
lengths(XS)->
	{V,Rest}=split(4,XS),
	[list2int(V)|lengths(Rest)].

list2int(L)->
	<<I:32/integer>> =list_to_binary(L), I.
	
int2list(I)->
	binary_to_list(<<I:32/integer>>).


