-module(live_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	handlers = [] :: [{string(), module()}],
	cache = [] :: [{string(), calendar:datetime()}],
	timeout = undefined :: timeout()
}).

-include_lib("kernel/include/file.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
	Handlers = [{code:lib_dir() ++ "/*/ebin/*.beam", live_beam}],
	Cache = populate(Handlers, []),
	Timeout = 5000,
	erlang:send_after(Timeout, self(), live),
	{ok, #state{handlers=Handlers, cache=Cache, timeout=Timeout}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(live, State=#state{handlers=Handlers, cache=Cache, timeout=Timeout}) ->
	Cache2 = live(Handlers, Handlers, Cache, Cache),
	erlang:send_after(Timeout, self(), live),
	{noreply, State#state{cache=Cache2}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

populate([], Cache) ->
	Cache;
populate([{Wildcard, _}|Tail], Cache) ->
	Files = filelib:wildcard(Wildcard),
	Cache2 = populate_files(Files, Cache),
	populate(Tail, Cache2).

populate_files([], Cache) ->
	Cache;
populate_files([Filename|Tail], Cache) ->
	case lists:keymember(Filename, 1, Cache) of
		true ->
			populate_files(Tail, Cache);
		false ->
			{ok, #file_info{mtime=MTime}} = file:read_file_info(Filename),
			populate_files(Tail, [{Filename, MTime}|Cache])
	end.

live([], AllHandlers, AllCache, FilteredCache) ->
	DeletedFiles = [F || {F, _} <- FilteredCache, {error, enoent} =:= file:read_file_info(F)],
	live_deleted(DeletedFiles, AllHandlers, AllCache);
live([{Wildcard, Handler}|Tail], AllHandlers, AllCache, UnusedCache) ->
	Files = filelib:wildcard(Wildcard),
	{AllCache2, UnusedCache2} = live_files(Files, AllCache, UnusedCache, Handler),
	live(Tail, AllHandlers, AllCache2, UnusedCache2).

live_files([], AllCache, UnusedCache, _) ->
	{AllCache, UnusedCache};
live_files([Filename|Tail], AllCache, UnusedCache, Handler) ->
	%% @todo raw in 18.0
	{ok, #file_info{mtime=MTime}} = file:read_file_info(Filename),
	case lists:keymember(Filename, 1, AllCache) of
		false ->
			spawn(fun() -> Handler:created(Filename) end),
			live_files(Tail, [{Filename, MTime}|AllCache], UnusedCache, Handler);
		true ->
			case lists:keyfind(Filename, 1, AllCache) of
				{_, MTime} ->
					live_files(Tail, AllCache,
						lists:keydelete(Filename, 1, UnusedCache),
						Handler);
				_ ->
					spawn(fun() -> Handler:updated(Filename) end),
					live_files(Tail,
						lists:keyreplace(Filename, 1, AllCache, {Filename, MTime}),
						lists:keydelete(Filename, 1, UnusedCache),
						Handler)
			end
	end.

live_deleted([], _, Cache) ->
	Cache;
live_deleted([Filename|Tail], Handlers, Cache) ->
	live_deleted_file(Filename, Handlers, filename:split(Filename)),
	live_deleted(Tail, Handlers, lists:keydelete(Filename, 1, Cache)).

live_deleted_file(_, [], _) ->
	ok;
live_deleted_file(Filename, [{Wildcard, Handler}|Tail], FilenameSegments) ->
	_ = case match_deleted(FilenameSegments, filename:split(Wildcard)) of
		true -> spawn(fun() -> Handler:deleted(Filename) end);
		false -> ok
	end,
	live_deleted_file(Filename, Tail, FilenameSegments).

match_deleted([], []) ->
	true;
match_deleted([], _) ->
	false;
match_deleted(_, []) ->
	false;
match_deleted([Same|T1], [Same|T2]) ->
	match_deleted(T1, T2);
match_deleted([_|T1], ["*"|T2]) ->
	match_deleted(T1, T2);
match_deleted([FS|T1], [WS|T2]) ->
	case lists:member($*, WS) of
		true ->
			RE = re:replace(re:replace(WS, "\\.", "\\\\.", [global]), "\\*", ".*", [global]),
			case re:run(FS, RE) of
				{match, [{0, L}]} when L =:= length(FS) ->
					match_deleted(T1, T2);
				_ ->
					false
			end;
		false ->
			false
	end.
