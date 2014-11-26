-module(live_beam).
-behaviour(live_handler).

-export([created/1]).
-export([updated/1]).
-export([deleted/1]).

created(Filename) ->
	Module = list_to_atom(filename:basename(Filename, ".beam")),
	load(Module, "load new").

updated(Filename) ->
	Module = list_to_existing_atom(filename:basename(Filename, ".beam")),
	load(Module, "reload").

load(Module, MessagePrefix) ->
	case code:is_sticky(Module) of
		true -> ok;
		false ->
			io:format(MessagePrefix ++ " module ~p~n", [Module]),
			code:purge(Module),
			{module, Module} = code:load_file(Module)
	end.

deleted(Filename) ->
	Module = list_to_existing_atom(filename:basename(Filename, ".beam")),
	case code:is_sticky(Module) of
		true -> ok;
		false ->
			io:format("unload module ~p~n", [Module]),
			code:purge(Module),
			erlang:delete_module(Module)
	end.
