%% @author vinod
%% @doc @todo Add description to guardian_test.


-module(guardian_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

-compile({parse_transform, guardian}).
-compile(export_all).



%% ====================================================================
%% Internal functions
%% ====================================================================
%% string_state(String, SubString) ->
%% 	case length(String) == 0 of
%% 		true ->
%% 			string_state_guard(guard1, String, SubString);
%% 		false ->
%% 			case string:equal(String, SubString) of
%% 				true ->
%% 					string_state_guard(guard2, String, SubString);
%% 				false ->
%% 					case str(String, SubString) > 0 of
%% 						true ->
%% 							string_state_guard(guard3, String, SubString);
%% 						false ->
%% 							string_state_guard(guard4, String, SubString)
%% 					end
%% 			end
%% 	end.
%% 
%% 
%% string_state_guard(guard1, String, SubString) ->
%% 	unloaded;
%% string_state_guard(guard2, String, SubString) ->
%% 	loaded_complete;
%% string_state_guard(guard3, String, SubString) ->
%% 	loaded;
%% string_state_guard(guard4, String, SubString) ->
%% 	overloaded.


string_state(String, SubString) when length(String) == 0 ->
	case true of
		true ->
			ok
	end,
	unloaded;
string_state(String, SubString) when string:equal(String, SubString) ->
	loaded_complete;
string_state(String, SubString) when str(String, SubString) > 0 ->
	loaded;
string_state(String, SubString) ->
	overloaded.

str(String, SubString) ->
	string:str(String, SubString).

test() ->
	string_state("hi", "h").
