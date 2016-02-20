%% @author vinod
%% @doc @todo Add description to guardian_test.


-module(guardian_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

-compile({parse_transform, guardian}).
%% -compile(export_all).


%% ====================================================================
%% Internal functions
%% ====================================================================
%% string_state(String, SubString) ->
%% 	Fun = fun(guard1, String, SubString) ->
%% 				  unloaded;
%% 			 (guard2, String, SubString) ->
%% 				  loaded_complete;
%% 			 (guard3, String, SubString) ->
%% 				  loaded;
%% 			 (guard4, String, SubString) ->
%% 				  overloaded
%% 		  end,
%% 				  
%% 	
%% 	case length(String) == 0 of
%% 		true ->
%% 			Fun(1, String, SubString);
%% 		false ->
%% 			case string:equal(String, SubString) of
%% 				true ->
%% 					Fun(2, String, SubString);
%% 				false ->
%% 					case str(String, SubString) > 0 of
%% 						true ->
%% 							Fun(3, String, SubString);
%% 						false ->
%% 							Fun(4, String, SubString)
%% 					end
%% 			end
%% 	end.


%% string_state_guard(guard1, String, SubString) ->
%% 	unloaded;
%% string_state_guard(guard2, String, SubString) ->
%% 	loaded_complete;
%% string_state_guard(guard3, String, SubString) ->
%% 	loaded;
%% string_state_guard(guard4, String, SubString) ->
%% 	overloaded.


string_state(String, SubString) when length(String) == 0 ->
    case SubString == "ok" of
        true ->
            loaded;
        false ->
            unloaded
    end;
string_state(String, SubString) when string:equal(String, SubString) ->
	loaded_complete;
string_state(String, SubString) when str(String, SubString) > 0 ->
	loaded;
string_state(String, SubString) when length(String) == length(SubString) ->
	loaded;
string_state(String, SubString) when greetings(String, [vinod, SubString]) == {welcome, String} ->
    loaded;
string_state(_String, _SubString) ->
	overloaded.

str(String, SubString) ->
	string:str(String, SubString).

-spec test() -> {atom(), term()}.
test() ->
	string_state("hi", "h"),
	greetings(vinod, [vinod]).


greetings(Name, AllowedNames) when lists:member(Name, AllowedNames) ->
	{welcome, Name};
greetings(UnknownName, _AllowedNames) ->
	{leave, UnknownName}.
