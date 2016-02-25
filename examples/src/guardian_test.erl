%% @author vinod
%% @doc @todo Add description to guardian_test.


-module(guardian_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([test/0]).

-compile({parse_transform, guardian}).
%% -compile(export_all).

-spec test() -> {atom(), term()}.
test() ->
    string_state("hi", "h"),
    greetings(vinod, [vinod]).

%% ====================================================================
%% Internal functions
%% ====================================================================

string_state("hi", SubString) when length(SubString) andalso SubString == "hi" ->
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
string_state(_String, "hi") ->
	overloaded.

str(String, SubString) ->
	string:str(String, SubString).

greetings(Name, AllowedNames) when lists:member(Name, AllowedNames) ->
	{welcome, Name};
greetings(UnknownName, _AllowedNames) ->
	{leave, UnknownName}.


%% 
%% string_state(_GuardVariable1, _GuardVariable2) ->
%%     Fun = fun (1, _String, SubString) ->
%%           case SubString == "ok" of
%%             true -> loaded;
%%             false -> unloaded
%%           end;
%%           (2, _String, _SubString) -> loaded_complete;
%%           (3, _String, _SubString) -> loaded;
%%           (4, _String, _SubString) -> loaded;
%%           (5, _String, _SubString) -> loaded;
%%           (6, _String, _PatternVar1) -> overloaded
%%       end,
%%     case length(_GuardVariable1) == 0 of
%%       true -> Fun(1, _GuardVariable1, _GuardVariable2);
%%       false ->
%%       case string:equal(_GuardVariable1, _GuardVariable2) of
%%         true -> Fun(2, _GuardVariable1, _GuardVariable2);
%%         false ->
%%         case str(_GuardVariable1, _GuardVariable2) > 0 of
%%           true -> Fun(3, _GuardVariable1, _GuardVariable2);
%%           false ->
%%               case length(_GuardVariable1) == length(_GuardVariable2)
%%               of
%%             true -> Fun(4, _GuardVariable1, _GuardVariable2);
%%             false ->
%%                 case greetings(_GuardVariable1,
%%                        [vinod, _GuardVariable2])
%%                    == {welcome, _GuardVariable1}
%%                 of
%%                   true -> Fun(5, _GuardVariable1, _GuardVariable2);
%%                   false -> 
%%                      case _GuardVariable2 == "hi" of
%%                        true -> Fun(6, _GuardVariable1, _GuardVariable2)
%%                 end
%%               end
%%         end
%%       end
%%     end.
%% 
%% str(String, SubString) -> string:str(String, SubString).
%% 
%% greetings(_GuardVariable1, _GuardVariable2) ->
%%     Fun = fun (1, Name, _AllowedNames) -> {welcome, Name};
%%           (2, UnknownName, _AllowedNames) -> {leave, UnknownName}
%%       end,
%%     case lists:member(_GuardVariable1, _GuardVariable2) of
%%       true -> Fun(1, _GuardVariable1, _GuardVariable2);
%%       false -> Fun(2, _GuardVariable1, _GuardVariable2)
%%     end.
