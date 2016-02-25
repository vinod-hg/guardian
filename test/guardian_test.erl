%% @author vinod
%% @doc @todo Add description to guardian_test.


-module(guardian_test).

%% ====================================================================
%% API functions
%% ====================================================================
%% -export([test/0]).

-compile({parse_transform, guardian}).
-compile(export_all).

-spec test() -> {atom(), term()}.
test() ->
    string_state("hi", "h"),
    greetings(vinod, [vinod]).

%% ====================================================================
%% Internal functions
%% ====================================================================

string_state("hi", SubString = "hi") when (length(SubString) orelse SubString == "hi") ,SubString == "hi" ->
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
string_state(_, "hi") ->
	overloaded.

str(String, SubString) ->
	string:str(String, SubString).

greetings(Name, AllowedNames) when lists:member(Name, AllowedNames) ->
    case {a} of
        _ ->
            ok
    end,
	{welcome, Name};
greetings(UnknownName, _AllowedNames) ->
	{leave, UnknownName}.


%% The above functions will be translated as below.
%% 
%% string_state("hi" = _GuardVariable1,
%%          (SubString = "hi") = _GuardVariable2) ->
%%     case (length(SubString) orelse SubString == "hi")
%%        andalso SubString == "hi"
%%     of
%%       true ->
%%       case SubString == "ok" of
%%         true -> loaded;
%%         false -> unloaded
%%       end;
%%       _ ->
%%       string_state_guardian1(_GuardVariable1, _GuardVariable2)
%%     end;
%% string_state(_GuardVariable1, _GuardVariable2) ->
%%     string_state_guardian1(_GuardVariable1,
%%                _GuardVariable2).
%% 
%% string_state_guardian1(String = _GuardVariable1,
%%                SubString = _GuardVariable2) ->
%%     case string:equal(String, SubString) of
%%       true -> loaded_complete;
%%       _ ->
%%       string_state_guardian2(_GuardVariable1, _GuardVariable2)
%%     end.
%% 
%% string_state_guardian2(String = _GuardVariable1,
%%                SubString = _GuardVariable2) ->
%%     case str(String, SubString) > 0 of
%%       true -> loaded;
%%       _ ->
%%       string_state_guardian3(_GuardVariable1, _GuardVariable2)
%%     end.
%% 
%% string_state_guardian3(String = _GuardVariable1,
%%                SubString = _GuardVariable2) ->
%%     case length(String) == length(SubString) of
%%       true -> loaded;
%%       _ ->
%%       string_state_guardian4(_GuardVariable1, _GuardVariable2)
%%     end.
%% 
%% string_state_guardian4(String = _GuardVariable1,
%%                SubString = _GuardVariable2) ->
%%     case greetings(String, [vinod, SubString]) ==
%%        {welcome, String}
%%     of
%%       true -> loaded;
%%       _ ->
%%       string_state_guardian5(_GuardVariable1, _GuardVariable2)
%%     end.
%% 
%% string_state_guardian5(_ = _GuardVariable1,
%%                "hi" = _GuardVariable2) ->
%%     overloaded.
%% 
%% str(String, SubString) -> string:str(String, SubString).
%% 
%% greetings(Name = _GuardVariable1,
%%       AllowedNames = _GuardVariable2) ->
%%     case lists:member(Name, AllowedNames) of
%%       true -> case {a} of _ -> ok end, {welcome, Name};
%%       _ ->
%%       greetings_guardian1(_GuardVariable1, _GuardVariable2)
%%     end.
%% 
%% greetings_guardian1(UnknownName = _GuardVariable1,
%%             _AllowedNames = _GuardVariable2) ->
%%     {leave, UnknownName}.