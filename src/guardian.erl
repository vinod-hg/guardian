%% @author vinod
%% @doc @todo Add description to guardian.


-module(guardian).

-export([parse_transform/2]).
-compile(export_all).

-define(PRINT(PrintMsg), io:format("~n~p~n", [PrintMsg])).

%% ====================================================================
%% API functions
%% ====================================================================


parse_transform(Forms, _Options) ->
	%io:format("Forms: ~p ~nOptions:~p~n~n",[Forms, Options]),
	NewForms = get_new_forms(Forms),
	%io:format("NewForms: ~p~n~n", [NewForms]),
	NewForms.

get_new_forms(Forms) ->
	lists:foldl(fun(Func = {function, _Line, _FunctionName, _Arity, Clauses}, FinalForms) ->
					  case check_clauses(Clauses) of
						  true ->
							  FinalForms ++ get_new_functions(Func);
						  false ->
							  FinalForms ++ [Func]
					  end;
				 (Any, FinalForms) ->
					  FinalForms ++ [Any]
			  end, [], Forms).

check_clauses([]) ->
	false;
check_clauses([{clause, _ClauseLineNo, _VarList, Conditions, _Body} | Clauses]) ->
%% 	?PRINT({ClauseLineNo, Conditions}),
	check_conditions(Conditions) orelse	check_clauses(Clauses).



check_conditions([]) ->
	false;
check_conditions([Condition | Rest]) ->
	check_condition(Condition) orelse check_conditions(Rest).

check_condition([]) ->
	false;
check_condition([{call, _LineNo, {remote,_, {_,_,Mod},{_,_,Fun}}, FunctionArgs } | Rest]) ->
%%   	?PRINT({call, Mod, Fun}),
	case erlang:is_builtin(Mod, Fun, length(FunctionArgs)) of
		true ->
			check_condition(Rest);
		false ->
			true
	end;
check_condition([{call, _LineNo, {_,_, Fun}, FunctionArgs } | Rest]) ->
%% 	?PRINT({call, Fun}),
	case erlang:is_builtin(erlang, Fun, length(FunctionArgs)) of
		true ->
			check_condition(Rest);
		false ->
			true
	end;
check_condition([{op,_LineNo,_Operation,LeftHand,RightHand } | Rest]) ->
%% 	?PRINT({op, LeftHand,RightHand}),
	check_condition([LeftHand]) orelse check_condition([RightHand]) orelse check_condition(Rest)
;
check_condition([_Any | Rest]) ->
%% 	?PRINT({unknown,Any}),
	check_condition(Rest).

get_new_functions(Func) ->
	[get_case_func(Func), get_new_clauses_func(Func)].

get_new_clauses_func({function, Line, FunctionName, Arity, Clauses}) ->
	NewFunctionName = list_to_atom(atom_to_list(FunctionName) ++ "_guard"),
	{NewClauses, _} = 
	lists:foldl(fun({clause, ClauseLineNo, VarList, _Conditions, Body} , {ClausesAcc, GuardNo}) ->
						Atom = list_to_atom("guard" ++ integer_to_list(GuardNo)),
						NewClause = {clause, ClauseLineNo, [{atom, ClauseLineNo, Atom}] ++ VarList, [], Body},
						{[NewClause] ++ ClausesAcc, GuardNo + 1}
				end, {[], 1}, Clauses),
	{function, Line, NewFunctionName, Arity + 1, NewClauses}.

get_case_func({function, Line, FunctionName, Arity, Clauses = [{clause, _ClauseLine, Vars, _Cond, _Body}|_]}) ->
	NewFunctionName = list_to_atom(atom_to_list(FunctionName) ++ "_guard"),
	NewBody =  	 get_case(Clauses, NewFunctionName, 1),
	NewClause = [{clause, Line, Vars, [], NewBody}],	
	{function, Line, FunctionName, Arity, NewClause}.
	

get_guard(GuardNo) ->
	list_to_atom("guard" ++ integer_to_list(GuardNo)).

get_case([{clause, ClauseLine, Vars, [[Cond]], _Body} | Clauses], NewFunction, GuardNo) ->
	[{'case', ClauseLine, Cond, 
	  get_true_clause(ClauseLine, Vars, NewFunction, GuardNo) ++ 
		  get_false_clause(Clauses, NewFunction, GuardNo + 1)}].
	  
get_true_clause(ClauseLine, Vars, NewFunction, GuardNo) ->
	 [{clause, ClauseLine, [{atom, ClauseLine, true}], [],
	  [{call,ClauseLine,
                 {atom,ClauseLine, NewFunction},
                 [{atom,ClauseLine, get_guard(GuardNo)}] ++ Vars}]}].

get_false_clause([{clause, ClauseLine, Vars, [], _Body}], NewFunction, GuardNo) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 [{call,ClauseLine, {atom,ClauseLine, NewFunction},
	   [{atom,ClauseLine,get_guard(GuardNo)}] ++ Vars}]}];

get_false_clause([{clause, ClauseLine, Vars, [], Body} | Rest], NewFunction, GuardNo) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 get_case([{clause, ClauseLine, Vars, [[{atom,50,true}]], Body} | Rest], NewFunction, GuardNo)}];

get_false_clause(Clauses = [{clause, ClauseLine, _Vars, [[_Cond]], _Body} | _], NewFunction, GuardNo) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 get_case(Clauses, NewFunction, GuardNo)}].
	 



