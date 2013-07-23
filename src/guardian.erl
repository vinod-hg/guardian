%% @author vinod
%% @doc @todo Add description to guardian.


-module(guardian).

-export([parse_transform/2]).
%% -compile(export_all).

-ifdef(debug).
-define(PRINT(PrintMsg), io:format("~n~p~n", [PrintMsg])).
-define(PRINT(PrintMsg, Params), io:format(PrintMsg, Params)).
-else.
-define(PRINT(PrintMsg), ok).
-define(PRINT(PrintMsg, Params), ok).
-endif.

%% ====================================================================
%% API functions
%% ====================================================================


parse_transform(Forms, _Options) ->
	?PRINT("Forms: ~p ~nOptions:~p~n~n", [Forms, _Options]),
	NewForms = get_new_forms(Forms),
	?PRINT("NewForms: ~p~n~n", [NewForms]),
	NewForms.

get_new_forms(Forms) ->
	lists:foldl(fun(Func = {function, _Line, _FunctionName, _Arity, Clauses}, FinalForms) ->
					  case check_clauses(Clauses) of
						  true ->
							  ?PRINT({true, _FunctionName}),
							  FinalForms ++ get_new_functions(Func);
						  false ->
							  ?PRINT({false, _FunctionName}),
							  FinalForms ++ [Func]
					  end;
				 (Any, FinalForms) ->
					  FinalForms ++ [Any]
			  end, [], Forms).

check_clauses([]) ->
	false;
check_clauses([{clause, _ClauseLineNo, _VarList, Conditions, _Body} | Clauses]) ->
	?PRINT({_ClauseLineNo, Conditions}),
	check_conditions(Conditions) orelse	check_clauses(Clauses).



check_conditions([]) ->
	false;
check_conditions([Condition | Rest]) ->
	check_condition(Condition) orelse check_conditions(Rest).

check_condition([]) ->
	false;
check_condition([{call, _LineNo, {remote,_, {_,_,Mod},{_,_,Fun}}, FunctionArgs } | Rest]) ->
  	?PRINT({call, Mod, Fun}),
	case is_allowed({Mod, Fun, length(FunctionArgs)}) of
		true ->
			check_condition(Rest);
		false ->
			true
	end;
check_condition([{call, _LineNo, {_,_, Fun}, FunctionArgs } | Rest]) ->
	?PRINT({call, Fun}),
	case is_allowed({erlang, Fun, length(FunctionArgs)}) of
		true ->
			check_condition(Rest);
		false ->
			true
	end;
check_condition([{op,_LineNo,_Operation,LeftHand,RightHand } | Rest]) ->
	?PRINT({op, LeftHand,RightHand}),
	check_condition([LeftHand]) orelse check_condition([RightHand]) orelse check_condition(Rest)
;
check_condition([_Any | Rest]) ->
	?PRINT({unknown,_Any}),
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

get_false_clause([], _NewFunction, _GuardNo) ->
	[];

get_false_clause([{clause, ClauseLine, Vars, [], _Body}], NewFunction, GuardNo) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 [{call,ClauseLine, {atom,ClauseLine, NewFunction},
	   [{atom,ClauseLine,get_guard(GuardNo)}] ++ Vars}]}];

get_false_clause([{clause, ClauseLine, Vars, [], Body} | Rest], NewFunction, GuardNo) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 get_case([{clause, ClauseLine, Vars, [[{atom,ClauseLine,true}]], Body} | Rest], NewFunction, GuardNo)}];

get_false_clause(Clauses = [{clause, ClauseLine, _Vars, [[_Cond]], _Body} | _], NewFunction, GuardNo) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 get_case(Clauses, NewFunction, GuardNo)}].
	 


is_allowed(Func) ->
	List = [{erlang,abs, 1},
			{erlang,binary_part,2},
			{erlang,binary_part,3},
			{erlang,bit_size,1},
			{erlang,byte_size,1},
			{erlang,element,2},
			{erlang,float,1},
			{erlang,hd,1},
			{erlang,is_atom,1},
			{erlang,is_binary,1},
			{erlang,is_bitstring,1},
			{erlang,is_boolean,1},
			{erlang,is_float,1},
			{erlang,is_function,1},
			{erlang,is_function,2},
			{erlang,is_integer,1},
			{erlang,is_list,1},
			{erlang,is_number,1},
			{erlang,is_pid,1},
			{erlang,is_port,1},
			{erlang,is_record,2},
			{erlang,is_record,3},
			{erlang,is_reference,1},
			{erlang,is_tuple,1},
			{erlang,length,1},
			{erlang,node,0},
			{erlang,node,1},
			{erlang,round,1},
			{erlang,self,0},
			{erlang,size,1},
			{erlang,tl,1},
			{erlang,trunc,1},
			{erlang,tuple_size,1}
		   ],
	lists:member(Func, List).

