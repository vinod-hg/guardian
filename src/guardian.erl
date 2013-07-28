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
							  % If there is a clause with not allowed function, then replace with case
							  ?PRINT({true, _FunctionName}),
							  FinalForms ++ get_new_functions(Func);
						  false ->
							  % If there is no clause with not allowed function, then do not change the function
							  ?PRINT({false, _FunctionName}),
							  FinalForms ++ [Func]
					  end;
				 (Any, FinalForms) ->
					  FinalForms ++ [Any]
			  end, [], Forms).


%% Check whether the function has guards that call not allowed functions
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


%% Change the function to case based.
get_new_functions(Func = {function, _Line, _FunctionName, Arity, _Clauses}) ->
	NewVarList = [ list_to_atom("GuardVariable" ++ integer_to_list(Seq)) || Seq <- lists:seq(1, Arity)],
	?PRINT(NewVarList),
	[get_case_func(Func, NewVarList)].

get_new_clauses_func({function, Line, _FunctionName, _Arity, Clauses}) ->
	{NewClauses, _} = 
	lists:foldl(fun({clause, ClauseLineNo, VarList, Conditions, Body} , {ClausesAcc, GuardNo}) ->
						Atom = list_to_atom("guard" ++ integer_to_list(GuardNo)),
						NewClause = {clause, ClauseLineNo, [{atom, ClauseLineNo, Atom}] ++ 
										 get_new_vars(VarList, Conditions, Body), [], Body},
						{[NewClause] ++ ClausesAcc, GuardNo + 1}
				end, {[], 1}, Clauses),
	[{match,Line, {var,Line,'Fun'}, {'fun',Line, {clauses, lists:reverse(NewClauses)}}}].

get_case_func(Func = {function, Line, FunctionName, Arity, Clauses = [{clause, _ClauseLine, _Vars, _Cond, _Body}|_]}, NewVarList) ->
	NewBody =  	get_new_clauses_func(Func) ++ get_case(Clauses, 'Fun', 1, NewVarList),
	NewClause = [{clause, Line, get_vars(NewVarList, Line), [], NewBody}],	
	{function, Line, FunctionName, Arity, NewClause}.
	

get_guard(GuardNo) ->
	list_to_atom("guard" ++ integer_to_list(GuardNo)).

get_case([{clause, ClauseLine, Vars, [[Cond]], _Body} | Clauses], NewFunction, GuardNo, NewVarList) ->
	[{'case', ClauseLine, get_new_cond(Cond, Vars, NewVarList), 
	  get_true_clause(ClauseLine, Vars, NewFunction, GuardNo, NewVarList) ++ 
		  get_false_clause(Clauses, NewFunction, GuardNo + 1, NewVarList)}].
	  
get_true_clause(ClauseLine, _Vars, NewFunction, GuardNo, NewVarList) ->
	 [{clause, ClauseLine, [{atom, ClauseLine, true}], [],
	  [{call,ClauseLine,
                 {var,ClauseLine, NewFunction},
                 [{atom,ClauseLine, get_guard(GuardNo)}] ++ get_vars(NewVarList, ClauseLine)}]}].

get_false_clause([], _NewFunction, _GuardNo, _NewVarList) ->
	[];

get_false_clause([{clause, ClauseLine, _Vars, [], _Body}], NewFunction, GuardNo, NewVarList) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 [{call,ClauseLine, {var,ClauseLine, NewFunction},
	   [{atom,ClauseLine,get_guard(GuardNo)}] ++ get_vars(NewVarList, ClauseLine)}]}];

get_false_clause([{clause, ClauseLine, Vars, [], Body} | Rest], NewFunction, GuardNo, NewVarList) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 get_case([{clause, ClauseLine, Vars, [[{atom,ClauseLine,true}]], Body} | Rest], NewFunction, GuardNo, NewVarList)}];

get_false_clause(Clauses = [{clause, ClauseLine, _Vars, [[_Cond]], _Body} | _], NewFunction, GuardNo, NewVarList) ->
	[{clause, ClauseLine, [{atom,ClauseLine,false}], [],
	 get_case(Clauses, NewFunction, GuardNo, NewVarList)}].
	 
%% convert from Variable Names to PT variable list
get_vars(VarList, LineNo) ->
	[ {var, LineNo, Var} || Var <- VarList].


%% Replace the variable names with the new Guard Variable names in the conditions
get_new_cond(Cond, Vars, NewVarList) ->
	VarZip = lists:zip([ Var || {_,_,Var} <- Vars], NewVarList),
	get_new_cond(Cond, VarZip).

get_new_cond(_Cond = {op, Line , Oper, LeftHand, RigHand}, VarZip) ->
	?PRINT(_Cond),
	{op, Line , Oper, get_new_cond(LeftHand, VarZip), get_new_cond(RigHand, VarZip)};

get_new_cond({call, LineNo, Remote = {remote,_, _,_}, VarList }, VarZip) ->
	{call, LineNo , Remote, replace_var(VarList, VarZip)};

get_new_cond({call, LineNo , Fun, VarList}, VarZip) ->
	{call, LineNo , Fun, replace_var(VarList, VarZip)};

get_new_cond(OtherCond, _VarZip) ->
	OtherCond.

%% Replace the Variable with the new variable	
replace_var(VarList, VarZip) ->
	?PRINT({VarList, VarZip}),
	lists:foldr(fun({var, Line, Var}, NewVarList) ->
						{_, NewVar} = lists:keyfind(Var, 1, VarZip),
						[{var, Line, NewVar} | NewVarList]
				end, [], VarList).
	
get_new_vars(VarList, Cond, Body) ->
	?PRINT({body, Cond, Body}),
	
%% 	?PRINT({Var, is_var_present(Var, Body)}),
	lists:foldr(fun({var,Line,Var}, NewVarList) ->
						case is_var_present(Var, Cond) andalso not is_var_present(Var, Body) of
							true ->
								[{var,Line,get_new_var(Var)}| NewVarList];
							false ->
								[{var,Line,Var} | NewVarList]
						end;
				   (AnyVar, NewVarList) ->
						[AnyVar, NewVarList]				
				end, [], VarList).

get_new_var(Var) ->
	list_to_atom("_" ++ atom_to_list(Var)).
	
	
is_var_present(Var, Var) ->
	true;
is_var_present(Var, Body) when is_list(Body) ->
	lists:foldl(fun(InnerBody, Present) ->
						is_var_present(Var, InnerBody) or Present
				end, false, Body);
is_var_present(Var, Body) when is_tuple(Body) ->
	is_var_present(Var, tuple_to_list(Body));
is_var_present(_Var, _Body) ->
	?PRINT(_Body),
	false.


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

