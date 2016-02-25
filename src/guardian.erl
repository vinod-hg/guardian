%% @author vinod
%% @doc @todo Add description to guardian.


-module(guardian).

-export([parse_transform/2]).
%% -compile(export_all).

-define(LOG(Level, PrintMsg, Params), io:format("[~p][~p] " ++ PrintMsg, [Level, ?LINE] ++ Params)).
-define(LOG(Level, Params), ?LOG(Level, "~p~n", [Params])).

-define(LOG_ERROR(PrintMsg), ?LOG(error, PrintMsg)).
-define(LOG_ERROR(PrintMsg, Params), ?LOG(error, PrintMsg, Params)).

-ifdef(debug).
-define(LOG_DEBUG(PrintMsg), ?LOG(debug, PrintMsg)).
-define(LOG_DEBUG(PrintMsg, Params), ?LOG(debug, PrintMsg, Params)).
-else.
-define(LOG_DEBUG(PrintMsg), ok).
-define(LOG_DEBUG(PrintMsg, Params), ok).
-endif.


-define(GUARD_VAR_START, "_GuardVariable").
-define(FUN, "_guardian").

%% ====================================================================
%% API functions
%% ====================================================================

-spec parse_transform([erl_parse:abstract_form()], [compile:option()]) -> [erl_parse:abstract_form()].
parse_transform(Forms, _Options) ->
    ?LOG_ERROR("Forms: ~p ~nOptions:~p~n~n", [Forms, _Options]),
    %[io:format("Form: ~p~n", [Form]) || Form <- Forms],
    NewForms = get_new_forms(Forms),
    ?LOG_ERROR("NewForms: ~p~n~n", [NewForms]),
    ?LOG_ERROR("~s~n", [erl_prettypr:format(erl_syntax:form_list(NewForms))]),
    NewForms.

get_new_forms(Forms) ->
    lists:reverse(
      lists:foldl(fun(Func = {function, _Line, _FunctionName, _Arity, Clauses}, FinalForms) ->
                          case is_clauses_bif(Clauses) of
                              false ->
                                  % If there is a clause with not allowed function, then replace with case
                                  ?LOG_DEBUG({true, _FunctionName}),
                                  get_new_function(Func) ++ FinalForms;
                              true ->
                                  % If there is no clause with not allowed function, then do not change the function
                                  ?LOG_DEBUG({false, _FunctionName}),
                                  [Func | FinalForms]
                          end;
                     (Any, FinalForms) ->
                          [Any | FinalForms]
                  end, [], Forms)).


%% Check whether the function has guards that call not allowed functions
is_clauses_bif([]) ->
    true;
is_clauses_bif([{clause, _ClauseLineNo, _VarList, Conditions, _Body} | Clauses]) ->
    ?LOG_DEBUG({_ClauseLineNo, Conditions}),
    is_conditions_bif(Conditions) andalso is_clauses_bif(Clauses).

is_conditions_bif([]) ->
    true;
is_conditions_bif([Condition | Rest]) ->
    is_condition_bif(Condition) andalso is_conditions_bif(Rest).

is_condition_bif([]) ->
    false;
is_condition_bif([{call, _LineNo, {remote,_, {_,_,Mod},{_,_,Fun}}, FunctionArgs } | Rest]) ->
    ?LOG_DEBUG({call, Mod, Fun}),
    is_guard_bif(Mod, Fun, length(FunctionArgs)) andalso is_condition_bif(Rest);
is_condition_bif([{call, _LineNo, {_,_, Fun}, FunctionArgs } | Rest]) ->
    ?LOG_DEBUG({call, Fun}),
    is_guard_bif(Fun, length(FunctionArgs)) andalso is_condition_bif(Rest);
is_condition_bif([{op,_LineNo,_Operation,LeftHand,RightHand } | Rest]) ->
    ?LOG_DEBUG({op, LeftHand,RightHand}),
    is_condition_bif([LeftHand]) andalso is_condition_bif([RightHand]) andalso is_condition_bif(Rest);
is_condition_bif([_Any | Rest]) ->
    ?LOG_ERROR({unknown,_Any}),
    is_condition_bif(Rest).


%% Change the function to case based. No of variables generated = Arity of the function.
get_new_function({function, Line, FunctionName, Arity, Clauses}) ->
    GenVarList = [ {var, Line, list_to_atom(?GUARD_VAR_START ++ integer_to_list(Seq))} || Seq <- lists:seq(1, Arity)],
    ?LOG_DEBUG(GenVarList),
%%     PatternClauses = replace_pattern(Clauses, GenVarList),
%%     NewBody =  	get_new_clauses_func(Line, PatternClauses) ++ get_case(PatternClauses, 'Fun', 1, GenVarList),
%%     NewBody = fun_to_case(Clauses, GenVarList),
    to_case(Clauses, GenVarList, FunctionName, 1, length(Clauses), []).

%% Move the header pattern to guards
%% replace_pattern(Clauses, GenVarList) ->
%%     lists:reverse(lists:foldl(
%%                     fun({clause, ClauseLineNo, PatternVarList, Conditions, Body} , ClausesAcc) ->
%%                             ?LOG_ERROR({PatternVarList, Conditions, Body}),
%%                             {NewVarList, ExtraCond} = patterns(PatternVarList, GenVarList),
%%                             NewClause = {clause, ClauseLineNo, NewVarList,
%%                                          {ExtraCond, Conditions}, Body},
%%                             [NewClause | ClausesAcc]
%%                     end, [], Clauses)).
    

%% Get an anonymous function based on the original function. Each one uniquely identified by th guard atom
%% get_new_clauses_func(Line, Clauses) ->
%%     {NewClauses, _} = 
%%         lists:foldl(fun({clause, ClauseLineNo, VarList, Conditions, Body} , {ClausesAcc, GuardNo}) ->
%%                             ?LOG_ERROR({VarList, Conditions, Body}),
%%                             NewClause = {clause, ClauseLineNo, [{integer, ClauseLineNo, GuardNo}] ++ 
%%                                              get_new_vars(VarList, Conditions, Body), [], Body},
%%                             {[NewClause] ++ ClausesAcc, GuardNo + 1}
%%                     end, {[], 1}, Clauses),
%%     [{match,Line, {var,Line,'Fun'}, {'fun',Line, {clauses, lists:reverse(NewClauses)}}}].


-define(CLAUSE(ClauseLine, Bool, ClauseBody), [{clause, ClauseLine, [{atom, ClauseLine, Bool}], [], ClauseBody}]).

%% Generate case clauses based on the function clauses.

to_case([Clause| Clauses], NewVarList, FunName, FunNum, TotalFuns, Funs) ->
    to_case(Clauses, NewVarList, FunName, FunNum + 1, TotalFuns,
            [get_fun(Clause, NewVarList, FunName, FunNum, TotalFuns) | Funs]);
to_case([], _NewVarList, _FunName, _FunNum, _TotalFuns, Funs) ->
    Funs.

get_fun(Clause, NewVarList, Fun, FunNum, TotalFuns) ->
    {function, get_line_no(Clause), get_guard_fun(Fun, FunNum-1), length(NewVarList),
     [{clause, get_line_no(Clause), NewVarList, [], get_case(Clause, NewVarList, Fun, FunNum, TotalFuns)}]}.

get_case({clause, ClauseLine, Vars, _Conds, _Body} = Clause, NewVarList, Fun, FunNum, TotalFuns) ->
    [{'case', ClauseLine, {tuple, ClauseLine, NewVarList},
      [{clause, ClauseLine, [{tuple, ClauseLine, Vars}], [],
         get_condition_case(Clause, NewVarList, Fun, FunNum, TotalFuns)}] ++ 
       get_next_fun_pattern_clause(Clause, NewVarList, Fun, FunNum, TotalFuns)
     }].

get_condition_case({clause, _ClauseLine, _Vars, [], Body}, _NewVarList, _Fun, _FunNum, _TotalFuns) ->
    Body;
get_condition_case({clause, ClauseLine, _Vars, Conds, Body}, NewVarList, Fun, FunNum, TotalFuns) ->
    [{'case', ClauseLine, get_new_cond(Conds),
      [{clause, ClauseLine, [{atom, ClauseLine, true}], [], Body}]
          ++ get_next_fun_clause(ClauseLine, NewVarList, Fun, FunNum, TotalFuns)}].

get_next_fun_pattern_clause({clause, ClauseLine, Vars, _Conds, _Body}, NewVarList, Fun, FunNum, TotalFuns) ->
    case is_pattern_match_expr(Vars) of
        true ->
            get_next_fun_clause(ClauseLine, NewVarList, Fun, FunNum, TotalFuns);
        false ->
            []
    end.


get_next_fun_clause(_ClauseLine, _NewVarList, _Fun, TotalFuns, TotalFuns) ->
    [];
get_next_fun_clause(ClauseLine, NewVarList, Fun, FunNum, _TotalFuns) ->
    [{clause, ClauseLine, [{var, ClauseLine, '_'}], [],
            [{call,ClauseLine, {atom,ClauseLine, get_guard_fun(Fun, FunNum)}, NewVarList}]}].

get_guard_fun(Fun, 0) ->
    Fun;
get_guard_fun(Fun, FunNum) ->
    list_to_atom(atom_to_list(Fun) ++ ?FUN ++ integer_to_list(FunNum)).

get_line_no(Expr) ->
    erlang:element(2, Expr).

get_new_cond(Conds) ->
    guards(Conds).

guards([G0|Gs]) ->
    add_cond(guard_and(G0), guards(Gs), 'orelse');
guards([]) ->
    [].

guard_and([G0|Gs]) ->
    add_cond(G0, guard_and(Gs), 'andalso');
guard_and([]) -> [].

add_cond([], Cond, _Type) ->
    Cond;
add_cond(Cond, [], _Type) ->
    Cond;
add_cond(Cond1, Cond2, Type) ->
    {op, erlang:element(2, Cond1), Type, Cond1, Cond2}.

%% Change the variable in pattern list
is_pattern_match_expr([]) -> false;
is_pattern_match_expr([{var, _Line, _V} | Ps]) ->
    is_pattern_match_expr(Ps);
is_pattern_match_expr(_) ->
    true.

%%       get_true_clause(ClauseLine, Vars, Body, NewVarList) ++ 
%%           fun_to_case(Clauses, NewVarList)}]
%% .

%% True case. The guard matcheds the case clause
get_true_clause(ClauseLine, Vars, Body, _NewVarList) ->
    [{clause, ClauseLine, Vars, [], Body}].


%% In false case create a new case function for next function clause with guard.
%% get_rest_clause([], _NewVarList) ->
%%     [];
%% 
%% get_rest_clause([{clause, ClauseLine, _Vars, [], _Body}], NewFunction, GuardNo, NewVarList) ->
%%     get_clause(ClauseLine, false, NewFunction, GuardNo, NewVarList);
%% 
%% get_rest_clause([{clause, ClauseLine, Vars, {[],[]}, Body} | Rest], NewFunction, GuardNo, NewVarList) ->
%%     ?CLAUSE(ClauseLine, false,
%%       get_case([{clause, ClauseLine, Vars, [[{atom,ClauseLine,true}]], Body} | Rest], NewFunction, GuardNo, NewVarList));
%% 
%% get_rest_clause(Clauses = [{clause, ClauseLine, _Vars, _Cond, _Body} | _], NewVarList) ->
%%     ?CLAUSE(ClauseLine, false, get_case(Clauses, NewFunction, GuardNo, NewVarList)).

%% get_clause(ClauseLine, Vars, NewVarList) ->
%%     [{clause, ClauseLine, Vars, [], [{call,ClauseLine,
%%               {var,ClauseLine, NewFunction},
%%               [{integer,ClauseLine, GuardNo}] ++ get_vars(NewVarList, ClauseLine)}]}].

%% convert from Variable Names to PT variable list
get_vars(VarList, LineNo) ->
    [ {var, LineNo, Var} || Var <- VarList].


%% Replace the variable names with the new Guard Variable names in the conditions
get_new_cond([[]], ClauseLine, _Vars, _NewVarList) ->
    [{atom,ClauseLine,true}];
get_new_cond(Conds, _ClauseLine, Vars, NewVarList) ->
    ?LOG_ERROR({here, Conds,Vars, NewVarList}),
    VarZip = lists:zip([Var || {_,_,Var} <- Vars], NewVarList),
    ?LOG_ERROR({guard, guards(Conds, VarZip)}),
    lists:flatten(guards(Conds, VarZip)).

guards([G0|Gs],_VZ) ->
    add_cond(guard_and(G0,_VZ), guards(Gs, _VZ), 'orelse');
guards([],_VZ) ->
    [].

guard_and([G0|Gs],_VZ) ->
    append_cond([gexpr(G0,_VZ)], guard_and(Gs,_VZ));
guard_and([],_VZ) -> [].

%% Go throught each expression in the guard and replace the variables
gexpr({var,_Line,_V} = Var, VarZip) -> replace_var(Var, VarZip);

gexpr({map,Line,Map0,Es0},_VZ) ->
    [Map1|Es1] = gexpr_list([Map0|Es0],_VZ),
    {map,Line,Map1,Es1};
gexpr({map,Line,Es0},_VZ) ->
    Es1 = gexpr_list(Es0,_VZ),
    {map,Line,Es1};
gexpr({map_field_assoc,Line,K,V},_VZ) ->
    Ke = gexpr(K,_VZ),
    Ve = gexpr(V,_VZ),
    {map_field_assoc,Line,Ke,Ve};
gexpr({map_field_exact,Line,K,V},_VZ) ->
    Ke = gexpr(K,_VZ),
    Ve = gexpr(V,_VZ),
    {map_field_exact,Line,Ke,Ve};
gexpr({cons,Line,H0,T0},_VZ) ->
    H1 = gexpr(H0,_VZ),
    T1 = gexpr(T0,_VZ),
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0},_VZ) ->
    Es1 = gexpr_list(Es0,_VZ),
    {tuple,Line,Es1};
gexpr({record_index,Line,Name,Field0},_VZ) ->
    Field1 = gexpr(Field0,_VZ),
    {record_index,Line,Name,Field1};
gexpr({record_field,Line,Rec0,Name,Field0},_VZ) ->
    Rec1 = gexpr(Rec0,_VZ),
    Field1 = gexpr(Field0,_VZ),
    {record_field,Line,Rec1,Name,Field1};
gexpr({record,Line,Name,Inits0},_VZ) ->
    Inits1 = grecord_inits(Inits0,_VZ),
    {record,Line,Name,Inits1};
gexpr({call,Line,F,As0},_VZ) ->
    As1 = gexpr_list(As0,_VZ),
    {call,Line,F,As1};
gexpr({bin,Line,Fs},_VZ) ->
    Fs2 = pattern_grp(Fs,_VZ),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0},_VZ) ->
    A1 = gexpr(A0,_VZ),
    {op,Line,Op,A1};
gexpr({op,Line,Op,L0,R0},_VZ) ->
    L1 = gexpr(L0,_VZ),
    R1 = gexpr(R0,_VZ),         %They see the same variables
    {op,Line,Op,L1,R1};
gexpr(OtherCond, _VarZip) ->
    ?LOG_DEBUG({other, OtherCond, _VarZip}),
    OtherCond.

gexpr_list([E0|Es],_VZ) ->
    E1 = gexpr(E0,_VZ),
    [E1|gexpr_list(Es,_VZ)];
gexpr_list([],_VZ) -> [].

grecord_inits([{record_field,Lf,{atom,La,F},Val0}|Is],_VZ) ->
    Val1 = gexpr(Val0,_VZ),
    [{record_field,Lf,{atom,La,F},Val1}|grecord_inits(Is,_VZ)];
grecord_inits([{record_field,Lf,{var,La,'_'},Val0}|Is],_VZ) ->
    Val1 = gexpr(Val0,_VZ),
    [{record_field,Lf,{var,La,'_'},Val1}|grecord_inits(Is,_VZ)];
grecord_inits([],_VZ) -> [].


%% Change the variable in pattern list
%% patterns(Ps, Vs) ->
%%     patterns(Ps, Vs, {[],[]}).
%% 
%% patterns([P1 = {var,_Line,_V}|Ps],[_V0|Vs],{NewPs, NewCond}) ->
%%     patterns(Ps, Vs, {[P1|NewPs], NewCond});
%% patterns([P1|Ps],[V0|Vs],{NewPs, NewCond}) ->
%%     Line = erlang:element(2,P1),
%%     NewVar = {var,Line,V0},
%%     patterns(Ps,Vs,{[NewVar|NewPs],append_cond(Line, P1, NewVar, NewCond)});
%% patterns([], [],{Ps,Cs}) -> {lists:reverse(Ps),Cs}.

append_cond(Line, P1, Var, []) ->
    [{op,Line,'==',Var,P1}];
append_cond(Line, P1, Var, [Cond]) ->
    [{op,Line,'andalso', Cond, {op,Line,'==',Var,P1}}].

append_cond(Cond, Conditions) ->
    add_cond(Cond, Conditions, 'andalso').

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs],_VZ) ->
    [{bin_element,L1,gexpr(E1,_VZ),gexpr(S1,_VZ),T1} | gexpr(Fs,_VZ)];
pattern_grp([],_VZ) ->
    [].

%% Replace the Variable with the new variable	
replace_var({var, Line, Var}, VarZip) ->
    ?LOG_DEBUG({Var, VarZip}),
    case lists:keyfind(Var, 1, VarZip) of
        {_, NewVar} ->
            {var, Line, NewVar};
        false ->
            {var, Line, Var}
    end.

%% If the parameter variabe is not present in the in the function body.
%% get_new_vars(VarList, Cond, Body) ->
%%     ?LOG_DEBUG({body, Cond, Body}),
%%     
%%     %% 	?PRINT({Var, is_var_present(Var, Body)}),
%%     lists:foldr(fun({var,Line,Var}, NewVarList) ->
%%                         ?LOG_DEBUG({var_present_cond, Var, is_var_present(Var, Cond)}),
%%                         ?LOG_DEBUG({var_not_present_body, Var, not is_var_present(Var, Body)}),
%%                         case is_var_present(Var, Cond) andalso not is_var_present(Var, Body) of
%%                             true ->
%%                                 [{var,Line,get_new_var(Var)}| NewVarList];
%%                             false ->
%%                                 [{var,Line,Var} | NewVarList]
%%                         end;
%%                    (AnyVar, NewVarList) ->
%%                         [AnyVar, NewVarList]				
%%                 end, [], VarList).
%% 
%% get_new_var(Var) ->
%%     list_to_atom("_" ++ atom_to_list(Var)).


is_var_present(Var, Var) ->
    true;
is_var_present(Var, Body) when is_list(Body) ->
    lists:foldl(fun(InnerBody, Present) ->
                        is_var_present(Var, InnerBody) or Present
                end, false, Body);
is_var_present(Var, Body) when is_tuple(Body) ->
    is_var_present(Var, tuple_to_list(Body));
is_var_present(_Var, _Body) ->
    false.

is_guard_bif(erlang, Name, Arity) ->
    erl_internal:guard_bif(Name, Arity);
is_guard_bif(_, _Name, _Arity) ->
    false.

is_guard_bif(Name, Arity) ->
    erl_internal:guard_bif(Name, Arity).
