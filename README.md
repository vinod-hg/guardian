guardian
========

Guardian allows calling (any local or across module) functions in guards using parse transform.

It is achieved by calling 

    -compile({parse_transform, guardian}).

The function with calling other functions in guards which are not allowed in function guards, is replaced 
with function with case statements.

Features:
---------
1. All functions in guards are supported.
2. Line numbers are of the actual function. For example if there is a runtime error in the guard function 
   the error report shown will have the line number of the original function and not the line of the generated 
   case function. Hence there is no change to the user. He can debug with his code without thinking of the 
   parsed case function.

Example: 
--------

    greetings(Name, AllowedNames) when lists:member(Name, AllowedNames) ->
      {welcome, Name};
    greetings(Name, AllowedNames) ->
      {leave, Name}.

The above function is replace with function having case clauses. 
This is hidden and it does not affect the user while debugging.

    greetings(GuarVariable1, GuardVariable2) ->
      Fun = fun(guard1, Name, _AllowedNames) ->
                    {welcome, Name};
               (guard2, Name, AllowedNames) ->
                    {leave, Name}
            end,
      case lists:member(GuarVariable1, GuardVariable2) of
        true ->
          Fun(guard1, GuarVariable1, GuardVariable2);
        false ->
          Fun(guard2, GuarVariable1, GuardVariable2)
      end.


Note:
-----
Please report issues/bugs and enhancement requests / suggestions so that it can be improved further.

