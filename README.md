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

    greetings(Name = _GuardVariable1, AllowedNames = _GuardVariable2) ->
        case lists:member(Name, AllowedNames) of
            true -> {welcome, Name};
            _ ->
                greetings_guardian1(_GuardVariable1, _GuardVariable2)
        end.
    
    greetings_guardian1(UnknownName = _GuardVariable1, _AllowedNames = _GuardVariable2) ->
        {leave, UnknownName}.


Note:
-----
Please report issues/bugs and enhancement requests / suggestions so that it can be improved further.

