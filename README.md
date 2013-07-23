guardian
========

Guardian allows calling (any local or accross module) functions in guards using parse transform.

It is acheived by calling 

    -compile({parse_transform, guardian}).

The function with guards calling other not allowed guard function, is replaced with function with case statements.

Example: 
--------

    greetings(Name, AllowedNames) when lists:member(Name, AllowedNames) ->
      {welcome, Name};
    greetings(Name, AllowedNames) ->
      {leave, Name}.

Currently this code is converted into 2 functions. The new function is created with _guard. 

    greetings(Name, AllowedNames) ->
      case lists:member(Name, AllowedNames) of
        true ->
          greetings_guard(guard1, Name, AllowedNames);
        false ->
          greetings_guard(guard2, Name, AllowedNames)
      end.
      
    greetings_guard(guard1, Name, AllowedNames) ->
      {welcome, Name};
    greetings_guard(guard2, Name, AllowedNames) ->
      {leave, Name}.


Current Limitations
------- -----------
1. Variable names in all the function clauses has to be same.
2. A new function with <function_name>_guard is created. Hence there cannot be any other function 
   with the same name as of the new function generated.
3. Warning messages are displayed for each of the clauses in which variables are used only in guard 
   and not in the function body. Workaround is to use variable names with startiing "_" which are 
   only used in function guards.


Please report issues/bugs and enhancement requests / suggesions so that it can be improved further.
Current limitation and other improvements will be provided in future.

