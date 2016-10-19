guardian
========

![travis_ci](https://travis-ci.org/vinod-hg/guardian.svg?branch=master)

Function guard is a very powerful feature of Erlang. But only BIF functions are supported. Guardian allows calling (any local or across module) functions in guards. This allows the code to be very short. Easy to code and maintain.

Usage:
------
It is achieved using parse transforms. Just add parse_transform as guardian in the source file or specify during compilation.

    -compile({parse_transform, guardian}).

Note:
-----
Please be careful while using non BIF functions in guards. There should not be any side effects as the guards are evaluated for each clause in sequence until a match clause is found every time the function is called.


Internal:
---------
* Only the function calling other functions in guards (non BIF), is replaced with function with case statements in the AST. All other functions are not transformed.
* It maintains the guard sequence.
* Line numbers are of the actual function. For example if there is a runtime error in the guard function the error report shown will have the line number of the original function and not the line of the generated case function. Hence there is no change to the developer. The code can be debugged without thinking of the transformed function.

Example: 
--------

    greetings(Name, AllowedNames) when lists:member(Name, AllowedNames) ->
      {welcome, Name};
    greetings(Name, AllowedNames) ->
      {leave, Name}.

The above function is replaced with code similar to the below functions. 
This is hidden and it does not affect the developer while compiling and debugging.

    greetings(Name = _GuardVariable1, AllowedNames = _GuardVariable2) ->
        case lists:member(Name, AllowedNames) of
            true -> {welcome, Name};
            _ ->
                greetings_guardian1(_GuardVariable1, _GuardVariable2)
        end.
    
    greetings_guardian1(UnknownName = _GuardVariable1, _AllowedNames = _GuardVariable2) ->
        {leave, UnknownName}.


Support:
--------
Please report issues/bugs and enhancement requests / suggestions so that it can be improved further.

