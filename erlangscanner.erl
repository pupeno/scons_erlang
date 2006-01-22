-module(erlangscanner).
-export([relApplications/1]).

relApplications([File]) ->
    {ok, {_,_,_,Applications}} = file:script(File),
    printList(applicationNames(Applications)).
        
applicationNames([]) ->
    [];
applicationNames([{ApplicationName,_}|Applications]) ->
    [ApplicationName | applicationNames(Applications)].
    
printList([]) ->
    ok;
printList([Name|Modules]) ->
    io:fwrite("~w~n", [Name]),
    printList(Modules).
    
    
