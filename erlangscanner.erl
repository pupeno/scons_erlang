-module(erlangscanner).
-export([relApplications/1, appModules/1]).

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
    
appModules([File]) ->
    {ok, {_, _, MetaData}} = file:script(File),
    printList(moduleNames(MetaData)).

moduleNames([]) ->
    [];
moduleNames([{modules, ModuleNames}|_]) ->
    ModuleNames;
moduleNames([_|MetaData]) ->
    moduleNames(MetaData).
    
    
