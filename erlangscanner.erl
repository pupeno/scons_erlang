-module(erlangscanner).
-export([relModules/1]).

relModules([File]) ->
    {ok, {_,_,_,Modules}} = file:script(File),
    printModules(moduleNames(Modules)).
        
moduleNames([]) ->
    [];
moduleNames([{ModuleName,_}|Modules]) ->
    [ModuleName | moduleNames(Modules)].
    
printModules([]) ->
    ok;
printModules([Name|Modules]) ->
    io:fwrite("~w~n", [Name]),
    printModules(Modules).
    
    
