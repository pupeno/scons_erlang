-module(erlangscanner).
-export([relModules/0]).

relModules() ->
    {ok, {_,_,_,Modules}, _} = file:path_script(["./"], "fanterlasticfour.rel"),
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
    
    
