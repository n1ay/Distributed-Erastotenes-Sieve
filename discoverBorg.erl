-module(discoverBorg).
-desc("wyszukiwanie węzłów w laboratorium 208").
-export([discoverSer/0,discoverPar/0]).

discoverSer() -> lists:map(
	fun(Z)-> net_adm:ping(list_to_atom(Z++"@"++Z))end 
	,
	lists:map(fun(X) -> "t" ++ if X<10 -> "0"; true -> "" end ++ integer_to_list(X) end,lists:seq(1,28))
	).
 
discoverPar() -> 
	lists:map(
		fun(Z)-> spawn(fun() ->net_adm:ping(list_to_atom(Z++"@"++Z))end)end
	,
	lists:map(fun(X) -> "t" ++ if X<10 -> "0"; true -> "" end ++ integer_to_list(X) end,lists:seq(1,28))
	).
