

-define(P(X), io:format("[~p, ~p]   ~p", [?MODULE, ?LINE, X])).

-define(P2(K, V), io:format("[~p, ~p]: " ++ K, [?MODULE, ?LINE] ++ V)).








