-module(nw).
%module node worker
-export([worker/5, part_size/0]).
-author('Kamil Kos').

part_size() -> 1000.

worker(PID, [], _, _, L) -> send_part(PID, L, []);
worker(PID, [H|T], N, Max, L) ->
 Result = H*N,
 if (Result > Max) -> worker(PID, T, 2, Max, L);
 	true -> worker(PID, [H|T], N+1, Max, [Result|L])
 end.

%wysyłanie kawałka listy (wielkość zdefiniowana w part_size())
send_part(PID, [], L) ->
	Len = length(L),
	if (Len > 0) -> PID ! {cross_num, L};
		true -> ok
	end;
send_part(PID, [H|T], L) ->
	Len = length(L),
	Size = part_size(),
	if (Len > Size) -> PID ! {cross_num, L},
		send_part(PID, [H|T], []);
		true -> send_part(PID, T, [H|L])
	end.
