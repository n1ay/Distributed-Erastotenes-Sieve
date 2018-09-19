-module(eras).
-author('Kamil Kos').
-on_load(init/0).
-export([ping/1, init_borg/0, init_ets/0, task_list/0, init_mem/1, start/0, free_mem/0, check_array/0, receiver/1]).
%-------------------------------------------------
% Rozproszone sito erastotenesa
% Projekt zaliczeniowy PWiR 
%-------------------------------------------------
%INIT

ping(borg) ->
	net_adm:ping('e2@borg'),
	net_adm:ping('e3@borg'),
	net_adm:ping('e4@borg'),
	net_adm:ping('e5@borg');

ping(kamil) ->
	net_adm:ping('e2@kamil-N75SL'),
	net_adm:ping('e3@kamil-N75SL'),
	net_adm:ping('e4@kamil-N75SL'),
	net_adm:ping('e5@kamil-N75SL').

init_borg() ->
	discoverBorg:discoverPar(),
	timer:sleep(500),
	c:nc(nw),
	Nodes = nodes(),
	init_ets(),
	Nodes.

init() ->
	ok = erlang:load_nif("./eras_nif", 0).

init_ets() ->
	ets:new(data, [set, private, named_table]),
	ets:insert(data, {nodes_list, nodes_list()}),
	ets:insert(data, {node_pointer, 1}),
	ets:insert(data, {receiver_pointer, 1}).

%-------------------------------------------------
%
receivers() -> get_jobs() div 3 + 2.
%-------------------------------------------------

%funkcje do zarządzania informacjami w ets

move_ets_pointer() ->
	[{node_pointer, Idx}] = ets:lookup(data, node_pointer),
	[{nodes_list, Nodes_list}] = ets:lookup(data, nodes_list),
	Len = length(Nodes_list),
	if (Idx >= Len) -> ets:insert(data, {node_pointer, 1});
		true -> ets:insert(data, {node_pointer, Idx+1})
	end.

move_receiver_pointer() ->
	[{receiver_pointer, Idx}] = ets:lookup(data, receiver_pointer),
	Limit = receivers(),
	if (Idx >= Limit) -> ets:insert(data, {receiver_pointer, 1});
		true -> ets:insert(data, {receiver_pointer, Idx+1})
	end.
	
get_node() ->
	[{node_pointer, Idx}] = ets:lookup(data, node_pointer),
	[{nodes_list, Nodes_list}] = ets:lookup(data, nodes_list),
	Len = length(Nodes_list),
	if (Idx > Len) -> Node = lists:nth(1, Nodes_list),
		ets:insert(data, {node_pointer, 1});
		true -> Node = lists:nth(Idx, Nodes_list)
	end,
	Node.

get_rec() ->
	[{receiver_pointer, Idx}] = ets:lookup(data, receiver_pointer), Idx.
	
get_rec_pid(ID) -> 
	[{{receiver, ID}, PID}] = ets:lookup(data, {receiver, ID}), PID.
	
delete_node(Host) ->
	[{nodes_list, Nodes_list}] = ets:lookup(data, nodes_list),
	New_nodes_list = lists:delete(Host, Nodes_list),
	ets:insert(data, {nodes_list, New_nodes_list}).

get_length() ->
	[{length, Len}] = ets:lookup(data, length), Len.

job_info(ID, jobs) ->
	[{{job_id, ID}, {_, Job}}] = ets:lookup(data, {job_id, ID}), Job;

job_info(ID, all) ->
	[{{job_id, ID}, {Sum, Job}}] = ets:lookup(data, {job_id, ID}), {Sum, Job}.

get_jobs() ->
	[{jobs, Count}] = ets:lookup(data, jobs), Count.

%-------------------------------------------------

nodes_list() -> nodes()++[node()].
task_list() -> lists:zip(nodes_list(), lists:map(fun(_)->1 end, nodes_list())).

count_tasks([]) -> 0;
count_tasks([{_, Threads}|Task_list]) -> Threads+count_tasks(Task_list). 

%lista węzłów wraz z ilością wątków, które można na nich odpalić
%[...{host, ilość wątków}...]

%-------------------------------------------------

supervise(0) -> kill_receivers(receivers());
supervise(N) ->
	io:format("supervising ~p threads...\n", [N]),
	receive
		{'EXIT', PID, normal} -> io:format("job done\n", []),
			[{PID, Host, ID, working}] = ets:lookup(data, PID),
			ets:insert(data, {PID, Host, ID, {'EXIT', normal}}),
			supervise(N-1);
		{'EXIT', PID, Reason} -> io:format("thread has been killed, exit info dumped to ets...", []),
			[{PID, Host, ID, working}] = ets:lookup(data, PID),
			ets:insert(data, {PID, Host, ID, {'EXIT', Reason}}),
			delete_node(Host),
			spawn_task(get_node(), ID),
			move_ets_pointer(),
			supervise(N)
		end.

kill_receivers(0) -> ok;
kill_receivers(N) ->
	get_rec_pid(N) ! kill,
	kill_receivers(N-1).

join_all(0) -> ok;
join_all(N) ->
	receive join -> join_all(N-1)
	end.

spawn_receiver(0) -> ok;
spawn_receiver(N) -> 
	PID = spawn(?MODULE, receiver, [self()]),
	ets:insert(data, {{receiver, N}, PID}),
	spawn_receiver(N-1).

receiver(PID) ->
	receive
		{cross_num, L} -> cross_list(L),
		receiver(PID);
		kill -> PID ! join
	end.
	
	
start_task([], _) -> ok;
start_task([{_, 0}|Tail], ID) -> start_task(Tail, ID);
start_task([{Host, Threads}|Tail], ID) ->
	spawn_task(Host, ID),
	start_task([{Host, Threads-1}|Tail], ID-1).

spawn_task(Host, ID) ->
	PID = spawn_link(Host, nw, worker, [get_rec_pid(get_rec()), job_info(ID, jobs), 2, get_length(), []]),
	move_receiver_pointer(),
	ets:insert(data, {PID, Host, ID, working}),
	process_flag(trap_exit, true).
	

%funkcja, która wykreśla z tablicy wszystkie liczby, które dostanie z tablicy	
cross_list([]) -> ok;	
cross_list([H|T]) -> cross_out(H),
	cross_list(T).

%-------------------------------------------------
%inicjalizacja i podział pracy
%podział zorganizowany jest następująco: tworzonych jest n pustych list,
%gdzie n to suma wszystkich wątków na wszystkich węzłach
%do pierwszej listy wrzucana jest liczba 2, znaczy to, że dany wątkek pracujący z tą listą
%będzie miał za zadanie znaleźć wszystkie liczby, będące wielokrotnością liczby 2, co daję L/2 operacji
%jest to największa ilość operacji jaką można narzucić wykreślając tylko wielokrotności jednej liczby, dlatego
%wypełniamy cyklicznie wszystkie pozostałe listy, aż do momentu spełnienia warunku, że suma operacji będzie większa
%niż L/2, wtedy wracamy do początku i już do samego końca wypełniamy resztę list z zadaniami
%

init_jobs(-1) ->
	Sum = get_length() div 2,
	ets:insert(data, {{job_id, 0}, {Sum, [2]}}),
	div_jobs(1, 3, get_jobs()-1, Sum, math:sqrt(get_length()), get_length());

init_jobs(ID) -> 
	ets:insert(data, {{job_id, ID}, {0, []}}),
	init_jobs(ID-1).



div_jobs(ID, N, Count, Sum, Limit, Len) ->
	if	(N > Limit) -> ok; 
		(ID > Count) -> div_jobs(1, N, Count, Sum, Limit, Len);
		true ->
			{Work, Jobs} = job_info(ID, all),
			if (Work > Sum) -> div_jobs2(0, N, Count, Limit, Len);
				true -> ets:insert(data, {{job_id, ID},{Work+Len div N, [N|Jobs]}}),
				div_jobs(ID+1, N+2, Count, Sum, Limit, Len)
			end
	end.

div_jobs2(ID, N, Count, Limit, Len) ->
	if (N > Limit) -> ok;
		(ID > Count) -> div_jobs2(0, N, Count, Limit, Len);
		true ->
			{Work, Jobs} = job_info(ID, all),
			ets:insert(data, {{job_id, ID},{Work+Len div N, [N|Jobs]}}),
			div_jobs2(ID+1, N+2, Count, Limit, Len)
	end.
		
%-------------------------------------------------
%jak to odpalić? ->
%uruchamianie na borgu:
% 1. init_borg()
% 2. init_mem(N) //N - górny zakres
% 3. start()
% 4. jeżeli chcemy uruchomić ponownie, przed kolejną inicjalizacją (init_mem(...))
% warto zwolnić poprzednio zaalokowaną pamięć (funkcja eras:free_mem())

% uruchamianie nie na borgu:
% musi być odpalony na węźle ( erl -sname itd. )
% 1. sprawdzić czy inne węzły są widoczne, jeżeli tak to -> 2.
% 2. uruchomić init_ets()
% 3. uruchomić init_mem(N), gdzie N - górny zakres znalezionych liczb pierwszych
% 4. start()
% 5. po zakończonej pracy można sprawdzić poprawność: check_array(),
% sprawdzić status zakończonych zadań ets:i(data) i zaleca się zwolnienie pamięci free_mem().

init_mem(N) ->
	init_array(N),
	ets:insert(data, {length, N}),
	Count = count_tasks(task_list()),
	ets:insert(data, {jobs, Count}),
	init_jobs(Count-1).

start() ->
	Count = count_tasks(task_list()),
	spawn_receiver(receivers()),
	start_task(task_list(), Count-1),
	supervise(Count),
	join_all(receivers()).
	%check_array().

%-------------------------------------------------
%erlang nif functions

check_array() -> exit(nif_library_not_loaded).
init_array(_N) -> exit(nif_library_not_loaded).
free_mem() -> exit(nif_library_not_loaded).
cross_out(_N) -> exit(nif_library_not_loaded).

%-------------------------------------------------
