%%%% -*- Mode: Prolog -*-

%dynamic predicates
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic node/4.

%%%------------------------------------------------

%new_graph/1
new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)).


%delete_graph/1
delete_graph(G) :-
    retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)).


%new_vertex/2
new_vertex(G, V) :-
    new_graph(G),
    vertex(G, V), !.
new_vertex(G, V) :-
    new_graph(G),
    assert(vertex(G, V)).


%graph_vertices/2
graph_vertices(G, Nw) :-
    findall(vertex(G, V), vertex(G, V), Vs),
    permutation(Vs, Nw).


%list_vertices/1
list_vertices(G) :-
    listing(vertex(G, _)).


%new_arc/4
new_arc(G, U, V, Weight) :-
    (arc(G, U, V, Weight);
     U == V), !.
new_arc(G, U, V, Weight) :-
    retract(arc(G, V, U, _)),
    assert(arc(G, U, V, Weight)), !.
new_arc(G, U, V, Weight) :-
    retract(arc(G, U, V, _)),
    assert(arc(G, U, V, Weight)).
new_arc(G, U, V, Weight) :-
    new_graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)).

%new_arc/3
new_arc(G, U, V) :-
    new_arc(G, U, V, 1).


%graph_arcs/2
graph_arcs(G, Nw) :-
    findall(arc(G, U, V, Weight), arc(G, U, V, Weight), Es),
    permutation(Es, Nw).


%adjs/3
adjs(G, V, Nw) :-
    findall(vertex(G, U), (arc(G, V, U, Weight); arc(G, U, V, Weight)), Os),
    permutation(Os, Nw).


%vertex_neighbours/3
vertex_neighbours(G, V, Nw) :-
    findall(arc(G, V, U, Weight),
            (arc(G, V, U, Weight); arc(G, U, V, Weight)), Np),
    permutation(Np, Nw).


%new_neighbours/3
new_neighbours(G, V, Nw) :-
    findall(arc(G, V, U, Weight),
            (arc(G, V, U, Weight); arc(G, U, V, Weight)), Nw).


%list_arcs/1
list_arcs(G) :-
    listing(arc(G, _, _, _)).


%list_graph/1
list_graph(G) :-
    list_arcs(G),
    listing(vertex(G, _)).


%salva/2
salva(_, []) :- !.
salva(G, [X | Xs]) :-
    arg(1, X, S),
    arg(2, X, I),
    arg(3, X, O),
    new_vertex(G, S),
    new_vertex(G, I),
    new_arc(G, S, I, O),
    salva(G, Xs).


%read_graph/2
read_graph(G, FileName) :-
    delete_graph(G),
    new_graph(G),
    csv_read_file(FileName,
		  Rows,
		  [functor(table), arity(3), separator(0'\t)]),
    salva(G, Rows).


%write_graph/2
write_graph(G, FileName) :-
    graph(G),
    write_graph(G, FileName, graph).

%write_graph/3
write_graph(G, FileName, Type) :-
    Type == 'edge',
    riduci_arco(G, J),
    csv_write_file(FileName,
		   J,
		   [functor(table), arity(3), separator(0'\t)]), !.
write_graph(G, FileName, Type) :-
    Type == 'graph',
    graph(G),
    findall(arc(U, V, Weight), arc(G, U, V, Weight), Xs),
    csv_write_file(FileName,
		   Xs,
		   [functor(table), arity(3), separator(0'\t)]).


%riduci_arco/2 (reduce arc) 
riduci_arco([], []).
riduci_arco([arc(_, U, V, W) | As], [arc(U, V, W) | Bs]) :-
    riduci_arco(As, Bs).

%%%---------------------------------------------------------


%new_heap/1
new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)).


%delete_heap/1
delete_heap(H) :-
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).


%heap_has_size/2
heap_has_size(H, S) :- heap(H, S).


%heap_empty/1
heap_empty(H) :- heap(H, _).


%heap_not_empty/1
heap_not_empty(H) :- not(heap_empty(H)).


%heap_head/3
heap_head(H, K, V) :-
    heap(H, _),
    heap_entry(H, 1, K, V).


%list_heap/1
list_heap(H) :-
    heap(H, _),
    listing(heap(H, _)),
    listing(heap_entry(H, _, _, _)).


%heap_incr/1
heap_incr(H) :-
    retract(heap(H, S)),
    S1 is S+1,
    assert(heap(H, S1)).


%heap_decr/1
heap_decr(H) :-
    retract(heap(H, S)),
    S0 is S-1,
    assert(heap(H, S0)).


%heap_exchange/3
heap_exchange(H, P, P) :-
    heap(H, _), !.
heap_exchange(H, P1, P2) :-
    heap(H, _),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K, V)),
    assert(heap_entry(H, P1, K, V)),
    assert(heap_entry(H, P2, K1, V1)).


%modify_key/4
modify_key(H, NewKey, OldKey, V) :-
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    I is truncate(P/2),
    heapify_reverse(H, I),
    heapify(H, P).


%heapify_reverse/2
heapify_reverse(H, I) :-
    heap(H, _),
    heap_fix(H, I, I, r).


%heapify/2
heapify(H, I) :-
    heap(H, _),
    heap_fix(H, I, I, f).


%heap_fix/4
heap_fix(_, 0, _, r) :- !.
heap_fix(H, I, Min, Flag) :-
    Left is 2*I,
    Right is Left + 1,
    (Min \= Left,
     P = Left;
     Min \= Right,
     P = Right),
    heap_entry(H, P, K1, _),
    heap_entry(H, Min, K2, _),
    K1 @< K2,
    heap_fix(H, I, P, Flag), !.
heap_fix(H, I, Min, Flag):-
    I \= Min,
    heap_exchange(H, I, Min),
    (Flag == r,
     I @> 1,
     P is truncate(I/2);
     Flag == f,
     P is Min),
    heap_fix(H, P, P, Flag), !.
heap_fix(_, I, I, _) :- !.
heap_fix(_, 1, _, r).


%heap_insert/3
heap_insert(H, K, V) :-
    not(heap_entry(H, _, _, V)),
    heap_has_size(H, S),
    T is S+1,
    assert(heap_entry(H, T, K, V)),
    heap_incr(H),
    I is truncate(T/2),
    heapify_reverse(H, I).


%heap_extract/3
heap_extract(H, K, V) :-
    heap_head(H, K, V),
    heap_has_size(H, S),
    heap_exchange(H, 1, S),
    retract(heap_entry(H, S, _, _)),
    heap_decr(H),
    heapify(H, 1).



%---------------------------------------------------------


%list_mst/1
list_mst(G) :-
    listing(vertex_key(G, _, _)),
    listing(vertex_previous(G, _, _)).


%new_node/2
new_node(G, [X | Xs]) :-
    arg(2, X, P),
    assert(vertex_previous(G, P, -1)),
    assert(vertex_key(G, P, inf)),
    new_node(G, Xs), !.
new_node(_, []).


%update_key/3
update_key(_, _, []).
update_key(G, U, [arc(G, U, V, W) | Arcs]) :-
    vertex_key(G, V, VK),
    (heap_entry(h, _, HK, V);
     HK is -1),
    ((HK == -1,
      (VK \= inf;
       heap_insert(h, W, V),
       retract(vertex_previous(G, V, _)),
       assert(vertex_previous(G, V, U))));
     (W @>= HK;
      modify_key(h, W, HK, V),
      retract(vertex_previous(G, V, _)),
      assert(vertex_previous(G, V, U)))),
    update_key(G, U, Arcs), !.


%delete_mst/1
delete_mst(G) :-
    retractall(vertex_key(G, _, _)),
    retractall(vertex_previous(G, _, _)).


%mst_prim/2
mst_prim(G, Source) :-
    delete_mst(G),
    vertex(G, Source),
    graph_vertices(G, Xs),
    new_node(G, Xs),
    delete_heap(h),
    new_heap(h),
    heap_insert(h, 0, Source),
    prim_ex(G).


%prim_ex/1
prim_ex(G) :-
    heap_extract(h, K, Minimo),
    retract(vertex_key(G, Minimo, _)),
    assert(vertex_key(G, Minimo, K)),
    new_neighbours(G, Minimo, Mins),
    update_key(G, Minimo, Mins),
    prim_ex(G), !.
prim_ex(_) :-
    heap_has_size(h, 0),
    delete_heap(h).


%mst_get/3
mst_get(G, Source, PreorderTree) :-
    retractall(node(_, _, _, _)),
    findall(node(G, V, U, K),
            (vertex_previous(G, V, U), U \= -1,
	     vertex_key(G, V, K)), Rs),
    create_prim(Rs),
    preorder(G, Source, [], PreorderTree).


%preorder/4
preorder(G, Source, Ys, [arc(G, Source, P, W) | Arcs]) :-
    findall(node(P, W), node(G, P, Source, W), Ts),
    min_arc(Ts, W),
    remove(Ts, W, Nw),
    sort(Nw, [P | _]),
    retract(node(G, P, Source, W)),
    preorder(G, P, [Source | Ys], Arcs), !.
preorder(G, _, [Y | Ys], Arcs) :-
    preorder(G, Y, Ys, Arcs), !.
preorder(G, _, _, []) :- not(node(G, _, _, _)).


%remove/3
remove([node(V, K) | Ts], K, [V | Nw]) :- remove(Ts, K, Nw), !.
remove([node(_, K1) | Ts], K, Nw) :- K1 \= K, remove(Ts, K, Nw), !.
remove([], _, []).


%create_prim/1
create_prim([T | Ts]) :-
    assert(T),
    create_prim(Ts).
create_prim([]).


%min_arc/2
min_arc([node(_, K1) | Ns], Min) :-
    min_arc(Ns, K1, Min).

%min_arc/3
min_arc([node(_, K1) | Ns], Min0, Min) :-
    (K1 @< Min0,
     Min1 = K1;
     Min1 = Min0),
    min_arc(Ns, Min1, Min).
min_arc([], Min, Min).
