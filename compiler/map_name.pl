%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(map_name, [ map_pred/3
		    , map_pred/2
		    , map_fun/2
		    , map_C_op/2
		    ]).

:- use_module([atoms]).

map_fun(F/N,Fm)	:- map_atom(F,Na), concat_atom([Na,'_',N],Fm).

map_pred(F,P)	:- flag(current_module,M,M),
		   map_pred(F,M,P).
map_pred(F,M,P)	:- map_fun(F,Fm),
		   map_atom(M,Mm),
		   concat_atom(['_',Mm,Fm],P).

map_C_op(@<,<).
map_C_op(@>,>).
map_C_op(@=<,<=).
map_C_op(@>=,>=).
map_C_op(==,==).
map_C_op(\==,'!=').
map_C_op(<,<).
map_C_op(>,>=).
map_C_op(=<,<=).
map_C_op(>=,>=).
map_C_op(=:=,==).
map_C_op(=\=,'!=').

%% map_C_op(//,/).
%% map_C_op(mod,'%').
%% map_C_op(rem,'%').
%% map_C_op(xor,^).
%% map_C_op(\,~).
%% map_C_op(/\,&).
%% map_C_op(\/,|).
%% map_C_op(O,O).


