%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(swi, [ report/1
               ]).

report(T)	:- write(user_error,T),
		   put(user_error,'\n').
