%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- use_module(comp).


%% main	:- read(F), comp_file(F).
main	:- '$arg_list'([_,F]), comp_file(F).
