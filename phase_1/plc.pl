%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- use_module(comp).


main	:- '$argv'([_,F]), comp_file(F).
