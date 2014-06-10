%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- use_module(comp).


main	:-	'$argv'([_,F|Opt]),
		process_options(Opt),
		comp_file(F).
