/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(atoms, [
		atoms/1,
		functors/1
	]).


atoms('$VAR').
atoms('$VARNAME').
atoms('$stream_position').
atoms('%').
atoms('').
atoms(',').
atoms('.').
atoms('|').
atoms(**).
atoms(*).
atoms(*->).
atoms(+).
atoms(+>).
atoms(-).
atoms(-->).
atoms(->).
atoms(/).
atoms(//).
atoms(/\).
atoms(:+).
atoms(:).
atoms(:-).
atoms(;).
atoms(<).
atoms(<<).
atoms(=).
atoms(=..).
atoms(=:=).
atoms(=<).
atoms(==).
atoms(=@=).
atoms(=\=).
atoms(>).
atoms(>=).
atoms(>>).
atoms(@<).
atoms(@=<).
atoms(@>).
atoms(@>=).
atoms([]).
atoms([]).
atoms(\+).
atoms(\).
atoms(\/).
atoms(\=).
atoms(\==).
atoms(\=@=).
atoms(^).
atoms(access).
atoms(alias).
atoms(append).
atoms(at).
atoms(atom).
atoms(binary).
atoms(bindvars).
atoms(boolean).
atoms(character_escapes).
atoms(cpu_time).
atoms(curly_notation).
atoms(dt).
atoms(end_of_file).
atoms(end_of_file).
atoms(eof_action).
atoms(eof_code).
atoms(error).
atoms(execute).
atoms(exist).
atoms(fail).
atoms(false).
atoms(fx).
atoms(fy).
atoms(heap_stack).
atoms(ignore_ops).
atoms(input).
atoms(integer).
atoms(is).
atoms(keep).
atoms(list_notation).
atoms(local_stack).
atoms(mark).
atoms(max_depth).
atoms(mod).
atoms(namevars).
atoms(none).
atoms(not).
atoms(not).
atoms(numbervars).
atoms(off).
atoms(on).
atoms(output).
atoms(past).
atoms(quiet).
atoms(quoted).
atoms(read).
atoms(read_only).
atoms(read_write).
atoms(real_time).
atoms(rem).
atoms(reposition).
atoms(reset).
atoms(runtime).
atoms(sheap_stack).
atoms(singletons).
atoms(space_args).
atoms(stderr).
atoms(subterm_positions).
atoms(syntax_errors).
atoms(system_time).
atoms(term_position).
atoms(text).
atoms(trail_stack).
atoms(true).
atoms(type).
atoms(update).
atoms(user).
atoms(user_error).
atoms(user_input).
atoms(user_output).
atoms(user_time).
atoms(variable_names).
atoms(write).
atoms(xf).
atoms(xfx).
atoms(xfy).
atoms(xor).
atoms(yf).
atoms(yfx).
atoms(yfy).
atoms({}).


functors('$VAR'/1).
functors('$VARNAME'/1).
functors('$stream_position'/3).
functors('.'/2).
functors((',')/2).
functors((*)/2).
functors((+)/1).
functors((+)/2).
functors((-)/1).
functors((-)/2).
functors((/)/2).
functors((//)/2).
functors((/\)/2).
functors((:)/2).
functors((<<)/2).
functors((=)/2).
functors((>>)/2).
functors((\)/1).
functors((\/)/2).
functors((^)/2).
functors((abs)/1).
functors((mod)/2).
functors((gcd)/2).
functors((rem)/2).
functors((sign)/1).
functors((xor)/2).
functors(alias/1).
functors(dt/6).
functors(end_of_stream/1).
functors(eof_action/1).
functors(file_name/1).
functors(hat/2).
functors(ip/4).
functors(max/2).
functors(min/2).
functors(mode/1).
functors(pipe/1).
functors(position/1).
functors(reposition/1).
functors(type/1).
functors({}/1).
