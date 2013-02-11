%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% For the purposes of licensing, I will permit myself to regard
%% this module as a "separate library", because it is not linked
%% with the resulting executable.  It is linked into the compiler,
%% as it runs.  As such, a "GNU Lesser General Public License" is
%% not really a problem; just make sure that you distribute this
%% source code file along with any binaries.
%%
%% ----------------
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see triq
%% @doc Parse transform for automatic exporting of prop_ functions.

-module(triq_autoexport).

-define(DEFAULT_PROP_PREFIX, "prop_").

-export([parse_transform/2]).

-define(CHECK,check).

parse_transform(Forms, Options) ->
    PropPrefix = proplists:get_value(triq_prop_prefix, Options,
				     ?DEFAULT_PROP_PREFIX),
    F = fun (Form, Set) ->
		t_form(Form, Set, PropPrefix)
	end,
    Exports = sets:to_list(lists:foldl(F, sets:new(), Forms)),
    t_rewrite(Forms, Exports).

t_form({function, _L, Name, 0, _Cs}, S, PropPrefix) ->
    N = atom_to_list(Name),
    case lists:prefix(PropPrefix, N) of
	true ->
	    sets:add_element({Name, 0}, S);
	false ->
	    S
    end;
t_form(_, S, _) ->
    S.

t_rewrite([{attribute,_,module,{Name,_Ps}}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
t_rewrite([{attribute,_,module,Name}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
t_rewrite([F | Fs], Exports) ->
    [F | t_rewrite(Fs, Exports)];
t_rewrite([], _Exports) ->
    [].    %% fail-safe, in case there is no module declaration

rewrite([{function,_,?CHECK,0,_}=F | Fs], As, Module, _GenQC) ->
    rewrite(Fs, [F | As], Module, false);
rewrite([F | Fs], As, Module, GenQC) ->
    rewrite(Fs, [F | As], Module, GenQC);
rewrite([], As, Module, GenQC) ->
    {if GenQC ->
	     Triq = {record_field,0,{atom,0,''},{atom,0,triq}},
	     [{function,0,?CHECK,0,
	       [{clause,0,[],[],
		 [{call,0,{remote,0,Triq,{atom,0,module}},
		   [{atom,0,Module}]}]}]}
	      | As];
	true ->
	     As
     end,
     GenQC}.

module_decl(Name, M, Fs, Exports) ->
    Module = if is_atom(Name) -> Name;
		true -> list_to_atom(packages:concat(Name))
	     end,
    {Fs1, GenQC} = rewrite(Fs, [], Module, true),
    Es = if GenQC -> [{?CHECK,0} | Exports];
	    true -> Exports
	 end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].
