:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

read_lisp(Form) :-
    current_input(In),

    phrase_from_stream(form(Form), In).

read_lisp_string(StrCodes, Form) :-
    phrase(form(Form), StrCodes).

ws --> blanks.

%% symbol code is any C symbol matching code and ?, -, +, *, /
symbol_code_extra(63). % ?
symbol_code_extra(45). % -
symbol_code_extra(43). % +
symbol_code_extra(42). % *
symbol_code_extra(47). % /

symbol_code(C) :- code_type(C, csym).
symbol_code(C) :- symbol_code_extra(C).
symbol_codef(C) :- code_type(C, csymf).
symbol_codef(C) :- symbol_code_extra(C).

symbol_codes([]) --> [].
symbol_codes([C|Cs]) --> { symbol_code(C) }, symbol_codes(Cs).

formt(F) --> ws, form(F).
form(list(Items)) --> "(", list_items(Items), ws, ")".
form(num(Number)) --> number(Number).
form(sym(Sym)) --> [C], { symbol_codef(C) },
                   string_without(" \t\n()", Cs),
                   { foreach(member(C, Cs), symbol_code(C)),
                     append([C], Cs, SymCs),
                     atom_codes(Sym, SymCs) }.

list_items([]) --> [].
list_items([Item|Items]) --> formt(Item), list_items(Items).

special(if).
special(when).
special(cond).
special(def).
special(sym(S)) :- special(S).
builtin(+).
builtin(-).
builtin(*).
builtin(/).
builtin(=).
builtin(sym(S)) :- builtin(S).
special_or_builtin(S) :- special(S).
special_or_builtin(S) :- builtin(S).

% DCG state
env(S), [S] --> [S]. % read env
env(S0, S1), [S1] --> [S0]. % update env

lookup(Sym, Val) --> env(Env), { writeln(env_is(Env)), Val = Env.Sym }.
define(Sym, Value) --> env(Ctx, Ctx.put(Sym, Value)).

eval_args([],[]) --> [].
eval_args([sym(Name)|Names], [V|Vs]) -->
    eval(V, Val),
    env(EnvIn, EnvOut),
    { put_dict(Name, EnvIn, Val, EnvOut) },
    eval_args(Names, Vs).

eval_all([],[]) --> [].
eval_all([Form|Forms], [Value|Values]) -->
    eval(Form, Value),
    eval_all(Forms, Values).

eval(num(N), N) --> []. % Numbers evaluate to themselves
eval(sym(S), Val) --> lookup(S, Val). % symbols lookup their values
eval(list([FnName|Args]), Result) -->
    { \+ special_or_builtin(FnName) }, % Not for special forms
    eval(FnName, fn(ArgNames,[Body])), % FIXME: eval all forms in body and return last
    %{writeln(got_arg_names(ArgNames))},
    env(EnvBefore), % save env
    eval_args(ArgNames,Args),
    env(EnvAfter),
    %{ writeln(env_after_args(EnvAfter, body(Body))) },
    eval(Body, Result),
    env(_, EnvBefore). % restore env

% Eval builtin functions
eval(list([sym(Builtin)|Args]), Result) -->
    { builtin(Builtin) },
    eval_all(Args, ArgVals),
    eval_builtin(Builtin, ArgVals, Result).

% Define a function
eval(list([sym(def),sym(Name),list(ArgNames)|Body]),Name) -->
    env(EnvIn, EnvOut),
    { put_dict(Name, EnvIn, fn(ArgNames,Body), EnvOut) }.


eval_builtin(*, [], 1) --> [].
eval_builtin(*, [X|Xs], Result) -->
    eval_builtin(*, Xs, Result0),
    { Result is X * Result0 }.

eval_builtin(+, [], 0) --> [].
eval_builtin(+, [X|Xs], Result) -->
    eval_builtin(+, Xs, Result0),
    { Result is X + Result0 }.

eval_builtin(-, [X], X) --> [].
eval_builtin(-, [X1,X2|Xs], Result) -->
    { X is X1 - X2 },
    eval_builtin(-, [X|Xs], Result).

eval_builtin(/, [X], X) --> [].
eval_builtin(/, [X1,X2|Xs], Result) -->
    { X is X1 / X2 },
    eval_builtin(/, [X|Xs], Result).

eval_builtin(=, Items, true) --> { forall((member(X, Items), member(Y, Items)), X=Y) }.
eval_builtin(=, Items, false) --> { \+ forall((member(X, Items), member(Y, Items)), X=Y) }.

repl :- current_input(In), stream_to_lazy_list(In, Input),
        repl(env{},Input).
repl(Env,Input) :-
    phrase(formt(Form), Input, Input1),
    writeln(got_form(Form)),
    phrase(eval(Form, Result), [Env], [Env1]),
    format('=> ~w~n', Result),
    repl(Env1,Input1).
