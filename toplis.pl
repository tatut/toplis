:- use_module(library(dcg/basics)).
:- set_prolog_flag(double_quotes, codes).

%%%%%%%%%%%%%%%
% Lisp reader %
%%%%%%%%%%%%%%%

ws --> blanks.

%% symbol code is any C symbol matching code and ?, -, +, *, /, =, <, >
symbol_code_extra(63). % ?
symbol_code_extra(45). % -
symbol_code_extra(43). % +
symbol_code_extra(42). % *
symbol_code_extra(47). % /
symbol_code_extra(61). % =
symbol_code_extra(60). % <
symbol_code_extra(62). % >

symbol_code(C) :- code_type(C, csym).
symbol_code(C) :- symbol_code_extra(C).
symbol_codef(C) :- code_type(C, csymf).
symbol_codef(C) :- symbol_code_extra(C).

symbol_codes([]) --> [].
symbol_codes([C|Cs]) --> { symbol_code(C) }, symbol_codes(Cs).

formt(F) --> ws, form(F).
form(Items) --> "(", list_items(Items), ws, ")".
form(Number) --> number(Number).
form(Sym) --> [C], { symbol_codef(C) },
              string_without(" \t\n()", Cs),
              { foreach(member(C, Cs), symbol_code(C)),
                append([C], Cs, SymCs),
                atom_codes(Sym, SymCs) }.

list_items([]) --> [].
list_items([Item|Items]) --> formt(Item), list_items(Items).

%%%%%%%%%%%%%%%%%%%%
% Lisp interpreter %
%%%%%%%%%%%%%%%%%%%%

% Define symbols that are special or builtin
special(if).
special(when).
special(cond).
special(def).
builtin(+).
builtin(-).
builtin(*).
builtin(/).
builtin(=).
builtin(<).
builtin(>).
builtin(car).
builtin(cdr).
builtin(cons).
builtin(list).
special_or_builtin(S) :- special(S).
special_or_builtin(S) :- builtin(S).

% DCG state
env(S), [S] --> [S]. % read env
env(S0, S1), [S1] --> [S0]. % update env

lookup(Sym, Val) --> env(Env), { Val = Env.Sym }.
define(Sym, Value) --> env(Ctx, Ctx.put(Sym, Value)).

eval_args([],[]) --> [].
eval_args([Name|Names], [V|Vs]) -->
    eval(V, Val),
    env(EnvIn, EnvOut),
    { put_dict(Name, EnvIn, Val, EnvOut) },
    eval_args(Names, Vs).

eval_all([],[]) --> [].
eval_all([Form|Forms], [Value|Values]) -->
    eval(Form, Value),
    eval_all(Forms, Values).

% Symbols that evaluate to themselves
special_symbol_value(nil).
special_symbol_value(true).
special_symbol_value(false).

eval(N, N) --> { number(N) }, []. % Numbers evaluate to themselves
eval(S, Val) --> { atom(S), \+ special_symbol_value(S) }, lookup(S, Val). % symbols lookup their values
eval(S, S) --> { special_symbol_value(S) }.
eval([FnName|Args], Result) -->
    { \+ special_or_builtin(FnName) }, % Not for special forms
    eval(FnName, fn(ArgNames,[Body])), % FIXME: eval all forms in body and return last
    %{writeln(got_arg_names(ArgNames))},
    env(EnvBefore), % save env
    eval_args(ArgNames,Args),
    %env(EnvAfter),
    %{ writeln(env_after_args(EnvAfter, body(Body))) },
    eval(Body, Result),
    env(_, EnvBefore). % restore env

% Eval builtin functions
eval([Builtin|Args], Result) -->
    { builtin(Builtin) },
    eval_all(Args, ArgVals),
    eval_builtin(Builtin, ArgVals, Result).

% Define a function
eval([def,Name,ArgNames|Body],Name) -->
    env(EnvIn, EnvOut),
    { put_dict(Name, EnvIn, fn(ArgNames,Body), EnvOut) }.

% if special form
eval([if,Test,Then,Else], Result) -->
    eval(Test, TestResult),
    eval_if_then_else(TestResult, Then, Else, Result).

eval_if_then_else(false, _, Else, Result) --> eval(Else, Result).
eval_if_then_else(nil, _, Else, Result) --> eval(Else, Result).
eval_if_then_else(E, Then, _, Result) -->
    { \+ E = false, \+ E = nil },
    eval(Then, Result).


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

eval_builtin(<, [_], _) --> [].
eval_builtin(<, [I1,I2|Items], true) --> { I1 < I2 }, eval_builtin(<, [I2|Items], true).
eval_builtin(<, [I1,I2|Items], false) --> { I1 >= I2 }.
eval_builtin(<, [_|Items], false) --> eval_builtin(<, Items, false).

eval_builtin(car, [[Car|_]], Car) --> [].
eval_builtin(car, [[]], nil) --> [].
eval_builtin(cdr, [[_|Cdr]], Cdr) --> [].
eval_builtin(cdr, [[]], nil) --> [].
eval_builtin(cons, [Car,Cdr], [Car|Cdr]) --> [].

eval_builtin(list, List, List) --> [].


repl :- current_input(In), stream_to_lazy_list(In, Input),
        repl(env{},Input).
repl(Env,Input) :-
    phrase(formt(Form), Input, Input1),
    %writeln(got_form(Form)),
    phrase(eval(Form, Result), [Env], [Env1]),
    format('=> ~w~n', [Result]),
    repl(Env1,Input1).

% Evaluate all text input, gathering all results in list (for testing)
eval_program(_, [], []).
eval_program(Env, Input, [Result|Results]) :-
    phrase(formt(Form), Input, RestOfInput),
    phrase(eval(Form,Result), [Env], [Env1]),
    eval_program(Env1, RestOfInput, Results), !.

%%%%%%%%%%%%%%
% Unit tests %
%%%%%%%%%%%%%%

:- begin_tests(toplis).
:- set_prolog_flag(double_quotes, codes).
:- use_module(toplis).

% prg defines text -> results mapping
prg("(* 42 10)", [420]).
prg("(def plus (a b) (+ a b)) (plus 40 2)", [plus, 42]).
prg("(if (= 1 2) 666 999)", [999]).
prg("(if (< 1 2) 666 999)", [666]).
prg("(def fib (n) (if (< n 3) 1 (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)", [fib, 55]).
prg("(car (cons 1 (cons 2 (list 3 4))))", [1]).
prg("(cdr (cons 1 (cons 2 (list 3 4))))", [[2,3,4]]).

test(prg, [forall(prg(Source,Results))]) :-
    once(toplis:eval_program(env{}, Source, ResultList)),
    Results = ResultList.

:- end_tests(toplis).
