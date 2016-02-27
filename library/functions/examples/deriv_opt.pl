
'deriv:der'(x,1).
'deriv:der'(C,0) :-
        number(C).
'deriv:der'(A+B,_929+_930) :-
        'deriv:der'(A,_929),
        'deriv:der'(B,_930).
'deriv:der'(C*A,C*_883) :-
        number(C),
        'deriv:der'(A,_883).
'deriv:der'(x**N,N*x**_831) :-
        integer(N),
        N>0,
        _831 is N-1.

'deriv:dere'(x,1).
'deriv:dere'(C,0) :-
        number(C).
'deriv:dere'(A+B,_723+_724) :-
        'deriv:dere'(A,_723),
        'deriv:dere'(B,_724).
'deriv:dere'(C*A,C*_677) :-
        number(C),
        'deriv:dere'(A,_677).
'deriv:dere'(x**N,N*x**_627) :-
        integer(N),
        N>0,
        'arithpreds:-'(N,1,_627).

'deriv:derf'(x,1).
'deriv:derf'(C,0) :-
        number(C).
'deriv:derf'(A+B,_519+_520) :-
        'deriv:derf'(A,_519),
        'deriv:derf'(B,_520).
'deriv:derf'(C*A,C*_473) :-
        number(C),
        'deriv:derf'(A,_473).
'deriv:derf'(x**N,N*x**_425) :-
        integer(N),
        N>0,
        'arithpreds:-'(N,1,_425).

