% *** html.pl *** (Version 98.2 for ciao)
% A Simple HTML Package for Prolog and CLP systems
% 
% Copyright (C) 1996/1997/1998 UPM-CLIP.
% 
% This package is free software; you can redistribute it and/or
% modify it under the terms of the GNU Library General Public
% License as published by the Free Software Foundation; either
% version 2 of the License, or (at your option) any later version.
% 
% This package is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Library General Public License for more details.
% 
% You should have received a copy of the GNU Library General Public
% License along with this package; if not, write to the Free
% Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
% 
% M. Hermenegildo (herme@fi.upm.es), D. Cabeza (dcabeza@fi.upm.es) and
% S. Varma (sacha@clip.dia.fi.upm.es) using input from L. Naish's forms
% and F. Bueno's previous Chat interface

:- module(html, [url_info/2, url_info_relative/3, url_query/2,
        output_html/1, html2terms/2, xml2terms/2, html_template/3,
        http_lines/3, 
        html_report_error/1, get_form_input/1, get_form_value/3,
        form_empty_value/1, text_lines/2, form_default/3, my_url/1,
        form_request_method/1, icon_address/2, html_protect/1],
        ['pillow/ops',dcg,assertions]).

:- use_module(library(strings),
        [write_string/1, get_line/1, whitespace/2, whitespace0/2, string/3]).
:- use_module(library(lists), [reverse/2, append/3, list_lookup/4]).
:- use_module(library(system)).
:- use_module(library('pillow/common')).

%%% Some icon addresses %%%

:- include(icon_address).

icon_address(warning,IAddress):-
        icon_base_address(BAddress),
        atom_concat(BAddress,'warning_large.gif',IAddress).
icon_address(dot,IAddress):-
        icon_base_address(BAddress),
        atom_concat(BAddress,'redball.gif',IAddress).
icon_address(clip,IAddress):-
        icon_base_address(BAddress),
        atom_concat(BAddress,'clip.gif',IAddress).
icon_address(pillow,IAddress):-
        icon_base_address(BAddress),
        atom_concat(BAddress,'pillow_d.gif',IAddress).

%%% HTML <-> Terms translation %%%

% The idea is to have a CIAO/Prolog syntax description of a document as
% a term and then use html2terms/2 to translate the term into a string for
% writing and to translate a string to a term for reading

%% Hook predicate to define macros
% Leave at least one so that no existence errors are generated
% on any system. Take care to not transform something into itself!
html_expansion(bf(X),b(X)).
html_expansion(it(X),i(X)).
html_expansion(pr,
    ref("http://www.clip.dia.fi.upm.es/miscdocs/pillow/pillow.html",
        image(Pillow, [alt="developed with PiLLoW",border=0,align=bottom]))
              ) :-
        icon_address(pillow,Pillow).

% Translate html format and send to current output
output_html(F) :-
        html_term(F,T,[]),
        write_string(T).

% Error handling
html_report_error(X) :-
        (icon_address(warning,Icon) -> Image = image(Icon); Image = ""),
        output_html([
          cgi_reply,
          start,
          title("Error Report"), 
          --,
          h1([ Image, ' Error:' ]),
          --,
          X,
          --,
          end]),
        flush_output,
        halt.
%% html_report_error(X) :-
%%         (icon_address(warning,Icon) -> Image = image(Icon); Image = ""),
%%         output_html([
%%           cgi_reply,
%%           start,
%%           title("html.pl Error Report"), 
%%           --,
%%           Image,
%%           h1("PiLLoW Error Report"),
%%           --,
%%           b("The following error was encountered: "),
%%           X,
%%           --,
%%           end]),
%%         flush_output,
%%         halt.

% HTML <-> Terms translation

html2terms(Chars, Terms) :-
        var(Chars), !,
        html_term(Terms, Chars, []).
html2terms(Chars, Terms) :-
        parse_html([], Terms, [], Chars, []).

% XML <-> Terms translation

xml2terms(Chars, Terms) :-
        var(Chars), !,
        html_term(Terms, Chars, []). % Uses the same as HTML
xml2terms(Chars, Terms) :-
        parse_xml([], Terms, [], Chars, []).

%% Terms -> HTML/XML translation %%

html_term(X) --> {var(X)}, !,
        "<b>**Warning free variable**</b>".
html_term(T) --> {html_expansion(T,NT)}, !,
        html_term(NT).
html_term(start) --> !, "<html>".
html_term(end)   --> !, "</html>".
html_term(--)  --> !, newline, "<hr>", newline.
html_term(\\) --> !, "<br>", newline.
html_term($)  --> !, newline, "<p>".
html_term(comment(C)) --> !,
        "<!-- ",atomic_or_string(C)," -->",
        newline.
html_term(declare(C)) --> !,
        "<!",atomic_or_string(C),">",
        newline.
% XML declaration
html_term(xmldecl(Atts)) --> !,
        "<?xml",
        html_atts(Atts),
        "?>".
html_term(image(Addr)) --> !,
        "<img",
        html_atts([src=Addr]),
        ">".
html_term(image(Addr,Atts)) --> !,
        "<img",
        html_atts([src=Addr|Atts]),
        ">".
html_term(ref(Addr,Text)) --> !,
        "<a",
        html_atts([href=Addr]),
        ">",
        html_term(Text),
        "</a>".
html_term(label(Label,Text)) --> !,
        "<a",
        html_atts([name=Label]),
        ">",
        html_term(Text),
        "</a>".
html_term(heading(L,X)) -->
        {number_codes(L,[N])}, !,
        html_env([0'h,N],X),
        newline.
html_term(itemize(L)) --> !,
        "<ul>",
        newline,
        html_items(L),
        "</ul>".
html_term(enumerate(L)) --> !,
        "<ol>",
        newline,
        html_items(L),
        "</ol>".
html_term(description(L)) --> !,
        "<dl>",
        newline,
        html_descriptions(L),
        "</dl>".
html_term(nice_itemize(Dot, L)) --> !,
        "<dl>",
        newline,
        {atom(Dot) -> atom_codes(Dot,D) ; D = Dot},
        html_nice_items(L, D),
        "</dl>".
html_term(preformatted(X)) --> !,
        "<pre>",
        newline,
        preformatted_lines(X),
        "</pre>".
html_term(entity(Name)) --> !,
        "&",atomic_or_string(Name),";".
% Forms
html_term(start_form) --> !,
        "<form",
        html_atts([method="POST"]),
        ">",
        newline.
html_term(start_form(Addr)) --> !, 
        "<form",
        html_atts([method="POST", action=Addr]),
        ">",
        newline.
html_term(start_form(Addr,Atts)) --> !, 
        "<form",
        html_atts([action=Addr|Atts]),
        ">",
        newline.
html_term(end_form) --> !,
        "</form>", newline.
html_term(checkbox(Name,on)) --> !,
        "<input",
        html_atts([name=Name,type=checkbox,checked]),
        ">".
html_term(checkbox(Name,_)) --> !,
        "<input",
        html_atts([name=Name,type=checkbox]),
        ">".
html_term(radio(Name,Value,Value)) --> !,
        "<input",
        html_atts([name=Name,type=radio,value=Value,checked]),
        ">".
html_term(radio(Name,Value,_)) --> !,
        "<input",
        html_atts([name=Name,type=radio,value=Value]),
        ">".
html_term(input(Type,Atts)) --> !,
        "<input",
        html_atts([type=Type|Atts]),
        ">".
html_term(textinput(Name,Atts,Text)) --> !,
        "<textarea",
        html_atts([name=Name|Atts]),
        ">",
        textarea_data(Text),
        "</textarea>".
html_term(menu(Name,Atts,Items)) --> !,
        "<select",
        html_atts([name=Name|Atts]),
        ">", newline,
        html_options(Items),
        "</select>".
html_term(option(Name,Val,Options)) --> !,
        "<select",
        html_atts([name=Name]),
        ">", newline,
        html_one_option(Options, Val),
        "</select>".
html_term(form_reply) --> !,
        "Content-type: text/html",
        newline,
        newline.
html_term(cgi_reply) --> !,
        "Content-type: text/html",
        newline,
        newline.
% Constructs
html_term(verbatim(Text)) --> !,
        html_quoted(Text).
html_term(nl) --> !, newline. % Just to improve HTML source readability
html_term([]) --> !.
html_term([E|Es]) --> !,
        html_term(E),
        html_term(Es).
html_term(begin(T)) --> {atom(T), atom_codes(T,TS)}, !,
        "<",string(TS),">".
html_term(begin(T,Atts)) --> {atom(T), atom_codes(T,TS)}, !,
        "<",string(TS),
        html_atts(Atts),
        ">".
html_term(end(T)) --> {atom(T), atom_codes(T,TS)}, !,
        "</",string(TS),">".
html_term(env(Name,Atts,Text)) --> {atom(Name), atom_codes(Name,NS)}, !,
        html_env_atts(NS,Atts,Text).
html_term(T$Atts) --> {atom(T), atom_codes(T,TS)}, !,
        "<",string(TS),
        html_atts(Atts),
        ">".
% XML empty element
html_term(elem(N,Atts)) --> {atom(N), atom_codes(N,NS)}, !,
        "<",string(NS),
        html_atts(Atts),
        "/>".
html_term(F) --> {F =.. [Env,X], atom_codes(Env, ES)}, !,
        html_env(ES,X).
html_term(F) --> {F =.. [Env,Atts,X], atom_codes(Env, ES)}, !,
        html_env_atts(ES,Atts,X).
html_term(C) --> {integer(C), C >= 0, C =< 255}, !, [C].
html_term(T) -->
        prolog_term(T).

newline --> [10].

html_atts([]) --> [].
html_atts([A|As]) -->
        " ",
        html_att(A),
        html_atts(As).

html_att(A=V) --> {atom_codes(A,AS)}, !,
        string(AS),"=""",atomic_or_string(V),"""".
html_att(A) -->  {atom_codes(A,AS)},
        string(AS).

html_env(E,I) -->
        "<",string(E),">",
        html_term(I),
        "</",string(E),">".

html_env_atts(E,Atts,I) -->
        "<",string(E),
        html_atts(Atts),
        ">",
        html_term(I),
        "</",string(E),">".

html_items([]) --> [].
html_items([It|Its]) -->
        "<li>",
        html_term(It),
        "</li>",
        newline,
        html_items(Its).

html_descriptions([]) --> [].
html_descriptions([D|Ds]) -->
        html_description(D),
        html_descriptions(Ds).

html_description((T,D)) --> !,
        "<dt>",
        html_term(T),
        "</dt>",
        newline,
        html_description(D).
html_description(D) -->
        "<dd>",
        html_term(D),
        "</dd>",
        newline.

html_nice_items([],_) --> [].
html_nice_items([It|Its],Dot) -->
        "<dd><img src=""",
        string(Dot),
        """ align=""bottom"" alt=""*"">",
        html_term(It),
        "</dd>",
        newline,
        html_nice_items(Its, Dot).

preformatted_lines([]) --> [].
preformatted_lines([X|Xs]) -->
        html_term(X),
        newline,
        preformatted_lines(Xs).

html_options([]) --> [].
html_options([Op|Ops]) -->
        html_option(Op),
        newline,
        html_options(Ops).

html_option($Op) --> !,
        "<option selected>",atomic_or_string(Op),"</option>".
html_option(Op) -->
        "<option>",atomic_or_string(Op),"</option>".

html_one_option([], _) --> [].
html_one_option([Op|Ops], Sel) -->
        "<option",
        html_one_option_sel(Op, Sel),
        ">",atomic_or_string(Op),"</option>",
        newline,
        html_one_option(Ops, Sel).

html_one_option_sel(Op, Op) --> !, " selected".
html_one_option_sel(_, _) --> "".

html_quoted(T) -->
        {atom(T) -> atom_codes(T,TS) ; TS = T},
        html_quoted_chars(TS).

html_quoted_chars([]) --> [].
html_quoted_chars([C|T]) -->
        html_quoted_char(C),
        html_quoted_chars(T).

html_quoted_char(0'>) --> !, "&gt;".
html_quoted_char(0'<) --> !, "&lt;".
html_quoted_char(0'&) --> !, "&amp;".
html_quoted_char(0'") --> !, "&quot;".
html_quoted_char(0' ) --> !, "&nbsp;".
html_quoted_char(C)   --> [C].

prolog_term(V) -->
        {var(V)}, !, "_".
prolog_term(T) -->
        {functor(T,F,A), name(F,FS)},
        string(FS), prolog_term_maybe_args(A,T).

prolog_term_maybe_args(0,_) --> !, "".
prolog_term_maybe_args(A,T) -->
        "(",
        prolog_term_args(1,A,T),
        ")".

prolog_term_args(N, N, T) --> !,
        {arg(N,T,A)},
        prolog_term(A).
prolog_term_args(N, M, T) -->
        {arg(N,T,A)},
        prolog_term(A),
        ",",
        {N1 is N+1},
        prolog_term_args(N1,M,T).

%% HTML -> Terms translation %%

html_template(Chars, Terms, Dict) :-
        parse_html([], Terms, Dict, Chars, []).

% see a '<' - possible item
parse_html(Stack,NStack,Dict) -->
        "<",
        {tidy_string(Stack, Stack2)},
        html_unit(Stack2,Stack3,Dict), !,
        parse_html(Stack3,NStack,Dict).
% build on an open string
parse_html([Elem|Stack],NStack,Dict) -->
        {nonvar(Elem), Elem = string(S,T)},
        [C], !,
        {T = [C|TT]},
        parse_html([string(S,TT)|Stack],NStack,Dict).
% open a new string
parse_html(Stack,NStack,Dict) -->
        [C], !,
        parse_html([string([C|T],T)|Stack],NStack,Dict).
% base case - close open strings
parse_html(Stack,NStack,_Dict) --> "",
        {tidy_string(Stack,Stack2),
         reverse(Stack2,NStack)}.

% env terminators
html_unit(S,NS,Dict) -->
        "/",
        html_tag(N),
        whitespace0,
        ">",
        { poptokenstack(N,S,NS,Dict) },
        !.
% comment
html_unit(S,[comment(Text)|S],_Dict) -->
        "!--",
        string(Text),
        "-->",
        !.
% declaration
html_unit(S,[declare(Text)|S],_Dict) -->
        "!",
        string(Text),
        ">",
        !.
% items
html_unit(S,[N$A|S],Dict) -->
        html_tag(N),
        html_tag_atts(A,Dict),
        whitespace0,
        ">",
        !.

html_tag(N) -->
        loupalpha(C),
        html_tag_rest(Cs),
        { atom_codes(N,[C|Cs]) }.

html_tag_rest([C|Cs]) -->
        html_tag_char(C), !,
        html_tag_rest(Cs).
html_tag_rest([]) --> "".

html_tag_char(C) --> loupalpha(C).
html_tag_char(C) --> digit(C).
html_tag_char(0'.) --> ".".
html_tag_char(0'-) --> "-".


html_tag_atts([],_Dict) --> "".
html_tag_atts([A|As],Dict) -->
        whitespace,
        html_tag_att(A,Dict),
        html_tag_atts(As,Dict).

% template variable
html_tag_att(A,Dict) -->
        "_", html_tag(N),
        {list_lookup(Dict, (=), N, V)},
        html_opt_value(A, V, Dict).
html_tag_att(A,Dict) -->
        html_tag(N),
        html_opt_value(A, N, Dict).

html_opt_value(N = V, N, Dict) -->
        whitespace0,
        "=",
        whitespace0,
        html_value(V, Dict), !.
html_opt_value(N, N,_Dict) --> "".

% template variable
html_value(V, Dict) -->
        "_", 
        html_tag(N),
        {list_lookup(Dict, (=), N, V)}.
% html_value(V,_Dict) --> http_lo_up_token(V). % People do not write valid HTML
html_value(V,_Dict) --> http_quoted_string(V).
html_value(V,_Dict) --> html_lax_value(V).

html_lax_value([C|Cs]) --> [C], { C \== 0'> , C > 32 },
        html_lax_value(Cs).
html_lax_value([]) --> "".

poptokenstack(EnvTag,Stack,NStack,Dict) :-
        pop_ts(EnvTag,Stack,[],NStack,Dict),
        !.
poptokenstack(EnvTag,Stack,[SlashEnvTag$[]|Stack],_) :-
        atom_concat('/',EnvTag,SlashEnvTag).

pop_ts(EnvTag,[Elem|S],Insides,NS,Dict) :-
        ( nonvar(Elem), Elem = EnvTag$Atts ->
            elem_or_template_var(EnvTag,Atts,Insides,E,Dict),
            NS = [E|S]
        ; pop_ts(EnvTag,S,[Elem|Insides],NS,Dict)
        ).

elem_or_template_var('v',_,[NameS],Var,Dict) :-
        catch(atom_codes(Name,NameS), _, fail),
        list_lookup(Dict, (=), Name, Var),  !.
elem_or_template_var(EnvTag,Atts,Insides,env(EnvTag,Atts,Insides),_).

tidy_string([Elem|Stack],[L|Stack]) :-
        nonvar(Elem), Elem = string(L,T), !, T = [].
tidy_string(Stack,Stack).

%% XML -> Terms translation %%

parse_xml(Stack,NStack,Dict) -->
        "<",
        {tidy_string(Stack, Stack2)},
        xml_unit(Stack2,Stack3,Dict), !,
        parse_xml(Stack3,NStack,Dict).
% build on an open string
parse_xml([Elem|Stack],NStack,Dict) -->
        {nonvar(Elem), Elem = string(S,T)},
        [C], !,
        {T = [C|TT]},
        parse_xml([string(S,TT)|Stack],NStack,Dict).
% open a new string
parse_xml(Stack,NStack,Dict) -->
        [C], !,
        parse_xml([string([C|T],T)|Stack],NStack,Dict).
% base case - close open strings
parse_xml(Stack,NStack,_Dict) --> "",
        {tidy_string(Stack,Stack2),
         reverse(Stack2,NStack)}.

% env terminators
xml_unit(S,NS,Dict) -->
        "/",
        xml_tag(N),
        whitespace0,
        ">",
        { poptokenstack(N,S,NS,Dict) },
        !.
% comment
xml_unit(S,[comment(Text)|S],_Dict) -->
        "!--",
        string(Text),
        "-->",
        !.
% declaration
xml_unit(S,[declare(Text)|S],_Dict) -->
        "!",
        string(Text),
        ">",
        !.
% xml declarations
xml_unit(S,[xmldecl(Ats)|S],Dict) -->
        "?xml",
        xml_tag_atts(Ats,Dict),
        whitespace0,
        "?>",
        !.
% elements or env beginnings
xml_unit(S,[El|S],Dict) -->
        xml_tag(N),
        xml_tag_atts(A,Dict),
        whitespace0,
        elem_envbeg(El, N, A),
        ">",
        !.

elem_envbeg(elem(N,A), N, A) -->
        "/", !.
elem_envbeg(N$A,N, A) --> "".

xml_tag(N) -->
        xml_tag_start(C),
        xml_tag_rest(Cs),
        { atom_codes(N,[C|Cs]) }.

xml_tag_atts([],_Dict) --> "".
xml_tag_atts([A|As],Dict) -->
        whitespace,
        xml_tag_att(A,Dict),
        xml_tag_atts(As,Dict).

xml_tag_att(N=V,Dict) -->
        xml_tag(N),
        whitespace0,
        "=",
        whitespace0,
        xml_value(V, Dict).

xml_value(V, Dict) -->
        "_", 
        xml_tag(N),
        {list_lookup(Dict, (=), N, V)}.
xml_value(V,_Dict) -->
        """",
        xml_quoted_string(0'",V).
xml_value(V,_Dict) -->
        "'",
        xml_quoted_string(0'',V).
xml_value(V,_Dict) --> % This is not correct syntax
        xml_bad_value(V).

xml_quoted_string(Q, []) --> [Q], !.
xml_quoted_string(Q, "&quot;"||Cs) -->
        """",
        xml_quoted_string(Q, Cs).
xml_quoted_string(Q, [C|Cs]) -->
        [C],
        xml_quoted_string(Q, Cs).

xml_bad_value([]) --> "".
xml_bad_value([C|Cs]) -->
        [C],
        xml_bad_value(Cs).

xml_tag_start(C) --> loalpha(C).
xml_tag_start(C) --> upalpha(C).
xml_tag_start(0'_) --> "_".
xml_tag_start(0':) --> ":".

xml_tag_rest([C|Cs]) -->
        xml_tag_char(C), !,
        xml_tag_rest(Cs).
xml_tag_rest([]) --> "".

xml_tag_char(C) --> loalpha(C).
xml_tag_char(C) --> upalpha(C).
xml_tag_char(C) --> digit(C).
xml_tag_char(0'_) --> "_".
xml_tag_char(0':) --> ":".
xml_tag_char(0'.) --> ".".
xml_tag_char(0'-) --> "-".

%%% Parsing of forms input %%%

% Receiving form input as a dictionary of attribute=value pairs
get_form_input(Dic) :-
        form_request_method(M),
        get_form_input_method(M, Dic), !.
get_form_input([]).

get_form_input_method('GET', Dic) :-
        getenvstr('QUERY_STRING',Q), Q \== [] ->
            append(Q,"&",Cs),
            form_urlencoded_to_dic(Dic, Cs, [])
      ; Dic = [].
get_form_input_method('POST', Dic) :-
        getenvstr('CONTENT_TYPE', ContentType),
        http_media_type(Type,Subtype,Params,ContentType,[]),
        get_form_input_of_type(Type,Subtype,Params,Dic).
get_form_input_method(M, _) :-
        html_report_error(['Unknown request method ', tt(M),
                           ' or bad request.']).

get_form_input_of_type(application, 'x-www-form-urlencoded', _, Dic) :-
        getenvstr('CONTENT_LENGTH', N),
        number_codes(No,N),
        ( No > 0 ->
            read_all(No,Cs,"&"),
            form_urlencoded_to_dic(Dic, Cs, [])
        ; Dic = []
        ).
get_form_input_of_type(multipart, 'form-data', Params, Dic) :-
        member((boundary=B), Params),
        name(B, BS),
        Boundary = [0'-,0'-|BS],
        get_lines_to_boundary(Boundary, _, End),
        get_multipart_form_data(End, Boundary, Dic).
get_form_input_of_type(Type,Subtype,_,_) :-
        html_report_error(['Unknown Content-type ',tt([Type,"/",Subtype]),
                           ' or bad request.']).

% read N chars from input (N>=0)
read_all(0) --> !, "".
read_all(N) -->
        {get_code(C)},
        [C],
        {N1 is N - 1},
        read_all(N1).

% Converts string "name1=val1&name2=val2&name3=&" into
% list of pairs [name1='val1', name2='val2', name3='$empty'] etc
% Funny chars, eg = and & never occur in vals (they appear as
% escape sequences)
form_urlencoded_to_dic([]) --> "".
form_urlencoded_to_dic([N1=V1|NVs]) -->
        chars_to(N,0'=),
        {name(N1, N)},
        chars_to(V,0'&),
        {expand_esc_plus(V,EV,[13,10]),
         http_lines(Ls, EV, []),
         to_value(Ls,V1)},
        form_urlencoded_to_dic(NVs).

chars_to([],C) --> [C].
chars_to([C|Cs],D) -->
        [C],
        {C \== D},
        chars_to(Cs,D).

% Expands escape sequences and converts "+" back into " " in a string
expand_esc_plus([]) --> "".
expand_esc_plus([0'+|Cs]) --> !,
        " ",
        expand_esc_plus(Cs).
expand_esc_plus([0'%,C1,C2|Cs]) --> !,
        {hex_digit(C1,D1),
         hex_digit(C2,D2),
         C is D1 * 16 + D2},
        [C],
        expand_esc_plus(Cs).
expand_esc_plus([C|Cs]) -->
        [C],
        expand_esc_plus(Cs).

hex_digit(C, D) :-
        (C >= 0'A ->
          D is ((C /\ 223) - 0'A) + 10 % 223 = bin(11011111)
        ;
          D is C - 0'0
        ).

to_value([L|Ls], V) :-
        to_value_(Ls, L, V).

to_value_([], [], '$empty') :- !.
to_value_([], L, V) :- !,
        name(V, L).        % if only a line, return an atom or number
to_value_(Ls, L, [L|Ls]).   % else, return the list of lines

http_lines([L|Ls]) -->
        http_line(L), !,
        http_lines(Ls).
http_lines([]) --> "".

% ----------------------------------------------------------------------------


% ----------------------------------------------------------------------------

get_multipart_form_data(end, _, []).
get_multipart_form_data(continue, Boundary, [N=V|NVs]) :-
        get_lines_to_boundary(Boundary, Lines, End),
        extract_name_value(Lines, N, V),
        get_multipart_form_data(End, Boundary, NVs).

get_lines_to_boundary(Boundary, Lines, End) :-
        get_line(Line),
        get_lines_to_boundary_(Line, Boundary, Lines, End).

get_lines_to_boundary_(Line, Boundary, Lines, End) :-
        append(Boundary, R, Line),
        check_end(R, End), !,
        Lines = [].
get_lines_to_boundary_(Line, Boundary, [Line|Lines], End) :-
        get_line(OtherLine),
        get_lines_to_boundary_(OtherLine, Boundary, Lines, End).

check_end([], continue).
check_end("--", end).

extract_name_value([L|Ls], N, V) :-
        head_and_body_lines(L, Ls, HLs, BLs),
        extract_name_type(HLs, N, T),
        extract_value(T, BLs, V).

head_and_body_lines([], BLs, [], BLs) :- !.
head_and_body_lines(HL, [L|Ls], [HL|HLs], BLs) :-
        head_and_body_lines(L, Ls, HLs, BLs).

extract_name_type(HLs, N, T) :-
        member(HL, HLs),
        content_disposition_header(Params, HL, []),
        extract_name(Params, N),
        extract_type(Params, T).

content_disposition_header(Params) -->
        "Content-Disposition: form-data",
        http_type_params(Params).

extract_name(Params, N) :-
        member((name=NS), Params), !,
        atom_codes(N, NS).

extract_type(Params, T) :-
        (
          member((filename=FS), Params) ->
              atom_codes(F, FS),
              T = file(F)
        ; T = data
        ).

extract_value(data, [L|Ls], V) :-
        to_value_(Ls, L, V).
extract_value(file(F), Ls, file(F,Ls)).

% ----------------------------------------------------------------------------

% Get value Val for attribute Var in dictionary Dic
% Does not fail: value is '' if not found.
get_form_value([],_Var,'').
get_form_value([Var=Val|_],Var,Val) :- !.
get_form_value([_|Dic],Var,Val) :- 
        get_form_value(Dic,Var,Val).

% Transform input from a text area to a list of lines
text_lines('$empty', []) :- !.
text_lines(A, [L]) :-
        atomic(A), !,
        name(A,L).
text_lines(T,T).

% Some generic help for dealing with the very weird things that empty text 
% areas and boxes can send
form_empty_value(T) :-
        text_lines(T, Ls),
        empty_lines(Ls).

empty_lines([]).
empty_lines([L|Ls]) :-
        whitespace0(L, []),
        empty_lines(Ls), !.

% Set form defaults
form_default(Val,Default,NewVal) :- 
        ( Val == '' -> NewVal = Default; NewVal = Val).

form_request_method(M) :-
        getenvstr('REQUEST_METHOD', MS),
        atom_codes(M,MS).

my_url(URL) :-
        getenvstr('SERVER_NAME', Server),
        getenvstr('SCRIPT_NAME', File),
        getenvstr('SERVER_PORT', Port),
        (
            Port = "80" ->
                mappend(["http://",Server,File], URL)
        ;   mappend(["http://",Server,[0':|Port],File], URL)
        ).

%% To compute GET parameters for CGI's
%  -- from an idea of Markus Fromherz <fromherz@parc.xerox.com> */

url_query(Args, URLArgs) :-
        params_to_string(Args, 0'?, URLArgs).

params_to_string([], _, "").
params_to_string([N=V|NVs], C, [C|String]) :-
        name(N,NS),
        name(V,VS),
        encoded_value(VS,EVS,Rest),
        params_to_string(NVs, 0'&, Rest),
        append(NS,[0'=|EVS],String).

encoded_value([]) --> "".
encoded_value([32|Cs]) --> !, % " " = [32]
        "+",
        encoded_value(Cs).
encoded_value([C|Cs]) -->
        {no_conversion(C)}, !,
        [C],
        encoded_value(Cs).
encoded_value([C|Cs]) -->
        {hex_chars(C,C1,C2)},
        [0'%,C1,C2],
        encoded_value(Cs).

no_conversion(0'*).
no_conversion(0'-).
no_conversion(0'.).
no_conversion(0'_).
no_conversion(C) :- C >= 0'0, C =< 0'9.
no_conversion(C) :- C >= 0'@, C =< 0'Z.
no_conversion(C) :- C >= 0'a, C =< 0'z.

hex_chars(C, H, L) :-
        Hn is C >> 4,
        hex_char(Hn,H),
        Ln is C /\ 15,
        hex_char(Ln,L).

hex_char(N,C) :- N < 10, !, C is N+0'0.
hex_char(N,C) :- C is N-10+0'A.


%%% URL encoding/decoding %%%

% ============================================================================
% url_info(+Url:(atom ; string), -Info:url_info)
% url_info(-Url:string, +Info:url_info)
%
% type url_info = http(Host:atom, Port:integer, Document:string)
%
% Extracts information from a URL, or creates a URL from information
% ============================================================================

url_info(Url, Info) :-
        atom(Url), !,
        atom_codes(Url, UrlStr),
        url_to_info(UrlStr, Info).
url_info(Url, Info) :-
        instantiated_string(Url), !,
        url_to_info(Url, Info).
url_info(Url, Info) :-
        info_to_url(Info, Url).

url_to_info(Url, http(Host,Port,Document)) :-
        http_url(Host, Port, Document, Url, []), !.
% More protocols may be added here...

http_url(Host,Port,Doc) -->
        "http://",
        internet_host(Host),
        optional_port(Port),
        http_document(Doc).

internet_host(Host) -->
        internet_host_char(C),
        internet_host_char_rest(Cs),
        {
            atom_codes(Host, [C|Cs])
        }.

internet_host_char_rest([C|Cs]) -->
        internet_host_char(C),
        internet_host_char_rest(Cs).
internet_host_char_rest([]) --> "".

internet_host_char(C) --> digit(C).
internet_host_char(C) --> loupalpha(C).
internet_host_char(0'-) --> "-".
internet_host_char(0'.) --> ".".

optional_port(Port) -->
        ":", !,
        parse_integer(Port).
optional_port(80) --> "".

http_document([0'/|Doc]) -->
        "/", !,
        rest(Doc).
http_document("/") --> "".

rest(S, S, []).

instantiated_string(S) :- var(S), !, fail.
instantiated_string([]).
instantiated_string([C|Cs]) :-
        integer(C),
        instantiated_string(Cs).

info_to_url(http(Host,Port,Document), Info) :- !,
        atom(Host),
        integer(Port),
        atom_codes(Host, HostS),
        port_codes(Port, PortS),
        mappend(["http://", HostS, PortS, Document], Info).
% More protocols may be added here...

port_codes(80, "") :- !.
port_codes(Port, [0':|PortS]) :-
        number_codes(Port, PortS).

% ============================================================================
% url_info_relative(+Url:(atom ; string), +Base:url_info, -Info:url_info)
%
% Extracts information from a URL, relative to a base page
% ============================================================================

url_info_relative(URL, Base, Info) :-
        atom(URL), !,
        atom_codes(URL, URLStr),
        url_info_relative(URLStr, Base, Info).
url_info_relative(URL, _Base, Info) :-
        url_info(URL, Info), !.
url_info_relative(Path, http(Host,Port,_), http(Host,Port,Path)) :-
        Path = [0'/|_], !.
url_info_relative(File, http(Host,Port,BaseDoc), http(Host,Port,Document)) :-
        \+ member(0':, File), % Naive check to ensure it is not a valid URL
        append(BasePath, BaseFile, BaseDoc),
        \+ member(0'/, BaseFile), !,
        append(BasePath, File, Document).

atomic_or_string(X) -->
        {atomic(X), name(X,S)}, !,
        string(S).
atomic_or_string(S) -->
        string(S).

textarea_data('$empty') --> [].
textarea_data(X) -->
        {atomic(X), name(X,S)}, !,
        string(S).
textarea_data(L) -->
        http_lines(L), !.
textarea_data(S) -->
        string(S).

:- meta_predicate html_protect(goal).

html_protect(Goal) :-
        catch(Goal,E,html_report_error(E)).
html_protect(_) :-
        html_report_error('Sorry, PiLLoW application failed.').

%%% Support predicates %%%

%% Concatenates a list of lists
mappend([], []).
mappend([S|Ss], R) :-
        append(S, R0, R),
        mappend(Ss, R0).

% ----------------------------------------------------------------------------
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*3+115,1999/11/24,00:58*36+'MET'), "Added file to
   version control.  (Manuel Hermenegildo)").

% ----------------------------------------------------------------------------
