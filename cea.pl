% cea.pl Catalogo Electronico Autonomo
%
% an integrator GUI interface for swish-e, the indexing system and a browser
% originally based on code from the library of examples in www.swi-prolog.org
% by Yaritza Vargas <vargas.yaritza@gmail.com> and Jacinto D�vila <jacinto@ula.ve>
% this is free software with the GPL3 license

:- module(cea,
	  [ cea/0, search_index/2, html_table_out/1, html_pairs_out/1, ajusta_camino/2
	  ]).
:- use_module(library(pce)).  % pce library grafica para botones,ventanas....
:- use_module(library(http/http_header)).
:- use_module(library(www_browser)).
:- use_module(library(http/html_write)).
:- use_module(library(url)).
:- use_module(library(process)).
:- use_module(library(listing)).
:- use_module(library(backcomp)).
% :- use_module('/usr/lib/swi-prolog/xpce/prolog/lib/pce_dispatch.pl').
:- use_module(library(socket)).
:- use_module(library(gensym)).
:- use_module(library(date)).
:- use_module(library(prolog_stack)).


:- require([ send_list/3 ]).

cea :-
  new(S, cea), %se crea el objeto cea para la interfaz grafica S:referencia y cea el objeto
  send(S, open).  %se invoca al metodo open para aperturar la interfaz

%browser('BROWSER', 'google-chrome-stable').
browser('BROWSER', firefox).

:- pce_begin_class(cea, frame).

initialise(F) :->
	"Crea GUI de CEA"::
	send(F, send_super, initialise, 'CEA: Catalogo Electronico Autonomo'),
	send(F, append, new(D, dialog('Catalogo Electronico Autonomo'))),
	send(D, append, label(reporter)),
	send(D, append, new(@query, text_item(consulta, ''))),
	send(D, append, new(@output, text_item(salida, 'salida.html'))),
	send(D, append, new(BTS, dialog_group(buttons, group))),
	send(BTS, gap, size(0, 40)),
	send(BTS, append, button(buscar, message(F, buscar))),
	send(BTS, append, button(guardar, message(F, guardar)), below),
	send(BTS, append, button(indices, message(F, indices)), below),
	send(BTS, append, button(salir, message(F, salir)), below),
	send(BTS, append, button(abrir, message(F, abrir)), below),
	send(BTS, layout_dialog),
	send(D, append, new(@browser, list_browser), right),
	send(@browser, alignment, left),
	send(@browser, width, 100),
	send(@browser, font, font(helvetica, roman, 15)),
	send(D, layout),
	send(@browser, bottom_side, BTS?bottom_side),
	send(@browser, right_side, @query?right_side),
	send(D, open).

%@browser and @query son variables especiales

buscar(_) :->
    "El usuario dice buscar"::
    send(@browser, clear),
    get(@query, selection, Consulta),
    object(Consulta, Desc),
    search_index(Desc, Reporte), % Reporte = (Head, Answers)
    guarda_reporte(Reporte), % corrige y guarda reporte
    reporte_cea((_H,Answers)), % recupera
    recorta(50, Answers, Ans), % actualiza lista de browsing
    forall(member((_,U,_,_), Ans), send(@browser, append, U)),
    send(@browser, open_message, message(@prolog, abre_navegador, @arg1?key)).

guardar(_) :->
    "Guardando el resultado de la busqueda"::
    % get(@query, selection, Consulta),
    % object(Consulta, Desc),
    get(@output, selection, Path),
    object(Path, PathFile),
    reporte_cea(Reporte) ->
    ( ( save_file(PathFile, Reporte),
      abre_navegador(PathFile) )
    ; true ).

indices(W) :->
    "El usuario abre el indice"::
    %shell('firefox index.html', Status),
    abre_navegador('index.html'),
    send(W, return, 0).

abrir(_) :->
     "Abrir el articulo seleccionado"::
     get(@browser, selection, Name),
     object(Name, Dict_item),
     get(Dict_item, label, Label),
     abre_navegador(Label).
     % invoca(firefox, Label).

salir(W) :->
     "Limpiar y salir"::
     free(@query),free(@output), free(@browser),
     send(W, wm_delete), halt(1).

:- pce_end_class.

:- pce_begin_class(swishe, process).

encoding(M,
	 Enc:encoding={ascii,iso_latin_1,text,utf8,unicode_be,unicode_le}) :->
	"Set the character encoding for the file"::
	(   get(M, input, File),
	    File \== @nil
	->  send(File, encoding, Enc)
	;   send(M, report, warning, "No file")
	).

:- pce_end_class.

save_file(PathFile, Reporte) :-
	catch( (
		open(PathFile, write, Stream, [encoding(utf8), alias(reportecea)]),
		with_output_to(reportecea, cea:html_out(Reporte)),
		close(Stream)
	), E, writeln(E)).

recorta(_, [], []).
recorta(0, _, []).
recorta(N, [F|R], [F|RR]) :- NN is N - 1, recorta(NN, R, RR).

guarda_reporte((H, Body)) :-
   corrige(Body, Corregido), % corrige camino al pdf
   retractall(reporte_cea(_)), assert(reporte_cea((H,Corregido))).

corrige([], []).
corrige([(Rank,Url,File,Size)|Rest], [(Rank, NewUrl, File, Size)|ORest] ) :-
	ajusta_camino(Url, NewUrl), writeln(NewUrl),
	corrige(Rest, ORest).

ajusta_camino(Url, New) :-
	name(Url, [_|RestUrl]),
	name(NoDot, RestUrl),
	concat('./indices', NoDot, New).

invoca(Programa, Argumento) :-
    concat(Programa, ' ', P),
    concat(P, Argumento, C),
    concat(C, ' &', Command), % run in background
    shell(Command, _).

abre_navegador(URL) :-
	browser(B, N), setenv(B,N),
	absolute_file_name(URL, TURL),
	catch(www_open_url(TURL), E, writeln(E)).

/* Processing information from the indexer */
search_index(Query, Reporte) :-
	retractall(salida(_)),
	( current_prolog_flag(windows, true) ->
          catch(call_winOS('swishe.bat ', [Query], Output), Excep, writeln(Excep)) % call to Windows
	; catch(call_unix_temp('swish-e', [' -f ./indices/index.swish-e -w ', Query], Output), Excep, writeln(Excep)) % call to Linux
	),
	assert(salida(Output)),
	reporte(Reporte,Output,_Rest).

call_unix(Utl, Args, Output) :-
	NewTerm =.. [process, Utl | Args],
	new(P, NewTerm),
	%send(P, environment, 'LANG', 'es_VE.UTF-8'),
	%send(P, environment, 'LANGUAGE', 'es_VE:es'),
	send(P, use_tty, @off),
	new(OS, string),
	send(P, record_separator, @nil),
	send(P, input_message, message(OS, append, @arg1)),
	send(P, open),
	send(P, wait),
	pce_string_to_list(OS, Output),
	send(P, done),
	send(OS, done).

call_unix_off(Utl, Args, Output) :-
	NewTerm =.. [process, Utl | Args],
	new(P, NewTerm),
	send(P, use_tty, @off),
	send(P, open),
	new(OS, string),
	repeat,
	(
	get(P, read_line, Line)
	-> send(OS, append, Line),
	fail
	;
	!
	),
	pce_string_to_list(OS, Output).

% would it run on a dvd?
call_unix_temp(Utl, Args, Output) :-
	tmp_file_stream(utf8, File, Stream),
	close(Stream),
	%File = 'temporal.tmp',
	%(exists_file(File) -> delete_file(File) ; true),
	append([Utl|Args], [' > ', File], Lista),
	concatena(Lista, Command),
         shell(Command, _Status),
	new(F, file(File, utf8)),
	send(F, open, read),
	new(OS, string),
	repeat,
	(
	get(F, read_line, Line)
	-> send(OS, append, Line),
	fail
	;
	!
	),
	pce_string_to_list(OS, Output),
	delete_file(File).

call_winOS(Command, Args, Output) :-
	append([Command|Args], ['salidaswishe.txt'], Lista),
	concatena(Lista, ActualCommand),
        shell(ActualCommand),
	open('salidaswishe.txt', read, Stream),
	read_lines(Stream, Lines),
	concatenart(Lines, Atoms),
	atom_string(Atoms, String),
	string_codes(String, Output),
	close(Stream).


pce_string_to_list(S, L) :-
	pce_string_to_list(S, 0, L).
pce_string_to_list(S, I, [C|T]) :-
	get(S, character, I, C), !,
	NI is I + 1,
	pce_string_to_list(S, NI, T).
pce_string_to_list(_, _, []).

% from Jan Wielemaker
call_unix_right(Utl, Args, Output) :-
        NewTerm =.. [process, Utl | Args],
        new(P, NewTerm),
        send(P, use_tty, @off),
        new(OS, string),
        send(P, record_separator, @nil),
        send(P, input_message, message(OS, append, @arg1)),
        send(P, open),
        send(P, wait),
        get(OS, value, Output),
        send(P, done),
        send(OS, done).

concatena([], '').
concatena([U|R], Comando) :-
	concatena(R, RComando),
	concat(U, ' ', PrevioU),
	concat(PrevioU, RComando, Comando).

concatenart([], '').
concatenart([U|R], Comando) :-
	concatenart(R, RComando),
	concat(U, '\n', PrevioU),
	concat(PrevioU, RComando, Comando).

read_lines(Out, Lines) :-
        read_line_to_codes(Out, Line1),
        read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Codes, Out, [Line|Lines]) :-
        atom_codes(Line, Codes),
        read_line_to_codes(Out, Line2),
        read_lines(Line2, Out, Lines).


test_concatena :-
	concatena(['swish-e',' -w ' ,'Asociaci�n de Profesores del Instituto Pedag�gico de Caracas UPEL'], 'swish-e -w Asociaci�n de Profesores del Instituto Pedag�gico de Caracas UPEL').


html_out((Head, Body)) :-
        phrase(web_page(Head, Body), Tokens),
        print_html(Tokens).

html_table_out((Head, Body)) :-
        phrase(web_table(Head, Body), Tokens),
        print_html(Tokens).

html_pairs_out(AttribsValues) :-
        phrase(web_table_pairs(AttribsValues), Tokens),
        print_html(Tokens).

%       emit page with title, header and table of matches

web_page((Query, Hits), Body) -->
        page([ title(['Busqueda de: ', Query])
             ], \web_table((Query, Hits), Body)
             ).

web_table((Query, Hits), Body) -->
	html([ div(''),
	       % img([src='./imagenes/educerecabecera.jpg', alt='EDUCERE-ULA'], []),
	       h2(align(center), ['La frase: ', Query]),
	       h3(align(center), ['Produjo ', Hits, ' resultados']),
               table([ align(center),
                       border(1),
                       width('100%')
                     ],
                     [ tr([ th('Relevancia'),
                            th('Archivo (URL implicito)'),
			    th('Tamano (en Bytes)')
                          ])
                     | \web_rows(Body)
                     ])
             ]).

web_table_pairs(Body) -->
	html([ div(''),
		%h2(align(center), ['La frase: ', Query]),
		%h3(align(center), ['Produjo ', Hits, ' resultados']),
               table([ align(left),
                       border(2),
                       width('100%')
                     ],
                     [ tr([ th('Registros enlazados a sus archivos')
                            %th('Archivo (URL implicito)'),
			 %   th('Tamaño (en Bytes)')
                          ])
                     | \web_pairs(Body)
                     ])
             ]).

%       emit the rows for the body of the table.

web_rows([]) -->
        [].
web_rows([(Rank, Url, File, Size)|T]) -->
        html([ tr([ td(Rank),
		    td(\predref(Url/File)),
                    td(em(Size))
                  ])
             ]), % { writeln((Rank, Url, File, Size)) },
        web_rows(T).

web_pairs([]) -->
        [].
web_pairs([(Author, File)|T]) -->
        html([ tr([ td(\predref(File/Author))
                  ])
             ]), % { writeln((Author, File)) },
        web_pairs(T).

%       predref(Url/File)
%
%       Emit File as a hyperlink to Url
%
%       we must do form-encoding for the name as it may contain illegal
%       characters.  www_form_encode/2 is defined in library(url).

predref(Url/File) -->
        { www_form_encode(Url, Encoded),
           sformat(Href, '~w', [Encoded]),
	  sformat(EFile, '~w', [File])
        },
        html(a(href(Href), [EFile])).


read_out([], _, []).
read_out(In, Out, Output) :-
    leer_atomos(In, Next, Tokens, _, _),
    read_out(Next, Out, RestTokens),
    append(Tokens, RestTokens, Output).

l :- get(cea('Buscador en Catalogo Electronico Autonomo'), prompt, Name),
     writeln(Name).
     % www_open_url(Name).

w_list([]).
w_list([D|R]) :- write(', \''), write(D), write('\''), w_list(R).

% qsave_program('cea.linux', [goal=cea, autoload=true, stand_alone=true]).

% test

test_s :-
	search_index('Asociaci�n de Profesores del Instituto Pedag�gico de Caracas UPEL', _A).

test_search(Desc, Answers) :-
   search_index(Desc, (_,Answers)).

test_swish1 :-
   shell('swish-e -f ./indices/index.swish-e -w "Asociaci�n de Profesores del Instituto Pedag�gico de Caracas UPEL"', _L).

test_swish3 :-
   shell('swish-e -f "./indices/index.swish-e" -w "Jacinto D�vila"', _L).

test_swish2 :-
   call_unix('swish-e', ['-w -f ./indices/index.swish-e ' ,'"Asociación de Profesores del Instituto Pedagógico de Caracas UPEL"'], Output), name(A, Output), writeln(A).

test_r :-
   call_unix('ls', ['-l'], Output), read_out(Output, _, Block), w_list(Block).

test_f :-
   name('# SWISH format: 2.4.7\n', L), formato(L, []).

test_enc(Con) :-
   name('# SWISH format: 2.4.7\n# Search words: Jean-Claude\n# Removed stopwords:\n # Number of hits: 5\n# Search time: 0,000 seconds\n# Run time: 0,013 seconds\n', Head),
   encabezado(Con, Head, []).

test_cuer(Data) :-
   name('1000 ./pdfs/references/OA/001-JCG2001-in-oldenburgs-long-shadow.pdf "001-JCG2001-in-oldenburgs-long-shadow.pdf" 1228721\n770 ./pdfs/references/OA/003-Current Sociology-2013-Vessuri&alii_pubd.pdf "003-Current Sociology-2013-Vessuri&alii_pubd.pdf" 787133\n608 ./pdfs/references/OA/002-PS2012-9780262517638_Open_Access_PDF_Version.pdf "002-PS2012-9780262517638_Open_Access_PDF_Version.pdf" 1546378\n497 ./pdfs/references/OA/008-JC-Guedon-2008-Brazil-final.pdf "008-JC-Guedon-2008-Brazil-final.pdf" 388987\n385 ./pdfs/references/OA/leyes-o-acuerdos/informeoaperu.pdf "informeoaperu.pdf" 677919\n.', B),
   cuerpo(Data, B, []).


% gramatica de la salida del indizador

reporte((Head,Data)) --> encabezado(Head), cuerpo(Data), {!}.

encabezado((Consulta, Hits)) --> formato, consulta(Query), stopwords, resultados(H),
	{name(Consulta, Query), name(Hits, H), !}.

% resultados(0) --> izq, end_of_line, period, {!}.

resultados(H) --> hits(H), searchtime, runtime, {!}.

formato --> izq, colon, texto(_), end_of_line.

consulta(T) --> izq, colon, whites, texto(T), end_of_line, {name(NF, T), writeln(NF)}.

stopwords --> izq, colon, texto(_S), end_of_line.
	%{name(NF, S), write(' ->'), write(NF), write('<-')}.

hits(H) --> izq, colon, whites, texto(H), end_of_line, {name(NF, H), writeln(NF)}.

searchtime --> izq, colon, texto(ST), end_of_line, {name(NF, ST), writeln(NF)}.

runtime --> izq, colon, texto(RT), end_of_line, {name(NF, RT), writeln(NF)}.

cuerpo([]) --> period, {!}.
cuerpo([I|R]) --> one_entry(I), cuerpo(R).

one_entry((Rank,U,File,S)) -->
  rank(R), whites, url(URL), whites, filename(F), whites, size(Size), end_of_line,
  {name(Rank, R), name(U, URL), name(File, F), name(S, Size), writeln(U),!}.

rank(R) --> texto(R).

url(URL) --> texto(URL).

filename(F) --> comilla, anything(F), comilla.

anything([]) --> [].
anything([C|R]) --> [C], {not(name('"',[C]))}, anything(R).

size(S) --> texto(S).

texto([A]) -->  alfanumerico(A).
texto([A|R]) --> alfanumerico(A), texto(R).

izq_unit --> [C],  % cualquier cosa menos : y fin de línea o de texto
   {not(name(':',[C])),not(name('\r',[C])),
    not(name('\n',[C])),not(name('.', [C])),
    not(char_type_char(C, fin, _))}.

izq --> izq_unit.
izq --> izq_unit, izq.

alfanumerico(C) --> [C],
   {not(name('\n',[C])),not(name('\r',[C])),
    (char_type_char(C, alfa, _);char_type_char(C,especial,_))}.

whites --> blanco.
whites --> blanco, whites, {!}.

blanco --> [C], {char_type_char(C, blanco,_)}.

end_of_line --> [C], {name('\n',[C]);name('\r',[C])}.

colon --> [C], {name(':',[C])}.

period --> [C], {name('.', [C])}.

comilla --> [C], {name('"', [C])}.

error --> texto(T), {name('err', T)}.

/* Tomado del resumidor.sourceforge.net */

% leer_atomos(-Atomos,-AtomosUpper,-ProximoC)
% Lee una línea del texto, separándola en una lista de átomos lower-case
% y upper-case respectivamente.

leer_atomos(BE, BS, Atomos, AtomosUpper, ProximoC) :-
    leer_caracter(BE, BN, PrimerC, PrimerCUpper, PrimerT),
    leer_oracion(BN, BS, PrimerC, PrimerCUpper, PrimerT, Atomos, AtomosUpper, ProximoC).

% leer_oracion(+PrimerC,+PrimerCUpper,+PrimerT,-Lista,-ListaUpper,-ProximoC)
% Dado el primer caracter lower y upper case, respectivamente, además
% del tipo de caracter correspondiente retorna la lista de palabras de
% la oración. La oración esta delimitada por cualquier caracter de fin,
% en especial el punto [46].

leer_oracion(B, B, Caracter,Caracter,fin,[],[],Caracter) :- !.

leer_oracion(B, B, 46,46,especial,[],[],46) :- !.

leer_oracion(BE, BS, _,_,blanco,Atomos,AtomosUpper,ProximoC) :-
    !,
    leer_atomos(BE, BS, Atomos,AtomosUpper,ProximoC).

leer_oracion(BE, BS, PrimerC,PrimerCUpper,Type,[PrimerC|Atomos],[AUpper|AtomosUpper],ProximoC) :-
    !, (Type=especial;Type=alfa),
    % name(A,[PrimerC]),
    name(AUpper,[PrimerCUpper]),
    leer_atomos(BE, BS, Atomos,AtomosUpper,ProximoC).

%% Tokenizer modificado por Jacinto Dávila - 2012 Junio 21
%% tomado de
% et.pl - M. Covington      2003 February 12
% etu.pl - Modified for Unicode - Donald Rogers     2006 July 17
%          email: dero9753@ihug.co.nz
%          Modified to cope with comma in numbers   2006 July 20

% ET the Efficient Tokenizer

%%
%% Character classification
%%

% char_type_char(+Char,-Type,-TranslatedChar)
%   Classifies all characters as letter, digit, special, etc.,
%   and also translates each character into the character that
%   will represent it, converting upper to lower case.
% modified to handle a code as input directly :JD

char_type_char(Code,Type,Tr) :-
   atom_chars(Char, [Code]),
   char_table(Char,Type,Tr),
   !.

% Donald changed this from special to letter.
% Using downcase_atom saves having an enormous table
% and should handle all languages.
% letter -> alfa
char_type_char(Char,alfa,Char) :-
   atom_chars(_,[Char]).
   %downcase_atom(L2,L3),
   %atom_chars(L3,[Char2]).

% End of line marks
% eol -> fin
char_table(end_of_file, fin, end_of_file).
char_table(-1, fin, end_of_file).
%char_table('\n',        fin, '\n'       ).

% Whitespace characters
% whitespace -> blanco
char_table(' ',     blanco,  ' ').     % blank
char_table('\t',    blanco,  ' ').     % tab
% char_table('\r',    blanco,  ' ').     % return
char_table('''',    blanco, '''').     % apostrophe does not translate to blank
% char_table('\n',    blanco, '\n').

% Donald removed the letter characters and replaced them by special characters.
% There are too many Unicode letters to put them all in a table.
% The third parameter may be useless, but maybe someone will want to convert
% some of the special characters.
% There may be other Unicode characters that need to be added.
% special -> especial
char_table('~',     especial,    '~' ).
char_table('`',     especial,    '`' ).
char_table('!',     especial,    '!' ).
char_table('@',     especial,    '@' ).
char_table('#',     especial,    '#' ).
char_table('$',     especial,    '$' ).
char_table('\u0025',especial,    '\u0025' ). %
char_table('^',     especial,    '^' ).
char_table('&',     especial,    '&' ).
char_table('*',     especial,    '*' ).
char_table('(',     especial,    '(' ).
char_table(')',     especial,    ')' ).
char_table('_',     especial,    '_' ).
char_table('-',     especial,    '-' ).
char_table('+',     especial,    '+' ).
char_table('=',     especial,    '=' ).
char_table('{',     especial,    '{' ).
char_table('[',     especial,    '[' ).
char_table('}',     especial,    '}' ).
char_table(']',     especial,    ']' ).
char_table('|',     especial,    '|' ).
char_table('\\',    especial,    '\\' ).
char_table(':',     especial,    ':' ).
char_table(';',     especial,    ';' ).
char_table('"',     especial,    '"' ).
char_table('<',     especial,    '<' ).
char_table(',',     especial,    ',' ).
char_table('>',     especial,    '>' ).
char_table('.',     especial,    '.' ).
char_table('?',     especial,    '?' ).
char_table('/',     especial,    '/' ).

% Digits
% digit -> alfa ; Ojo, corregir
char_table('0',   alfa,     '0' ).
char_table('1',   alfa,     '1' ).
char_table('2',   alfa,     '2' ).
char_table('3',   alfa,     '3' ).
char_table('4',   alfa,     '4' ).
char_table('5',   alfa,     '5' ).
char_table('6',   alfa,     '6' ).
char_table('7',   alfa,     '7' ).
char_table('8',   alfa,     '8' ).
char_table('9',   alfa,     '9' ).

% Everything else is a letter character.


% fin cea

