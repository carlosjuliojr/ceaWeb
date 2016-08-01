% *************************************************************************
% * ceaWeb.pl                                                             *
% * Página web principal del Catalogo Electronico Autonomo partir de un  *
% * código con HTML termerizado en Prolog. Proyecto final tesis.          *
% * Este programa es Software Libre. Desarrollado por                     *
% * Julio Jaimes y Jacinto Davila, basándose la app CEA dise#ada por      *
% * por Jacinto Davila y Yaritza Vargas                                   *
% *************************************************************************

% Se debe correr previamente mathematica.pl.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- use_module(library(http/http_client)).

:- http_handler(css('css/bootstrap.css'), http_reply_file('css/bootstrap.css', []), []).
%:- http_handler(css('css/bootstrap.min.css'), http_reply_file('css/bootstrap.min.css', []), []).
:- http_handler(js('js/bootstrap.js'), http_reply_file('js/bootstrap.js', []), []).
:- http_handler(js('js/jquery.min.js'), http_reply_file('js/jquery.min.js', []), []).
%:- http_handler(images('images/educerecabecera.png'), http_reply_file('images/educerecabecera.png', []), []).

:- http_handler(css('css/myStyles.css'), http_reply_file('css/myStyles.css', []), []).

:- html_resource(jquery, [virtual(true),
	   requires('https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js')]).
:- html_resource(bootstrap_js, [virtual(true), ordered(true),
	    requires([jquery, 'http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css'])]).


:- html_resource(js('js/bootstrap.js'), [requires(js('js/jquery.min.js'))]).


http:location(css, root(css), []).
http:location(js, root(js), []).
%http:location(images, root(images), []).
http:location(fonts, root(fonts), []).
http:location(css, root(css), []).


% **********************************************************************

:- http_handler('/', say_hi, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

say_hi(Request) :-

	reply_html_page(
	   [title('CEA: Catalogo Electronico Autonomo.')],
	    [\page_content(Request)]).

% **********************************************************************
% Contenido de la página principal.

page_content(_Request) -->
	html(
	   [

	    \html_requires(jquery),
	    \html_requires(bootstrap_js),
		\html_requires(css('css/bootstrap.css')),
		%\html_requires(css('css/bootstrap.min.css')),

		\html_requires(js('js/jquery.min.js')),
		\html_requires(js('js/bootstrap.js')),

	    \head,
	    \enlaces

     ]).




% **********************************************************************
% Mediante "head" se incluye una cabecera a la página, con la imagen
% de educere la revista venezolana de educacion.

head --> html(
			header(
				[div( class='img-responsive container row col-xs-12 col-sm-12 col-md-12 col-lg-12',
					img([src='https://s32.postimg.org/rclj1hved/educerecabecera.jpg', class='img-responsive',
					% img([src='images/educerecabecera.jpg', class='img-responsive',
					 style='float: center; margin: 0 auto;'])

					),

					h1([ align='center' ],'CEA: Catálogo Electrónico Atónomo'),
 					p([ align= 'center'], script(' document.write(Date())') )


			])).



% **********************************************************************
% Mediante "enlaces" se incluye el código que genera la barra de
% navegación con el menú desplegable que contiene los enlaces a los otros
% temas de matemáticas y a otras páginas web.
%



enlaces --> html(body([

		\barra_nav,
		\busqueda,
		\foot

	])).


barra_nav --> html(nav(
	[class="navbar navbar-inverse", method='POST'],
	[

	div([class='container-fluid'],
		[
			div([class='navbar-header'],[
				a([class='navbar-brand'],'CEA *************************')
				]),

			ul([class='nav navbar-nav', class='col-md-4'],[

				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/buscar', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Buscar']))),
				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/guardar', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Guardar']))),
				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='index.html', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Indices']))),
				li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/abrir', method='POST'],
						input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Abrir']))),
		        li(form([style='align: left; margin: 0px 0px 0px 100px;',action='/salir', method='POST'],
		        	input([name='submit', type='submit', class='btn btn-md btn-success btn-block', value='Salir'])))
				]),
		div([class='navbar-header'],[
				a([class='navbar-brand'],'CEA *************************')
				])

		])])).


busqueda --> html(div(form([style='align: left; margin: 0px 0px 0px 100px;',action='/buscar', method='POST'], [


		div([class='container-fluid'],[

			p(div([class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',style='margin:0px;'],
			  h4(class='venacti', b('Ingrese la busqueda a realizar'))
			 )),
			p(div(class='col-xs-11 col-sm-11 col-md-11 col-lg-11', html(\['<hr></hr>']))),

			p(div(class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',

			  input([name=entrada, class='form-control input-md',type=textarea, placeholder='Informacion a buscar'])
			      )),

			p(div([class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',style='margin:0px;'],
			  h4(class='venacti',b('Nombre del archivo de salida'))
			 )),
			p(div(class='form-group col-xs-11 col-sm-11 col-md-11 col-lg-11',

			    input([name=salida, class='form-control input-md', type=textarea, placeholder=' ejemplo: Salida'])
			      )
			 ),



			p(div(class='form-group col-xs-2 col-sm-2 col-md-2 col-lg-2',
		  input([name=submit, type=submit, class='btn btn-md btn-success btn-block', value='Buscar'])))


	      ])]))).

% **********************************************************************
% Mediante "foot" se genera el pie de la página. En esta sección se
% presenta un botón para subir a la parte de arriba de la página, los
% datos de los desarrolladores de la misma y, por último, la fecha y
% hora actual.

:- http_handler('/buscar', landing_pad, []).

landing_pad(Request) :-
        member(method(post), Request), !,
        http_read_data(Request, Data, []),

		procesa(Data, E2, S2),
		reply_html_page(
		    [title('Consulta')],
		    [\page_content([E2,S2], Request)]).


% **********************************************************************
% Contenido de la página de preguntas y respuestas (pr).

procesa(Data, Entrada, Salida) :-
	member(entrada=Pre, Data),
	member(salida=Res, Data),
	(Pre\='' -> read_term_from_atom(Pre, Entrada, []) ; true),
	(Res\='' -> read_term_from_atom(Res, Salida, []) ; true),
	buscar(Entrada).

page_content([E2,S2], _Request) -->
	html(
	   [

	    \html_requires(jquery),
	    \html_requires(bootstrap_js),
		\html_requires(css('css/bootstrap.css')),
		%\html_requires(css('css/bootstrap.min.css')),

		\html_requires(js('js/jquery.min.js')),
		\html_requires(js('js/bootstrap.js')),

	    \head,

	    div(class='container-fluid',[
	    form([style='align: left; margin: 0px 0px 0px 100px;',action='javascript:history.back();', method='POST'], [
	    \['<br></br>'],


		div([class='text-center'],[
	  p([style='font-size: 20pt', title='Entrada'], b('Resultados')),
		textarea([name=resultados, class='form-control', type=textarea, rows=15,  placeholder='Resultados'], E2)]),


	    \['<br></br>'],
	    p(div(class='form-group col-xs-5 col-sm-5 col-md-5 col-lg-5',
	    input([name=submit, type=submit, class='btn btn-md btn-success btn-block', value='Volver'])))])

	    ])
	    ,

	    \foot

	    ]).



foot --> html(footer(
	[

		div([class='text-center'],'Catalogo Electrónico Atónomo © 2016.'),
		div([class='text-center'],'Diseñado por Julio Jaimes R. y Jacinto Davila.'),
		p([ align= 'center'], script(' document.write(Date())') ),
		(div([id='subir',class='text-right'],
		input([name='img-subir', type=image,  src='http://s15.postimg.org/82q42vo5j/icon_top.png', onclick='window.location.href = "#top";', alt='Subir'])))
   ])).
/***************************************************************************************/
/*ACA VA CODIGO DEL CEA PARA EMPEZAR A MODIFICAR*/

%@browser and @query son variables especiales probando dsfss sd f sf sd

buscar(Desc) :-
   % "El usuario dice buscar"::
    %send(@browser, clear),
    %get(@query, selection, Consulta),
    %object(Consulta, Desc),
    search_index(Desc, Reporte), % Reporte = (Head, Answers)
    guarda_reporte(Reporte), % corrige y guarda reporte
    reporte_cea((_H,Answers)), % recupera
    recorta(50, Answers, Ans). % actualiza lista de browsing
   % forall(member((_,U,_,_), Ans), send(@browser, append, U)),
   % send(@browser, open_message, message(@prolog, abre_navegador, @arg1?key)).


   /* Processing information from the indexer */
search_index(Query, Reporte) :-
	retractall(salida(_)),
	( current_prolog_flag(windows, true) ->
          catch(call_winOS('swishe.bat ', [Query], Output), Excep, writeln(Excep)) % call to Windows
	; catch(call_unix_temp('swish-e', [' -f ./indices/index.swish-e -w ', Query], Output), Excep, writeln(Excep)) % call to Linux
	),
	assert(salida(Output)),
	reporte(Reporte,Output,_Rest).

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


?- server(5000).

