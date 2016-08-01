% **********************************************************************
% * index.pl version alfa 1.0 2015-3-4                                 *
% * Páginas web principal y de Lógica generadas a partir de un         *
% * código con HTML termerizado en Prolog. Proyecto ULAnix             *
% * mathematica. Este programa es Software Libre. Desarrollado por     *
% * Ixhel Mejías, basándose en la página web diseñada e implementada   *
% * por Santiago y Sánchez (2014), disponible en:                      *
% * http://nux.ula.ve/mathematica/mat_discretas.zip.                   *
% **********************************************************************

% Se debe correr previamente mathematica.pl.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- use_module(library(http/http_client)).

:- http_handler(css('css/bootstrap.css'), http_reply_file('css/bootstrap.css', []), []).
:- http_handler(css('css/bootstrap.min.css'), http_reply_file('css/bootstrap.min.css', []), []).
:- http_handler(js('js/bootstrap.js'), http_reply_file('js/bootstrap.js', []), []).
:- http_handler(js('js/jquery-3.0.0.min.js'), http_reply_file('js/jquery-3.0.0.min.js', []), []).
:- http_handler(js('js/jquery.min.js'), http_reply_file('js/jquery.min.js', []), []).
%:- http_handler(images('images/educerecabecera.png'), http_reply_file('images/educerecabecera.png', []), []).

:- http_handler(css('css/myStyles.css'), http_reply_file('css/myStyles.css', []), []).


:- html_resource(jquery, [virtual(true),
	   requires('https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js')]).
:- html_resource(bootstrap_js, [virtual(true), ordered(true),
	    requires([jquery, 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js'])]).


:- html_resource(js('js/bootstrap.js'), [requires(js('js/jquery.min.js'))]).





http:location(css, root(css), []).
http:location(js, root(js), []).
%http:location(images, root(images), []).

http:location(css, root(css), []).

% **********************************************************************

:- http_handler('/', landing_pad, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

landing_pad(Request) :-

	reply_html_page(
		\['<!DOCTYPE html>'],
	   [title('CEA: Catalogo Electronico Autonomo.')],
	    [\page_content(Request)]).

% **********************************************************************
% Contenido de la página principal.

page_content(_Request) -->
	html(
	   [
	   \html_requires(css('css/bootstrap.css')),
	   \html_requires(css('css/bootstrap.min.css')),

		\html_requires(js('js/jquery.min.js')),
		\html_requires(js('js/bootstrap.js')),

		\html_requires(jquery),
		\html_requires(css('css/myStyles.css')),
		%\html_requires('images/educerecabecera.png'),





	    \head,
	    \barraNav


     ]).


% **********************************************************************
% Mediante "head" se incluye una cabecera a la página, con la imagen
% de ULAnix Mathematica.

head --> html(
			header(
				[div(class='img-responsive container row col-xs-12 col-sm-12 col-md-12 col-lg-12',
					img([src='https://s32.postimg.org/rclj1hved/educerecabecera.jpg', class='img-responsive',
					% img([src='images/educerecabecera.jpg', class='img-responsive',
					 style='float: left; margin: 0px 0px 0px 100px;'])
					)
				])
			).


/*head --> html(header([div(class='img-responsive container row col-xs-12 col-sm-12 col-md-12 col-lg-12',img([src='http://s14.postimg.org/dossnbh4h/ULAnix.jpg', class='img-responsive', style='float: left; margin: 0px 0px 0px 100px;']))])).*/
% **********************************************************************
% Mediante "enlaces" se incluye el código que genera la barra de
% navegación con el menú desplegable que contiene los enlaces a los otros
% temas de matemáticas y a otras páginas web.
%
%  quede aquiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii ojooooooooooooooo
/*
enlaces --> html(form(method='POST', [

		 \[


		'<nav id="menu-responsive" class="navbar navbar-default" role="navigation" >
  <div class="container-fluid">
    <!-- Brand and toggle get grouped for better mobile display -->
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1" aria-expanded="false">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <span class="navbar-brand" href="#">CEA</span>
    </div>

    <!-- Collect the nav links, forms, and other content for toggling -->
    <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
      <ul class="nav navbar-nav">
        <li class="active" ><a href="agregar.php">Buscar <span class="sr-only">(current)</span><span class="glyphicon glyphicon-plus"></span></a></li>
        <li><a href="buscar.php">Guardar <span class="glyphicon glyphicon-cog "></span></a></li>

        <li><a href="exportar.php">Indices<span class="glyphicon glyphicon-save"></span></a></li>
        <li><a href="exportar.php">Abrir<span class="glyphicon glyphicon-save"></span></a></li>

      </ul>

        <ul class="nav navbar-nav navbar-right">
        <li><a href="salir.php">Salir <span class="glyphicon glyphicon-user"></span></a></li>

    </div><!-- /.navbar-collapse -->
  </div><!-- /.container-fluid -->
</nav>
</div>']

				     ]) ).

*/

barraNav --> html(form(method='POST', [

		 \[
		 '
		 	<nav class="">
  <div class="container-fluid">
    <div class="navbar-header">
      <a class="navbar-brand" href="#">WebSiteName</a>
    </div>
    <ul class="nav navbar-nav">
      <li class="active"><a href="#">Home</a></li>
      <li><a href="#">Page 1</a></li>
      <li><a href="#">Page 2</a></li>
      <li><a href="#">Page 3</a></li>
    </ul>
  </div>
</nav>
/*
<div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1" >
	<div class="row" >
	  <div class="col-sm-1">

barra_nav --> html(nav(
	[class='navbar navbar-inverse', method='POST'],
	[

	div([class = 'container-fluid'],
		[
			div([class='navbar-header'],
				[
			a([class='navbar-brand'],'CEA ******************************************************')
			]),

		ul([class='nav navbar-nav', class='col-md-4'],
			[

			a([class='navbar-brand'],'CEA ******************************************************')


			])

		])])).

	        <div class="navbar navbar-inverse"  class="col-xs-6">
	          <ul class="nav navbar-nav">
	            <li><a href="#"><span class="label label-primary">Buscar</span></a></li>
	            <li><a href="#"><span  class="label label-primary">Guardar</span></span></a></li>
	            <li><a href="#"><span class="label label-primary">Indices</span></a></li>
	            <li><a href="#"><span class="label label-primary">Abrir</span></a></li>
	            <li><a href="#"><span class="label label-danger">Salir</span></a></li>

	          </ul>
	        </div><!--/.nav-collapse -->


	  </div>
	</div>
</div>*/

		']

				     ]) ).

% **********************************************************************
% Mediante "busqueda" se genera la sección de la interfaz donde el
% usuario interactúa con el tutor a través de las preguntas y
% respuestas. Además, se presenta al usuario todas las preguntas de
% Lógica sugeridas.

% **********************************************************************
% * Fuente: servicio.pl version alfa 0.1 2014-10-16                    *
% * Editado por Ixhel Mejías                                           *
% **********************************************************************


?- server(8000).

/*
enlaces --> html(body( method='POST', [


		 \[
		'

<nav class="navbar navbar-inverse">


  <div class="container-fluid">
	    <div class="navbar-header">
	      <a class="navbar-brand" href="#">CEA ******************************************************</a>
	    </div>
	        <ul class="nav navbar-nav" class="col-md-4">
		        <li><button name="submit" type="submit" class="btn btn-md btn-success btn-block" value="Buscar" formaction="/buscar" formmethod="POST" > Buscar <span class="glyphicon glyphicon-search"></span></button></li>
		        <li><button name="submit" type="submit" class="btn btn-md btn-success btn-block" value="Guardar" formaction="/guardar" formmethod="POST"> Guardar <span class="glyphicon glyphicon-save"></span></button></li>
		        <li><button name="submit" type="submit" class="btn btn-md btn-success btn-block" value="Indices" formaction="/indices" formmethod="POST"> Indices <span class="glyphicon glyphicon-list"></span></button></li>
		        <li><button name="submit" type="submit" class="btn btn-md btn-success btn-block" value="Abrir" formaction="/abrir" formmethod="POST"> Abrir <span class="glyphicon glyphicon-folder-open"></span></button></li>
		        <li><button name="submit" type="submit" class="btn btn-md btn-success btn-block" value="Salir" formaction="/salir" formmethod="POST"> Salir <span class="glyphicon glyphicon-remove"></span></button></li>


	  <a class="navbar-brand" href="#"> ****************************************************** CEA</a>
  </div>
</nav>

	<div class="container-fluid">
		<div class="form-group col-xs-11 col-sm-11 col-md-11 col-lg-11"style="margin:0px">
		  <h4 class="venacti", class= "text-center">Búsqueda Detallada del Catalogo Electrónico Atónomo </h4>
		</div>

		<div class="col-xs-11 col-sm-11 col-md-11 col-lg-11">

			<div class="form-group col-xs-11 col-sm-11 col-md-11 col-lg-11">
		  		<h3 class= "text-center">Consulta<input name=p,  class="form-control input-md",type=textarea, placeholder="Ingrese lo que desea buscar." ></h3>
		    </div>

			<div class="form-group col-xs-11 col-sm-11 col-md-11 col-lg-11">
		   		 <h3 class="text-center">Salida<input name=r, class="form-control input-md", type=textarea, placeholder="Defina el nombre del archivo de salida.">
				 </h3>
			</div>



		<div class="form-group">
				  <h1 class= "text-center"><label for="comment">Resultados</label></h1>


				  <textarea class="form-control" rows="15" id="comment"></textarea>
			</div>

		</div>

	</div>
</div>



']

				     ])  ).

				     */