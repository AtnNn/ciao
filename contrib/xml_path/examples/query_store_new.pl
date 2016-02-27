
% This fact stores an index, the URL of the queried xml document and a given 
% query over it, including the constraints

query_store(1, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/ej1.xml', 
	[X,Y,Z], Query):-
	Query = (product@val(_,"prueba")::(quantity(X), 'time-left'(Y), 
                      negotiation::preference::price(Z)) with X * Z .>. Y).

query_store(2, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/ej1.xml',[X,Y,L], Q) :-
	Q = product@val(product_name,_)::(quantity(X), location(L), 'time-left'(Y)).

query_store(3, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/ej2.xml', 	
	[X,Y,Z], Q):-	
	Q = (product@val(_,"prueba")::(quantity(X), 'time-left'(Y), 
                  negotiation::preference::price(Z)) with X * Z .>. Y).

query_store(4, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/article1098.xml',
	[X,Y], Q) :-
       Q = nitf::(head::docdata::'doc-id'@val('id-string',"020918050")::(Y),
                  body::'body.head'::abstract::p(X)).

query_store(5, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/article1000.xml',
	[X,Y], Q) :-
       Q = nitf::(head::docdata::'doc-id'@val('id-string',Y),
                  body::'body.head'::abstract::p(X)).

query_store(6, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/nitf.xml',
	[X,Y], Q) :-
        Q = nitf@(val('change.date',"7 July 2000"), val('change.time',"1900"))::(head::docdata::'doc-id'@val('id-string',Y),body::'body.head'::abstract::p(X)).
%        Q = nitf@(val('change.date',"4 July 2000"), val('change.time',"1900"))::(head::(title("Amphibien vor dem Aussterben"), docdata::'doc-id'@val('id-string',Y)),body::'body.head'::abstract::p(X)).

query_store(7, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/article1000.xml',
	[X], Q) :-
       Q = body::'body.head'::abstract::p(X).

query_store(8, 'http://clip.dia.fi.upm.es/Tests/xml_files/xmldocs/nitf2.xml',
	[X], Q) :-
	Q = abstract::p(X).
