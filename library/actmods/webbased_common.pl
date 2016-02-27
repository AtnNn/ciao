
:- module(webbased_common, [common_url/1, common_path/1], []).

server_public_address(
	http('www.clip.dia.fi.upm.es',80,
	      "/ciao/library/actmods/examples/webbased_server/webbased_db/webbased_server_address"),
'/usr/lib/httpd/ciao/library/actmods/examples/webbased_server/webbased_db/webbased_server_address'
		     ).

% URL of script which knows the address of the server
common_url(URL):- server_public_address(URL,_).

% PATH for the above script
common_path(PATH):- server_public_address(_,PATH).
