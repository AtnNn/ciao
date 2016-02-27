:- module(persdbtr, [persistent_tr/2], []).

persistent_tr((:- persistent(F/A,D)),
	[(:- data(F/A)),
	 '$is_persistent'(F/A,D)]).

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

