:- module(field_type, [fieldType/1],[assertions,isomodes]).

:- comment(author, "G@..{o}ran Smedb@..{a}ck").


:- comment(version(1*9+174,2003/12/04,17:47*35+'CET'), "Added author
info.  (Manuel Carro)").

:- comment(version(0*1+0,1998/12/09,13:30*46+'MET'), "Documented.  (Goran
   Smedback)").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred fieldType(+FieldTypeId)
   :: atm
   # "Boolean predicate used to check the fieldTypeId with the defiened.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fieldType('MFColor').
fieldType('MFFloat').
fieldType('MFInt32').
fieldType('MFNode').
fieldType('MFRotation').
fieldType('MFString').
fieldType('MFVec2f').
fieldType('MFVec3f').

fieldType('SFBool').
fieldType('SFColor').
fieldType('SFFloat').
fieldType('SFImage').
fieldType('SFInt32').
fieldType('SFNode').
fieldType('SFRotation').
fieldType('SFString').
fieldType('SFTime').
fieldType('SFVec2f').
fieldType('SFVec3f').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% End:

:- comment(version_maintenance,dir('../../version')).

