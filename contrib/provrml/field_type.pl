:- module(field_type, [fieldType/1],[assertions,isomodes]).


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
%% update-version-comments: "../../version"
%% End:

