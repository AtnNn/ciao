%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% DOCUMENTATION FILE ON CLASS SYNTAX
%%
%% AUTHOR : Angel Fernandez Pineda
%%
%% CLIP Laboratory - Technical University Of Madrid.
%%
%%         - Distributed under the Ciao Prolog license terms -
%%
%%------------------------------------------------------------------------

:- use_package(assertions).

:- comment(nodoc,assertions).

:- comment(filetype,package).

:- use_module(library('objects/objects_rt'),
	[
	    constructor/1,
	    class_name/1,
	    interface_name/1,
	    instance_id/1,
	    class_source/1,
	    interface_source/1,
	    method_spec/1,
	    virtual_method_spec/1
	]).

%% Error reporting appendix:

:- include(library('class/class_error_doc')).

%%------------------------------------------------------------------------
%%
%% MANUAL GENERAL HEADERS
%%
%%------------------------------------------------------------------------

:- comment(title,
	"Declaring classes and interfaces").

:- comment(author,"Angel Fernandez Pineda").

:- comment(copyright,"@include{Copyright.Manuals}").

:- comment(summary,
	"This section will explain how to declare a class/interface 
         using O'Ciao.").

:- comment(module,
	"O'Ciao classes are declared in the same way as traditional
         prolog modules. The general mechanism of @em{source expansion}
         will translate object-oriented declarations to normal prolog
         code. This is done transparently to the user.

         Abstract @index{interfaces} are restricted classes which
         declare exported predicates with no implementation. The implementation
         itselt will be provided by some class using an @decl{implements/1}
         declaration. Only @decl{export/1} and @decl{data/1} declarations are
         allowed when declaring an interface. Normal classes may treated 
         as interfaces just ignoring all exported predicate implementations.
        ").

:- comment(usage,
	"To declare a class the compiler must be told to use the @tt{class}
         @em{source expansion}. To do so, source code must start with a module
         declaration which loads the class package:
@begin{verbatim} 
           :- class(ClassName).
@end{verbatim} 
         @noindent or a @decl{module/3} declaration, as follows:      
@begin{verbatim} 
           :- module(ClassName,[],[class]).
@end{verbatim} 

         @noindent @concept{interfaces} are declared in a similar way:
@begin{verbatim} 
           :- interface(InterfaceName).
@end{verbatim} 
         
         Please, do not use SICStus-like module declaration, with a non-empty 
         export list. In other case, some non-sense errors will be 
         reported by normal Ciao module system.

         Most of the regular Ciao declarations may be used when defining
         a class, such as @decl{concurrent/1},@decl{dynamic/1},
         @decl{discontiguous/1},@decl{multifile/1}, and so on.

         However, there are some restrictions wich apply 
         to those declarations:
          @begin{itemize} 
           @item @decl{meta_predicate/1} declaration is not allowed to hold
                 @index{addmodule and pred(N) meta-arguments}, 
                 except for previously declared multifiles.
           @item Attribute and multifile predicates must be declared 
                 before any clause of the related predicate.
           @item There is no sense in declaring an attribute as meta_predicate.
          @end{itemize} 

         It is a good practique to put all your declarations at the
         very begining of the file, just before the code itself.
        ").

%%------------------------------------------------------------------------
%%
%% DOC ON SOME TYPES
%%
%%------------------------------------------------------------------------


%%------------------------------------------------------------------------
%%
%% CLASS DECLARATION DOC
%%
%%------------------------------------------------------------------------

%:- comment(class/1,
%	"If you are so old fashioned, you may use a normal module declaration
%         as follows:
%    
%         @tt{:- module(ClassName,[],[class]).}
%
%         But class/1 declaration is a better fashioned way in order to
%         declare a class :
%
%         :- class(ClassName).
%        ").

%:- decl class(ClassName) :
%	(class_name(ClassName)) #
%	"Declares current source to be a class.".

%class(_).

%%------------------------------------------------------------------------
%%
%% PUBLIC METHOD DECLARATION
%%
%%------------------------------------------------------------------------

:- comment(export/1,
	"Declares a method or attribute to be part of the 
          @index{public interface}. 

         The public interface is the set of predicates wich will be accesible
         from any code establishing an usage relationship with this class
         (see @decl{use_class/1} for further information).

         Publishing an attribute or method is very similar to 
         @em{exporting} a predicate in a Prolog module.

         Whether an inherited and exported predicate is @concept{overriden}, 
	 it must be explicitly exported again.

         An inherited (but not exported) predicate may become exported,
         without overriding it by the usage of this declaration.
        ").

:- decl export(Spec) : (method_spec(Spec)) #
	"@var{Spec} will be part of the public (exported) interface.".

%:- comment(doinclude,export/1).

%export(_).

%%------------------------------------------------------------------------

:- comment(public/1,
	"Just an alias for @decl{export/1}.").

:- decl public(Spec) : (method_spec(Spec)) #
	"This declaration may be used instead of @decl{export/1}.".

%:- comment(doinclude,public/1).

%public(_).

%%------------------------------------------------------------------------
%%
%% INHERITABLE METHOD DECLARATION
%%
%%------------------------------------------------------------------------

:- comment(inheritable/1,
	"Declares a method or attribute to be inherited by descendant classes.
         Notice that all @bf{public predicates are inheritable by default}.
         There is no need to mark them as inheritable.

         Traditionaly, object oriented languages makes use of the
         @index{protected} concept. Inheritable/1 may be used
         as the same concept.

         The set of inheritable predicates is called the 
         @index{inheritable interface}.
        ").

:- decl inheritable(MethodSpec) : (method_spec(MethodSpec)) #
	"@var{MethodSpec} is accessible to descendant classes.".

%:- comment(doinclude,inheritable/1).

%inheritable(_).

%%------------------------------------------------------------------------
%%
%% ATTRIBUTE DECLARATION
%%
%%------------------------------------------------------------------------

:- comment(data/1,
	"Declares an @index{attribute} at current class. Attributes 
         are used to build the internal state of instances. So, each
         instance will own a particular copy of those attribute definitions.
         In this way, one instance may have different state from another.

         O'Ciao attributes are restricted to hold simple facts. It is not
         possible to hold a Head :- Body clause at an instance attribute.

         Notice that attributes are @index{multi-evaluated} by nature,
         and may be manipulated by the habitual @bf{assert/retract} family 
         of predicates.

         Attributes may also be initialized. In order to do so, simply
         put some clauses after the attribute definition. Each time an
         instance is created, its initial state will be built from those
         @index{initialization clauses}.

         Note: whether a data/1 declaration appears inside an interface, 
         it will be automatically exported.
        ").

:- decl data(Spec) : (method_spec(Spec)) #
	"@var{Spec} is an attribute.".

%:- comment(doinclude,data/1).

%data(_).

%%------------------------------------------------------------------------

:- comment(dynamic/1,
	"Just an alias for @decl{data/1}.
        ").

:- decl dynamic(Spec) : (method_spec(Spec)) #
	"You may use this declaration instead of @decl{data/1}.".

%:- comment(doinclude,dynamic/1).

%dynamic(_).

%%------------------------------------------------------------------------

:- comment(concurrent/1,
	"Declares a @index{concurrent attribute} at current class. 
         Concurrent attributes are just the same as normal attributes,
         those declared using @decl{data/1}, except for they may freeze
         the calling thread instead of failing when no more choice points
         are remaining on the concurrent attribute.

         In order to get more information about concurrent behavior take
         a look to the concurrent/1 built-in declaration on 
         Ciao Prolog module system.
        ").

:- decl concurrent(Spec) : (method_spec(Spec)) #
	"Declares @var{Spec} to be a concurrent attribute.".

%:- comment(doinclude,concurrent/1).

%concurrent(_).

%%------------------------------------------------------------------------
%%
%% INHERITANCE DECLARATION
%%
%%------------------------------------------------------------------------

:- comment(inherit_class/1,
	"Makes any public and/or inheritable predicate at inherited class 
         to become accesible by any instance derived from current class.

         Inherited class is also called the @index{super class}.

         Only one inherit_class/1 declaration is allowed to be present
         at current source.

         Notice that inheritance is @concept{public} by default. Any
         public and/or inheritable declaration will remain the same to
         descendant classes. However, any inherited predicate may be
         @em{overriden} (redefined).

         A predicate is said to be @index{overriden} when 
         it has been inherited from super class, 
         but there are clauses (or a @decl{data/1} declaration)
         present at current class for such a predicate.

         Whether a @bf{public} predicate is overriden,
         the local definition must also be exported, otherwise an error
         is reported.

         Whether an @bf{inheritable} predicate (not public) is overriden,
         the local definition must also be marked as inheritable or exported, 
         otherwise an error is also reported.

        Note: whether inherit_class/1 appears inside an interface, it will
        be used as an @decl{implements/1} declaration.
        ").

:- decl inherit_class(Source) : (class_source(Source)) #
	"Establish an @index{inheritance relationship} between current
         class and the class defined at @var{Source} file.".

%:- comment(doinclude,inherit_class/1).

%inherit_class(_).

%%------------------------------------------------------------------------

:- comment(implements/1,
	"Forces current source to provide an implementation for the given 
         interface file. Such interface file may declare another class 
         or a specific interface.

         Every public predicate present at given interface file will
         be automatically declared as public at current source, 
         so you @bf{must} 
         provide an implementation for such predicates.

         The effect of this declaration is called @index{interface
         inheritance},and there is no restriction on the number of
         implements/1 declarations present at current code.
        ").

:- decl implements(Interface) : 
	interface_source(Interface) #
       "Current source is supposed to provide an implementation for 
        @var{Interface}.".

%:- comment(doinclude,implements/1).

%implements(_).

%%------------------------------------------------------------------------
%%
%% VIRTUAL METHOD DECL
%%
%%------------------------------------------------------------------------

:- comment(virtual/1,
	"This declaration may be used whenever descendant classes 
         are to implement different versions of a given predicate.

         @index{virtual} predicates give a chance to handle, in an 
         uniform way, different implementations of the same functionality.

         Whether a virtual predicate is declared as a method,
	 there must be at least one clause of it present at current source. 
         Whenever no special implementation is needed at current class,
         a never-fail/allways-fail clause may be defined 
         (depending on your needs). For example:

@begin{verbatim}
   :- virtual([ test1/1 , test2/2 ]).
   test1(_).
   test2(_,_) :- fail.
@end{verbatim}

         This kind of virtual methods are also known as 
         @index{abstract methods}, since implementation is fully delegated
         to descendant classes.
         
         An attribute may be also declared as a virtual one, but there is 
         no need to write clauses for it.
         ").

:- decl virtual(VirtualMethodSpec) : 
	(virtual_method_spec(VirtualMethodSpec)) #
	"All calls to @var{VirtualMethodSpec} predicate in current source
         will use the most descendant implementation of it.".

%:- comment(doinclude,virtual/1).

%virtual(_).

%%------------------------------------------------------------------------
%%
%% INHERITED PREDICATE CALLING
%%
%%------------------------------------------------------------------------

:- comment(inherited/1,
	"This predicate qualificator may be used whenever you need 
         to reference an attribute or method on the super class. 
         
         Since methods and attributes may be @concept{redefined}, this
         qualificator is need to distinguish between a locally declared
         predicate and the inherited one, which has the same name.

         There is no need to use inherited/1 if a particular inherited 
         predicate has not been redefined at current class.
        ").

:- pred inherited(Goal) : callable #
	"References a given @var{Goal} at the super class".

inherited(_).

%%------------------------------------------------------------------------
%%
%% RETRIEVING SELF INSTANCE ID
%%
%%------------------------------------------------------------------------

:- comment(self/1,
	"Determines which instance is currently executing self/1 goal.

         Predicate will fail if argument is not a free variable.
         Otherwise, it will allways succeed, retrieving the 
         instance identifier which is executing current code.

         This functionality is very usefull since an object must 
         have knowledge of other object's identifier in order to
         send messages to it.For example:

         :- concurrent ack/0.

         send_data_to_object(Data,Obj) :-
                self(X),
                Obj:take_this(Data,X),
		current_fact(ack).

         acknowledge :-
                asserta_fact(ack).  

         take_this(Data,Sender) :-
                validate_data(Data),
		Sender:acknowledge.
        ").

:- pred self(Variable) : (var(Variable)) => instance_id(Variable) #
	"Retrieves current instance identifier in @var{Variable}".

self(_).

%%------------------------------------------------------------------------
%%
%% CONSTRUCTOR DECLARATIONS
%%
%%------------------------------------------------------------------------

:- comment(constructor/0,
	"A @index{constructor} is a special case of method which 
         complains the following conditions:

          @begin{itemize}
           @item The constructor functor matches the current class name.
           @item A constructor may hold any number of arguments.
           @item If an inheritance relationship was defined, an inherited
                 constructor must be manually called (see below).
           @item When instance creation takes place, any of the declared 
                 constructors are implicitly called. The actual constructor
                 called depends on the @pred{new/2} goal specified by the
                 user.
          @end{itemize}
 
        This is a simple example of constructor declaration for the foo class:
         
        @begin{verbatim}
           foo :- 
               display('an instance was born').
@end{verbatim}

        Constructor declaration is not mandatory, and there may be more
        than one constructor declarations (with different arity) at the 
        source code.

        This functionality is usefull when some computation is needed at
        instance creation. For example: opening a socket, clearing the screen,
        etc.

        Whenever an inheritance relationship is established, and there is
        any constructor defined at the super class, you must call manually an
        inherited constructor. Here is an example:

        @begin{verbatim}
           :- class(foo).
           :- inherit_class(myclass).

           foo :-
               myclass(0),
               display('an instance was born').

           foo(N) :- myclass(N).
@end{verbatim}

        Consequences may be unpredictable, if you forget to call an inherited
        constructor. You should also take care not to call an inherited 
        constructor twice.

        All defined constructors are inheritable by default.
        A constructor may also be declared as public (by the user), but it
        is not mandatory.
       ").

:- pred constructor #
	"Constructors are implicitly declared".

constructor.

%%------------------------------------------------------------------------
%%
%% DESTRUCTOR DECLARATIONS
%%
%%------------------------------------------------------------------------

:- comment(destructor/0,
	"A @index{destructor} is a special case of method which 
         will be automatically called when instance destruction takes place.

         A destructor will never be wanted to be part of the public
         interface, and there is no need to mark them as inheritable,
         since all inherited destructors are called by O'Ciao just before 
         yours.

         This is a simple example of destructor declaration:
         
        @begin{verbatim}
           destructor :- 
               display('goodbye, cruel world!!!').
@end{verbatim}

        Destructor declaration is not mandatory. Failure or sucess of 
        destructors will be ignored by O'Ciao, and they will be called
        only once.

        This functionality is useful when some computation is need at
        instance destruction. For example: closing an open file.
       ").

:- pred destructor #
	"Destructors are implicitly declared".

destructor.

%%------------------------------------------------------------------------
%% BUGS / TO IMPLEMENT IN THE FUTURE
%%------------------------------------------------------------------------

:- comment(bug,
	"addmodule and pred(N) meta-arguments are not allowed on 
         meta-predicates.
	").

%%------------------------------------------------------------------------
%% VERSION CONTROL
%%------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+162,2001/12/04,16:02*58+'CET'), "Fixed bug that
   prevented concurrent attributes from being recognized. 
   (Francisco Bueno Carrillo)").

:- comment(version(1*5+162,2000/06/14,17:07*15+'CEST'), "Changed 
   @code{goal_expansion(Goal,Exp,Module,InstanceID)} to
   @code{goal_expansion(Goal,InstanceID,Exp,Module)}. Same for
   @code{fact_expansion/4} and @code{spec_expansion/4}. Changes in
   @file{class_tr_aux.pl}  (Francisco Bueno Carrillo)").

:- comment(version(1*5+89,2000/03/24,14:24*48+'CET'), "Error reporting section
   has been moved to separate appendix (Angel Fernandez Pineda)").

:- comment(version(1*5+3,1999/11/29,18:06*27+'MET'), "Fixed minor bug on
   assert/retract expansion. Documentation on
   interfaces has been mixed with documentation on classes 
   (Angel Fernandez Pineda).").

:- comment(version(1*3+76,1999/10/13,18:29*01+'MEST'), "Fixed bug which
   caused virtual methods to crash when called from a destructor.  (Angel
   Fernandez Pineda)").

:- comment(version(1*3+69,1999/10/04,20:53*26+'MEST'), "Fixed big bug on
   clause body expansion.  (Angel Fernandez Pineda)").

:- comment(version(1*3+65,1999/09/30,12:41*00+'MEST'), "Attributes may be
   declared as public with no restriction.  (Angel Fernandez Pineda)").

:- comment(version(1*3+64,1999/09/29,20:00*56+'MEST'), "Now,classes will
   expand any call to objects derived from itself.  (Angel Fernandez
   Pineda)").

:- comment(version(1*3+37,1999/07/21,09:56*33+'MEST'), "Fixed little bug:
   an inherited (but not exported) predicate may become exported 
   by a descendant class without overriding it. (Angel Fernandez Pineda)").

:- comment(version(1*3+32,1999/07/14,14:56*35+'MEST'), "Attributes are now
   allowed to be public, but only for calling porpouses.  (Angel Fernandez
   Pineda)").

:- comment(version(1*3+29,1999/07/12,15:43*08+'MEST'), "Added support for
   multiple (interface) inheritance. Inheritance is now
   public by default.  (Angel Fernandez Pineda)").

:- comment(version(1*3+20,1999/07/07,09:14*48+'MEST'), "Changed error
   reporting style to fit Ciao compiler style.  (Angel Fernandez Pineda)").

:- comment(version(1*3+17,1999/07/05,18:47*59+'MEST'), "Fixed bug on
   run-time expansions (Angel Fernandez Pineda)").

:- comment(version(1*3+15,1999/07/05,14:54*37+'MEST'), "Format on class
   templates has changed (Angel Fernandez Pineda)").

:- comment(version(1*3+2,1999/06/16,18:05*42+'MEST'), "New semantic errors
   are checked (Angel Fernandez Pineda)").

:- comment(version(1*3+3,1999/06/15,16:06*09+'MEST'), "concurrent/1
   declaration now working.  (Angel Fernandez Pineda)").

:- comment(version(1*3+2,1999/06/15,15:41*33+'MEST'), "O'Ciao adapted to
   new Ciao-1.3 compiler. New features added for optimization porpouses.
   (Angel Fernandez Pineda)").

:- comment(version(0*9+55,1999/04/22,16:38*10+'MEST'), "Added documentation
   on self/1 and error reporting (Angel Fernandez Pineda)").

:- comment(version(0*9+53,1999/04/15,17:27*17+'MEST'), "Updated
   documentation due to PL2TEXI changes (Angel Fernandez Pineda)").

:- comment(version(0*9+52,1999/04/15,17:13*29+'MEST'), "Dependency between
   .itf files is now solved on second pass expansion.  (Angel Fernandez
   Pineda)").

:- comment(version(0*9+48,1999/04/13,15:36*15+'MEST'), "Re-inheritance is
   now working, but we cannot declare public attributes until predicate
   aliases are implemented.  (Angel Fernandez Pineda)").

:- comment(version(0*9+22,1999/03/25,17:27*54+'MET'), "Updated
   documentation to include destructor declaration.  (Angel Fernandez
   Pineda)").

:- comment(version(0*9+15,1999/03/22,19:45*59+'MET'), "Virtual methods now
   working. Other bugs on re-exportation already fixed.  (Angel Fernandez
   Pineda)").

:- comment(version(0*9+5,1999/03/12,12:55*56+'MET'), "Taking advantage of
   second pass expansion (Angel Fernandez Pineda)").

:- comment(version(0*8+43,1999/03/08,18:59*42+'MET'), "Implemented
   expansion for constructor declaration (Angel Fernandez Pineda)").

:- comment(version(0*8+38,1999/02/25,20:04*46+'MET'), "Documentation first
   created (Angel Fernandez Pineda)").

%%------------------------------------------------------------------------
