:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc( module , "

We wanted to be able, at least temporarily, to have both the old
interface (original of Christian Holzbaur), for backwards
compatibility (we have already several clp models running above it)
and two new interfaces, accessible by Ciao packages, which implemented
per-module attributes and modules with multiple attributes.

The first and most simple of these packages is mattr_global, which
allows attributed variables with at most one simultaneous attribute
from each module.  The builtins for attribute manipulation would be:

@pred{set_attr(Var, Attr)}: Attaches (local) attribute @var{Attr} to
@var{Var} (the previous local attribute, if any, is gone).

@pred{get_attr(Var, Attr)}: @var{Attr} is the local attribute of @var{Var}
(fails if there is not).

@pred{detach_attr(Var)}:
    Erases local attribute of @var{Var}.

The behavior of attributed variables in unification is:

@begin{itemize}
@item If an attributed variable is unified with a normal variable,
  unification proceeds normally.

@item If an attributed variable is unified with a non-variable term @var{Term},
  then the variable will be detached from all its attributes and unified
  with Term, and for each module @tt{Module} which had an attribute @var{Attr} in
  the variable, a goal @pred{Module:check_attribute(Attr, Term)} will be
  invoked.  If (and when) any of the goals fail, the entire unification
  fails.  To be able to control the order in which those goals are
  invoked, the declaration @tt{:- attribute_priority(P).} occurring in a
  module assigns a certain priority of execution to the attributes set
  by the module.

@item If two attributed variables are unified, then both will be detached
  from attributes, unified, and then for each pair of attributes
  belonging to the same module @tt{Module} (coming from different attributed
  variables) the goal @pred{Module:combine_attributes(Attr1,Attr2,Sol)} will be
  called.  Parameter @var{Sol} will have the (parcial) result of the
  unification, and can be any term.  Those parcial results will be
  conveniently combined, possibly calling goals check_attribute/2 if
  the result is a non-variable term, or in case the result is an
  attributed variable, copying all attributes which do not have a
  counterpart in the other attribute set into the result.

  Some examples of unification results follow. In the examples
  @tt{@{m1:a1,m2:a2@}} represents an attributed variable with attribute a1 from
  module m1 and attribute a2 from module m2):

  @tt{@{m1:a1@} = @{m2:a2@}} => @tt{@{m1:a1,m2:a2@}}

  @tt{@{m1:a1,m2:a3@}} = @tt{@{m2:a2@}}

  where @tt{m2:combine_attributes(a3,a2,Sol)} yields @tt{Sol = @{m2:a4@}}

  => @tt{@{m1:a1,m2:a4@}}

  @tt{@{m1:a1,m2:a3@} = @{m2:a2@}}

  where @pred{m2:combine_attributes(a3,a2,Sol)} yields @tt{Sol = k}
    and @tt{m1:check_attribute(a1,k)} succeeds => @tt{k}
@end{itemize}

There is then a package mattr_local, which is built above the previous,
which allows to have several attributes in a single module.  In this
package, the declaration

@tt{:- attribute at1, at2/2.}

defines the local attributes of the module.  The attributes can have
predefined arity @tt{at2/2} or changing arity (at1).

The builtins for attribute manipulation would be adapted in this way:

@pred{set_attr(Var,Attr)}:
    Attaches (local) attribute @var{Attr} to @var{Var}. Replaces other possible
    attribute with the same name in @var{Var}.

@pred{get_attr(Var,Attr)}:
    Variable @var{Var} has a local attribute @var{Attr}.  Gives all solutions on
    backtracking. @var{Attr} can be instantiated, in this case only the value for
    that particular attribute is returned.

@pred{detach_attr(Var,Attr)}:
    Erases attribute @var{Attr} of @var{Var} (if set).

The unification algorith is the same as before, but
check_local_attribute and combine_local_attributes are used, which
receive, instead of a single attribute, a term of the form
@pred{module(At1,At2,...)}.  This term, in any case, does not need to be
explicitly used, because a function (@@)/2 is defined by the package
so that @tt{at_i@@module(at_1, ..., at_i(...), ...)} returns
@tt{at_i(...)}, and then the hook predicates can be coded as

@begin{verbatim}
combine_attr( X , Y , V ) :-
	at3@@X == at3@@Y,
        at2@@X = at2(N),
        ...
@end{verbatim}

Note that all this is still preliminary.
").
