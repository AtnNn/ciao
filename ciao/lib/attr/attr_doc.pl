:- use_package(assertions).
:- doc(nodoc,assertions). 

:- doc( title, "Attributes Variables Package").
:- doc( author, "R@'{e}my Haemmerl@'{e}").
:- doc( version(1,2011/06/02), "first implementation").

:- doc(module, "This package import automatically attributes variables
manipulation predicates, @tt{put_attr/2}, @tt{get_attr/2}, and
@tt{del_attr/2} (See module @tt{library(attr(attr_rt))}), and set up
the following hooks:

@begin{itemize} 

@item @tt{attr_unify_hook(+AttValue, +VarValue)}.

Hook that must be defined in the module using package @tt{attr}. Is is
called after the attributed variable of that module has been unified
with a non-var term, possibly another attributed
variable. @var{AttValue} is the attribute that was associated to the
variable in this module and @var{VarValue} is the new value of the
variable. Normally this predicate fails to veto binding the variable
to @var{VarValue}, forcing backtracking to undo the binding. If
@var{VarValue} is another attributed variable the hook often combines
the two attribute and associates the combined attribute with
@var{VarValue} using @tt{attr_rt:put_attr/2}.

@item @tt{attribute_goal(+Var, Goal)}.

This optional hook, if it is
defined in a module using puckage @tt{attr}, is used by
@tt{attr_rt:copy_term/3} to project attributes of that  module to
residual goals." ||
%It is also used by the toplevel to obtain residual
% goals after executing a query.
"
@end{itemize}

In the following example we give an an implementation of
@tt{freeze/2}. We name it @tt{myfreeze/2} in order to avoid a name
clash with the built-in predicate of the same name. The code is
avaiblable in the module @tt{library(attr(example(myfreeze)))}

@begin{verbatim}
@includeverbatim{attr/example/myfreeze.pl}.
@end{vertim}

").