:- use_package(assertions).
:- doc(nodoc, assertions).

:- doc(title,    "The Ciao Costmodel").
:- doc(subtitle, "REFERENCE MANUAL").
:- doc(subtitle, "@em{Generated/Printed on:} @today{}").

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").
%% :- doc(copyright,"Copyright @copyright{} Manuel Hermenegildo
%% 
%% @include{Copyright.Manuals}
%% 
%% ").

:- doc(usage, "The ciao costmodel is used by including
   @tt{costmodel} in the package inclusion list of a module, or by
   means of an explicit @tt{:- use_package(costmodel).} directive.").

:- doc(summary, "The costmodel module provides predicates to 
   estimate the parameters of the average cost models once that already 
   have the model (obtained using analysis techniques or manually). 
   The estimation is limited to the linear generalized models although in 
   real situations model could be non-linear.").

:- doc(module, "The ciao profiler provides a high-level way to obtain 
the parameters of an average cost model.

The system consists of two main parts. One is the statistical library for 
computing the linear model (in @tt{'contrib/math'}) and the other is the 
package that allows to write cost assertions in the program. 
Once the model of the predicate is available we have to annotate the 
program with a cost assertion as follows:

@begin{verbatim}
	:- costmodel Pred :: Distribution => Model .
@end{verbatim}   
where @tt{Pred} is the name of the predicate and the type of the arguments, 
@tt{Distribution} is the list with the test distribution used to generate 
test data and @tt{Model} is the cost model that is going to be used.

@tt{Distribution} can be uniform (@tt{uniform(A.B)} in which the values are 
generated following a uniform distribution in the interval @tt{[A.B]}) 
or gausian (@tt{gausian(Mean. StDev)} in which the parameter values are 
generated following a gausian distribution with mean @tt{Mean} and standard 
deviation @tt{StDev}.

@tt{Model} has the following structure:

@begin{verbatim}
	:- model(Number, ArgList, ParamList, GoalCost, SpecificModel).
@end{verbatim}
where @tt{Number} is the amount of test data that is going to be generated, 
@tt{ArgList} is a list of variables that represent the arguments of the 
predicate @tt{Pred}, @tt{ParamList} is the list with the cost model parameters,
@tt{GoalCost} is a Prolog expression that allows to compute the values of the 
variables in @tt{ParamList} when the ones in @tt{ArgList} are instanciated and 
@tt{SpecificModel} indicates the model that is going to be used. For linear 
generalized model its expression is @tt{linearmodel(Baselist)} where 
@tt{Baselist} is a list of arithmetic expressions that represent the base 
functions.

Then performing a query like the followin one (in which @tt{File} is the module
of the file that contains both the program and the assertion):

@begin{verbatim}
	:- doscotmodel(File, Pred, Distribution, Model, Data, Coefs).
@end{verbatim}   
we obtain the test data (@tt{Data}) and the parameters of the cost model 
(@tt{Coefs}).
The costmodel can also be used to perform linear regression on
execution time (data) directly measured over a platform. 
For this purpose the query must be:

@begin{verbatim}
        :- genregression(Data, SpecificModel, ParamList, Coefs).
@end{verbatim}
").