proc foo {} {

    global prolog_variables
    
    if {[prolog p(yes)]} {set pv 1}  else {set pv 0}
    puts "Value of p: $pv"

    prolog r(X,Y)
    set qv $prolog_variables(X)
    set rv $prolog_variables(Y)
    puts "Value of q: $qv"
    puts "Value of r: $rv"
}

