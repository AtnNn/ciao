# Some auxiliary functions to do terminal IO

# Jose F. Morales

# TODO: Merge with mkf_aux
normal_message() {
    echo "   $*" 1>&2
}

bold_message() {
    echo ":: $*" 1>&2
}

bolder_message() {
    echo "== $*" 1>&2
}

ask_yesno() {
#    local qst_what
    qst_what=$1
    
    while [ -t ]; do
	printf "%s" "${qst_what} (y/n) "
	read qst_ok
	if [ x"$qst_ok" = x"y" ]; then
	    return 0
	elif [ x"$qst_ok" = x"n" ]; then
	    return 1
	else
	    echo "Unrecognized answer. Please type 'y' for 'yes' or 'n' for 'no'."
	fi
    done
}
