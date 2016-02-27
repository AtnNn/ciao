# A Registry of Commands
# Author: Jose F. Morales

# DESCRIPTION
#
#   A set of operations to define a switch of commands and help
#   message. The switch code and the help message are built
#   automatically from individual definitions grouped together.
#
# OPERATIONS
#
#   register_command <command_name>
#
#     Registers the command <command_name>, associated with the
#     function 'do__<command_name>', which must be provided by the
#     user.
#
#   command_group <description>
#
#     Defines a command group. All commands registered later will be
#     part of this group.
#
#   command_group_exp <description>
#
#     Like 'command_group', but it is only described in the expert help.
#
#   command_help <<EOF
#     <help text>
#   EOF
#
#     Writes a chunk of help text associated to the current command
#     group.
#
#   command_help_exp <<EOF
#     <help text>
#   EOF
#
#     Writes a chunk of expert help text associated to the current
#     command group.
#
#   build_command_case <command_name>
#
#     This builds and loads the 'command_case' function.
#
#   command_case [arguments]
#
#     The command case function (is not available until all the
#     commands are registered and 'build_command_case' is executed).
#     The first argument will be the command, the next arguments will
#     be passed as command arguments. 
#
#     The following user functions must be defined:
#
#     - do__<command_name>: Code for the command <command_name>
#     - do__unknown: Code executed when no command is recognized
#     - do__no_command: Code executed when no command is specified
#
#   show_help
#
#     Shows in standard output the full command help.
#
#   show_help_exp
#
#     Shows in standard output the full command expert help.
#
#   load_command_groups [group list]
#   
#     Load the specified groups of commands from the directory
#     'command_group_dir'.
#
# ---------------------------------------------------------------------------

COMMAND_LIST=""
register_command() {
    COMMAND_LIST="${COMMAND_LIST} $1"
}

build_command_case() {
    build_command_case__2 > ${self}/ciaosetup_modules/auto/command_case
    . ${self}/ciaosetup_modules/auto/command_case
}

build_command_case__2() {
#    local cmd
#    local cmd2
    echo "command_case() {"
#    echo "    local act"
    echo "    act="'"$1"'
    echo "    if [ -z "'"$act"'" ]; then"
    echo "        do__no_command"
    echo "        return"
    echo "    fi"
    echo "    shift"
    echo "    case "'"$act"'" in"
    for cmd in ${COMMAND_LIST}; do
	# For compatibility, register two versions of the command name,
	# one with '_' other with '-'. Both do the same thing.
	cmd2="`echo "$cmd" | sed -e 's/_/-/g'`" # replace _ by -
	echo "        ${cmd}) do__${cmd} "'$@'" ;;"
	if [ "${cmd}" != "${cmd2}" ]; then
	    echo "        ${cmd2}) do__${cmd} "'$@'" ;;"
	fi
    done
    echo "        "'*'") do__unknown "'$act'" ;;"
    echo "    esac"
    echo "}"
}

mkdir -p ${self}/ciaosetup_modules/auto
command_help_file=${self}/ciaosetup_modules/auto/command_help
command_help_exp_file=${self}/ciaosetup_modules/auto/command_help_exp
printf "" > ${command_help_file}
printf "" > ${command_help_exp_file}

# Sets the current command group description (basic and expert)
command_group() {
    printf "\n%s:\n" "$*" >> ${command_help_file}
    printf "\n%s:\n" "$*" >> ${command_help_exp_file}
}

# Sets the current command group description (expert only)
command_group_exp() {
    printf "\n%s:\n" "$*" >> ${command_help_exp_file}
}

# Sets command help (basic and expert)
command_help() {
#    local help
    help=`cat`
    echo "$help" >> ${command_help_file}
    echo "$help" >> ${command_help_exp_file}
}

# Sets command help (expert only)
command_help_exp() {
    cat >> ${command_help_exp_file}
}

# Shows the basic help
show_help() {
    cat ${command_help_file}
}

# Shows the expert help
show_help_exp() {
    cat ${command_help_exp_file}
}

# Load command groups from enviroment variable 'command_group_dir'
load_command_groups() {
    for i in $*; do
	. "${command_group_dir}/group_$i.sh"
    done
}
