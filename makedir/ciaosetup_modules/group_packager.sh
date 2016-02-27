# Group of commands for the generation of installation packages for
# supported platforms.

# todo: Does documentation explains this?
#   "Steps to make and publish a distribution:
#    ./ciaosetup configure
#    ./ciaosetup build
#    ./ciaosetup docs
#    cd dist
#    lpmake install"

command_group_exp "Installation Package Generation"
command_help_exp <<EOF
  installer_win32    [not documented]
  installer_rpm      [not documented]
  installer_rpm_spec [not documented]
  installer_app      [not documented]
  installer_macport  [not documented]
  installer_pkg      [not documented]
  installer_tgz      [not documented]
  installer_tbz      [not documented]
  installer_bin      [not documented]
  installer_bin_tgz  [not documented]
  installer_bin_tbz  [not documented]
  installer_noa      [not documented]
  installer_noa_tgz  [not documented]
  installer_noa_tbz  [not documented]
  installer_raw      [not documented]
  installer_raw_tgz  [not documented]
  installer_raw_tbz  [not documented]
EOF

register_command "installer_win32"
do__installer_win32() {
    lpmake installer_win32
}

register_command "installer_rpm"
do__installer_rpm() {
    get_install_options "$@"
    lpmake installer_rpm
}

register_command "installer_rpm_spec"
do__installer_rpm_spec() {
    lpmake CiaoDE.spec
}

register_command "installer_app"
do__installer_app() {
    lpmake installer_app
}

register_command "installer_macport"
do__installer_macport() {
    lpmake Portfile
}

register_command "installer_pkg"
do__installer_pkg() {
    lpmake installer_pkg
}

register_command "installer_src"
do__installer_src() {
    lpmake installer_src
}

register_command "installer_tgz"
do__installer_tgz() {
    lpmake installer_tgz
}

register_command "installer_tbz"
do__installer_tbz() {
    lpmake installer_tbz
}

register_command "installer_bin"
do__installer_bin() {
    lpmake installer_bin
}

register_command "installer_bin_tgz"
do__installer_bin_tgz() {
    lpmake installer_bin_tgz
}

register_command "installer_bin_tbz"
do__installer_bin_tbz() {
    lpmake installer_bin_tbz
}

register_command "installer_noa"
do__installer_noa() {
    lpmake installer_noa
}

register_command "installer_noa_tgz"
do__installer_noa_tgz() {
    lpmake installer_noa_tgz
}

register_command "installer_noa_tbz"
do__installer_noa_tbz() {
    lpmake installer_noa_tbz
}

register_command "installer_raw"
do__installer_raw() {
    lpmake installer_raw
}

register_command "installer_raw_tgz"
do__installer_raw_tgz() {
    lpmake installer_raw_tgz
}

register_command "installer_raw_tbz"
do__installer_raw_tbz() {
    lpmake installer_raw_tbz
}

