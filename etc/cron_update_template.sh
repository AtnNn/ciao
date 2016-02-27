#!/bin/sh
#
# MINIMAL template for daily cron update at our main clip server.
#
# ***************************************************************************
# ********** All real stuff is done in ${ciaode_update_script} **************
# ***************************************************************************
#         (so that it is not necessary to change any cron file to 
#       modify how the installation and distribution site is updated)
#
# INSTRUCTIONS: 
#
#   Copy the contents of file into your preferred /etc/cron.daily
#   location to implement daily source updates. This code is designed
#   for our particular installation, but it could be generalized for
#   any other server.
#
# NOTES:
#
#   * Keep etc/cron_update_template.sh up to date to any significant
#     changes in this code.
#
#   * Keep this code as simple as possible
#
# AUTHOR: Jose F. Morales (based on previous code by Edison Mera)

ciaode_update() {
    ciaode_update_tmpdir=/tmp/ciaode_update
    ciaode_update_script=update_at_clip.sh
    ciaode_update_log=/var/log/ciaode_update.log
    # Fetch ciaode_update_script
    mkdir -p ${ciaode_update_tmpdir}
    cd ${ciaode_update_tmpdir}
    /usr/bin/svn export --force file:///home/clip/SvnReps/Systems/CiaoDE/trunk/etc/${ciaode_update_script} ${ciaode_update_script}
    # Set owner and group
    chown clip:clip ${ciaode_update_script}
    # Execute ciaode_update_script
    sudo -H -u clip ${ciaode_update_tmpdir}/${ciaode_update_script} > ${ciaode_update_log} 2>&1 || cat ${ciaode_update_log}
}
ciaode_update
