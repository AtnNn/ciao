#!/bin/sh

# A simple script to test the documentation output for several
# backends and manuals.

# Note:
#
# The testing code is far from perfect. A much cleaner and clever test
# code could be implemented.
#
# Its purpose is just to serve as an oracle to point out differences
# in lpdoc output. Comparison is textual and may give many false
# positives on detecting problems; so human help is required.
#
# The tests are neither optimal: some of them are huge, full manuals.
#
# Author: Jose F. Morales

# Obtain the directory where this script is located
old_dir=`pwd`; cd `dirname $0`; self=`pwd`; cd ${old_dir}; old_dir=

# ---------------------------------------------------------------------------

# Where CiaoDE is located
ciaode="${self}/../.."
CIAOBUILDDIR="${ciaode}/build"

# ---------------------------------------------------------------------------
# Directory where the saved results go.

# TODO: Share with 'optim_comp/ciaotool_modules/config.sh'

regression_name="CiaoRegressionData"
regression_dir="${ciaode}/${regression_name}"
regression_repo="ssh://clip.dia.fi.upm.es/home/clip/Systems/${regression_name}"

lpdoc_regression_dir="${regression_dir}/lpdoc"

ensure_regression_dir() {
    if [ -r "${regression_dir}/.git" ]; then
	true
    else
	cat <<EOF
ERROR: Cannot locate regression data directory

  The directory:
    \`${regression_dir}' 
  is not found or does not seem to be a clone of the regression
  repository.

  Please, clone it by typing:

  $ cd ${ciaode}
  $ git clone co ${regression_repo}

EOF
	exit -1
    fi
}
# ---------------------------------------------------------------------------

# This is a hack... it has the version number hardwired.
function get_bundle_file() {
    case $1 in
	ciao) echo "ciao-1.15.0" ;;
	ciaopp_ref_man) echo "ciaopp-1.2.0" ;;
	ciaopp_doc) echo "ciaopp_internals-1.2.0" ;;
	lpdoc) echo "lpdoc-3.0.0" ;;
	# for testing
	ciaotest) echo "ciao" ;;
	singlelpdoc) echo "singlelpdoc" ;;
	*) echo ""
    esac
}

function get_bundle_dir() {
    case $1 in
	ciao) echo "${ciaode}/ciao" ;;
	ciaopp_ref_man) echo "${ciaode}/ciaopp" ;;
	ciaopp_doc) echo "${ciaode}/ciaopp" ;;
	lpdoc) echo "${ciaode}/lpdoc" ;;
	# for testing
	ciaotest) echo "${ciaode}/lpdoc/tests/ciaotest" ;;
	singlelpdoc) echo "${ciaode}/lpdoc/tests/singlelpdoc" ;;
	*) echo ""
    esac
}

function get_bundle_outdir() {
    case $1 in
	ciao) echo "${CIAOBUILDDIR}/doc" ;;
	ciaopp_ref_man) echo "${CIAOBUILDDIR}/doc" ;;
	ciaopp_doc) echo "${CIAOBUILDDIR}/doc" ;;
	lpdoc) echo "${CIAOBUILDDIR}/doc" ;;
	# for testing
	ciaotest) echo "${ciaode}/lpdoc/tests/ciaotest" ;;
	singlelpdoc) echo "${ciaode}/lpdoc/tests/singlelpdoc" ;;
	*) echo ""
    esac
}

function get_bundle_cmd() {
    case $1 in
	ciao) echo "lpmake docs" ;;
	ciaopp_ref_man) echo "lpmake docs" ;;
	ciaopp_doc) echo "lpmake docs" ;;
	lpdoc) echo "lpmake docs" ;;
	# for testing
	ciaotest) cat <<EOF
lpdoc all
EOF
	    ;;
	singlelpdoc) cat <<EOF
lpdoc singlelpdoc.texi && \
lpdoc singlelpdoc.html && \
echo > singlelpdoc.infoindex && \
echo > singlelpdoc.info && \
echo > singlelpdoc.pdf && \
echo > singlelpdoc.dvi && \
echo > singlelpdoc.manl
EOF
	    ;;
	*) echo ""
    esac
}

# ---------------------------------------------------------------------------

#SUFFIXES="pdf dvi html manl texi info infoindex"

# Update Lpdoc, rebuild the documentation for ${bundle}, and compare
function full_docs() {
    update_lpdoc
    mkdir -p "${bundle_outdir}"
    cd "${bundle_outdir}"
    # TODO: use lpdoc realclean, etc.
    rm -f *.texic_gr *.texic_rr *.html_gr *.html_rr *.manl_gr *.manl_rr *.texic_dr *.html_dr *.ascii_dr *.manl_dr *.texic *.texi *.bbl *.blg *.el *.manl *.info *.infoindex
    rm -rf *.html
    rm -rf *.tmp-html
    rm -rf *.tmp-man
    rm -rf *.tmp-texinfo
    rebuild_docs
    compare_docs
}

# Rebuild lpdoc if necessary
function update_lpdoc() {
    ${ciaode}/ciaosetup build lpdoc
}

# Rebuild the documentation for ${bundle}
function rebuild_docs() {
    cd "${bundle_dir}"
    time eval "${bundle_cmd}"
}

# Just compare the output of the documentation generation
function compare_docs() {
    cd "${bundle_outdir}"
    echo "Comparing PDF (just size)"
    ls -la "${bundle_file}.pdf" "${lpdoc_regression_dir}/${bundle_file}.pdf-saved"
    echo "Comparing DVI (just size)"
    ls -la "${bundle_file}.dvi" "${lpdoc_regression_dir}/${bundle_file}.dvi-saved"
    echo "Comparing html"
    mydiff "${bundle_file}.html" "${lpdoc_regression_dir}/${bundle_file}.html-saved"
    echo "Comparing manl"
    mydiff "${bundle_file}.manl" "${lpdoc_regression_dir}/${bundle_file}.manl-saved"
    echo "Comparing texi"
    mydiff "${bundle_file}.texi" "${lpdoc_regression_dir}/${bundle_file}.texi-saved"
    echo "Comparing info"
    mydiff "${bundle_file}.info" "${lpdoc_regression_dir}/${bundle_file}.info-saved"
    echo "Comparing infoindex"
    mydiff "${bundle_file}.infoindex" "${lpdoc_regression_dir}/${bundle_file}.infoindex-saved"
}

function mydiff() {
    if [ x"${DIFFCMD}" == x"" ]; then
	# Be silent, just say if files differ
	diff -q $1 $2
    else
	diff -q $1 $2 || "${DIFFCMD}" $1 $2
    fi
}

# Save the output of the documentation generation
function save_docs() {
    cd "${bundle_outdir}"
    echo "Saving PDF"
    cp "${bundle_file}.pdf" "${lpdoc_regression_dir}/${bundle_file}.pdf-saved"
    echo "Saving DVI"
    cp "${bundle_file}.dvi" "${lpdoc_regression_dir}/${bundle_file}.dvi-saved"
    echo "Saving html"
    cp "${bundle_file}.html" "${lpdoc_regression_dir}/${bundle_file}.html-saved"
    echo "Saving manl"
    cp "${bundle_file}.manl" "${lpdoc_regression_dir}/${bundle_file}.manl-saved"
    echo "Saving texi"
    cp "${bundle_file}.texi" "${lpdoc_regression_dir}/${bundle_file}.texi-saved"
    echo "Saving info"
    cp "${bundle_file}.info" "${lpdoc_regression_dir}/${bundle_file}.info-saved"
    echo "Saving infoindex"
    cp "${bundle_file}.infoindex" "${lpdoc_regression_dir}/${bundle_file}.infoindex-saved"
}

# -----------------------------------------------------------------
# TODO: use lpdoc for those commands

# Open the HTML documentation
function open_html() {
    open "${bundle_outdir}/${bundle_file}.html/${bundle}.html"
}

# Open the HTML documentation
function open_pdf() {
    open "${bundle_outdir}/${bundle_file}.pdf"
}

# -----------------------------------------------------------------

do_help() {
    cat <<EOF
Usage: `basename $0` ACTION BUNDLE

where ACTION is one of:

  full          Update lpdoc, rebuild documentation, and compare the output

  update-lpdoc  Update lpdoc

  rebuild       Rebuild documentation

  compare       Briefly compare the documentation output
  compare-diff  Compare the documentation output with diff (standard output)
  compare-meld  Compare the documentation output with meld (graphical interface)
  save          Save the documentation output

  open-html     Open the generated HTML documentation
  open-pdf      Open the generated PDF documentation

  help          Show this message
EOF
}

# -----------------------------------------------------------------

action=$1
shift
bundle=$1
shift

function pick_bundle() {
    bundle_file=`get_bundle_file "${bundle}"`
    if [ x"${bundle_file}" == x"" ]; then
	echo "Unrecognized bundle \`${bundle}'"
	do_help
	exit -1
    fi
    bundle_outdir=`get_bundle_outdir "${bundle}"`
    bundle_dir=`get_bundle_dir "${bundle}"`
    bundle_cmd=`get_bundle_cmd "${bundle}"`
}

ensure_regression_dir

case ${action} in
    full)         pick_bundle ; full_docs ;;
    #
    update-lpdoc) update_lpdoc ;;
    #
    rebuild)      pick_bundle ; rebuild_docs ;;
    #
    compare)      pick_bundle ; compare_docs ;;
    compare-diff) DIFFCMD=diff; pick_bundle ; compare_docs ;;
    compare-meld) DIFFCMD=meld; pick_bundle ; compare_docs ;;
    save)         pick_bundle ; save_docs ;;
    #
    open-html)    pick_bundle ; open_html ;;
    open-pdf)     pick_bundle ; open_pdf ;;
    #
    help)         do_help; exit -1 ;;
    *)            do_help; exit -1 ;;
esac

