#!/bin/bash

# I don't want the command line build to increment the build number automatically
# Backup the LPI file. After the build, restore those files to reset the build number.

function build_default () {

	local app="${1}"
	local cpu=$(uname -m)
	local os=$(uname -s)

	[[ "${os}" == "Darwin" ]] && os=darwin
	[[ "${os}" == "Linux" ]] && os=linux
	local util=${app}_${cpu}_${os}
	[[ -f "${util}" ]] && rm "${util}"
	cp ${app}.ver ${app}.lpi
	[[ -f ${app} ]] && rm ${app}
	lazbuild -B -r ${app}.lpr
	[[ -e ${app} ]] || exit 1
	mv ${app} ${util}

}

function build_app () {

	local app="${1}"

	cp ${app}.lpi ${app}.ver
	build_default ${app}
	cp ${app}.ver ${app}.lpi
	rm ${app}.ver

}

function lines () {
	m=$(wc -l *.pas *.lpr | tail -n 1 | cut -d 't' -f 1)

	echo "Total lines:      "${m}
	echo
}

build_app csvutil
lines

