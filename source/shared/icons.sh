#!/bin/sh

function notice () {
	echo '-------------------------------------------------------------------------------'
    echo "The icons that are not redistributable were provided by and Copyright Icons8 "
    echo "http://icons8.com"
    echo
    echo "A collection of the needed icons can be freely downloaded from their website"
    echo 'at https://icons8.com/icons/share-collections/uRjmCz4CbAnb at 100px resolution'
    if [[ ${#*} -eq 0 ]] ; then
      	echo
      	echo "Once downloaded, extract the files into a subdirectory called icons8 and run"
      	echo "this script again to create the resource file need to compile the program."
    fi;
	echo '-------------------------------------------------------------------------------'
}

if [[ -d 'icons8' ]] ; then
	owd="${PWD}"
    pushd icons8 >/dev/null
    [[ -d "APP-FDNLS" ]] && cd APP-FDNLS
    [[ -d "app-fdnls" ]] && cd app-fdnls
    [[ ! -f notice.txt ]] && notice message >notice.txt
    echo "notice.txt=icons-notice">filelist.txt
    if ! -f 'unknown-flag.png' ]] ; then
	    [[ -f '../unknown-flag.png' ]] && cp -v '../unknown-flag.png' .
	    [[ -f '../../unknown-flag.png' ]] && cp -v '../../unknown-flag.png' .
    fi
    echo "const" >filelist.inc
    echo "  IconFiles : array of string = (" >>filelist.inc
    prev='unknown-flag'
    for i in *.png ; do
    	[[ "${prev}" != '' ]] && echo "    '${prev}',">>filelist.inc
    	x="${i%.*}"
    	x="${x%-100*}"
    	x="${x#*icons8-}"
    	prev="${x}"
    	echo "${i}=icons100-${x}">>filelist.txt
    done
    echo "    '${prev}'">>filelist.inc
    echo "  );" >>filelist.inc
    lazres "${owd}/icons.lrs" @filelist.txt
    popd >/dev/null
else
	echo '------------------------------------------------------------------------------'
    echo "The icons used are not redistributable and Copyright Icons8, http://icons8.com"
    echo
    echo "A collection of the needed icons can be freely downloaded from their website"
    echo 'at https://icons8.com/icons/share-collections/uRjmCz4CbAnb at 100px resolution'
    echo
    echo "Once downloaded, extract the files into a subdirectory called icons8 and run"
    echo "this script again to create the resource file need to compile the program."
	echo '------------------------------------------------------------------------------'
    exit 1
fi;

