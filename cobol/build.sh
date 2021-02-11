#!/bin/zsh

COBC=/usr/local/Cellar/gnu-cobol/3.1.2/bin/cobc
COBOL_STANDARD=cobol2014

# save start location
LOCATION_START=`pwd`
LOCATION_SCRIPT="${0:A:h}"

# start working
cd ${LOCATION_SCRIPT}

LOCATION_TARGET="./bin"
LOCATION_SOURCE="./src"
LOCATION_CPY="./cpy"
LOCATION_TEST="./test"

export COBCPY=${LOCATION_SCRIPT}/${LOCATION_CPY}


rm -Rf ./bin && mkdir ./bin
#${COBC} ${LOCATION_SOURCE}/W2N.cob -o ${LOCATION_TARGET}/W2N
#${COBC} -x ${LOCATION_TEST}/English.cbl ${LOCATION_SOURCE}/W2N.cob  -o ${LOCATION_TARGET}/test_en.exe

cd bin
${COBC} -std=COBOL85 -v -m ../${LOCATION_SOURCE}/W2N.cob
${COBC} -std=${COBOL_STANDARD} ../${LOCATION_SOURCE}/W2N.cob
cd ..
${COBC} -x ${LOCATION_TEST}/English.cbl -o ${LOCATION_TARGET}/test_en.exe

# copy datafiles
mkdir ${LOCATION_TARGET}/data
iconv -f UTF-8 -t UTF-16BE ${LOCATION_SOURCE}/data/config_de.properties >${LOCATION_TARGET}/data/config_de.properties
iconv -f UTF-8 -t UTF-16BE ${LOCATION_SOURCE}/data/config_en.properties >${LOCATION_TARGET}/data/config_en.properties
iconv -f UTF-8 -t UTF-16BE ${LOCATION_SOURCE}/data/config_fr.properties >${LOCATION_TARGET}/data/config_fr.properties
iconv -f UTF-8 -t UTF-16BE ${LOCATION_SOURCE}/data/config_hi.properties >${LOCATION_TARGET}/data/config_hi.properties
iconv -f UTF-8 -t UTF-16BE ${LOCATION_SOURCE}/data/config_pt.properties >${LOCATION_TARGET}/data/config_pt.properties
iconv -f UTF-8 -t UTF-16 ${LOCATION_SOURCE}/data/config_ru.properties >${LOCATION_TARGET}/data/config_ru.properties

# go back to start location
cd ${LOCATION_START}


#EOF
