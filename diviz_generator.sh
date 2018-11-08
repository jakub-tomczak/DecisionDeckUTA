#!/bin/bash

XMCDA_FILES_DIR=XMCDA_files

R_PACKAGE_DIR=Rpackage
DESCRIPTION_FILENAME=description-wsDD.xml
DESCRIPTION_FILE=$XMCDA_FILES_DIR/$DESCRIPTION_FILENAME
OUT_DIR=diviz
GENERATOR=../decision-deck-webservices-infrastructure/python/generate_from_description.py
TEMPLATE=rjava_ws_prg

TEST_DIR_NAME=tests
TESTS_DIR=$OUT_DIR/$TEST_DIR_NAME

R_MERGER=merger.py
MERGE_SRC_PATH=$R_PACKAGE_DIR/R
MERGE_OUT_PATH=$OUT_DIR/src
MERGED_FILENAME=calculationsUTA.R

if [ ! -e $DESCRIPTION_FILE ]
then
    echo "File $DESCRIPTION_FILE doesn't exist."
    exit 1
fi

if [ -e $OUT_DIR/$DESCRIPTION_FILENAME ]; then
    diff $DESCRIPTION_FILE $OUT_DIR/$DESCRIPTION_FILENAME &> /dev/null 
    RES=$?
    if [ $RES -eq 1 ]; then
        cp $OUT_DIR/$DESCRIPTION_FILENAME /tmp/
        echo "[WARNING!]"
        echo "$DESCRIPTION_FILE and $OUT_DIR/$DESCRIPTION_FILENAME are different, backup copy of $OUT_DIR/$DESCRIPTION_FILENAME  moved to tmp" 
    fi
fi

if [ ! -d $OUT_DIR ]
then
    echo "Creating $OUT_DIR."
    mkdir OUT_DIR
else
    if [ -d $TESTS_DIR ]
    then
        echo "Moving $TESTS_DIR to tmp location, $OUT_DIR"
        mv $TESTS_DIR /tmp
    fi
    echo "Clearing $OUT_DIR."
    rm -rf $OUT_DIR/*
fi

echo "Executing: $GENERATOR -o $OUT_DIR $DESCRIPTION_FILE $TEMPLATE"
$GENERATOR -o $OUT_DIR $DESCRIPTION_FILE $TEMPLATE

if [ ! -d $TESTS_DIR ] #check wheter tests dir exists - generator returns 0 even if failed
then
    echo "Generator failed"
    exit 1
fi

echo "Merging UTA files"
./$R_MERGER $MERGE_SRC_PATH $MERGE_OUT_PATH $MERGED_FILENAME

if [ -d /tmp/$TEST_DIR_NAME ]
then
    echo "Restoring tests"
    rm -rf $TESTS_DIR/*
    mv /tmp/$TEST_DIR_NAME/* $TESTS_DIR/
    rm -rf /tmp/$TEST_DIR_NAME
fi

#R --slave --vanilla --file=diviz/src/UTACLI_XMCDAv2.R --args "diviz/tests/in1.v2" "diviz/tests/out1.v2"