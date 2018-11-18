#!/bin/bash

romPath=`echo $1 | sed s/\\ /\ /g`
txtPath=`echo $1 | sed s/"ch8"/"txt"/g`
echo "Starting $romPath"
echo $txtPath
echo ""
echo "========================================="
echo ""
cat "$txtPath"
echo ""
echo "========================================="
sbt "run $romPath"