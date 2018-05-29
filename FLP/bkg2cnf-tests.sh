#!/bin/sh

compile="ghc bkg-2-cnf.hs -o bkg-2-cnf"
mode="chmod +x bkg-2-cnf"

eval $compile
eval $mode

FILES=input/*
for f in $FILES
do
	for i in "-i" "-1" "-2"
	do
		echo "\nExecuting test | ./bkg-2-cnf "$i $f
	

		out=`./bkg-2-cnf $i $f | grep -o . | sort`
		ref=` cat output/${f}_${i}| grep -o . | sort`
		
		echo "-------------------------------------------------"	
		
		if [ "$out" = "$ref" ]; then
		   echo "                    PASSED                      |"
	    else
	       echo "                    FAILED                      |"
		fi 

		echo "-------------------------------------------------"

	done
done

