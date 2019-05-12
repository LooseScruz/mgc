#!/bin/bash

for test in examples/*.mgc;
do
	echo $test;
	./mgc.native $test > ll;
	echo "";
	# ./llvm_do.sh;
done;