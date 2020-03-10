#!/bin/bash

wrapper_name=kaf31_math_wrapper.wl
wrapper_path=/usr/bin/$wrapper_name

if test -f $wrapper_path
then
	echo "wolfram mathematica wrapper exists: $wrapper_path"
	echo "remove current version..."
	rm $wrapper_path
	echo "continue..."
fi


echo "create wolfram mathematica wrapper: $wrapper_name"
touch $wrapper_name
chmod +x $wrapper_name

echo "get wolframscript path..."
wolfram_script_path=$(whereis wolframscript | tr -s " " "\n" | grep wolframscript$)
echo "wolframscript path: $wolfram_script_path"

echo "#!$wolfram_script_path -script" >> $wrapper_name
echo >> $wrapper_name
echo "value = ToExpression[\$ScriptCommandLine[[2]]];" >> $wrapper_name
echo "Print[value];" >> $wrapper_name
echo >> $wrapper_name

echo "move wolfam mathamatica wrapper: $(pwd)/$wrapper_name -> $wrapper_path"
echo "bye..."
mv $wrapper_name $wrapper_path



