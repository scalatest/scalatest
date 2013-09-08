#!/bin/bash

# 2013-0906 Darlene Wallach script to replace Microsoft left (&#8220;) and 
#                           right (&#8221;) double quote marks with nicer style
#                           &ldquo; and &rdquo; in all scaladoc files for 
#                           ScalaTest

echo `pwd`

# getting all the scala files in the src directories
for file in `find ./src -type f | grep '\.scala'`
do
# find the scala files with a Microsoft style left double quote &#8220;
grep "&#8220;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style left double quote with &ldquo; 
sed -i -e "s/\&#8220;/\&ldquo;/" ${file}
fi
# while we still have the file, 
# check for Microsoft style right double quote 
grep "&#8221;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style right double quote with &rdquo; 
sed -i -e "s/\&#8221;/\&rdquo;/" ${file}
fi
done

# getting all the java files in the src directories
for file in `find ./src/ -type f | grep '\.java'`
do
# find the java files with a Microsoft style left double quote &#8220;
grep "&#8220;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style left double quote with &ldquo; 
sed -i -e "s/\&#8220;/\&ldquo;/" ${file}
fi
# while we still have the file, 
# check for Microsoft style right double quote
grep "&#8221;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style right double quote with &rdquo; 
sed -i -e "s/\&#8221;/\&rdquo;/" ${file}
fi
done

# getting all the scala files in the examples directories
for file in `find ./examples/src/ -type f | grep '\.scala'`
do
# find the scala files with a Microsoft style left double quote &#8220;
grep "&#8220;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style left double quote with &ldquo; 
sed -i -e "s/\&#8220;/\&ldquo;/" ${file}
fi
# while we still have the file, 
# check for Microsoft style right double quote 
grep "&#8221;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style right double quote with &rdquo; 
sed -i -e "s/\&#8221;/\&rdquo;/" ${file}
fi
done

# getting all the java files in the examples directories
for file in `find ./examples/src/ -type f | grep '\.java'`
do
# find the java files with a Microsoft style left double quote &#8220;
grep "&#8220;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style left double quote with &ldquo; 
sed -i -e "s/\&#8220;/\&ldquo;/" ${file}
fi
# while we still have the file, 
# check for Microsoft style right double quote
grep "&#8221;" ${file} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place 
# replace Microsoft style right double quote with &rdquo; 
sed -i -e "s/\&#8221;/\&rdquo;/" ${file}
fi
done
