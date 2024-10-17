#!/bin/bash

# 2013-0827 Darlene Wallach script to update the Artima, Inc. Copyright date 
#                           in all scala and java files in all the src and 
#                           examples directories

echo `pwd`

# set the variable for the Artima, Inc. Copyright year for the current year
theYear=`date +%Y`
# set the replace string for sed
curCopyright="Copyright 2001-${theYear}"

# getting all the scala files in the jvm directories
for file in `find ./jvm -type f | grep '\.scala'`
do
# find the scala files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done

# getting all the scala files in the js directories
for file in `find ./js -type f | grep '\.scala'`
do
# find the scala files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done

# getting all the scala files in the native directories
for file in `find ./native -type f | grep '\.scala'`
do
# find the scala files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done

# getting all the scala files in the dotty directories
for file in `find ./dotty -type f | grep '\.scala'`
do
# find the scala files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done

# getting all the java files in the jvm directories
for file in `find ./jvm/ -type f | grep '\.java'`
do
# find the java files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done

# getting all the scala files in the examples directories
for file in `find ./examples/src/ -type f | grep '\.scala'`
do
# find the scala files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done

# getting all the java files in the examples directories
for file in `find ./examples/src/ -type f | grep '\.java'`
do
# find the java files with an outdated Artima, Inc. Copyright
grep Copyright ${file} | grep "Artima, Inc." | grep -v ${theYear} > /dev/null
if [ $? == 0 ]
then
# use sed to edit the file in place - current year for Artima, Inc. Copyright 
sed -i -e "s/Copyright 2001-20../$curCopyright/" ${file}
fi
done
