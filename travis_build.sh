#!/bin/bash

export JVM_OPTS="-server -Xms2048M -Xmx2048M -Xss8M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=1024M -XX:-UseGCOverheadLimit"
#this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
while true; do echo "..."; sleep 60; done &
sbt ++$TRAVIS_SCALA_VERSION compile
kill %1

sbt ++$TRAVIS_SCALA_VERSION testQuick
rc=$?
echo first try, exitcode $rc      
if [[ $rc != 0 ]] ; then
  sbt ++$TRAVIS_SCALA_VERSION testQuick
  rc=$?
  echo second try, exitcode $rc
fi
echo final, exitcode $rc
exit $rc