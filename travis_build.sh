#!/bin/bash

export JVM_OPTS="-server -Xms2G -Xmx2G -Xss8M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=1024M -XX:-UseGCOverheadLimit"
export MODE=$1

if [[ $MODE = 'Compile' ]] ; then
  #this echo is required to keep travis alive, because some compilation parts are silent for more than 10 minutes
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile test:compile #gentests has .dependsOn(scalatest  % "test->test"), so it is common
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'Main' ]] ; then
  echo "Doing 'sbt test'"

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

fi

if [[ $MODE = 'Gentests' ]] ; then
  echo "Doing 'sbt gentests/test'"
  export JVM_OPTS="-server -Xms5G -Xmx6G -Xss8M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=1024M -XX:-UseGCOverheadLimit"
  
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION gentests/test:compile #try to reduce presure on sbt, for OOM
  sbt ++$TRAVIS_SCALA_VERSION gentests/test
  rc=$?
  kill %1  
  exit $rc
fi

if [[ $MODE = 'Gentests' ]] ; then
  sbt ++$TRAVIS_SCALA_VERSION publishSigned
  sbt ++$TRAVIS_SCALA_VERSION scalautils/publishSigned
fi