#!/bin/bash

export JVM_OPTS="-server -Xms2G -Xmx4G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"
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

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
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

if [[ $MODE = 'genMustMatchersTests' ]] ; then
  echo "Doing 'sbt genMustMatchersTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx4G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"
  
  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests/test
  rc=$?
  kill %1  
  exit $rc
fi

if [[ $MODE = 'genGenTests' ]] ; then
  echo "Doing 'sbt genGenTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genGenTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genTablesTests' ]] ; then
  echo "Doing 'sbt genTablesTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genTablesTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genInspectorsTests' ]] ; then
  echo "Doing 'sbt genInspectorsTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genInspectorsShorthandsTests' ]] ; then
  echo "Doing 'sbt genInspectorsShorthandsTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsShorthandsTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genTheyTests' ]] ; then
  echo "Doing 'sbt genTheyTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genTheyTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genContainTests1' ]] ; then
  echo "Doing 'sbt genContainTests1/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genContainTests1/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genContainTests2' ]] ; then
  echo "Doing 'sbt genContainTests2/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genContainTests2/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genSortedTests' ]] ; then
  echo "Doing 'sbt genSortedTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genSortedTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genLoneElementTests' ]] ; then
  echo "Doing 'sbt genLoneElementTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genLoneElementTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'genEmptyTests' ]] ; then
  echo "Doing 'sbt genEmptyTests/test'"
  export JVM_OPTS="-server -Xms1G -Xmx3G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M -XX:-UseGCOverheadLimit"

  while true; do echo "..."; sleep 60; done &
  sbt ++$TRAVIS_SCALA_VERSION compile
  sbt ++$TRAVIS_SCALA_VERSION genEmptyTests/test
  rc=$?
  kill %1
  exit $rc
fi

if [[ $MODE = 'Publish' ]] ; then
  sbt ++$TRAVIS_SCALA_VERSION publishSigned
  sbt ++$TRAVIS_SCALA_VERSION scalactic/publishSigned
fi