#!/bin/bash

# Maven Central and Bintray are unreachable over HTTPS
if [[ "$TRAVIS_JDK_VERSION" == "openjdk6" ]]; then
  SBT_OPTS="-Dsbt.override.build.repos=true -Dsbt.repository.config=./.sbtrepos"
fi

export SBT_OPTS="$SBT_OPTS -Xms2G -Xmx3G -Xss10M -XX:NewRatio=8 -XX:-UseGCOverheadLimit"
export MODE=$1

if [[ $MODE = 'RegularTests1' ]] ; then
  echo "Doing 'sbt genRegularTests1/test'"

  sbt ++$TRAVIS_SCALA_VERSION genRegularTests1/test
  rc=$?
  echo first try, exitcode $rc      
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests1/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests2' ]] ; then
  echo "Doing 'sbt genRegularTests2/test'"

  sbt ++$TRAVIS_SCALA_VERSION genRegularTests2/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests2/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests3' ]] ; then
  echo "Doing 'sbt genRegularTests3/test'"

  sbt ++$TRAVIS_SCALA_VERSION genRegularTests3/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests3/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests4' ]] ; then
  echo "Doing 'sbt genRegularTests4/test'"

  sbt ++$TRAVIS_SCALA_VERSION genRegularTests4/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests4/testQuick
    rc=$?
    echo second try, exitcode $rc
    if [[ $rc != 0 ]] ; then
      sbt ++$TRAVIS_SCALA_VERSION genRegularTests4/testQuick
      rc=$?
      echo third try, exitcode $rc
    fi
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'RegularTests5' ]] ; then
  echo "Doing 'sbt genRegularTests5/test'"

  sbt ++$TRAVIS_SCALA_VERSION genRegularTests5/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION genRegularTests5/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'ScalacticTests' ]] ; then
  echo "Doing 'sbt scalactic-test/test'"

  sbt ++$TRAVIS_SCALA_VERSION scalactic-test/test
  rc=$?
  echo first try, exitcode $rc
  if [[ $rc != 0 ]] ; then
    sbt ++$TRAVIS_SCALA_VERSION scalactic-test/testQuick
    rc=$?
    echo second try, exitcode $rc
  fi
  echo final, exitcode $rc
  exit $rc

fi

if [[ $MODE = 'genMustMatchersTests1' ]] ; then
  echo "Doing 'sbt genMustMatchersTests1/test'"
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests1/test
fi

if [[ $MODE = 'genMustMatchersTests2' ]] ; then
  echo "Doing 'sbt genMustMatchersTests2/test'"
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests2/test
fi

if [[ $MODE = 'genMustMatchersTests3' ]] ; then
  echo "Doing 'sbt genMustMatchersTests3/test'"
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests3/test
fi

if [[ $MODE = 'genMustMatchersTests4' ]] ; then
  echo "Doing 'sbt genMustMatchersTests4/test'"
  sbt ++$TRAVIS_SCALA_VERSION genMustMatchersTests4/test
fi

if [[ $MODE = 'genGenTests' ]] ; then
  echo "Doing 'sbt genGenTests/test'"

  while :; do echo -n ...; sleep 300; done &
  sbt ++$TRAVIS_SCALA_VERSION genGenTests/test
fi

if [[ $MODE = 'genTablesTests' ]] ; then
  echo "Doing 'sbt genTablesTests/test'"

  sbt ++$TRAVIS_SCALA_VERSION genTablesTests/test
fi

if [[ $MODE = 'genInspectorsTests' ]] ; then
  echo "Doing 'sbt genInspectorsTests/test'"

  sbt ++$TRAVIS_SCALA_VERSION genInspectorsTests/test
fi

if [[ $MODE = 'genInspectorsShorthandsTests1' ]] ; then
  echo "Doing 'sbt genInspectorsShorthandsTests1/test'"
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsShorthandsTests1/test
fi

if [[ $MODE = 'genInspectorsShorthandsTests2' ]] ; then
  echo "Doing 'sbt genInspectorsShorthandsTests2/test'"
  sbt ++$TRAVIS_SCALA_VERSION genInspectorsShorthandsTests2/test
fi

if [[ $MODE = 'genTheyTests' ]] ; then
  echo "Doing 'sbt genTheyTests/test'"

  sbt ++$TRAVIS_SCALA_VERSION genTheyTests/test
fi

if [[ $MODE = 'genContainTests1' ]] ; then
  echo "Doing 'sbt genContainTests1/test'"

  sbt ++$TRAVIS_SCALA_VERSION genContainTests1/test
fi

if [[ $MODE = 'genContainTests2' ]] ; then
  echo "Doing 'sbt genContainTests2/test'"

  sbt ++$TRAVIS_SCALA_VERSION genContainTests2/test
fi

if [[ $MODE = 'genSortedTests' ]] ; then
  echo "Doing 'sbt genSortedTests/test'"

  sbt ++$TRAVIS_SCALA_VERSION genSortedTests/test
fi

if [[ $MODE = 'genLoneElementTests' ]] ; then
  echo "Doing 'sbt genLoneElementTests/test'"

  sbt ++$TRAVIS_SCALA_VERSION genLoneElementTests/test
fi

if [[ $MODE = 'genEmptyTests' ]] ; then
  echo "Doing 'sbt genEmptyTests/test'"

  sbt ++$TRAVIS_SCALA_VERSION genEmptyTests/test
fi

#if [[ $MODE = 'genSafeStyleTests' ]] ; then
#  echo "Doing 'sbt genSafeStyleTests/test'"
#
#  sbt ++$TRAVIS_SCALA_VERSION genSafeStyleTests/test
#fi

if [[ $MODE = 'examples' ]] ; then
  echo "Doing 'sbt examples/test'"

  project examples
  sbt ++$TRAVIS_SCALA_VERSION examples/test:compile
fi

if [[ $MODE = 'examplesJS' ]] ; then
  echo "Doing 'sbt examplesJS/test'"

  sbt ++$TRAVIS_SCALA_VERSION examplesJS/test:compile
fi

if [[ $MODE = 'Publish' ]] ; then
  sbt ++$TRAVIS_SCALA_VERSION publishSigned
  sbt ++$TRAVIS_SCALA_VERSION scalactic/publishSigned
fi
