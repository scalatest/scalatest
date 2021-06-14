ScalaTest
=========

[![Build Status](https://travis-ci.org/scalatest/scalatest.png?branch=3.1.x)](https://travis-ci.org/scalatest/scalatest)

ScalaTest is a free, open-source testing toolkit for Scala and
Java programmers.

Official Website: http://www.scalatest.org/

Using ScalaTest
---------------

### Setup

Please visit [Download and Setup](https://www.scala-sbt.org/1.x/docs/Setup.html) for download and setup instructions.

### Quick Start

Please visit [Quick Start](http://www.scalatest.org/quick_start) for steps to get started quickly.


Building ScalaTest
------------------

### Pre-Requisites

The followings are needed for building ScalaTest:

*   JDK 8
*   [SBT 1.3.13](https://www.scala-sbt.org/1.x/docs/Setup.html)

use the following SBT options instead:

    SBT_OPTS="-Xms512M -Xmx3000M -Xss1M -XX:NewRatio=8"

### Building and Running Tests

This command will build and run the regular tests:

  `$ sbt test`

To run generated all tests, you'll need to increase maximum heap size to at least -Xmx5000M, and use this command instead:

    $ rm -rf gentests
    $ sbt gentests/test
  
You can also run different groups generated tests separately: 
    
    $ rm -rf gentests
    $ sbt genMustMatchersTests1/test
    $ sbt genMustMatchersTests2/test
    $ sbt genMustMatchersTests3/test
    $ sbt genMustMatchersTests4/test
    $ sbt genGenTests/test
    $ sbt genTablesTests/test
    $ sbt genInspectorsTests/test
    $ sbt genInspectorsShorthandsTests1/test
    $ sbt genInspectorsShorthandsTests2/test
    $ sbt genTheyTests/test
    $ sbt genContainTests1/test
    $ sbt genContainTests2/test
    $ sbt genSortedTests/test
    $ sbt genLoneElementTests/test
    $ sbt genEmptyTests/test

What it does is simply switch to gentests project and run test.

To run scala-js tests: 

```
$ sbt scalatestAppJS/clean
$ sbt scalacticTestJS/test:compile
$ sbt scalacticTestJS/test
$ sbt scalatestTestJS/test:compile
$ sbt scalatestTestJS/test
```

To run scala-native tests: 

```
$ sbt -Dscalatest.skip.jdk.check=true ++2.11.12 scalatestAppNative/clean
$ sbt -Dscalatest.skip.jdk.check=true ++2.11.12 scalacticTestNative/test:compile
$ sbt -Dscalatest.skip.jdk.check=true ++2.11.12 scalacticTestNative/test
$ sbt -Dscalatest.skip.jdk.check=true ++2.11.12 scalatestTestNative/test:compile
$ sbt -Dscalatest.skip.jdk.check=true ++2.11.12 scalatestTestNative/test
```

### Building Examples

You can build examples project using this command: 

  `$ sbt examples/compile`

### Packaging

You can package the ScalaTest JAR file using this command:

  `$ sbt package`

The resulting JAR file will be produced in target/scala-2.11/.

You can also publish it to your local Ivy repository using this command:

  `$ sbt publishLocal`

Or publish it to local maven repository using this command:

  `$ sbt publishM2`

### Publishing

To publish to Sonatype, you first need to make sure you have the following:

*   A GPG client is installed on your command line path. For more information, please refer to [GNU Privacy Guard Website](http://www.gnupg.org/).
*   You have created your GPG keys and distributed your public key to hkp://pool.sks-keyservers.net/. For more information, please refer to [How To Generate PGP Signatures With Maven](https://docs.sonatype.org/display/Repository/How+To+Generate+PGP+Signatures+With+Maven).
*   You have been granted the right to publish using org.scalatest and org.scalactic domain.

By default, ScalaTest build will read your Sonatype credentials from ~/.ivy2/.credentials, which is a properties file that looks like this:

    realm=Sonatype Nexus Repository Manager
    host=oss.sonatype.org
    user=xxxxxxxx
    password=xxxxxxxx

You can use SCALATEST_NEXUS_LOGIN and SCALATEST_NEXUS_PASSWORD environment variables to override Sonatype credentials.

For signing, ScalaTest build will use ~/.gnupg/secring.gpg by default and prompt for GPG passphase if required.  Alternatively you can use SCALATEST_GPG_FILE to use a different GPG file, and use SCALATEST_GPG_PASSPHASE to provide GPG passphase to avoid input prompt.

If you would like to export a particular private key into a separate GPG file, you can use the following command:

  `$ gpg --export-secret-keys example@example.com > example-secret-key.gpg`

With Sonatype credentials and GPG file in place, you can now publish to Sonatype.

Before publishing any patch release, binary compatibility with previous version should be checked:

    $ export SCALAJS_VERSION=0.6.33
    $ sbt ++2.10.7 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestCore/package scalatestCore/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFeatureSpec/package scalatestFeatureSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFlatSpec/package scalatestFlatSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFreeSpec/package scalatestFreeSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFunSuite/package scalatestFunSuite/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFunSpec/package scalatestFunSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestPropSpec/package scalatestPropSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestRefSpec/package scalatestRefSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestWordSpec/package scalatestWordSpec/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestDiagrams/package scalatestDiagrams/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestMatchersCore/package scalatestMatchersCore/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestShouldMatchers/package scalatestShouldMatchers/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestMustMatchers/package scalatestMustMatchers/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestCoreJS/package scalatestCoreJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFeatureSpecJS/package scalatestFeatureSpecJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFlatSpecJS/package scalatestFlatSpecJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFreeSpecJS/package scalatestFreeSpecJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFunSuiteJS/package scalatestFunSuiteJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestFunSpecJS/package scalatestFunSpecJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestPropSpecJS/package scalatestPropSpecJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestWordSpecJS/package scalatestWordSpecJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestDiagramsJS/package scalatestDiagramsJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestMatchersCoreJS/package scalatestMatchersCoreJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestShouldMatchersJS/package scalatestShouldMatchersJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestMustMatchersJS/package scalatestMustMatchersJS/mimaReportBinaryIssues

    $ export SCALAJS_VERSION=1.5.1
    $ sbt ++2.11.12 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestCore/package scalatestCore/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFeatureSpec/package scalatestFeatureSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFlatSpec/package scalatestFlatSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFreeSpec/package scalatestFreeSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFunSuite/package scalatestFunSuite/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFunSpec/package scalatestFunSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestPropSpec/package scalatestPropSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestRefSpec/package scalatestRefSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestWordSpec/package scalatestWordSpec/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestDiagrams/package scalatestDiagrams/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestMatchersCore/package scalatestMatchersCore/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestShouldMatchers/package scalatestShouldMatchers/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestMustMatchers/package scalatestMustMatchers/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestCoreJS/package scalatestCoreJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFeatureSpecJS/package scalatestFeatureSpecJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFlatSpecJS/package scalatestFlatSpecJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFreeSpecJS/package scalatestFreeSpecJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFunSuiteJS/package scalatestFunSuiteJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestFunSpecJS/package scalatestFunSpecJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestPropSpecJS/package scalatestPropSpecJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestWordSpecJS/package scalatestWordSpecJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestDiagramsJS/package scalatestDiagramsJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestMatchersCoreJS/package scalatestMatchersCoreJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestShouldMatchersJS/package scalatestShouldMatchersJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestMustMatchersJS/package scalatestMustMatchersJS/mimaReportBinaryIssues

    $ sbt ++2.12.13 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestCore/package scalatestCore/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFeatureSpec/package scalatestFeatureSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFlatSpec/package scalatestFlatSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFreeSpec/package scalatestFreeSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFunSuite/package scalatestFunSuite/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFunSpec/package scalatestFunSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestPropSpec/package scalatestPropSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestRefSpec/package scalatestRefSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestWordSpec/package scalatestWordSpec/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestDiagrams/package scalatestDiagrams/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestMatchersCore/package scalatestMatchersCore/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestShouldMatchers/package scalatestShouldMatchers/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestMustMatchers/package scalatestMustMatchers/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestCoreJS/package scalatestCoreJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFeatureSpecJS/package scalatestFeatureSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFlatSpecJS/package scalatestFlatSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFreeSpecJS/package scalatestFreeSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFunSuiteJS/package scalatestFunSuiteJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestFunSpecJS/package scalatestFunSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestPropSpecJS/package scalatestPropSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestWordSpecJS/package scalatestWordSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestDiagramsJS/package scalatestDiagramsJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestMatchersCoreJS/package scalatestMatchersCoreJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestShouldMatchersJS/package scalatestShouldMatchersJS/mimaReportBinaryIssues
    $ sbt ++2.12.13 scalatestMustMatchersJS/package scalatestMustMatchersJS/mimaReportBinaryIssues

    $ sbt ++2.13.5 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestCore/package scalatestCore/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFeatureSpec/package scalatestFeatureSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFlatSpec/package scalatestFlatSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFreeSpec/package scalatestFreeSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFunSuite/package scalatestFunSuite/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFunSpec/package scalatestFunSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestPropSpec/package scalatestPropSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestRefSpec/package scalatestRefSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestWordSpec/package scalatestWordSpec/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestDiagrams/package scalatestDiagrams/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestMatchersCore/package scalatestMatchersCore/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestShouldMatchers/package scalatestShouldMatchers/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestMustMatchers/package scalatestMustMatchers/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestCoreJS/package scalatestCoreJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFeatureSpecJS/package scalatestFeatureSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFlatSpecJS/package scalatestFlatSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFreeSpecJS/package scalatestFreeSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFunSuiteJS/package scalatestFunSuiteJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestFunSpecJS/package scalatestFunSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestPropSpecJS/package scalatestPropSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestWordSpecJS/package scalatestWordSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestDiagramsJS/package scalatestDiagramsJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestMatchersCoreJS/package scalatestMatchersCoreJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestShouldMatchersJS/package scalatestShouldMatchersJS/mimaReportBinaryIssues
    $ sbt ++2.13.5 scalatestMustMatchersJS/package scalatestMustMatchersJS/mimaReportBinaryIssues

To publish scalatest modules for jvm, js, native and dotty, use the following commands: 

    $ ./publish.sh
