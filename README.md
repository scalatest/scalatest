ScalaTest
=========

![Build Status](https://github.com/scalatest/scalatest/actions/workflows/ci.yml/badge.svg)

ScalaTest is a free, open-source testing toolkit for Scala and
Java programmers.

Official Website: http://www.scalatest.org/

💖 Support ScalaTest
--------------------

![Sponsor ScalaTest](https://img.shields.io/badge/sponsor-scalatest-ff69b4?logo=github-sponsors)](https://github.com/sponsors/scalatest)

ScalaTest has been a cornerstone of testing in the Scala ecosystem for over 17 years. It’s trusted by countless developers and teams to write expressive, flexible, and robust tests. We’ve always believed in keeping ScalaTest free and open source, but maintaining a tool used so widely takes time, care, and ongoing effort.

If ScalaTest has saved you time, helped you ship better software, or become a key part of your development workflow, please consider supporting our work. Your sponsorship helps us dedicate time to fixing bugs, improving documentation, adding new features, and keeping ScalaTest reliable for the entire community.

👉 [Become a sponsor for ScalaTest](https://github.com/sponsors/scalatest) to help keep Scala’s most widely used testing library thriving!

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
*   [SBT 1.11.4](https://www.scala-sbt.org/1.x/docs/Setup.html)

use the following SBT options instead:

    SBT_OPTS="-Xms512M -Xmx3000M -Xss1M -XX:NewRatio=8"

### Building and Running Tests

This command will build and run the regular tests:

  `$ sbt test`

To include flicker tests, you'll need to set environment variable SCALATEST_RUN_FLICKER_TESTS=true:

```
> export SCALATEST_RUN_FLICKER_TESTS=true
> sbt test
```

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

    $ sbt ++2.12.20 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestCore/package scalatestCore/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFeatureSpec/package scalatestFeatureSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFlatSpec/package scalatestFlatSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFreeSpec/package scalatestFreeSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFunSuite/package scalatestFunSuite/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFunSpec/package scalatestFunSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestPropSpec/package scalatestPropSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestRefSpec/package scalatestRefSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestWordSpec/package scalatestWordSpec/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestDiagrams/package scalatestDiagrams/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestMatchersCore/package scalatestMatchersCore/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestShouldMatchers/package scalatestShouldMatchers/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestMustMatchers/package scalatestMustMatchers/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestCoreJS/package scalatestCoreJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFeatureSpecJS/package scalatestFeatureSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFlatSpecJS/package scalatestFlatSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFreeSpecJS/package scalatestFreeSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFunSuiteJS/package scalatestFunSuiteJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestFunSpecJS/package scalatestFunSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestPropSpecJS/package scalatestPropSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestWordSpecJS/package scalatestWordSpecJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestDiagramsJS/package scalatestDiagramsJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestMatchersCoreJS/package scalatestMatchersCoreJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestShouldMatchersJS/package scalatestShouldMatchersJS/mimaReportBinaryIssues
    $ sbt ++2.12.20 scalatestMustMatchersJS/package scalatestMustMatchersJS/mimaReportBinaryIssues

    $ sbt ++2.13.16 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestCore/package scalatestCore/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFeatureSpec/package scalatestFeatureSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFlatSpec/package scalatestFlatSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFreeSpec/package scalatestFreeSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFunSuite/package scalatestFunSuite/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFunSpec/package scalatestFunSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestPropSpec/package scalatestPropSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestRefSpec/package scalatestRefSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestWordSpec/package scalatestWordSpec/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestDiagrams/package scalatestDiagrams/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestMatchersCore/package scalatestMatchersCore/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestShouldMatchers/package scalatestShouldMatchers/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestMustMatchers/package scalatestMustMatchers/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestCoreJS/package scalatestCoreJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFeatureSpecJS/package scalatestFeatureSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFlatSpecJS/package scalatestFlatSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFreeSpecJS/package scalatestFreeSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFunSuiteJS/package scalatestFunSuiteJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestFunSpecJS/package scalatestFunSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestPropSpecJS/package scalatestPropSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestWordSpecJS/package scalatestWordSpecJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestDiagramsJS/package scalatestDiagramsJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestMatchersCoreJS/package scalatestMatchersCoreJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestShouldMatchersJS/package scalatestShouldMatchersJS/mimaReportBinaryIssues
    $ sbt ++2.13.16 scalatestMustMatchersJS/package scalatestMustMatchersJS/mimaReportBinaryIssues

To publish scalatest modules for jvm, js, native and dotty, use the following commands:

    $ ./publish.sh
