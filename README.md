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
*   [SBT 1.2.8](https://www.scala-sbt.org/1.x/docs/Setup.html)

use the following SBT options instead:

    SBT_OPTS="-server -Xms512M -Xmx3000M -Xss1M  -XX:+UseConcMarkSweepGC -XX:NewRatio=8"

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

    $ sbt ++2.11.12 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatest/package scalatest/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.11.12 scalatestJS/package scalatestJS/mimaReportBinaryIssues

    $ sbt ++2.10.7 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatest/package scalatest/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.10.7 scalatestJS/package scalatestJS/mimaReportBinaryIssues

    $ sbt ++2.12.11 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.12.11 scalatest/package scalatest/mimaReportBinaryIssues
    $ sbt ++2.12.11 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.12.11 scalatestJS/package scalatestJS/mimaReportBinaryIssues

    $ sbt ++2.13.2 scalactic/package scalactic/mimaReportBinaryIssues
    $ sbt ++2.13.2 scalatest/package scalatest/mimaReportBinaryIssues
    $ sbt ++2.13.2 scalacticJS/package scalacticJS/mimaReportBinaryIssues
    $ sbt ++2.13.2 scalatestJS/package scalatestJS/mimaReportBinaryIssues

To publish scalactic, scalatest and scalatest-app use the following command:

    $ ./publish.sh
