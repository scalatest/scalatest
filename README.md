ScalaTest
=========

[![Build Status](https://travis-ci.org/scalatest/scalatest.png?branch=master)](https://travis-ci.org/scalatest/scalatest)

ScalaTest is a free, open-source testing toolkit for Scala and
Java programmers.

Official Website: http://www.scalatest.org/

Using ScalaTest
---------------

### Setup

Please visit [Download and Setup](http://www.scalatest.org/download) for download and setup instructions.

### Quick Start

Please visit [Quick Start](http://www.scalatest.org/quick_start) for steps to get started quickly.


Building ScalaTest
------------------

### Pre-Requisites

The followings are needed for building ScalaTest:

*   JDK 6 or 7
*   [SBT 0.13.1](http://www.scala-sbt.org/0.13.1/docs/Getting-Started/Setup.html)

and use the following options in your SBT launch file:

    SBT_OPTS="-server -Xms512M -Xmx2200M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M"

### Building and Running Tests

This command will build and run the regular tests:

  `$ sbt test`

To run generated tests, you'll need to increase maximum heap size to -Xmx5120M, and use this command instead:

  `$ sbt gentests/test`

What it does is simply switch to gentests project and run test.

### Packaging

You can package the ScalaTest JAR file using this command:

  `$ sbt package`

The resulting JAR file will be produced in target/scala-2.10/.

You can also publish it to your local Ivy repository using this command:

  `$ sbt publishLocal`

Or publish it to local maven repository using this command:

  `$ sbt publishM2`

### Publishing

To publish to Sonatype, you first need to make sure you have the following:

*   A GPG client is installed on your command line path. For more information, please refer to [GNU Privacy Guard Website](http://www.gnupg.org/).
*   You have created your GPG keys and distributed your public key to hkp://pool.sks-keyservers.net/. For more information, please refer to [How To Generate PGP Signatures With Maven](https://docs.sonatype.org/display/Repository/How+To+Generate+PGP+Signatures+With+Maven).
*   You have been granted the right to publish using org.scalatest and org.scalautils domain.

Before publish, you need to set the following environment variables correctly:

*   SCALATEST_NEXUS_LOGIN - Sonatype login name
*   SCALATEST_NEXUS_PASSWORD - Sonatype login password
*   SCALATEST_GPG_FILE - Location of GPG file
*   SCALATEST_GPG_PASSPHASE - The passphrase for the GPG file

You can use the following command to export your private key into a GPG file:

  `$ gpg --export-secret-keys example@example.com > example-secret-key.gpg`

To publish ScalaTest, use the following command:

  `$ sbt publish-signed`

To publish ScalaUtils, use the following command:

  `$ sbt "project scalautils" "publish-signed"`
