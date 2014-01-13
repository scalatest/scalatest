ScalaTest
=========

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
*   [SBT 0.13.0](http://www.scala-sbt.org/0.13.0/docs/Getting-Started/Setup.html)

and use the following options in your SBT launch file:

    SBT_OPTS="-server -Xms512M -Xmx2048M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:NewRatio=8 -XX:MaxPermSize=512M"

### Building and Running Tests

This command will build and run the regular tests:

  `$ sbt test`

To run generated tests, you'll need to increase maximum heap size to -Xmx5120M, and use this command instead:

  `$ sbt "project gentests" "test"`

What it does is simply switch to gentests project and run test.

### Packaging

You can package the ScalaTest JAR file using this command:

  `$ sbt package`

The resulting JAR file will be produced in target/scala-2.10/.

You can also publish it to your local Ivy repository using this command:

  `$ sbt publishLocal`

Or publish it to local maven repository using this command:

  `$ sbt publishM2`
