package org.scalatest.tools

import java.io.File
import java.util.regex.Matcher.quoteReplacement

import scala.collection.mutable
import scala.xml.XML
import scala.xml.NodeSeq

private[scalatest] case class Durations(file: File) {
  val suites = mutable.Set[Suite]()

  if (file.exists) {
    val durationsXml = XML.loadFile(file)

    for (suiteXml <- durationsXml \ "suite") {
      val suite = Suite("" + (suiteXml \ "@suiteId"),
                        "" + (suiteXml \ "@suiteName"))
      suites += suite

      for (testXml <- suiteXml \ "test") {
        val test = Test("" + (testXml \ "@testName"))
        suite.tests += test

        val previous = testXml \ "previous"
        test.previousNum = (previous \ "@num").toString.toInt
        test.previousAverage = (previous \ "@average").toString.toInt

        for (durationXml <- testXml \ "duration") {
          val duration = Duration((durationXml \ "@run").toString,
                                  (durationXml \ "@millis").toString.toInt)
          test.durations = duration :: test.durations
        }
        test.durations = test.durations.reverse
      }
    }
  }
  
  //
  // Adds test results from specified xml to this Duration.  The xml is
  // in the format of a run file.
  //
  // The 'run' parameter is the timestamp identifier for the run.
  //
  def addTests(run: String, runXml: NodeSeq) {
    for (suite <- runXml \\ "suite") {
      val suiteId   = (suite \ "@id").toString
      val suiteName = (suite \ "@name").toString

      for (test <- suite \ "test") {
        val result = (test \ "@result").toString

        if (result == "succeeded") {
          val testName = (test \ "@name").toString
          val millis = (test \ "@duration").toString.toInt

          addDuration(suiteId, suiteName, testName, run, millis)
        }
      }
    }
  }

  def toXml: String = {
    val DurationsTemplate =
      """|<durations>
         |$suites$</durations>
         |""".stripMargin

    val buf = new StringBuilder

    for (suite <- suites) buf.append(suite.toXml)

    DurationsTemplate.replaceFirst("""\$suites\$""",
                                   quoteReplacement(buf.toString))
  }

  def addDuration(suiteId: String, suiteName: String, testName: String,
                  run: String, millis: Int)
  {
    def getSuite(): Suite = {
      val suiteOption = suites.find(suite => suite.suiteId == suiteId)

      if (suiteOption.isDefined) {
        suiteOption.get
      }
      else {
        val newSuite = Suite(suiteId, suiteName)
        suites += newSuite
        newSuite
      }
    }

    def getTest(): Test = {
      val suite = getSuite
      val testOption = suite.tests.find(test => test.name == testName)

      if (testOption.isDefined) {
        testOption.get
      }
      else {
        val newTest = Test(testName)
        suite.tests += newTest
        newTest
      }
    }

    def archiveOldestDuration(test: Test) {
      val oldestDuration = test.durations.last
      test.durations = test.durations.dropRight(1)

      test.previousAverage =
        (test.previousAverage * test.previousNum + oldestDuration.millis) /
        (test.previousNum + 1)

      test.previousNum += 1
    }
    
    val test = getTest()
    test.durations ::= new Duration(run, millis)

    if ((test.numberOfDurations * 0.8) > test.previousNum) {
      archiveOldestDuration(test)
    }
  }

  case class Suite(suiteId: String, suiteName: String) {
    val tests = mutable.Set[Test]()

    def toXml: String = {
      val SuiteTemplate =
        """|  <suite suiteId="$suiteId$" suiteName="$suiteName$">
           |$tests$  </suite>
           |""".stripMargin

      val buf = new StringBuilder

      for (test <- tests) buf.append(test.toXml)

      SuiteTemplate.
        replaceFirst("""\$suiteId\$""",   quoteReplacement(suiteId)).
        replaceFirst("""\$suiteName\$""", quoteReplacement(suiteName)).
        replaceFirst("""\$tests\$""",     quoteReplacement(buf.toString))
    }
  }

  case class Test(name: String) {
    var previousNum = 0
    var previousAverage = 0
    var durations = List[Duration]()

    def numberOfDurations = previousNum + durations.size

    def toXml: String = {
      val TestTemplate =
        """|    <test testName="$testName$">
           |      <previous num="$previousNum$" average="$previousAverage$"/>
           |$durations$    </test>
           |""".stripMargin

      val buf = new StringBuilder

      for (duration <- durations) buf.append(duration.toXml)

      TestTemplate.
        replaceFirst("""\$testName\$""",        quoteReplacement(name)).
        replaceFirst("""\$previousNum\$""",     previousNum.toString).
        replaceFirst("""\$previousAverage\$""", previousAverage.toString).
        replaceFirst("""\$durations\$""",       quoteReplacement(buf.toString))
    }

    def runCount: Int = {
      previousNum + durations.size
    }

    def computeNewAvg: Int = {
      durations.foldLeft(0)(_ + _.millis) / durations.size
    }
  }

  case class Duration(run: String, millis: Int) {
    def toXml: String = {
      val DurationTemplate =
        """|      <duration run="$run$" millis="$millis$"/>
           |""".stripMargin

      DurationTemplate.
        replaceFirst("""\$run\$""", run).
        replaceFirst("""\$millis\$""", millis.toString)
    }
  }

}

