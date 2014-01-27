/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.tools

import org.scalatest._
import org.scalatest.events.Event
import org.scalatest.events.Ordinal
import org.scalatest.events.AlertProvided
import scala.collection.mutable
import java.util.regex.Pattern
import java.net.URL
import java.io.File
import org.scalatest.tools.Runner.deglobSuiteParams

class RunnerSpec extends Spec with PrivateMethodTester {

  def `parseArgsIntoLists should work correctly using deprecated args` {

    // this is how i solved the problem of wanting to reuse these val names, runpathList, reportersList, etc.
    // by putting them in a little verify method, it gets reused each time i call that method
    def verify(
      args: Array[String],
      expectedRunpathList: List[String],
      expectedReporterList: List[String],
      expectedSuitesList: List[String],
      expectedRunAgainsList: List[String],
      expectedJunitsList: List[String],
      expectedPropsList: List[String],
      expectedIncludesList: List[String],
      expectedExcludesList: List[String],
      expectedConcurrentList: List[String],
      expectedMemberOfList: List[String],
      expectedBeginsWithList: List[String],
      expectedTestNGList: List[String],
      expectedSuffixes: Option[Pattern], 
      expectedChosenStyleList: List[String], 
      expectedScaleFactorList: List[String], 
      expectedTestSortingReporterTimeoutList: List[String],
      expectedSlowpokeList: List[String]
    ) = {

      val ParsedArgs(
        runpathList,
        reportersList,
        suitesList,
        runAgainsList,
        junitsList,
        propsList,
        includesList,
        excludesList,
        concurrentList,
        memberOfList,
        beginsWithList,
        testNGList,
        suffixes, 
        chosenStyleList, 
        spanScaleFactorList,
        testSortingReporterTimeoutList,
        slowpokeList
      ) = Runner.parseArgs(args)

      assert(runpathList === expectedRunpathList)
      assert(reportersList === expectedReporterList)
      assert(suitesList === expectedSuitesList)
      assert(runAgainsList === expectedRunAgainsList)
      assert(junitsList === expectedJunitsList)
      assert(propsList === expectedPropsList)
      assert(includesList === expectedIncludesList)
      assert(excludesList === expectedExcludesList)
      assert(concurrentList === expectedConcurrentList)
      assert(memberOfList === expectedMemberOfList)
      assert(beginsWithList === expectedBeginsWithList)
      assert(testNGList === expectedTestNGList)
      assert(chosenStyleList === expectedChosenStyleList)
      assert(spanScaleFactorList == expectedScaleFactorList)
      assert(testSortingReporterTimeoutList == expectedTestSortingReporterTimeoutList)
      assert(slowpokeList == expectedSlowpokeList)
      if (expectedSuffixes.isEmpty) {
        assert(suffixes.isEmpty)
      } else {
        assert(!suffixes.isEmpty)
        assert(suffixes.get.toString === expectedSuffixes.get.toString)
      }
    }

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out", "-p"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-p"),
      List("-g", "-g", "-f", "file.out"),
      Nil,
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array(),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-n", "JustOne", "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "JustOne"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo", "-m", "com.example.webapp",
          "-w", "com.example.root"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Try a TestNGSuite
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo", "-m", "com.example.webapp",
          "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Try a junit Suite
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-j", "junitTest", "-j", "junitTest2",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne"),
      Nil,
      List("-j", "junitTest", "-j", "junitTest2"),
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -u option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -q option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-q", "Spec|Suite",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite)$")), 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -q option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-q", "Spec", "-q", "Suite",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite)$")), 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -Q option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")), 
      Nil, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -W option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-W", "60", "60", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")), 
      Nil, 
      Nil, 
      Nil,
      List("-W", "60", "60")
    )
    intercept[IllegalArgumentException] {
      Runner.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W", "sixty", "60")
      )
    }
    intercept[IllegalArgumentException] {
      Runner.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W", "60", "sixty")
      )
    }
    intercept[IllegalArgumentException] {
      Runner.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W", "60")
      )
    }
    intercept[IllegalArgumentException] {
      Runner.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W")
      )
    }
  }
  
  def `parseArgsIntoLists should work correctly using non-deprecated args` {

    // this is how i solved the problem of wanting to reuse these val names, runpathList, reportersList, etc.
    // by putting them in a little verify method, it gets reused each time i call that method
    def verify(
      args: Array[String],
      expectedRunpathList: List[String],
      expectedReporterList: List[String],
      expectedSuitesList: List[String],
      expectedRunAgainsList: List[String],
      expectedJunitsList: List[String],
      expectedPropsList: List[String],
      expectedIncludesList: List[String],
      expectedExcludesList: List[String],
      expectedConcurrentList: List[String],
      expectedMemberOfList: List[String],
      expectedBeginsWithList: List[String],
      expectedTestNGList: List[String],
      expectedSuffixes: Option[Pattern], 
      expectedChosenStyleList: List[String], 
      expectedSpanScaleFactorList: List[String], 
      expectedTestSortingReporterTimeoutList: List[String]
    ) = {

      val ParsedArgs(
        runpathList,
        reportersList,
        suitesList,
        runAgainsList,
        junitsList,
        propsList,
        includesList,
        excludesList,
        concurrentList,
        memberOfList,
        beginsWithList,
        testNGList,
        suffixes, 
        chosenStyleList, 
        spanScaleFactorList, 
        testSortingReporterTimeoutList,
        slowpokeList
      ) = Runner.parseArgs(args)

      assert(runpathList === expectedRunpathList)
      assert(reportersList === expectedReporterList)
      assert(suitesList === expectedSuitesList)
      assert(runAgainsList === expectedRunAgainsList)
      assert(junitsList === expectedJunitsList)
      assert(propsList === expectedPropsList)
      assert(includesList === expectedIncludesList)
      assert(excludesList === expectedExcludesList)
      assert(concurrentList === expectedConcurrentList)
      assert(memberOfList === expectedMemberOfList)
      assert(beginsWithList === expectedBeginsWithList)
      assert(testNGList === expectedTestNGList)
      assert(chosenStyleList === expectedChosenStyleList)
      assert(spanScaleFactorList == expectedSpanScaleFactorList)
      assert(testSortingReporterTimeoutList == expectedTestSortingReporterTimeoutList)
      if (expectedSuffixes.isEmpty) {
        assert(suffixes.isEmpty)
      } else {
        assert(!suffixes.isEmpty)
        assert(suffixes.get.toString === expectedSuffixes.get.toString)
      }
    }

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out", "-R"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-p"),
      List("-g", "-g", "-f", "file.out"),
      Nil,
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array(),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-n", "JustOne", "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "JustOne"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
            "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
            "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo", "-m", "com.example.webapp",
          "-w", "com.example.root"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )
    // Try a TestNGSuite
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-s", "SuiteTwo", "-m", "com.example.webapp",
          "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )
    // Try a junit Suite
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-j", "junitTest", "-j", "junitTest2",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne"),
      Nil,
      List("-j", "junitTest", "-j", "junitTest2"),
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -u option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -q option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-q", "Spec|Suite",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite)$")), 
      Nil, 
      Nil, 
      Nil
    )
    // Test -q option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-q", "Spec", "-q", "Suite",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite)$")), 
      Nil, 
      Nil, 
      Nil
    )
    // Test -Q option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")), 
      Nil, 
      Nil, 
      Nil
    )
    // Test -F option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-F", "200"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")), 
      Nil, 
      List("-F", "200"), 
      Nil
    )
    // Test -T option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-T", "20"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")), 
      Nil, 
      Nil, 
      List("-T", "20")
    )
    // Test -h option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-h", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-h", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )
    // Test -h -Y option
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-h", "directory/",
          "-Y", "mystyles.css", "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-h", "directory/", "-Y", "mystyles.css"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )

    // Test -M
    verify(
      Array("-M", "target/doovers.txt"),
      Nil,
      List("-M", "target/doovers.txt"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    // Test -M some more
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g",
          "-M", "target/doovers.txt",
          "-h", "directory/", "-Y", "mystyles.css", "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
          "-M", "target/doovers2.txt",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-M", "target/doovers.txt", "-h", "directory/", "-Y", "mystyles.css", "-M", "target/doovers2.txt"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )

    // Test -A
    verify(
      Array("-A", "target/doovers.txt"),
      Nil,
      Nil,
      Nil,
      List("-A", "target/doovers.txt"),
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      Nil,
      None, 
      Nil, 
      Nil, 
      Nil
    )

    // Test -A some more
    verify(
      Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g",
          "-A", "target/doovers.txt",
          "-h", "directory/", "-Y", "mystyles.css", "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
          "-A", "target/doovers2.txt",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-p", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-h", "directory/", "-Y", "mystyles.css"),
      List("-s", "SuiteOne"),
      List("-A", "target/doovers.txt", "-A", "target/doovers2.txt"),
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-c"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None, 
      Nil, 
      Nil, 
      Nil
    )
  }

  object `parseCompoundArgIntoSet should` {
    def `work correctly` {
      assertResult(Set("Cat", "Dog")) {
        Runner.parseCompoundArgIntoSet(List("-n", "Cat Dog"), "-n")
      }
    }
    def `merge overlapping values` {
      assertResult(Set("tag", "tag2", "tag3")) {
        Runner.parseCompoundArgIntoSet(List("-l", "tag tag2", "-l", "tag2 tag3"),"-l")
      }
    }
  }

  object `parseCompoundArgIntoList should parse` {
    def `single` {
      assertResult(List("tag")) {
        Runner.parseCompoundArgIntoList(List("-l", "tag"),"-l")
      }
    }
    def `multi` {
      assertResult(List("tag","tag2")) {
        Runner.parseCompoundArgIntoList(List("-l", "tag tag2"),"-l")
      }
    }
    def `different pairs` {
      assertResult(List("tag", "tag2", "tag3", "tag4")) {
        Runner.parseCompoundArgIntoList(List("-l", "tag tag2", "-l", "tag3 tag4"),"-l")
      }
    }
    def `overlapping pairs` {
      assertResult(List("tag", "tag2", "tag2", "tag3")) {
        Runner.parseCompoundArgIntoList(List("-l", "tag tag2", "-l", "tag2 tag3"),"-l")
      }
    }
  }

  val parseConfigSet = PrivateMethod[Set[ReporterConfigParam]]('parseConfigSet)

  object `parseConfigSet should` {
    object `handle string reporter options for reminders` {
      object `with full stack traces (G)` {
        def `including canceled tests` {
          assertResult(Set(PresentReminderWithFullStackTraces)) {
            Runner invokePrivate parseConfigSet("-oG")
          }
          assertResult(Set(PresentReminderWithFullStackTraces)) {
            Runner invokePrivate parseConfigSet("-eG")
          }
          assertResult(Set(PresentReminderWithFullStackTraces)) {
            Runner invokePrivate parseConfigSet("-fG")
          }
        }
        def `excluding canceled tests` {
          assertResult(Set(PresentReminderWithFullStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-oGK")
          }
          assertResult(Set(PresentReminderWithFullStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-eGK")
          }
          assertResult(Set(PresentReminderWithFullStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-fGK")
          }
        }
      }
      object `with short stack traces (T)` {
        def `including canceled tests` {
          assertResult(Set(PresentReminderWithShortStackTraces)) {
            Runner invokePrivate parseConfigSet("-oT")
          }
          assertResult(Set(PresentReminderWithShortStackTraces)) {
            Runner invokePrivate parseConfigSet("-eT")
          }
          assertResult(Set(PresentReminderWithShortStackTraces)) {
            Runner invokePrivate parseConfigSet("-fT")
          }
        }
        def `excluding canceled tests` {
          assertResult(Set(PresentReminderWithShortStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-oTK")
          }
          assertResult(Set(PresentReminderWithShortStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-eTK")
          }
          assertResult(Set(PresentReminderWithShortStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-fTK")
          }
        }
      }
      object `with no stack traces (I)` {
        def `including canceled tests` {
          assertResult(Set(PresentReminderWithoutStackTraces)) {
            Runner invokePrivate parseConfigSet("-oI")
          }
          assertResult(Set(PresentReminderWithoutStackTraces)) {
            Runner invokePrivate parseConfigSet("-eI")
          }
          assertResult(Set(PresentReminderWithoutStackTraces)) {
            Runner invokePrivate parseConfigSet("-fI")
          }
        }
        def `excluding canceled tests` {
          assertResult(Set(PresentReminderWithoutStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-oIK")
          }
          assertResult(Set(PresentReminderWithoutStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-eIK")
          }
          assertResult(Set(PresentReminderWithoutStackTraces, PresentReminderWithoutCanceledTests)) {
            Runner invokePrivate parseConfigSet("-fIK")
          }
        }
      }
    }
  }

  def `parseConfigSet should work correctly` {

    intercept[NullPointerException] {
      Runner invokePrivate parseConfigSet(null)
    }
    intercept[IllegalArgumentException] {
      Runner invokePrivate parseConfigSet("-oYZTFUPBISARG-")
    }
    intercept[IllegalArgumentException] {
      Runner invokePrivate parseConfigSet("-")
    }
    intercept[IllegalArgumentException] {
      Runner invokePrivate parseConfigSet("")
    }

    assertResult(Set(FilterTestStarting)) {
      Runner invokePrivate parseConfigSet("-oN")
    }
    assertResult(Set(FilterTestSucceeded)) {
      Runner invokePrivate parseConfigSet("-oC")
    }
    assertResult(Set(FilterTestIgnored)) {
      Runner invokePrivate parseConfigSet("-oX")
    }
    assertResult(Set(FilterTestPending)) {
      Runner invokePrivate parseConfigSet("-oE")
    }
    assertResult(Set(FilterSuiteStarting)) {
      Runner invokePrivate parseConfigSet("-oH")
    }
    assertResult(Set(FilterSuiteCompleted)) {
      Runner invokePrivate parseConfigSet("-oL")
    }
    assertResult(Set(FilterInfoProvided)) {
      Runner invokePrivate parseConfigSet("-oO")
    }
    assertResult(Set(PresentWithoutColor)) {
      Runner invokePrivate parseConfigSet("-oW")
    }
    assertResult(Set(PresentAllDurations)) {
      Runner invokePrivate parseConfigSet("-oD")
    }
    assertResult(Set(PresentFullStackTraces)) {
      Runner invokePrivate parseConfigSet("-oF")
    }
    assertResult(Set[ReporterConfigParam]()) {
      Runner invokePrivate parseConfigSet("-f")
    }
    assertResult(Set[ReporterConfigParam]()) {
      Runner invokePrivate parseConfigSet("-u")
    }

    assertResult(Set(FilterInfoProvided, PresentWithoutColor)) {
      Runner invokePrivate parseConfigSet("-oOW")
    }
    assertResult(Set(FilterInfoProvided, PresentWithoutColor)) {
      Runner invokePrivate parseConfigSet("-oWO") // Just reverse the order of the params
    }
    val allOpts = Set(
      FilterInfoProvided,
      FilterSuiteCompleted,
      FilterSuiteStarting,
      FilterTestIgnored,
      FilterTestPending,
      FilterTestStarting,
      FilterTestSucceeded,
      PresentAllDurations,
      PresentWithoutColor,
      PresentFullStackTraces
    )
    assertResult(allOpts) {
      Runner invokePrivate parseConfigSet("-oNCXEHLOWDF")
    }
  }
                                         
  def `parseReporterArgsIntoSpecs should work correctly` {
    intercept[NullPointerException] {
      Runner.parseReporterArgsIntoConfigurations(null)
    }
    intercept[NullPointerException] {
      Runner.parseReporterArgsIntoConfigurations(List("Hello", null, "World"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("Hello", "-", "World"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("Hello", "", "World"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-g", "-l", "-o"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("Hello", " there", " world!"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-g", "-o", "-g", "-e"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-o", "-o", "-g", "-e"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-e", "-o", "-g", "-e"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-f")) // Can't have -f last, because need a file name
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-u")) // Can't have -u last, because need a directory name
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-r")) // Can't have -r last, because need a reporter class
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-k")) // Can't have -k last, because need a host and port
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-K")) // Can't have -k last, because need a host and port
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-k", "localhost")) // Can't have -k host last, because need a port
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-K", "localhost")) // Can't have -k host last, because need a port
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-k", "localhost", "abc")) // -k port number must be integer.
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-K", "localhost", "abc")) // -k port number must be integer.
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(Nil)
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-C")) // Can't have -C last, because need a reporter class
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-h"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-M")) // needs a file name argument
    }
    intercept[IllegalArgumentException] {
      Runner.parseReporterArgsIntoConfigurations(List("-h", "html", "-Y"))
    }
    assertResult(new ReporterConfigurations(Some(new GraphicReporterConfiguration(Set())), Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-g"))
    }
    assertResult(new ReporterConfigurations(Some(new GraphicReporterConfiguration(Set(FilterSuiteCompleted))), Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-gL"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */Some(new StandardOutReporterConfiguration(Set())), None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-o"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */Some(new StandardOutReporterConfiguration(Set(FilterTestSucceeded,FilterTestIgnored))), None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-oCX"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, Some(new StandardErrReporterConfiguration(Set())), Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-e"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, Some(new StandardErrReporterConfiguration(Set(PresentFullStackTraces))), Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-eF"))
    }
    assertResult(new ReporterConfigurations(None, List(new FileReporterConfiguration(Set(), "theFilename")), Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-f", "theFilename"))
    }
    assertResult(new ReporterConfigurations(None, Nil, List(new MemoryReporterConfiguration("theFilename")), Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-M", "theFilename"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, List(new JunitXmlReporterConfiguration(Set(), "target")), /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-u", "target"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, List(new JunitXmlReporterConfiguration(Set(), "target")), /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-uN", "target"))
    }
    assertResult(new ReporterConfigurations(None, List(new FileReporterConfiguration(Set(FilterTestStarting), "theFilename")), Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-fN", "theFilename"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, List(new CustomReporterConfiguration(Set(), "the.reporter.Class")), Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-r", "the.reporter.Class"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, List(new CustomReporterConfiguration(Set(FilterTestPending), "the.reporter.Class")), Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-rE", "the.reporter.Class"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, List(new XmlSocketReporterConfiguration("localhost", 8888)), Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-k", "localhost", "8888"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, List(new XmlSocketReporterConfiguration("localhost", 8888), new XmlSocketReporterConfiguration("another host", 1234)), Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-k", "localhost", "8888", "-k", "another host", "1234"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, List(new SocketReporterConfiguration("localhost", 8888)))) {
      Runner.parseReporterArgsIntoConfigurations(List("-K", "localhost", "8888"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, List(new SocketReporterConfiguration("localhost", 8888), new SocketReporterConfiguration("another host", 1234)))) {
      Runner.parseReporterArgsIntoConfigurations(List("-K", "localhost", "8888", "-K", "another host", "1234"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, List(new HtmlReporterConfiguration(Set(), "html", None)), Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-h", "html"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, List(new HtmlReporterConfiguration(Set(), "html", Some(new File("MyStyle.css").toURI.toURL))), Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-h", "html", "-Y", "MyStyle.css"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, List(new HtmlReporterConfiguration(Set(), "htmldir", None), new HtmlReporterConfiguration(Set(), "html", Some(new File("MyStyle.css").toURI.toURL))), Nil, Nil, Nil)) {
      Runner.parseReporterArgsIntoConfigurations(List("-h", "htmldir", "-h", "html", "-Y", "MyStyle.css"))
    }
  }

  def `parseSuiteArgsIntoClassNameStrings should work correctly` {
    intercept[NullPointerException] {
      Runner.parseSuiteArgsIntoNameStrings(null, "-j")
    }
    intercept[NullPointerException] {
      Runner.parseSuiteArgsIntoNameStrings(List("-j", null, "-j"), "-j")
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j"), "-j")
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j", "-j"), "-j")
    }
    assertResult(List("SweetSuite", "OKSuite")) {
      Runner.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j", "OKSuite"), "-j")
    }
    assertResult(List("SweetSuite", "OKSuite", "SomeSuite")) {
      Runner.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j", "OKSuite", "-j", "SomeSuite"), "-j")
    }
  }

  def `parseRunpathArgIntoList should work correctly` {
    intercept[NullPointerException] {
      Runner.parseRunpathArgIntoList(null)
    }
    intercept[NullPointerException] {
      Runner.parseRunpathArgIntoList(List("-p", null))
    }
    intercept[NullPointerException] {
      Runner.parseRunpathArgIntoList(List(null, "serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseRunpathArgIntoList(List("-p"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseRunpathArgIntoList(List("-p", "bla", "bla"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseRunpathArgIntoList(List("-pX", "bla"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseRunpathArgIntoList(List("-p", "  "))
    }
    intercept[IllegalArgumentException] {
      Runner.parseRunpathArgIntoList(List("-p", "\t"))
    }
    assertResult(List("bla")) {
      Runner.parseRunpathArgIntoList(List("-p", "bla"))
    }
    assertResult(List("bla", "bla", "bla")) {
      Runner.parseRunpathArgIntoList(List("-p", "bla bla bla"))
    }
    assertResult(List("serviceuitest-1.1beta4.jar", "myjini", "http://myhost:9998/myfile.jar")) {
      Runner.parseRunpathArgIntoList(List("-p", "serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar"))
    }
    assertResult(List("\\", "c:\\", "c:\\Program Files", "c:\\Documents and Settings", "\\", "myjini")) {
      Runner.parseRunpathArgIntoList(List("-p", """\ c:\ c:\Program\ Files c:\Documents\ and\ Settings \ myjini"""))
    }
  }

  def `parsePropertiesArgsIntoMap should work correctly` {
    intercept[NullPointerException] {
      Runner.parsePropertiesArgsIntoMap(null)
    }
    intercept[NullPointerException] {
      Runner.parsePropertiesArgsIntoMap(List("-Da=b", null))
    }
    intercept[IllegalArgumentException] {
      Runner.parsePropertiesArgsIntoMap(List("-Dab")) // = sign missing
    }
    intercept[IllegalArgumentException] {
      Runner.parsePropertiesArgsIntoMap(List("ab")) // needs to start with -D
    }
    intercept[IllegalArgumentException] {
      Runner.parsePropertiesArgsIntoMap(List("-D=ab")) // no key
    }
    intercept[IllegalArgumentException] {
      Runner.parsePropertiesArgsIntoMap(List("-Dab=")) // no value
    }
    assertResult(Map("a" -> "b", "cat" -> "dog", "Glorp" -> "Glib")) {
      Runner.parsePropertiesArgsIntoMap(List("-Da=b", "-Dcat=dog", "-DGlorp=Glib"))
    }
  }

  def `deprecatedCheckArgsForValidity should work correctly` {
    intercept[NullPointerException] {
      Runner.checkArgsForValidity(null)
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-p", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-z", "testWildcard", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-k", "hostname", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-K", "hostname", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-p", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite", "-c"))
    }
  }
  
  def `parseSuiteArgs should work correctly` {
    intercept[NullPointerException] {
      Runner.parseSuiteArgs(null)
    }
    intercept[NullPointerException] {
      Runner.parseSuiteArgs(List("-s", null, "-s", "suite2"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-s", "-s"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-s", "suite1", "-s"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-sG", "suite1"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-t", "-t", "testname"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-t", "testname", "-t"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-t", "testname", "-z"))
    }
    intercept[IllegalArgumentException] {
      Runner.parseSuiteArgs(List("-z"))
    }
    intercept[IllegalArgumentException] {
      // -i without -s should not be supported, as for example current command is having -s -i, there's no way to tell the next -i should be a -i without -s.
      // -i should only be used together with -s to select nested suite.
      Runner.parseSuiteArgs(List("-i", "suite1"))
    }
    intercept[IllegalArgumentException] {
      // -sX -t should not be supported, as -s -t should be used to select a specific test.
      Runner.parseSuiteArgs(List("-sX", "suite1", "-t", "test1"))
    }
    intercept[IllegalArgumentException] {
      // -iX should not be supported, as a nested suite's nested suites should not be included, if it is included, we have to figure out the way to specify if 
      // nested suite's nested suite's nested suites (and endless down the tree) should be implemented.
      Runner.parseSuiteArgs(List("-s", "suite1", "-iX", "nested1"))
    }

    val (suites1, tests1) = Runner.parseSuiteArgs(List("-s", "suite1", "-s", "suite2"))
    assert(tests1.length === 0)
    assert(suites1.length === 2)
    assert(suites1(0).className === "suite1")
    assert(suites1(0).testNames.length === 0)
    assert(suites1(1).className === "suite2")
    assert(suites1(1).testNames.length === 0)
    
    val (suites2, tests2) = Runner.parseSuiteArgs(List("-s", "suite1", "-t", "test1", "-t", "test2", "-s", "suite2"))
    assert(tests2.length === 0)
    assert(suites2.length === 2)
    assert(suites2(0).className === "suite1")
    assert(suites2(0).testNames.length === 2)
    assert(suites2(0).testNames(0) === "test1")
    assert(suites2(0).testNames(1) === "test2")
    assert(suites2(1).className === "suite2")
    assert(suites2(1).testNames.length === 0)
    
    val (suites3, tests3) = Runner.parseSuiteArgs(List("-s", "suite1", "-i", "nested1"))
    assert(tests3.length === 0)
    assert(suites3.length === 1)
    assert(suites3(0).className === "suite1")
    assert(suites3(0).testNames.length === 0)
    assert(suites3(0).nestedSuites.length === 1)
    assert(suites3(0).nestedSuites(0).suiteId === "nested1")
    assert(suites3(0).nestedSuites(0).testNames.length === 0)
    
    val (suites4, tests4) = Runner.parseSuiteArgs(List("-s", "suite1", "-i", "nested1", "-t", "test1", "-t", "test2"))
    assert(tests4.length === 0)
    assert(suites4.length === 1)
    assert(suites4(0).className === "suite1")
    assert(suites4(0).testNames.length === 0)
    assert(suites4(0).nestedSuites.length === 1)
    assert(suites4(0).nestedSuites(0).suiteId === "nested1")
    assert(suites4(0).nestedSuites(0).testNames.length === 2)
    assert(suites4(0).nestedSuites(0).testNames(0) === "test1")
    assert(suites4(0).nestedSuites(0).testNames(1) === "test2")
    
    val (suites5, tests5) = Runner.parseSuiteArgs(List("-s", "suite1", "-z", "test1", "-z", "test2", "-s", "suite2"))
    assert(tests5.length === 0)
    assert(suites5.length === 2)
    assert(suites5(0).className === "suite1")
    assert(suites5(0).testNames.length === 0)
    assert(suites5(0).wildcardTestNames.length === 2)
    assert(suites5(0).wildcardTestNames(0) === "test1")
    assert(suites5(0).wildcardTestNames(1) === "test2")
    assert(suites5(1).className === "suite2")
    assert(suites5(1).wildcardTestNames.length === 0)
    assert(suites5(1).testNames.length === 0)
    
    val (suites6, tests6) = Runner.parseSuiteArgs(List("-s", "suite1", "-t", "test1", "-z", "test2", "-s", "suite2"))
    assert(tests6.length === 0)
    assert(suites6.length === 2)
    assert(suites6(0).className === "suite1")
    assert(suites6(0).testNames.length === 1)
    assert(suites6(0).testNames(0) === "test1")
    assert(suites6(0).wildcardTestNames.length === 1)
    assert(suites6(0).wildcardTestNames(0) === "test2")
    assert(suites6(1).className === "suite2")
    assert(suites6(1).wildcardTestNames.length === 0)
    assert(suites6(1).testNames.length === 0)
    
    val (suites7, tests7) = Runner.parseSuiteArgs(List("-s", "suite1", "-i", "nested1", "-z", "test1", "-z", "test2"))
    assert(tests7.length === 0)
    assert(suites7.length === 1)
    assert(suites7(0).className === "suite1")
    assert(suites7(0).testNames.length === 0)
    assert(suites7(0).nestedSuites.length === 1)
    assert(suites7(0).nestedSuites(0).suiteId === "nested1")
    assert(suites7(0).nestedSuites(0).testNames.length === 0)
    assert(suites7(0).nestedSuites(0).wildcardTestNames.length === 2)
    assert(suites7(0).nestedSuites(0).wildcardTestNames(0) === "test1")
    assert(suites7(0).nestedSuites(0).wildcardTestNames(1) === "test2")
    
    val (suites8, tests8) = Runner.parseSuiteArgs(List("-s", "suite1", "-i", "nested1", "-t", "test1", "-z", "test2"))
    assert(tests8.length === 0)
    assert(suites8.length === 1)
    assert(suites8(0).className === "suite1")
    assert(suites8(0).testNames.length === 0)
    assert(suites8(0).nestedSuites.length === 1)
    assert(suites8(0).nestedSuites(0).suiteId === "nested1")
    assert(suites8(0).nestedSuites(0).testNames.length === 1)
    assert(suites8(0).nestedSuites(0).testNames(0) === "test1")
    assert(suites8(0).nestedSuites(0).wildcardTestNames.length === 1)
    assert(suites8(0).nestedSuites(0).wildcardTestNames(0) === "test2")

    //
    // A list starting with a -t should produce a corresponding test spec.
    //
    val (suites9, tests9) = Runner.parseSuiteArgs(List("-t", "test1"))
    assert(suites9.isEmpty)
    assert(tests9.length === 1)
    assert(tests9(0).spec === "test1")
    assert(tests9(0).isSubstring === false)

    //
    // A list starting with a -z should produce a corresponding test spec.
    //
    val (suites11, tests11) = Runner.parseSuiteArgs(List("-z", "substr1"))
    assert(suites11.isEmpty)
    assert(tests11.length === 1)
    assert(tests11(0).spec === "substr1")
    assert(tests11(0).isSubstring === true)

    //
    // A few -t's and -z's.
    //
    val (suites12, tests12) =
      Runner.parseSuiteArgs(List("-t", "test1",
                                 "-z", "substr1",
                                 "-t", "test2",
                                 "-t", "test3",
                                 "-z", "substr2"))
    assert(suites12.isEmpty)
    assert(tests12.length === 5)
    assert(tests12(0).spec === "test1")
    assert(tests12(0).isSubstring === false)
    assert(tests12(1).spec === "substr1")
    assert(tests12(1).isSubstring === true)
    assert(tests12(2).spec === "test2")
    assert(tests12(2).isSubstring === false)
    assert(tests12(3).spec === "test3")
    assert(tests12(3).isSubstring === false)
    assert(tests12(4).spec === "substr2")
    assert(tests12(4).isSubstring === true)
  }
  
  def `checkArgsForValidity should work correctly` {
    intercept[NullPointerException] {
      Runner.checkArgsForValidity(null)
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-R", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-z", "test name wildcard", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-R", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite", "-P"))
    }
  }
  def `checkArgsForValidity should recognize -W (for slowpoke detector)` {
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-W", "60", "60"))
    }
    assertResult(None) {
      Runner.checkArgsForValidity(Array("-W", "60", "60", "-h", "htmldir"))
    }
  }
  
  def `parseChosenStylesIntoChosenStyleSet should work correctly` {
    intercept[IllegalArgumentException] {
      Runner.parseChosenStylesIntoChosenStyleSet(List("-a", "aStyle"), "-y")
    }
    intercept[IllegalArgumentException] {
      Runner.parseChosenStylesIntoChosenStyleSet(List("-y"), "-y")
    }
    intercept[IllegalArgumentException] {
      Runner.parseChosenStylesIntoChosenStyleSet(List("-y", "aStyle", "-y"), "-y")
    }
    val singleStyle = Runner.parseChosenStylesIntoChosenStyleSet(List("-y", "aStyle"), "-y")
    assert(singleStyle.size === 1)
    assert(singleStyle.contains("aStyle"))
    val multiStyle = Runner.parseChosenStylesIntoChosenStyleSet(List("-y", "aStyle", "-y", "bStyle", "-y", "cStyle"), "-y")
    assert(multiStyle.size === 3)
    assert(multiStyle.contains("aStyle"))
    assert(multiStyle.contains("bStyle"))
    assert(multiStyle.contains("cStyle"))
  }
  
  def `parseDoubleArgument should work correctly` {
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-a", "123"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-F", "abc"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-F"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-F", "123", "-F"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-F", "123", "-F", "456"), "-F", 1.0)
    }
    val spanScaleFactor = Runner.parseDoubleArgument(List("-F", "888"), "-F", 1.0)
    assert(spanScaleFactor === 888)
    
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-a", "123"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-T", "abc"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-T"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-T", "123", "-T"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      Runner.parseDoubleArgument(List("-T", "123", "-T", "456"), "-T", 15.0)
    }
    val testSortingReporterTimeout = Runner.parseDoubleArgument(List("-T", "888"), "-T", 15.0)
    assert(spanScaleFactor === 888)
  }
  
  def `parseConcurrentConfig should work correctly` {
    val emptyConcurrentConfig = Runner.parseConcurrentConfig(List.empty)
    assert(emptyConcurrentConfig.numThreads === 0)
    assert(emptyConcurrentConfig.enableSuiteSortingReporter === false)
    
    val singleDashP = Runner.parseConcurrentConfig(List("-c"))
    assert(singleDashP.numThreads === 0)
    assert(singleDashP.enableSuiteSortingReporter === false)
    
    val multiDashP = Runner.parseConcurrentConfig(List("-c", "-c"))
    assert(multiDashP.numThreads === 0)
    assert(multiDashP.enableSuiteSortingReporter === false)
    
    val singleDashPThreadNum = Runner.parseConcurrentConfig(List("-c10"))
    assert(singleDashPThreadNum.numThreads === 10)
    assert(singleDashPThreadNum.enableSuiteSortingReporter === false)
    
    val multiDashPThreadNum = Runner.parseConcurrentConfig(List("-c10", "-c5"))
    assert(multiDashPThreadNum.numThreads === 10)
    assert(multiDashPThreadNum.enableSuiteSortingReporter === false)
    
    val singleDashPS = Runner.parseConcurrentConfig(List("-cS"))
    assert(singleDashPS.numThreads === 0)
    assert(singleDashPS.enableSuiteSortingReporter === true)
    
    val multiDashPS = Runner.parseConcurrentConfig(List("-c", "-cS"))
    assert(multiDashPS.numThreads === 0)
    assert(multiDashPS.enableSuiteSortingReporter === true)
    
    val singleDashPSThreadNum = Runner.parseConcurrentConfig(List("-cS8"))
    assert(singleDashPSThreadNum.numThreads === 8)
    assert(singleDashPSThreadNum.enableSuiteSortingReporter === true)
    
    val multipDashPSThreadNum = Runner.parseConcurrentConfig(List("-cS8", "-c10"))
    assert(multipDashPSThreadNum.numThreads === 8)
    assert(multipDashPSThreadNum.enableSuiteSortingReporter === true)
  }

  def `deglobSuiteParams should work correctly` {
    val suiteParam =
      SuiteParam("", Array.empty[String], Array.empty[String],
                 Array.empty[NestedSuiteParam])

    val classNames =
      List(
        "foo.FooSpec",
        "foo.tools.FooToolsSpec",
        "foo.tools.FooToolsSuite",
        "foo.events.EventsFooSuite")

    def runDeglob(globs: List[String], expecteds: List[String]) {
      val globSuiteParams =
        globs.map(glob => suiteParam.copy(className = glob))

      val result = deglobSuiteParams(globSuiteParams, classNames.toSet)

      assert(result.map(_.className).toSet === expecteds.toSet)
    }

    runDeglob(List("org.*"), Nil)
    runDeglob(List("f?.*"),  Nil)
    runDeglob(List("f??.*"), classNames)
    runDeglob(List("foo.tools.*"),
              List("foo.tools.FooToolsSpec",
                   "foo.tools.FooToolsSuite"))
    runDeglob(List("*.Foo*"),
              List("foo.FooSpec",
                   "foo.tools.FooToolsSpec",
                   "foo.tools.FooToolsSuite"))
    runDeglob(List("*.Foo*ls*"),
              List("foo.tools.FooToolsSpec",
                   "foo.tools.FooToolsSuite"))
    runDeglob(List("*FooS[u]*"),
              List("foo.events.EventsFooSuite"))
    runDeglob(List("*FooS[up]*"),
              List("foo.FooSpec",
                   "foo.events.EventsFooSuite"))
  }

  def `readMemoryFiles should issue alert if a Memento isn't runnable` {
    val events = mutable.Set.empty[Event]

    var tracker = new Tracker(new Ordinal(12))

    val reporter = new Reporter {
      def apply(event: Event) {
        events += event
      }
    }

    Runner.readMemoryFiles(
      List("src/test/scala/org/scalatest/tools/memoryfile.eg"),
      reporter, tracker)

    assert(1 === events.filter(_.isInstanceOf[AlertProvided]).size)
  }
}
