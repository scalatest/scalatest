/*
 * Copyright 2001-2015 Artima, Inc.
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

import java.util.regex.Pattern
import org.scalatest._
import java.io.File
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.funspec.AnyFunSpec

class ArgsParserSpec extends AnyFunSpec {

  it("parseArgsIntoLists should throw IllegalArgumentException using long-deprecated args") {

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
                expectedScaleFactorList: List[String],
                expectedTestSortingReporterTimeoutList: List[String],
                expectedSlowpokeList: List[String]
                ) = {

      val ParsedArgs(
      // SKIP-SCALATESTJS,NATIVE-START
      runpathList,
      // SKIP-SCALATESTJS,NATIVE-END
      reportersList,
      suitesList,
      // SKIP-SCALATESTJS,NATIVE-START
      runAgainsList,
      junitsList,
      propsList,
      // SKIP-SCALATESTJS,NATIVE-END
      includesList,
      excludesList,
      // SKIP-SCALATESTJS,NATIVE-START
      concurrentList,
      // SKIP-SCALATESTJS,NATIVE-END
      memberOfList,
      beginsWithList,
      // SKIP-SCALATESTJS,NATIVE-START
      testNGList,
      suffixes,
      spanScaleFactorList,
      testSortingReporterTimeoutList,
      slowpokeList,
      // SKIP-SCALATESTJS,NATIVE-END
      seedList
      ) = ArgsParser.parseArgs(args)

      // SKIP-SCALATESTJS,NATIVE-START
      assert(runpathList === expectedRunpathList)
      // SKIP-SCALATESTJS,NATIVE-END
      assert(reportersList === expectedReporterList)
      // SKIP-SCALATESTJS,NATIVE-START
      assert(suitesList === expectedSuitesList)
      assert(runAgainsList === expectedRunAgainsList)
      assert(junitsList === expectedJunitsList)
      assert(propsList === expectedPropsList)
      // SKIP-SCALATESTJS,NATIVE-END
      assert(includesList === expectedIncludesList)
      assert(excludesList === expectedExcludesList)
      // SKIP-SCALATESTJS,NATIVE-START
      assert(concurrentList === expectedConcurrentList)
      // SKIP-SCALATESTJS,NATIVE-END
      assert(memberOfList === expectedMemberOfList)
      // SKIP-SCALATESTJS,NATIVE-START
      assert(beginsWithList === expectedBeginsWithList)
      assert(testNGList === expectedTestNGList)
      assert(spanScaleFactorList == expectedScaleFactorList)
      assert(testSortingReporterTimeoutList == expectedTestSortingReporterTimeoutList)
      assert(slowpokeList == expectedSlowpokeList)
      if (expectedSuffixes.isEmpty) {
        assert(suffixes.isEmpty)
      } else {
        assert(!suffixes.isEmpty)
        assert(suffixes.get.toString === expectedSuffixes.get.toString)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

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

    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Try a TestNGSuite
    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Try a junit Suite
    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Test -u option
    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Test -q option
    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Test -q option
    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Test -Q option
    intercept[IllegalArgumentException] {
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
        Nil
      )
    }

    // Test -W option
    intercept[IllegalArgumentException] {
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
        List("-W", "60", "60")
      )
    }

    intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W", "sixty", "60")
      )
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W", "60", "sixty")
      )
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W", "60")
      )
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(
        Array("-c", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-p",
          "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
          "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
          "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-W")
      )
    }
  }

  it("parseArgsIntoLists should work correctly using non-deprecated args") {

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
                expectedSpanScaleFactorList: List[String],
                expectedTestSortingReporterTimeoutList: List[String],
                expectedSeedList: List[String]
                ) = {

      val ParsedArgs(
      // SKIP-SCALATESTJS,NATIVE-START
      runpathList,
      // SKIP-SCALATESTJS,NATIVE-END
      reportersList,
      suitesList,
      // SKIP-SCALATESTJS,NATIVE-START
      runAgainsList,
      junitsList,
      propsList,
      // SKIP-SCALATESTJS,NATIVE-END
      includesList,
      excludesList,
      // SKIP-SCALATESTJS,NATIVE-START
      concurrentList,
      // SKIP-SCALATESTJS,NATIVE-END
      memberOfList,
      beginsWithList,
      // SKIP-SCALATESTJS,NATIVE-START
      testNGList,
      suffixes,
      spanScaleFactorList,
      testSortingReporterTimeoutList,
      slowpokeList,
      // SKIP-SCALATESTJS,NATIVE-END
      seedList
      ) = ArgsParser.parseArgs(args)

      // SKIP-SCALATESTJS,NATIVE-START
      assert(runpathList === expectedRunpathList)
      // SKIP-SCALATESTJS,NATIVE-END
      assert(reportersList === expectedReporterList)
      // SKIP-SCALATESTJS,NATIVE-START
      assert(suitesList === expectedSuitesList)
      assert(runAgainsList === expectedRunAgainsList)
      assert(junitsList === expectedJunitsList)
      assert(propsList === expectedPropsList)
      // SKIP-SCALATESTJS,NATIVE-END
      assert(includesList === expectedIncludesList)
      assert(excludesList === expectedExcludesList)
      // SKIP-SCALATESTJS,NATIVE-START
      assert(concurrentList === expectedConcurrentList)
      // SKIP-SCALATESTJS,NATIVE-END
      assert(memberOfList === expectedMemberOfList)
      // SKIP-SCALATESTJS,NATIVE-START
      assert(beginsWithList === expectedBeginsWithList)
      assert(testNGList === expectedTestNGList)
      assert(spanScaleFactorList == expectedSpanScaleFactorList)
      assert(testSortingReporterTimeoutList == expectedTestSortingReporterTimeoutList)
      if (expectedSuffixes.isEmpty) {
        assert(suffixes.isEmpty)
      } else {
        assert(!suffixes.isEmpty)
        assert(suffixes.get.toString === expectedSuffixes.get.toString)
      }
      // SKIP-SCALATESTJS,NATIVE-END
      assert(seedList === expectedSeedList)
    }

    // SKIP-SCALATESTJS,NATIVE-START
    verify(
      Array("-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
        "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-f", "file.out", "-R"),
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-R"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne", "-s", "SuiteTwo"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-f", "file.out"),
      List("-s", "SuiteOne"),
      Nil,
      List("-j", "junitTest", "-j", "junitTest2"),
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")),
      List("-F", "200"),
      Nil,
      Nil
    )
    // Test -T option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
        "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-u", "directory/",
        "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne", "-Q", "-q", "foo",
        "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml", "-T", "20"),
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-u", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      Some(Pattern.compile(".*(Spec|Suite|foo)$")),
      Nil,
      List("-T", "20"),
      Nil
    )
    // Test -h option
    verify(
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
        "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-h", "directory/",
        "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
        "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-h", "directory/"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
        "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g", "-h", "directory/",
        "-Y", "mystyles.css", "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
        "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-h", "directory/", "-Y", "mystyles.css"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
        "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g",
        "-M", "target/doovers.txt",
        "-h", "directory/", "-Y", "mystyles.css", "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
        "-M", "target/doovers2.txt",
        "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-M", "target/doovers.txt", "-h", "directory/", "-Y", "mystyles.css", "-M", "target/doovers2.txt"),
      List("-s", "SuiteOne"),
      Nil,
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
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
      Array("-P", "-g", "-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188", "-R",
        "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\"", "-g",
        "-A", "target/doovers.txt",
        "-h", "directory/", "-Y", "mystyles.css", "-n", "One Two Three", "-l", "SlowTests", "-s", "SuiteOne",
        "-A", "target/doovers2.txt",
        "-m", "com.example.webapp", "-w", "com.example.root", "-b", "some/path/file.xml"),
      List("-R", "\"serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar\""),
      List("-g", "-g", "-h", "directory/", "-Y", "mystyles.css"),
      List("-s", "SuiteOne"),
      List("-A", "target/doovers.txt", "-A", "target/doovers2.txt"),
      Nil,
      List("-Dincredible=whatshername", "-Ddbname=testdb", "-Dserver=192.168.1.188"),
      List("-n", "One Two Three"),
      List("-l", "SlowTests"),
      List("-P"),
      List("-m", "com.example.webapp"),
      List("-w", "com.example.root"),
      List("-b", "some/path/file.xml"),
      None,
      Nil,
      Nil,
      Nil
    )
    // SKIP-SCALATESTJS,NATIVE-END

    // Test to parse -S
    verify(
      Array("-S", "123456789"),
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
      List("-S", "123456789")
    )
  }

  describe("parseCompoundArgIntoSet should") {
    it("work correctly") {
      assertResult(Set("Cat", "Dog")) {
        ArgsParser.parseCompoundArgIntoSet(List("-n", "Cat Dog"), "-n")
      }
    }
    it("merge overlapping values") {
      assertResult(Set("tag", "tag2", "tag3")) {
        ArgsParser.parseCompoundArgIntoSet(List("-l", "tag tag2", "-l", "tag2 tag3"),"-l")
      }
    }
  }

  describe("parseCompoundArgIntoList should parse") {
    it("single") {
      assertResult(List("tag")) {
        ArgsParser.parseCompoundArgIntoList(List("-l", "tag"),"-l")
      }
    }
    it("multi") {
      assertResult(List("tag","tag2")) {
        ArgsParser.parseCompoundArgIntoList(List("-l", "tag tag2"),"-l")
      }
    }
    it("different pairs") {
      assertResult(List("tag", "tag2", "tag3", "tag4")) {
        ArgsParser.parseCompoundArgIntoList(List("-l", "tag tag2", "-l", "tag3 tag4"),"-l")
      }
    }
    it("overlapping pairs") {
      assertResult(List("tag", "tag2", "tag2", "tag3")) {
        ArgsParser.parseCompoundArgIntoList(List("-l", "tag tag2", "-l", "tag2 tag3"),"-l")
      }
    }
  }

  describe("parseConfigSet should") {
    describe("handle string reporter options for reminders") {
      describe("with full stack traces (G)") {
        it("including canceled tests") {
          assertResult(Set(PresentReminderWithFullStackTraces)) {
            ArgsParser.parseConfigSet("-oG")
          }
          assertResult(Set(PresentReminderWithFullStackTraces)) {
            ArgsParser.parseConfigSet("-eG")
          }
          assertResult(Set(PresentReminderWithFullStackTraces)) {
            ArgsParser.parseConfigSet("-fG")
          }
        }
        it("excluding canceled tests") {
          assertResult(Set(PresentReminderWithFullStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-oGK")
          }
          assertResult(Set(PresentReminderWithFullStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-eGK")
          }
          assertResult(Set(PresentReminderWithFullStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-fGK")
          }
        }
      }
      describe("with short stack traces (T)") {
        it("including canceled tests") {
          assertResult(Set(PresentReminderWithShortStackTraces)) {
            ArgsParser.parseConfigSet("-oT")
          }
          assertResult(Set(PresentReminderWithShortStackTraces)) {
            ArgsParser.parseConfigSet("-eT")
          }
          assertResult(Set(PresentReminderWithShortStackTraces)) {
            ArgsParser.parseConfigSet("-fT")
          }
        }
        it("excluding canceled tests") {
          assertResult(Set(PresentReminderWithShortStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-oTK")
          }
          assertResult(Set(PresentReminderWithShortStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-eTK")
          }
          assertResult(Set(PresentReminderWithShortStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-fTK")
          }
        }
      }
      describe("with no stack traces (I)") {
        it("including canceled tests") {
          assertResult(Set(PresentReminderWithoutStackTraces)) {
            ArgsParser.parseConfigSet("-oI")
          }
          assertResult(Set(PresentReminderWithoutStackTraces)) {
            ArgsParser.parseConfigSet("-eI")
          }
          assertResult(Set(PresentReminderWithoutStackTraces)) {
            ArgsParser.parseConfigSet("-fI")
          }
        }
        it("excluding canceled tests") {
          assertResult(Set(PresentReminderWithoutStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-oIK")
          }
          assertResult(Set(PresentReminderWithoutStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-eIK")
          }
          assertResult(Set(PresentReminderWithoutStackTraces, PresentReminderWithoutCanceledTests)) {
            ArgsParser.parseConfigSet("-fIK")
          }
        }
      }
    }
  }

  it("parseConfigSet should work correctly") {

    intercept[NullArgumentException] {
      ArgsParser.parseConfigSet(null)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseConfigSet("-oYZTFUPBISARG-")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseConfigSet("-")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseConfigSet("")
    }

    assertResult(Set(FilterTestStarting)) {
      ArgsParser.parseConfigSet("-oN")
    }
    assertResult(Set(FilterTestSucceeded)) {
      ArgsParser.parseConfigSet("-oC")
    }
    assertResult(Set(FilterTestIgnored)) {
      ArgsParser.parseConfigSet("-oX")
    }
    assertResult(Set(FilterTestPending)) {
      ArgsParser.parseConfigSet("-oE")
    }
    assertResult(Set(FilterSuiteStarting)) {
      ArgsParser.parseConfigSet("-oH")
    }
    assertResult(Set(FilterSuiteCompleted)) {
      ArgsParser.parseConfigSet("-oL")
    }
    assertResult(Set(FilterInfoProvided)) {
      ArgsParser.parseConfigSet("-oO")
    }
    assertResult(Set(PresentWithoutColor)) {
      ArgsParser.parseConfigSet("-oW")
    }
    assertResult(Set(PresentAllDurations)) {
      ArgsParser.parseConfigSet("-oD")
    }
    assertResult(Set(PresentFullStackTraces)) {
      ArgsParser.parseConfigSet("-oF")
    }
    assertResult(Set(PresentFilePathname)) {
      ArgsParser.parseConfigSet("-oV")
    }
    assertResult(Set(PresentJson)) {
      ArgsParser.parseConfigSet("-oJ")
    }
    assertResult(Set[ReporterConfigParam]()) {
      ArgsParser.parseConfigSet("-f")
    }
    assertResult(Set[ReporterConfigParam]()) {
      ArgsParser.parseConfigSet("-u")
    }

    assertResult(Set(FilterInfoProvided, PresentWithoutColor)) {
      ArgsParser.parseConfigSet("-oOW")
    }
    assertResult(Set(FilterInfoProvided, PresentWithoutColor)) {
      ArgsParser.parseConfigSet("-oWO") // Just reverse the order of the params
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
      ArgsParser.parseConfigSet("-oNCXEHLOWDF")
    }
  }

  it("parseSuiteArgsIntoClassNameStrings should work correctly") {
    intercept[NullArgumentException] {
      ArgsParser.parseSuiteArgsIntoNameStrings(null, "-j")
    }
    intercept[NullArgumentException] {
      ArgsParser.parseSuiteArgsIntoNameStrings(List("-j", null, "-j"), "-j")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j"), "-j")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j", "-j"), "-j")
    }
    assertResult(List("SweetSuite", "OKSuite")) {
      ArgsParser.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j", "OKSuite"), "-j")
    }
    assertResult(List("SweetSuite", "OKSuite", "SomeSuite")) {
      ArgsParser.parseSuiteArgsIntoNameStrings(List("-j", "SweetSuite", "-j", "OKSuite", "-j", "SomeSuite"), "-j")
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  it("parseReporterArgsIntoSpecs should work correctly") {
    intercept[NullArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(null)
    }
    intercept[NullArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("Hello", null, "World"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("Hello", "-", "World"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("Hello", "", "World"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-g", "-l", "-o"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("Hello", " there", " world!"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-g", "-o", "-g", "-e"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-o", "-o", "-g", "-e"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-e", "-o", "-g", "-e"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-f")) // Can't have -f last, because need a file name
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-u")) // Can't have -u last, because need a directory name
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-C")) // Can't have -C last, because need a reporter class
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-k")) // Can't have -k last, because need a host and port
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-K")) // Can't have -k last, because need a host and port
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-k", "localhost")) // Can't have -k host last, because need a port
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-K", "localhost")) // Can't have -k host last, because need a port
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-k", "localhost", "abc")) // -k port number must be integer.
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-K", "localhost", "abc")) // -k port number must be integer.
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(Nil)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-C")) // Can't have -C last, because need a reporter class
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-h"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-M")) // needs a file name argument
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-h", "html", "-Y"))
    }
    assertResult(new ReporterConfigurations(Some(new GraphicReporterConfiguration(Set())), Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-g"))
    }
    assertResult(new ReporterConfigurations(Some(new GraphicReporterConfiguration(Set(FilterSuiteCompleted))), Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-gL"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */Some(new StandardOutReporterConfiguration(Set())), None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-o"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */Some(new StandardOutReporterConfiguration(Set(FilterTestSucceeded,FilterTestIgnored))), None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-oCX"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, Some(new StandardErrReporterConfiguration(Set())), Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-e"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, Some(new StandardErrReporterConfiguration(Set(PresentFullStackTraces))), Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-eF"))
    }
    assertResult(new ReporterConfigurations(None, List(new FileReporterConfiguration(Set(), "theFilename")), Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-f", "theFilename"))
    }
    assertResult(new ReporterConfigurations(None, Nil, List(new MemoryReporterConfiguration("theFilename")), Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-M", "theFilename"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, List(new JunitXmlReporterConfiguration(Set(), "target")), /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-u", "target"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, List(new JunitXmlReporterConfiguration(Set(), "target")), /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-uN", "target"))
    }
    assertResult(new ReporterConfigurations(None, List(new FileReporterConfiguration(Set(FilterTestStarting), "theFilename")), Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-fN", "theFilename"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, List(new CustomReporterConfiguration(Set(), "the.reporter.Class")), Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-C", "the.reporter.Class"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, List(new CustomReporterConfiguration(Set(FilterTestPending), "the.reporter.Class")), Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-CE", "the.reporter.Class"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, List(new XmlSocketReporterConfiguration("localhost", 8888)), Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-k", "localhost", "8888"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, List(new XmlSocketReporterConfiguration("localhost", 8888), new XmlSocketReporterConfiguration("another host", 1234)), Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-k", "localhost", "8888", "-k", "another host", "1234"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, List(new SocketReporterConfiguration("localhost", 8888)))) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-K", "localhost", "8888"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, Nil, Nil, Nil, List(new SocketReporterConfiguration("localhost", 8888), new SocketReporterConfiguration("another host", 1234)))) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-K", "localhost", "8888", "-K", "another host", "1234"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, List(new HtmlReporterConfiguration(Set(), "html", None)), Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-h", "html"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, List(new HtmlReporterConfiguration(Set(), "html", Some(new File("MyStyle.css").toURI.toURL))), Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-h", "html", "-Y", "MyStyle.css"))
    }
    assertResult(new ReporterConfigurations(None, Nil, Nil, Nil, /*Nil, Nil, */None, None, List(new HtmlReporterConfiguration(Set(), "htmldir", None), new HtmlReporterConfiguration(Set(), "html", Some(new File("MyStyle.css").toURI.toURL))), Nil, Nil, Nil)) {
      ArgsParser.parseReporterArgsIntoConfigurations(List("-h", "htmldir", "-h", "html", "-Y", "MyStyle.css"))
    }
  }

  it("parseRunpathArgIntoList should work correctly") {
    intercept[NullArgumentException] {
      ArgsParser.parseRunpathArgIntoList(null)
    }
    intercept[NullArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List("-R", null))
    }
    intercept[NullArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List(null, "serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List("-R"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List("-R", "bla", "bla"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List("-pX", "bla"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List("-R", "  "))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseRunpathArgIntoList(List("-R", "\t"))
    }
    assertResult(List("bla")) {
      ArgsParser.parseRunpathArgIntoList(List("-R", "bla"))
    }
    assertResult(List("bla", "bla", "bla")) {
      ArgsParser.parseRunpathArgIntoList(List("-R", "bla bla bla"))
    }
    assertResult(List("serviceuitest-1.1beta4.jar", "myjini", "http://myhost:9998/myfile.jar")) {
      ArgsParser.parseRunpathArgIntoList(List("-R", "serviceuitest-1.1beta4.jar myjini http://myhost:9998/myfile.jar"))
    }
    assertResult(List("\\", "c:\\", "c:\\Program Files", "c:\\Documents and Settings", "\\", "myjini")) {
      ArgsParser.parseRunpathArgIntoList(List("-R", """\ c:\ c:\Program\ Files c:\Documents\ and\ Settings \ myjini"""))
    }
  }
  // SKIP-SCALATESTJS,NATIVE-END

  it("parsePropertiesArgsIntoMap should work correctly") {
    intercept[NullArgumentException] {
      ArgsParser.parsePropertiesArgsIntoMap(null)
    }
    intercept[NullArgumentException] {
      ArgsParser.parsePropertiesArgsIntoMap(List("-Da=b", null))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parsePropertiesArgsIntoMap(List("-Dab")) // = sign missing
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parsePropertiesArgsIntoMap(List("ab")) // needs to start with -D
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parsePropertiesArgsIntoMap(List("-D=ab")) // no key
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parsePropertiesArgsIntoMap(List("-Dab=")) // no value
    }
    assertResult(Map("a" -> "b", "cat" -> "dog", "Glorp" -> "Glib")) {
      ArgsParser.parsePropertiesArgsIntoMap(List("-Da=b", "-Dcat=dog", "-DGlorp=Glib"))
    }
  }

  it("deprecatedCheckArgsForValidity should work correctly") {
    intercept[IllegalArgumentException] {
      ArgsParser.checkArgsForValidity(null)
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-C", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-z", "testWildcard", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-k", "hostname", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-K", "hostname", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-C", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite", "-P"))
    }
  }

  // SKIP-SCALATESTJS,NATIVE-START
  it("parseSuiteArgs should work correctly") {
    intercept[NullArgumentException] {
      ArgsParser.parseSuiteArgs(null)
    }
    intercept[NullArgumentException] {
      ArgsParser.parseSuiteArgs(List("-s", null, "-s", "suite2"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-s", "-s"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-s", "suite1", "-s"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-sG", "suite1"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-t", "-t", "testname"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-t", "testname", "-t"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-t", "testname", "-z"))
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseSuiteArgs(List("-z"))
    }
    intercept[IllegalArgumentException] {
      // -i without -s should not be supported, as for example current command is having -s -i, there's no way to tell the next -i should be a -i without -s.
      // -i should only be used together with -s to select nested suite.
      ArgsParser.parseSuiteArgs(List("-i", "suite1"))
    }
    intercept[IllegalArgumentException] {
      // -sX -t should not be supported, as -s -t should be used to select a specific test.
      ArgsParser.parseSuiteArgs(List("-sX", "suite1", "-t", "test1"))
    }
    intercept[IllegalArgumentException] {
      // -iX should not be supported, as a nested suite's nested suites should not be included, if it is included, we have to figure out the way to specify if
      // nested suite's nested suite's nested suites (and endless down the tree) should be implemented.
      ArgsParser.parseSuiteArgs(List("-s", "suite1", "-iX", "nested1"))
    }

    val (suites1, tests1) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-s", "suite2"))
    assert(tests1.length === 0)
    assert(suites1.length === 2)
    assert(suites1(0).className === "suite1")
    assert(suites1(0).testNames.length === 0)
    assert(suites1(1).className === "suite2")
    assert(suites1(1).testNames.length === 0)

    val (suites2, tests2) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-t", "test1", "-t", "test2", "-s", "suite2"))
    assert(tests2.length === 0)
    assert(suites2.length === 2)
    assert(suites2(0).className === "suite1")
    assert(suites2(0).testNames.length === 2)
    assert(suites2(0).testNames(0) === "test1")
    assert(suites2(0).testNames(1) === "test2")
    assert(suites2(1).className === "suite2")
    assert(suites2(1).testNames.length === 0)

    val (suites3, tests3) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-i", "nested1"))
    assert(tests3.length === 0)
    assert(suites3.length === 1)
    assert(suites3(0).className === "suite1")
    assert(suites3(0).testNames.length === 0)
    assert(suites3(0).nestedSuites.length === 1)
    assert(suites3(0).nestedSuites(0).suiteId === "nested1")
    assert(suites3(0).nestedSuites(0).testNames.length === 0)

    val (suites4, tests4) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-i", "nested1", "-t", "test1", "-t", "test2"))
    assert(tests4.length === 0)
    assert(suites4.length === 1)
    assert(suites4(0).className === "suite1")
    assert(suites4(0).testNames.length === 0)
    assert(suites4(0).nestedSuites.length === 1)
    assert(suites4(0).nestedSuites(0).suiteId === "nested1")
    assert(suites4(0).nestedSuites(0).testNames.length === 2)
    assert(suites4(0).nestedSuites(0).testNames(0) === "test1")
    assert(suites4(0).nestedSuites(0).testNames(1) === "test2")

    val (suites5, tests5) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-z", "test1", "-z", "test2", "-s", "suite2"))
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

    val (suites6, tests6) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-t", "test1", "-z", "test2", "-s", "suite2"))
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

    val (suites7, tests7) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-i", "nested1", "-z", "test1", "-z", "test2"))
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

    val (suites8, tests8) = ArgsParser.parseSuiteArgs(List("-s", "suite1", "-i", "nested1", "-t", "test1", "-z", "test2"))
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
    val (suites9, tests9) = ArgsParser.parseSuiteArgs(List("-t", "test1"))
    assert(suites9.isEmpty)
    assert(tests9.length === 1)
    assert(tests9(0).spec === "test1")
    assert(tests9(0).isSubstring === false)

    //
    // A list starting with a -z should produce a corresponding test spec.
    //
    val (suites11, tests11) = ArgsParser.parseSuiteArgs(List("-z", "substr1"))
    assert(suites11.isEmpty)
    assert(tests11.length === 1)
    assert(tests11(0).spec === "substr1")
    assert(tests11(0).isSubstring === true)

    //
    // A few -t's and -z's.
    //
    val (suites12, tests12) =
      ArgsParser.parseSuiteArgs(List("-t", "test1",
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
  // SKIP-SCALATESTJS,NATIVE-END

  it("checkArgsForValidity should work correctly") {
    intercept[IllegalArgumentException] {
      ArgsParser.checkArgsForValidity(null)
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-R", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-z", "test name wildcard", "-g", "-eFBA", "-s", "MySuite"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-Ddbname=testdb", "-Dserver=192.168.1.188", "-R", "serviceuitest-1.1beta4.jar", "-g", "-eFBA", "-s", "MySuite", "-P"))
    }
  }
  it("checkArgsForValidity should recognize -W (for slowpoke detector)") {
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-W", "60", "60"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-W", "60", "60", "-h", "htmldir"))
    }
  }

  it("checkArgsForValidity should recognize -S") {
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-S", "1234"))
    }
    assertResult(None) {
      ArgsParser.checkArgsForValidity(Array("-S", "1234", "-h", "htmldir"))
    }
  }

  it("parseDoubleArgument should work correctly") {
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-a", "123"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-F", "abc"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-F"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-F", "123", "-F"), "-F", 1.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-F", "123", "-F", "456"), "-F", 1.0)
    }
    val spanScaleFactor = ArgsParser.parseDoubleArgument(List("-F", "888"), "-F", 1.0)
    assert(spanScaleFactor === 888)

    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-a", "123"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-T", "abc"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-T"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-T", "123", "-T"), "-T", 15.0)
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseDoubleArgument(List("-T", "123", "-T", "456"), "-T", 15.0)
    }
    val testSortingReporterTimeout = ArgsParser.parseDoubleArgument(List("-T", "888"), "-T", 15.0)
    assert(spanScaleFactor === 888)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  it("parseConcurrentConfig should work correctly") {
    val emptyConcurrentConfig = ArgsParser.parseConcurrentConfig(List.empty)
    assert(emptyConcurrentConfig.numThreads === 0)
    assert(emptyConcurrentConfig.enableSuiteSortingReporter === false)

    val singleDashP = ArgsParser.parseConcurrentConfig(List("-P"))
    assert(singleDashP.numThreads === 0)
    assert(singleDashP.enableSuiteSortingReporter === false)

    val multiDashP = ArgsParser.parseConcurrentConfig(List("-P", "-P"))
    assert(multiDashP.numThreads === 0)
    assert(multiDashP.enableSuiteSortingReporter === false)

    intercept[IllegalArgumentException] {
      ArgsParser.parseConcurrentConfig(List("-P0"))
    }

    val singleDashPThreadNum = ArgsParser.parseConcurrentConfig(List("-P10"))
    assert(singleDashPThreadNum.numThreads === 10)
    assert(singleDashPThreadNum.enableSuiteSortingReporter === false)

    intercept[IllegalArgumentException] {
      ArgsParser.parseConcurrentConfig(List("-P-10"))
    }

    val multiDashPThreadNum = ArgsParser.parseConcurrentConfig(List("-P10", "-P5"))
    assert(multiDashPThreadNum.numThreads === 10)
    assert(multiDashPThreadNum.enableSuiteSortingReporter === false)

    val singleDashPS = ArgsParser.parseConcurrentConfig(List("-PS"))
    assert(singleDashPS.numThreads === 0)
    assert(singleDashPS.enableSuiteSortingReporter === true)

    val multiDashPS = ArgsParser.parseConcurrentConfig(List("-P", "-PS"))
    assert(multiDashPS.numThreads === 0)
    assert(multiDashPS.enableSuiteSortingReporter === true)

    val singleDashPSThreadNum = ArgsParser.parseConcurrentConfig(List("-PS8"))
    assert(singleDashPSThreadNum.numThreads === 8)
    assert(singleDashPSThreadNum.enableSuiteSortingReporter === true)

    intercept[IllegalArgumentException] {
      ArgsParser.parseConcurrentConfig(List("-PS-8"))
    }

    val multipDashPSThreadNum = ArgsParser.parseConcurrentConfig(List("-PS8", "-P10"))
    assert(multipDashPSThreadNum.numThreads === 8)
    assert(multipDashPSThreadNum.enableSuiteSortingReporter === true)
  }
  // SKIP-SCALATESTJS,NATIVE-END

  it("parseLongArgument should work correctly") {
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-a", "123"), "-S")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-S", "abc"), "-S")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-S"), "-S")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-S", "123", "-S"), "-S")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-S", "123", "-S", "456"), "-S")
    }
    val spanScaleFactor = ArgsParser.parseLongArgument(List("-S", "888"), "-S")
    assert(spanScaleFactor === Some(888))

    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-a", "123"), "-S")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-T", "abc"), "-S")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-T"), "-T")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-T", "123", "-T"), "-T")
    }
    intercept[IllegalArgumentException] {
      ArgsParser.parseLongArgument(List("-T", "123", "-T", "456"), "-T")
    }
    val testSortingReporterTimeout = ArgsParser.parseLongArgument(List("-T", "888"), "-T")
    assert(testSortingReporterTimeout === Some(888))
  }

  it("""parseArgs should disallow -t"something""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-t\"something", " to test\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -t\"something")
  }

  it("""parseArgs should disallow -z"something""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-z\"something", " to test\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -z\"something")
  }

  it("""parseArgs should disallow -M"aFile""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-M\"aFile", ".txt\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -M\"aFile")
  }

  it("""parseArgs should disallow -u"aDirectory""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-u\"aDirectory", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -u\"aDirectory")
  }

  it("""parseArgs should disallow -n"tag""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-n\"tag", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -n\"tag")
  }

  it("""parseArgs should disallow -l"tag""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-l\"tag", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -l\"tag")
  }

  it("""parseArgs should disallow -s"suite""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-s\"suite", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -s\"suite")
  }

  it("""parseArgs should disallow -A"aFile""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-A\"aFile", ".txt\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -A\"aFile")
  }

  it("""parseArgs should disallow -i"nested""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-i\"nested", "suite", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -i\"nested")
  }

  it("""parseArgs should disallow -j"junit""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-j\"junit", "class", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -j\"junit")
  }

  it("""parseArgs should disallow -m"package""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-m\"package", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -m\"package")
  }

  it("""parseArgs should disallow -w"package""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-w\"package", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -w\"package")
  }

  it("""parseArgs should disallow -b"aFile""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-b\"aFile", ".txt\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -b\"aFile")
  }

  it("""parseArgs should disallow -q"suffix""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-q\"suffix", "name\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -q\"suffix")
  }

  it("""parseArgs should disallow -Q"wrong""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-Q\"wrong", "thing\""))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -Q\"wrong")
  }

  it("""parseArgs should disallow -k"super""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-k\"super", "host\"", "9000"))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -k\"super")
  }

  it("""parseArgs should disallow -K"super""") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-K\"super", "host\"", "9000"))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -K\"super")
  }

  it("parseArgs should disallow -F2") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-F2"))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -F2")
  }

  it("parseArgs should disallow -T20") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-T20"))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -T20")
  }

  it("parseArgs should disallow -W1") {
    val e = intercept[IllegalArgumentException] {
      ArgsParser.parseArgs(Array("-W1", "2"))
    }
    assert(e.getMessage == "Argument unrecognized by ScalaTest's Runner: -W1")
  }

}
