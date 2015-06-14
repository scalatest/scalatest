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

import java.io.File
import java.net.URL
import java.util.regex.Pattern

import org.scalatest.{ConfigMap, Resources}

import scala.collection.mutable.ListBuffer

private[tools] object ArgsParser {

  // Returns an Option[String]. Some is an error message. None means no error.
  private[scalatest] def checkArgsForValidity(args: Array[String]) = {

    if (args == null)
      throw new IllegalArgumentException("args was null.")

    val lb = new ListBuffer[String]
    val it = args.iterator.buffered
    while (it.hasNext) {
      val s = it.next
      if (s.startsWith("-r"))
        throw new IllegalArgumentException(
          "ERROR: -r has been deprecated for a very long time and is no "+
            "longer supported, to prepare for reusing it for a different "+
            "purpose in the near future. Please change all uses of -r to -C.")
      else if (s.startsWith("-c"))
        throw new IllegalArgumentException(
          "ERROR: -c has been deprecated for a very long time and is no "+
            "longer supported, to prepare for reusing it for a different "+
            "purpose in the near future. Please change all uses of -c to -P.")
      else if (s.startsWith("-p"))
        throw new IllegalArgumentException(
          "ERROR: -p has been deprecated for a very long time and is no "+
            "longer supported, to prepare for reusing it for a different "+
            "purpose in the near future. Please change all uses of -p to -R.")
      else if (
        s.startsWith("-R") ||
          s.startsWith("-f") ||
          s.startsWith("-M") ||
          s.startsWith("-A") ||
          s.startsWith("-u") ||
          //s.startsWith("-d") ||
          //s.startsWith("-a") ||
          s.startsWith("-C") ||
          s.startsWith("-n") ||
          /* s.startsWith("-x") || */
          s.startsWith("-l") ||
          s.startsWith("-s") ||
          s.startsWith("-i") ||
          s.startsWith("-j") ||
          s.startsWith("-m") ||
          s.startsWith("-w") ||
          s.startsWith("-b") ||
          s.startsWith("-y") ||
          s.startsWith("-t") ||
          s.startsWith("-z") ||
          s.startsWith("-q") ||
          s.startsWith("-Q") ||
          s.startsWith("-F") ||
          s.startsWith("-T")
      ) {
        if (it.hasNext)
          it.next
      }
      else if (s.startsWith("-k") || s.startsWith("-K") || s.startsWith("-W")) {
        if (it.hasNext)
          it.next
        if (it.hasNext)
          it.next
      }
      else if (s.startsWith("-h")) {
        if (it.hasNext)
          it.next
        if (it.hasNext && it.head == "-Y") {
          it.next
          it.next
        }
      }
      else if (!s.startsWith("-D") && !s.startsWith("-g") && !s.startsWith("-o") && !s.startsWith("-e") && !s.startsWith("-P")) {
        lb += s
      }
    }
    val argsList = lb.toList
    if (argsList.length != 0) {
      val argWord = "Argument" + (if (argsList.size == 1) "" else "s")
      Some(argWord + " unrecognized by ScalaTest's Runner: " + argsList.mkString("", ", ", "."))
    }
    else
      None
  }

  // SKIP-SCALATESTJS-START
  //
  // Examines concurrent option arg to see if it contains an optional numeric
  // value representing the number of threads to use, e.g. -P10 for 10 threads.
  //
  // It also examines for the 'S' argument, e.g. -PS or -PS10, which when specified,
  // will enable the SuiteSortingReporter.
  //
  // It's possible for user to specify the -P option multiple times on the
  // command line, although it isn't particularly useful.  This method scans
  // through multiples until it finds one with a number appended and uses
  // that.  If none have a number it just returns 0.  If anyone of the -P comes
  // with the 'S' option, SuiteSortingReporter will be enabled.
  //
  private[scalatest] def parseConcurrentConfig(concurrentList: List[String]): ConcurrentConfig = {
    val threadOpt = concurrentList.find(s => s.matches("-P\\d+") || s.matches("-PS\\d+"))
    val numThreads = threadOpt match {
      case Some(arg) =>
        if (arg.startsWith("-PS"))
          arg.substring(3).toInt
        else
          arg.substring(2).toInt
      case None      => 0
    }

    val enableSuiteSortingReporter = concurrentList.find(_.startsWith("-PS")).isDefined

    ConcurrentConfig(numThreads, enableSuiteSortingReporter)
  }

  // Command line args are in seconds. Here I convert them already to millis, which is needed by DispatchReporter
  private[scalatest] def parseSlowpokeConfig(slowpokeArgs: List[String]): Option[SlowpokeConfig] =
    if (!slowpokeArgs.isEmpty)
      Some(SlowpokeConfig(slowpokeArgs(1).toLong * 1000, slowpokeArgs(2).toLong * 1000))
    else None
  // SKIP-SCALATESTJS-END

  //
  // Generates a Pattern based on suffixes passed in by user.  Pattern
  // matches class names that end with one of the specified suffixes.
  //
  def genSuffixesPattern(suffixesList: List[String]): Option[Pattern] = {
    if (suffixesList.isEmpty)
      None
    else
      Some(Pattern.compile(".*(" + suffixesList.mkString("|") + ")$"))
  }

  private[scalatest] def parseArgs(args: Array[String]): ParsedArgs = {

    // SKIP-SCALATESTJS-START
    val runpath = new ListBuffer[String]()
    // SKIP-SCALATESTJS-END
    val reporters = new ListBuffer[String]()
    // SKIP-SCALATESTJS-START
    val suites = new ListBuffer[String]()
    val tryAgains = new ListBuffer[String]()
    val junits = new ListBuffer[String]()
    val props = new ListBuffer[String]()
    // SKIP-SCALATESTJS-END
    val includes = new ListBuffer[String]()
    val excludes = new ListBuffer[String]()
    // SKIP-SCALATESTJS-START
    val concurrent = new ListBuffer[String]()
    // SKIP-SCALATESTJS-END
    val membersOnly = new ListBuffer[String]()
    val wildcard = new ListBuffer[String]()
    // SKIP-SCALATESTJS-START
    val testNGXMLFiles = new ListBuffer[String]()
    // SKIP-SCALATESTJS-END
    val suffixes = new ListBuffer[String]()
    // SKIP-SCALATESTJS-START
    val chosenStyles = new ListBuffer[String]()
    val spanScaleFactor = new ListBuffer[String]()
    val testSortingReporterTimeout = new ListBuffer[String]()
    val slowpoke = new ListBuffer[String]()
    // SKIP-SCALATESTJS-END

    val it = args.iterator.buffered
    while (it.hasNext) {

      val s = it.next

      if (s.startsWith("-D")) {
        // SKIP-SCALATESTJS-START
        props += s
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s.startsWith("-R")) {
        // SKIP-SCALATESTJS-START
        runpath += s
        if (it.hasNext)
          runpath += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s.startsWith("-g")) {
        // SKIP-SCALATESTJS-START
        reporters += s
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s.startsWith("-o")) {
        reporters += s
      }
      else if (s.startsWith("-e")) {
        // SKIP-SCALATESTJS-START
        reporters += s
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s.startsWith("-f")) {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext)
          reporters += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-M") {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext)
          reporters += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-u") {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext)
          reporters += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      /*else if (s.startsWith("-d")) {
        reporters += s
        if (it.hasNext)
          reporters += it.next
      }
      else if (s.startsWith("-a")) {
        reporters += s
        if (it.hasNext)
          reporters += it.next
      }
      else if (s.startsWith("-x")) {
        reporters += s
        if (it.hasNext)
          reporters += it.next
      }*/
      else if (s.startsWith("-h")) {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext)
          reporters += it.next
        if (it.hasNext && it.head == "-Y") {
          reporters += it.next
          if (it.hasNext)
            reporters += it.next
        }
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-n") {
        includes += s
        if (it.hasNext)
          includes += it.next
      }
      else if (s == "-l") {
        excludes += s
        if (it.hasNext)
          excludes += it.next
      }
      else if (s.startsWith("-C")) {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext)
          reporters += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-s") {
        // SKIP-SCALATESTJS-START
        suites += s
        if (it.hasNext)
          suites += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-A") {
        // SKIP-SCALATESTJS-START
        tryAgains += s
        if (it.hasNext)
          tryAgains += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-i") {
        // SKIP-SCALATESTJS-START
        suites += s
        if (it.hasNext)
          suites += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-t") {
        // SKIP-SCALATESTJS-START
        suites += s
        if (it.hasNext)
          suites += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-z") {
        // SKIP-SCALATESTJS-START
        suites += s
        if (it.hasNext)
          suites += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-j") {
        // SKIP-SCALATESTJS-START
        junits += s
        if (it.hasNext)
          junits += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-m") {

        membersOnly += s
        if (it.hasNext)
          membersOnly += it.next
      }
      else if (s == "-w") {

        wildcard += s
        if (it.hasNext)
          wildcard += it.next
      }
      else if (s.startsWith("-P")) {
        // SKIP-SCALATESTJS-START
        concurrent += s
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-b") {
        // SKIP-SCALATESTJS-START
        testNGXMLFiles += s
        if (it.hasNext)
          testNGXMLFiles += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-q") {
        if (it.hasNext)
          suffixes += it.next()
      }
      else if (s == "-Q") {
        suffixes += "Spec|Suite"
      }
      else if (s == "-k") {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext && !it.head.startsWith("-")) // for host
          reporters += it.next
        if (it.hasNext && !it.head.startsWith("-")) // for port
          reporters += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-K") {
        // SKIP-SCALATESTJS-START
        reporters += s
        if (it.hasNext && !it.head.startsWith("-")) // for host
          reporters += it.next
        if (it.hasNext && !it.head.startsWith("-")) // for port
          reporters += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-y") {
        // SKIP-SCALATESTJS-START
        chosenStyles += s
        if (it.hasNext)
          chosenStyles += it.next()
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-F") {
        // SKIP-SCALATESTJS-START
        spanScaleFactor += s
        if (it.hasNext)
          spanScaleFactor += it.next()
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-T") {
        // SKIP-SCALATESTJS-START
        testSortingReporterTimeout += s
        if (it.hasNext)
          testSortingReporterTimeout += it.next
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else if (s == "-W") {
        // SKIP-SCALATESTJS-START
        def isParsableAsInt(s: String): Boolean =
          try { s.toInt; true } catch { case _: NumberFormatException => false }
        slowpoke += s
        if (it.hasNext) {
          if (isParsableAsInt(it.head)) slowpoke += it.next()
          else throw new IllegalArgumentException("-W must be followed by a valid integer specifying the delay, but got: " + it.head)
        }
        else throw new IllegalArgumentException("-W must be followed by a valid integer specifying the delay")
        if (it.hasNext) {
          if (isParsableAsInt(it.head)) slowpoke += it.next()
          else throw new IllegalArgumentException("-W must be followed by two valid integers, the second specifying the period, but got: " + it.head)
        }
        else throw new IllegalArgumentException("-W must be followed by two valid integers, the second specifying the period")
        // SKIP-SCALATESTJS-END
        //SCALATESTJS-ONLY throw new IllegalArgumentException("Argument not supported by ScalaTest-js: " + s)
      }
      else {
        throw new IllegalArgumentException("Argument unrecognized by ScalaTest's Runner: " + s)
      }
    }

    ParsedArgs(
      // SKIP-SCALATESTJS-START
      runpath.toList,
      // SKIP-SCALATESTJS-END
      reporters.toList,
      // SKIP-SCALATESTJS-START
      suites.toList,
      tryAgains.toList,
      junits.toList,
      props.toList,
      // SKIP-SCALATESTJS-END
      includes.toList,
      excludes.toList,
      // SKIP-SCALATESTJS-START
      concurrent.toList,
      // SKIP-SCALATESTJS-END
      membersOnly.toList,
      wildcard.toList,
      // SKIP-SCALATESTJS-START
      testNGXMLFiles.toList,
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY genSuffixesPattern(suffixes.toList)
      // SKIP-SCALATESTJS-START
      genSuffixesPattern(suffixes.toList),
      chosenStyles.toList,
      spanScaleFactor.toList,
      testSortingReporterTimeout.toList,
      slowpoke.toList
      // SKIP-SCALATESTJS-END
    )
  }

  // Used to parse -j, -m, and -w args, one of which will be passed as a String as dashArg
  def parseSuiteArgsIntoNameStrings(args: List[String], dashArg: String) = {

    if (args == null)
      throw new NullPointerException("args was null")

    if (args.exists(_ == null))
      throw new NullPointerException("an arg String was null")

    if (dashArg != "-j" && dashArg != "-w" && dashArg != "-m" && dashArg != "-b")
      throw new IllegalArgumentException("dashArg invalid: " + dashArg)
    /*
    <<<<<<< .working TODOCS: Is the above the correct way to merge these?
        if (dashArg != "-j" && dashArg != "-w" && dashArg != "-m" && dashArg != "-b")
          throw new IllegalArgumentException("dashArg invalid: " + dashArg)
    =======
        if (dashArg != "-j" && dashArg != "-s" && dashArg != "-w" && dashArg != "-m" && dashArg != "-b")
          throw new NullPointerException("dashArg invalid: " + dashArg)
    >>>>>>> .merge-right.r3653
    */

    val lb = new ListBuffer[String]
    val it = args.iterator
    while (it.hasNext) {
      val dashS = it.next
      if (dashS != dashArg)
        throw new IllegalArgumentException("Every other element, starting with the first, must be " + dashArg)
      if (it.hasNext) {
        val suiteName = it.next
        if (!suiteName.startsWith("-"))
          lb += suiteName
        else
          throw new IllegalArgumentException("Expecting a Suite class name or package name to follow " + dashArg + ", but got: " + suiteName)
      }
      else
        throw new IllegalArgumentException("Last element must be a Suite class name or package name, not a " + dashArg + ".")
    }
    lb.toList
  }

  /**
   * Returns a possibly empty ConfigSet containing configuration
   * objects specified in the passed reporterArg. Configuration
   * options are specified immediately following
   * the reporter option, as in:
   *
   * -oFA
   *
   * If no configuration options are specified, this method returns an
   * empty ConfigSet. This method never returns null.
   */
  def parseConfigSet(reporterArg: String): Set[ReporterConfigParam] = {

    if (reporterArg == null)
      throw new NullPointerException("reporterArg was null")

    if (reporterArg.length < 2)
      throw new IllegalArgumentException("reporterArg < 2")

    // The reporterArg passed includes the initial -, as in "-oFI",
    // so the first config param will be at index 2
    val configString = reporterArg.substring(2)
    val it = configString.iterator
    var set = Set[ReporterConfigParam]()
    while (it.hasNext)
      it.next match {
        case 'Y' =>  throw new IllegalArgumentException("Use of Y was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        case 'Z' => throw new IllegalArgumentException("Use of Z was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        //case 'P' =>throw new IllegalArgumentException("Use of P was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        case 'B' =>throw new IllegalArgumentException("Use of B was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        // case 'S' => // Use for Short Stack Traces
        case 'A' =>throw new IllegalArgumentException("Use of A was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        //case 'R' =>throw new IllegalArgumentException("Use of R was deprecated in ScalaTest 1.0 and removed in 1.5. Please check the Scaladoc documentation of org.scalatest.Runner for information on valid Reporter config parameters.")
        case 'I' => set += PresentReminderWithoutStackTraces
        case 'T' => set += PresentReminderWithShortStackTraces
        case 'G' => set += PresentReminderWithFullStackTraces
        case 'K' => set += PresentReminderWithoutCanceledTests
        case 'N' => set += FilterTestStarting
        case 'C' => set += FilterTestSucceeded
        case 'X' => set += FilterTestIgnored
        case 'E' => set += FilterTestPending
        case 'H' => set += FilterSuiteStarting
        case 'L' => set += FilterSuiteCompleted
        case 'O' => set += FilterInfoProvided
        case 'P' => set += FilterScopeOpened
        case 'Q' => set += FilterScopeClosed
        case 'R' => set += FilterScopePending
        case 'M' => set += FilterMarkupProvided
        case 'W' => set += PresentWithoutColor
        case 'F' => set += PresentFullStackTraces
        case 'S' => set += PresentShortStackTraces
        case 'D' => set += PresentAllDurations
        case 'U' => set += PresentUnformatted
        case c: Char => {

          // this should be moved to the checker, and just throw an exception here with a debug message. Or allow a MatchError.
          val msg1 = Resources.invalidConfigOption(String.valueOf(c)) + '\n'
          val msg2 =  Resources.probarg(reporterArg) + '\n'

          throw new IllegalArgumentException(msg1 + msg2)
        }
      }
    set
  }

  // SKIP-SCALATESTJS-START
  def parseReporterArgsIntoConfigurations(args: List[String]) = {
    //
    // Checks to see if any args are smaller than two characters in length.
    // Allows a one-character arg if it's a directory-name parameter, to
    // permit use of "." for example.  Allows a one-character arg if it's
    // a number.
    //
    def argTooShort(args: List[String]): Boolean = {
      args match {
        case Nil => false

        case "-u" :: directory :: list => argTooShort(list)
        //case "-d" :: directory :: list => argTooShort(list)
        //case "-a" :: number    :: list => argTooShort(list)
        //case "-x" :: directory :: list => argTooShort(list)

        case x :: list =>
          if (x.length < 2) true
          else              argTooShort(list)
      }
    }

    if (args == null)
      throw new NullPointerException("args was null")

    if (args.exists(_ == null))
      throw new NullPointerException("an arg String was null")

    if (argTooShort(args)) // TODO: check and print out a user friendly message for this; maybe use an accumulating Or?
      throw new IllegalArgumentException("an arg String was less than 2 in length: " + args)

    for (dashX <- List("-g", "-o", "-e")) {
      if (args.toList.count(_.startsWith(dashX)) > 1) // TODO: also check and print a user friendly message for this
        throw new IllegalArgumentException("Only one " + dashX + " allowed")
    }

    // TODO: also check and print a user friendly message for this
    // again here, i had to skip some things, so I had to use an iterator.
    val it = args.iterator.buffered
    while (it.hasNext)
      it.next.take(2).toString match {
        case "-g" =>
        case "-o" =>
        case "-e" =>
        case "-f" =>
          if (it.hasNext)
            it.next // scroll past the filename
          else
            throw new IllegalArgumentException("-f needs to be followed by a file name arg: ")
        case "-M" =>
          if (it.hasNext)
            it.next // scroll past the filename
          else
            throw new IllegalArgumentException("-M needs to be followed by a file name arg: ")
        case "-u" =>
          if (it.hasNext) {
            val directoryName = it.next
            val directory = new File(directoryName)
            if (!directory.isDirectory) {
              try {
                directory.mkdirs()
                if (!directory.exists)
                  throw new IllegalArgumentException("Unable to create directory: " + directory.getAbsolutePath)
              }
              catch {
                case se: SecurityException =>
                  throw new IllegalArgumentException("Unable to create directory: " + directory.getAbsolutePath)
              }
            }
            else if (directory.isFile)
              throw new IllegalArgumentException(directory.getAbsolutePath + " is a file, directory expected.")
          }
          else {
            throw new IllegalArgumentException("-u needs to be followed by a directory name arg: ")
          }
        /*case "-d" =>
          if (it.hasNext) {
            val directory = it.next
            if (!(new File(directory).isDirectory))
              throw new IllegalArgumentException(
                "arg for -d option is not a directory [" + directory + "]")
            else {}
          }
          else {
            throw new IllegalArgumentException("-d needs to be followed by a directory name arg: ")
          }
        case "-a" =>
          if (it.hasNext) {
            def isValidInt(text: String): Boolean =
              try { text.toInt; true } catch { case _: Throwable => false }

            val number = it.next
            if (!(isValidInt(number)))
              throw new IllegalArgumentException(
                "arg for -a option is not a number [" + number + "]")
            else {}
          }
          else {
            throw new IllegalArgumentException("-a needs to be followed by a number arg: ")
          }
        case "-x" =>
          if (it.hasNext) {
            val directory = it.next
            if (!(new File(directory).isDirectory))
              throw new IllegalArgumentException(
                "arg for -x option is not a directory [" + directory + "]")
            else {}
          }
          else {
            throw new IllegalArgumentException("-x needs to be followed by a directory name arg: ")
          }*/
        case "-h" =>
          if (it.hasNext) {
            it.next // scroll past the filename
            if (it.hasNext && it.head == "-Y") {
              it.next // scroll past the -Y
              if (it.hasNext)
                it.next // scroll past the css file name
              else
                throw new IllegalArgumentException("-Y needs to be followed by a file name arg: ")
            }
          }
          else
            throw new IllegalArgumentException("-h needs to be followed by a directory name arg: ")
        case "-C" =>
          if (it.hasNext)
            it.next // scroll past the reporter class
          else
            throw new IllegalArgumentException("-C needs to be followed by a reporter class name arg: ")
        case "-k" =>
          if (it.hasNext && !it.head.startsWith("-")) {
            it.next
            if (it.hasNext && !it.head.startsWith("-")) {
              try {
                it.next.toInt
              }
              catch {
                case _: Throwable =>
                  throw new IllegalArgumentException("port number must be an integer")
              }
            }
            else
              throw new IllegalArgumentException("-k needs to be followed by a host name and port number" )
          }
          else
            throw new IllegalArgumentException("-k needs to be followed by a host name and port number" )
        case "-K" =>
          if (it.hasNext && !it.head.startsWith("-")) {
            it.next
            if (it.hasNext && !it.head.startsWith("-")) {
              try {
                it.next.toInt
              }
              catch {
                case _: Throwable =>
                  throw new IllegalArgumentException("port number must be an integer")
              }
            }
            else
              throw new IllegalArgumentException("-K needs to be followed by a host name and port number" )
          }
          else
            throw new IllegalArgumentException("-K needs to be followed by a host name and port number" )
        case "-C" =>
          if (it.hasNext)
            it.next // scroll past the reporter class
          else
            throw new IllegalArgumentException("-C needs to be followed by a reporter class name arg: ")
        case arg: String =>
          throw new IllegalArgumentException("An arg started with an invalid character string: " + arg)
      }

    val graphicReporterConfigurationOption =
      args.find(arg => arg.startsWith("-g")) match {
        case Some(dashGString) =>
          val configSet = parseConfigSet(dashGString)
          if (configSet.contains(PresentShortStackTraces))
            throw new IllegalArgumentException("Cannot specify an S (present short stack traces) configuration parameter for the graphic reporter (because it shows them anyway): " + dashGString)
          if (configSet.contains(PresentFullStackTraces))
            throw new IllegalArgumentException("Cannot specify an F (present full stack traces) configuration parameter for the graphic reporter (because it shows them anyway): " + dashGString)
          if (configSet.contains(PresentWithoutColor))
            throw new IllegalArgumentException("Cannot specify a W (present without color) configuration parameter for the graphic reporter: " + dashGString)
          if (configSet.contains(PresentAllDurations))
            throw new IllegalArgumentException("Cannot specify a D (present all durations) configuration parameter for the graphic reporter (because it shows them all anyway): " + dashGString)
          if (configSet.contains(PresentUnformatted))
            throw new IllegalArgumentException("Cannot specify a U (present unformatted) configuration parameter for the graphic reporter: " + dashGString)
          Some(new GraphicReporterConfiguration(configSet))
        case None => None
      }

    def buildFileReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[FileReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-f"))
          lb += new FileReporterConfiguration(parseConfigSet(arg), it.next)
      }
      lb.toList
    }
    val fileReporterConfigurationList = buildFileReporterConfigurationList(args)

    def buildMemoryReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[MemoryReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-M"))
          lb += MemoryReporterConfiguration(it.next)
      }
      lb.toList
    }
    val memoryReporterConfigurationList = buildMemoryReporterConfigurationList(args)

    def buildJunitXmlReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[JunitXmlReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-u"))
          lb += new JunitXmlReporterConfiguration(Set[ReporterConfigParam](),
            it.next)
      }
      lb.toList
    }
    val junitXmlReporterConfigurationList =
      buildJunitXmlReporterConfigurationList(args)

    /*def buildDashboardReporterConfigurationList(args: List[String]) = {
      def fetchNumFilesArg: Int = {
        var numFiles: Option[Int] = None
        val it = args.iterator

        while (!numFiles.isDefined && it.hasNext) {
          val arg = it.next
          if (arg.startsWith("-a"))
            numFiles = Some(it.next.toInt)
        }
        numFiles.getOrElse(DefaultNumFilesToArchive)
      }

      val numFilesToArchive = fetchNumFilesArg
      val it = args.iterator
      val lb = new ListBuffer[DashboardReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-d"))
          lb += new DashboardReporterConfiguration(Set[ReporterConfigParam](),
                                                   it.next, numFilesToArchive)
      }
      lb.toList
    }
    val dashboardReporterConfigurationList =
      buildDashboardReporterConfigurationList(args)

    def buildXmlReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[XmlReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-x"))
          lb += new XmlReporterConfiguration(Set[ReporterConfigParam](),
                                             it.next)
      }
      lb.toList
    }
    val xmlReporterConfigurationList = buildXmlReporterConfigurationList(args)*/

    def buildHtmlReporterConfigurationList(args: List[String]):
    List[HtmlReporterConfiguration] =
    {
      if (args.isEmpty)
        Nil
      else if (!args.head.startsWith("-h"))
        buildHtmlReporterConfigurationList(args.tail)
      else if (args.tail.isEmpty)
        throw new IllegalArgumentException(
          "-h cannot be last, expected HTML output directory name to follow.")
      else {
        val (configSet, dir, cssFile, remainingArgs) = parseHtmlArgs(args)
        new HtmlReporterConfiguration(configSet, dir, cssFile) ::
          buildHtmlReporterConfigurationList(remainingArgs)
      }
    }

    //
    // Parses one set of -h arguments: configuration settings, html output
    // directory, and optional cssFile, from an argument list whose first
    // argument begins with "-h".  Returns parsed values plus list of
    // remaining arguments.
    //
    // -h argument list consists of string "-h" with optional configuration
    // letters appended (none are currently defined), a directory where
    // output html is to be written, and optionally a "-Y" argument followed
    // by the path to a custom CSS file to be used.  E.g.:
    //
    //   -hABC target/htmldir -Y src/resources/my.css
    //
    def parseHtmlArgs(args: List[String]):
    (Set[ReporterConfigParam], String, Option[URL], List[String]) =
    {
      val configSet = parseConfigSet(args.head)
      val directory = args(1)
      val (cssFile, remainingArgs) =
        if ((args.size > 2) && args(2) == "-Y") {
          if (args.size < 4)
            throw new IllegalArgumentException(
              "-Y cannot be last, expected CSS file name to follow.")
          else
            (Some(new File(args(3)).toURI.toURL), args.drop(4))
        }
        else
          (None, args.drop(2))

      (configSet, directory, cssFile, remainingArgs)
    }

    val htmlReporterConfigurationList = buildHtmlReporterConfigurationList(args)

    val standardOutReporterConfigurationOption =
      args.find(arg => arg.startsWith("-o")) match {
        case Some(dashOString) => Some(new StandardOutReporterConfiguration(parseConfigSet(dashOString)))
        case None => None
      }

    val standardErrReporterConfigurationOption =
      args.find(arg => arg.startsWith("-e")) match {
        case Some(dashEString) => Some(new StandardErrReporterConfiguration(parseConfigSet(dashEString)))
        case None => None
      }

    def buildCustomReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[CustomReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-C")) {
          val dashCString = arg
          val customReporterClassName = it.next
          val configSet = parseConfigSet(dashCString)
          if (configSet.contains(PresentShortStackTraces))
            throw new IllegalArgumentException("Cannot specify an S (present short stack traces) configuration parameter for a custom reporter: " + dashCString + " " + customReporterClassName)
          if (configSet.contains(PresentFullStackTraces))
            throw new IllegalArgumentException("Cannot specify an F (present full stack traces) configuration parameter for a custom reporter: " + dashCString + " " + customReporterClassName)
          if (configSet.contains(PresentWithoutColor))
            throw new IllegalArgumentException("Cannot specify a W (without color) configuration parameter for a custom reporter: " + dashCString + " " + customReporterClassName)
          if (configSet.contains(PresentAllDurations))
            throw new IllegalArgumentException("Cannot specify a D (present all durations) configuration parameter for a custom reporter: " + dashCString + " " + customReporterClassName)
          if (configSet.contains(PresentUnformatted))
            throw new IllegalArgumentException("Cannot specify a U (present unformatted) configuration parameter for a custom reporter: " + dashCString + " " + customReporterClassName)
          lb += new CustomReporterConfiguration(configSet, customReporterClassName)
        }
      }
      lb.toList
    }
    val customReporterConfigurationList = buildCustomReporterConfigurationList(args)

    def buildXmlSocketReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[XmlSocketReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-k")) {
          if (!it.hasNext)
            throw new IllegalArgumentException("-k must be followed by host and port")
          val host = it.next
          if (!it.hasNext)
            throw new IllegalArgumentException("-k must be followed by host and port")
          val port = it.next.toInt
          lb += new XmlSocketReporterConfiguration(host, port)
        }
      }
      lb.toList
    }
    val xmlSocketReporterConfigurationList = buildXmlSocketReporterConfigurationList(args)

    def buildSocketReporterConfigurationList(args: List[String]) = {
      val it = args.iterator
      val lb = new ListBuffer[SocketReporterConfiguration]
      while (it.hasNext) {
        val arg = it.next
        if (arg.startsWith("-K")) {
          if (!it.hasNext)
            throw new IllegalArgumentException("-K must be followed by host and port")
          val host = it.next
          if (!it.hasNext)
            throw new IllegalArgumentException("-K must be followed by host and port")
          val port = it.next.toInt
          lb += new SocketReporterConfiguration(host, port)
        }
      }
      lb.toList
    }
    val socketReporterConfigurationList = buildSocketReporterConfigurationList(args)

    // Here instead of one loop, i go through the loop several times.
    new ReporterConfigurations(
      graphicReporterConfigurationOption,
      fileReporterConfigurationList,
      memoryReporterConfigurationList,
      junitXmlReporterConfigurationList,
      //dashboardReporterConfigurationList,
      //xmlReporterConfigurationList,
      standardOutReporterConfigurationOption,
      standardErrReporterConfigurationOption,
      htmlReporterConfigurationList,
      customReporterConfigurationList,
      xmlSocketReporterConfigurationList,
      socketReporterConfigurationList
    )
  }

  //
  // Parses suite args -s, -i, -t, and -z into lists.
  //
  // Accepts a list of args, a sequence containing:
  //  - "-s" followed by a suite name
  //  - "-i" followed by a suite id
  //  - "-t" followed by a test name
  //  - "-z" followed by a test name substring
  //
  // If the list starts with a -s, then the following -i, -t, and -z args
  // are associated with the preceding -s.
  //
  // Unaffiliated -t and -z args are returned in a separate list, as test
  // specs for which the corresponding Suites will have to be found during
  // discovery.
  //
  private[scalatest] def parseSuiteArgs(args: List[String]): (List[SuiteParam], List[TestSpec]) = {
    val OpeningDashArgs = Set("-s", "-z", "-t")

    if (args == null)
      throw new NullPointerException("args was null")

    if (args.exists(_ == null))
      throw new NullPointerException("an arg String was null")

    val lb = new ListBuffer[SuiteParam]
    val tb = new ListBuffer[TestSpec]
    val it = args.iterator.buffered
    while (it.hasNext) {
      val dashArg = it.next

      if (dashArg == "-i")
        throw new IllegalArgumentException("-i argument must follow a -s")
      else if (!OpeningDashArgs.contains(dashArg))
        throw new IllegalArgumentException("unexpected argument ["+
          dashArg + "]")

      if (!it.hasNext)
        throw new IllegalArgumentException(
          "argument "+ dashArg +" must be followed by a name")

      val argVal = it.next

      if (argVal.startsWith("-"))
        throw new IllegalArgumentException(
          "Expecting a name to follow "+ dashArg +", but got ["+ argVal +"]")

      dashArg match {
        case "-t" =>
          tb += TestSpec(argVal, false)

        case "-z" =>
          tb += TestSpec(argVal, true)

        case "-s" =>
          val (testNames, wildcardTestNames) =
            if (it.hasNext && (it.head == "-t" || it.head == "-z")) {
              val testNamesBuffer = new ListBuffer[String]()
              val wildcardTestNamesBuffer = new ListBuffer[String]()
              while (it.hasNext && (it.head == "-t" || it.head == "-z")) {
                val dashTest = it.next
                if (dashTest == "-t")
                  testNamesBuffer += it.next
                else
                  wildcardTestNamesBuffer += it.next
              }
              (testNamesBuffer.toArray, wildcardTestNamesBuffer.toArray)
            }
            else
              (Array.empty[String], Array.empty[String])

          val nestedSuites =
            if (it.hasNext && it.head == "-i") {
              val nestedLb = new ListBuffer[NestedSuiteParam]()

              while (it.hasNext && it.head == "-i") {
                val dashI = it.next()
                val suiteId = it.next
                val suiteIdTestNamesBuffer = new ListBuffer[String]()
                val suiteIdWildcardTestNamesBuffer = new ListBuffer[String]()

                while (it.hasNext && (it.head == "-t" || it.head == "-z")) {
                  val dashTest = it.next
                  if (dashTest == "-t")
                    suiteIdTestNamesBuffer += it.next
                  else
                    suiteIdWildcardTestNamesBuffer += it.next
                }
                nestedLb += new NestedSuiteParam(suiteId, suiteIdTestNamesBuffer.toArray, suiteIdWildcardTestNamesBuffer.toArray)
              }
              nestedLb.toArray
            }
            else
              Array.empty[NestedSuiteParam]

          lb += SuiteParam(argVal, testNames, wildcardTestNames, nestedSuites)

        case _ =>
          throw new Exception("unexpected dash arg ["+ dashArg +"]")
      }
    }
    (lb.toList, tb.toList)
  }

  //
  // Given a list of args consisting of pairs of strings "-A"
  // followed by a file name, generates a list of just the
  // file names.
  //
  def parseAgainArgs(args: List[String]): List[String] = {
    val buf = new ListBuffer[String]
    val it = args.iterator

    while (it.hasNext) {
      if (it.next() != "-A")
        throw new Exception("unexpected arg ["+ args +"]")

      if (!it.hasNext)
        throw new IllegalArgumentException(
          "-A argument must be followed by a file name")

      buf += it.next()
    }
    buf.toList
  }
  // SKIP-SCALATESTJS-END

  //
  // Determines whether specified token is complete or partial.
  //
  // Tokens are considered partial if they end with a backslash, since
  // backslash is used to escape spaces that would otherwise be
  // treated as delimiters within the path string.
  //
  // Exceptions are cases where the token ends in a backslash
  // but is still considered a complete token because it constitutes
  // a valid representation of a root directory on a windows system,
  // e.g. "c:\" or just "\".
  //
  private val ROOT_DIR_PATTERN = Pattern.compile("""(?i)\\|[a-z]:\\""")
  private def isCompleteToken(token: String): Boolean = {
    val matcher = ROOT_DIR_PATTERN.matcher(token)

    matcher.matches() || (token(token.length - 1) != '\\')
  }

  //
  // Splits a space-delimited path into its component parts.
  //
  // Spaces within path elements may be escaped with backslashes, e.g.
  // "c:\Documents\ And\ Settings c:\Program\ Files"
  //
  // See comments for isCompleteToken() below for exceptions.
  //
  private val START_TOKEN_PATTERN = Pattern.compile("""^\s*(.*?)(\s|$)""")
  private val FULL_TOKEN_PATTERN  = Pattern.compile("""^\s*(.+?)(((?<=[^\\])\s)|$)""")
  private def splitPath(pathArg: String): List[String] = {
    val path = pathArg.trim

    if (path.isEmpty) Nil
    else {
      val startMatcher = START_TOKEN_PATTERN.matcher(path)

      if (!startMatcher.find())
        throw new RuntimeException("unexpected startMatcher path [" +
          path + "]")
      val token = startMatcher.group(1)

      if (isCompleteToken(token)) {
        token :: splitPath(path.substring(startMatcher.end))
      }
      else {
        val fullMatcher = FULL_TOKEN_PATTERN.matcher(path)

        if (!fullMatcher.find())
          throw new RuntimeException("unexpected fullMatcher path [" +
            path + "]")
        val fullToken = fullMatcher.group(1).replaceAll("""\\(\s)""", "$1")

        fullToken :: splitPath(path.substring(fullMatcher.end))
      }
    }
  }

  def parseCompoundArgIntoSet(args: List[String], expectedDashArg: String): Set[String] =
    Set() ++ parseCompoundArgIntoList(args, expectedDashArg)

  def parseRunpathArgIntoList(args: List[String]): List[String] = parseCompoundArgIntoList(args, "-R")

  def parseCompoundArgIntoList(args: List[String], expectedDashArg: String): List[String] = {

    if (args == null)
      throw new NullPointerException("args was null")

    if (args.exists(_ == null))
      throw new NullPointerException("an arg String was null")

    if (args.length == 0) {
      List()
    }
    else if (args.length % 2 == 0) {
      def parsePair(dashArg: String, compoundArg: String) = {
        if (dashArg != expectedDashArg)
          throw new IllegalArgumentException("First arg must be " + expectedDashArg + ", but was: " + dashArg)

        if (compoundArg.trim.isEmpty)
          throw new IllegalArgumentException("The argument string must actually include some non-whitespace characters.")

        splitPath(compoundArg)
      }
      args.grouped(2).flatMap(p => parsePair(p(0), p(1))).toList
    }
    else {
      throw new IllegalArgumentException("Compound arg must be either zero-length or have even number of args: " + args)
    }
  }

  def parseChosenStylesIntoChosenStyleSet(args: List[String], dashArg: String) = {
    val it = args.iterator
    val lb = new ListBuffer[String]()
    while (it.hasNext) {
      val dash = it.next
      if (dash != dashArg)
        throw new IllegalArgumentException("Every other element, starting with the first, must be " + dashArg)
      if (it.hasNext) {
        lb += it.next
      }
      else
        throw new IllegalArgumentException("Last element must be a style name, not a " + dashArg + ".")
    }
    lb.toSet
  }

  def parseDoubleArgument(args: List[String], dashArg: String, defaultValue: Double): Double = {
    val it = args.iterator
    val lb = new ListBuffer[Double]()
    while (it.hasNext) {
      val dash = it.next
      if (dash != dashArg)
        throw new IllegalArgumentException("Every other element, starting with the first, must be " + dashArg)
      if (it.hasNext) {
        val spanString = it.next
        try {
          lb += spanString.toDouble
        }
        catch {
          case e: NumberFormatException =>
            throw new IllegalArgumentException(dashArg + " must be followed by a number, but '" + spanString + "' is not a number.")
        }
      }
      else
        throw new IllegalArgumentException("Last element must be a number, not a " + dashArg + ".")
    }
    if (lb.size == 0)
      defaultValue
    else if (lb.size == 1)
      lb(0)
    else
      throw new IllegalArgumentException("Only one " + dashArg + " can be specified.")
  }

  def parsePropertiesArgsIntoMap(args: List[String]): ConfigMap = {

    if (args == null)
      throw new NullPointerException("args was null")

    if (args.exists(_ == null))
      throw new NullPointerException("an arg String was null")

    if (args.exists(_.indexOf('=') == -1))
      throw new IllegalArgumentException("A -D arg does not contain an equals sign.")

    if (args.exists(!_.startsWith("-D")))
      throw new IllegalArgumentException("A spice arg does not start with -D.")

    if (args.exists(_.indexOf('=') == 2))
      throw new IllegalArgumentException("A spice arg does not have a key to the left of the equals sign.")

    if (args.exists(arg => arg.indexOf('=') == arg.length - 1))
      throw new IllegalArgumentException("A spice arg does not have a value to the right of the equals sign.")

    val tuples = for (arg <- args) yield {
      val keyValue = arg.substring(2) // Cut off the -D at the beginning
      val equalsPos = keyValue.indexOf('=')
      val key = keyValue.substring(0, equalsPos)
      val value = keyValue.substring(equalsPos + 1)
      (key, value)
    }

    new ConfigMap(Map(tuples: _*))
  }

}