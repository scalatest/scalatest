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

import java.io.PrintWriter
import java.util.regex.Pattern
import scala.io.Source

import org.scalatest.events.TestFailed
import org.scalatest.events.TestCanceled
import org.scalatest.events.SuiteAborted
import org.scalatest.events.Event
import Memento._

/**
 * A record of a test to be run again.  Mementos get stored
 * into a specified file when tests are run using Runner's -M
 * option, and then stored Mementos can be rerun by using
 * option -A.
 */
private[tools] case class Memento(eventName: String,
                                  className: Option[String],
                                  suiteId:   String,
                                  testName:  Option[String])
{
  //
  // Generates a one-line string containing class's encoded
  // fields separated by spaces, so they can be stored as a line
  // in a file and reconstituted back into an object later.
  //
  // E.g.:
  //
  // TestFailed Some(org.eg.HelloSuite) org.eg.HelloSuite Some(this_is_a_fail)
  //
  def singleLine: String =
    eventName        +" "+
    className        +" "+
    encode(suiteId)  +" "+
    encode(testName.toString)

  //
  // Creates a SuiteParam object from a Memento.
  //
  // If the Memento's class name is the same as its suite id,
  // then the test name is stored in the SuiteParam's top-level
  // testNames array.  Otherwise the suite id and test name are
  // stored in an element in the SuiteParam's nestedSuites
  // array.
  //
  def toSuiteParam: SuiteParam = {
    val testNamesArray = testName match {
      case Some(name) => Array(name)
      case None => Array.empty[String]
    }

    val classNameStr = className.getOrElse("unknown")

    val (testNames, nestedSuites) =
      if (suiteId == classNameStr)
        (testNamesArray, Array.empty[NestedSuiteParam])
      else {
        val nestedSuiteParam =
          NestedSuiteParam(suiteId, testNamesArray, Array())

        (Array.empty[String], Array(nestedSuiteParam))
      }

    SuiteParam(classNameStr, testNames, Array(), nestedSuites)
  }
}

private[tools] object Memento {
  //
  // Generates a Memento object from an Event.
  //
  def apply (event: Event): Memento = {
    val (eventName: String,
         className: Option[String],
         suiteId:   String,
         testName:  Option[String]
        ) =
      event match {
        case e: TestFailed =>
          ("TestFailed",   e.rerunner, e.suiteId,  Some(e.testName))
        case e: TestCanceled =>
          ("TestCanceled", e.rerunner, e.suiteId,  Some(e.testName))
        case e: SuiteAborted =>
          ("SuiteAborted", e.rerunner, e.suiteId,  None)
        case e =>
          ("unexpected",   None,       e.toString, None)
      }

    Memento(eventName, className, suiteId, testName)
  }

  //
  // Generates a list of Mementos from a file containing
  // strings created using Memento.singleLine.
  //
  def readFromFile(fileName: String): List[Memento] = 
    for (line <- Source.fromFile(fileName).getLines.toList)
      yield fromString(line)

  //
  // Writes Mementos to a file as single-line strings, sorted
  // alphabetically.
  //
  def writeToFile(fileName: String, mementos: Set[Memento]) {
    val out = new PrintWriter(fileName, "UTF-8")

    val lines = mementos.map(_.singleLine).toList.sortWith(_<_)

    lines.foreach(out.println)
    out.close()
  }

  //
  // Constructs a Memento object from a single-line string.
  //
  def fromString(str: String): Memento = {
    val splits = str.split(" ")

    if (splits.length != 4)
      throw new Exception("bad line format ["+ str +"]")

    val eventName = splits(0)
    val className = optionFromString(splits(1))
    val suiteId   = decode(splits(2))
    val testName  = optionFromString(decode(splits(3)))

    Memento(eventName, className, suiteId, testName)
  }

  //
  // Constructs an Option from a string formatted as "Some(...)" or
  // "None".
  //
  val SomeNonePat = Pattern.compile("""^(Some\(|None)(.*?)\)?$""")
  def optionFromString(str: String): Option[String] = {
    val matcher = SomeNonePat.matcher(str)
    if (!matcher.find())
      throw new Exception("bad option string ["+ str +"]")

    matcher.group(1) match {
      case "Some(" => Some(matcher.group(2))
      case "None"  => None
      case _       => throw new Exception("bad Option format ["+ str +"]")
    }
  }

  //
  // Encodes a string, converting newlines and spaces to
  // encoded replacements.
  //
  def encode(text: String): String = {
    text.
      replaceAll("""\\""", """\\5c""").
      replaceAll("\n", """\\1f""").
      replaceAll("_", """\\5f""").
      replaceAll(" ", "_")
  }

  //
  // Undoes encoding performed by encode function above.
  //
  def decode(text: String): String = {
    text.
      replaceAll("_", " ").
      replaceAll("""\\5f""", "_").
      replaceAll("""\\1f""", "\n").
      replaceAll("""\\5c""", """\\""")
  }
}
