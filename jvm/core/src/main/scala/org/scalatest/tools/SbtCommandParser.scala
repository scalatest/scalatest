/*
 * Copyright 2001-2024 Artima, Inc.
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

/*import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical

private[scalatest] class SbtCommandParser extends StandardTokenParsers {

  lexical.delimiters ++= List("(", ")", "--", ",", "=", "\"")
  lexical.reserved ++= List("st", "include", "exclude", "membersonly", "wildcard", "suite", "junit", "testng", "dashboard", "file", "filename",
      "config", "directory", "stdout", "stderr", "graphic", "junitxml", "dashboard", "html", "reporterclass", "dashboard", "concurrent")

  def parseCommand(command: String): Unit = {
    val tokens = new lexical.Scanner(command)
    val result = phrase(cmd)(tokens)
    result match {
      case Success(tree, _) => println("success: " + tree)
      case e: NoSuccess => {
        Console.err.println(e)
      }
    }
  }

  def parseResult(command: String) = {
    val tokens = new lexical.Scanner(command)
    phrase(cmd)(tokens)
  }

  def cmd: Parser[Any] = "st" ~ opt(dashArgs)

  def dashArgs: Parser[Any] = "--" // ~ opt(args)

/*
  def stArgs: Parser[Any] = rep(stArgsOpt)

  def stArgsOpt: Parser[Any] = include | 
                              exclude | 
                              concurrent | 
                              membersonly | 
                              wildcard | 
                              suite | 
                              junit | 
                              testng | 
                              stdout | 
                              stderr | 
                              graphic | 
                              file | 
                              junitxml | 
                              dashboard | 
                              html | 
                              reporterclass

  def include: Parser[Any] = "include" ~ list
  def exclude: Parser[Any] = "exclude" ~ list
 
  def concurrent: Parser[Any] = "concurrent"
  def membersonly: Parser[Any] = "membersonly" ~ list
  def wildcard: Parser[Any] = "wildcard" ~ list

  def list: Parser[Any] = "(" ~> repsep(stringLit, ",") <~ ")"
 
  def stdout: Parser[Any] = "stdout" ~ opt("(" ~ config ~ ")")
  def stderr: Parser[Any] = "stderr" ~ opt("(" ~ config ~ ")")
  def graphic: Parser[Any] = "graphic" ~ opt("(" ~ limitedConfig ~ ")")
  def file: Parser[Any] = "file" ~ "(" ~ "filename" ~ "=" ~ stringLit ~ opt("," ~ config) ~ ")"
  def junitxml: Parser[Any] = "junitxml" ~ "(" ~ "directory" ~ "=" ~ stringLit ~ ")"
  def dashboard: Parser[Any] = "dashboard" ~ "(" ~ "directory" ~ "=" ~ stringLit ~ opt("," ~ archive) ~ ")"
  def html: Parser[Any] = "html" ~ "(" ~ "filename" ~ "=" ~ stringLit ~ opt("," ~ config) ~ ")"
  def reporterclass: Parser[Any] = "reporterclass" ~ "(" ~ "classname=" ~ stringLit ~ opt("," ~ limitedConfig) ~ ")"

  def archive: Parser[Any] = "archive" ~ "=" ~ "\"" ~ numericLit ~ "\""

  def config: Parser[Any] = "config" ~ "=" ~ "\"" ~ rep(configOpt) ~ "\""
 
  def configOpt: Parser[Any] = "dropteststarting" | 
                           "droptestsucceeded" | 
                           "droptestignored" | 
                           "droptestpending" | 
                           "dropsuitestarting" | 
                           "dropsuitecompleted" | 
                           "dropinfoprovided" | 
                           "nocolor" | 
                           "shortstacks" | 
                           "fullstacks" | 
                           "durations"
  
  def limitedConfig: Parser[Any] = "config=\"" ~ rep(limitedConfigOpt) ~ "\""
                           
  def limitedConfigOpt: Parser[Any] = "dropteststarting" | 
                                  "droptestsucceeded" | 
                                  "droptestignored" | 
                                  "droptestpending" | 
                                  "dropsuitestarting" | 
                                  "dropsuitecompleted" | 
                                  "dropinfoprovided"
*/
}

private[scalatest] object SbtCommandParser {
  def main(args: Array[String]): Unit = {
    
    (new SbtCommandParser).parseCommand("""st""")
/*
    (new SbtCommandParser).parseCommand("""st include("a", "b", "c")""")
    (new SbtCommandParser).parseCommand("""st exclude("a", "b", "c")""")
    (new SbtCommandParser).parseCommand("""st exclude("a", "b", "c") concurrent""")
    (new SbtCommandParser).parseCommand("""st membersonly("a", "b", "c") stdout""")
    (new SbtCommandParser).parseCommand("""st wildcard("a", "b", "c") stdout(config = "dropteststarting droptestpending")""")
*/
  }
}*/

