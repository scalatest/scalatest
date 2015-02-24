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

import java.io._
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object GenResources {

  def resourcesTemplate(methods: String): String =
    s"""package org.scalactic
      |
      |import java.util.ResourceBundle
      |import java.text.MessageFormat
      |
      |private[scalactic] object Resources {
      |
      |lazy val resourceBundle = ResourceBundle.getBundle("org.scalactic.ScalacticBundle")
      |
      |private def makeString(resourceName: String, args: Array[Any]): String = {
      |  val raw = resourceBundle.getString(resourceName)
      |  val msgFmt = new MessageFormat(raw)
      |  msgFmt.format(args.toArray)
      |}
      |
      |$methods
      |
      |}
    """.stripMargin

  def failureMessagesTemplate(methods: String): String =
    s"""package org.scalactic
       |
       |private[scalactic] object FailureMessages {
       |
       |def decorateToStringValue(o: Any): String = Prettifier.default(o)
       |
       |
       |
       |$methods
       |
       |}
    """.stripMargin

  val paramRegex = "\\{\\d+\\}".r

  case class KeyValue(key: String, value: String)

  object KeyValueParser extends JavaTokenParsers {
    def equalParser: Parser[String] = "="
    def keyParser: Parser[String] = """([\p{L}_$][\p{L}\p{N}_$]*\.)*[\p{L}_$][\p{L}\p{N}_$]*""".r  // use Java class name parser for now for key
    def valueParser: Parser[String] = ".*".r
    def keyValueParser: Parser[KeyValue] = keyParser ~ equalParser ~ valueParser ^^ {
      case ~(~(key, _), value) => KeyValue(key, value)
    }
    def parse(s: String): KeyValue =
      parseAll(keyValueParser, s) match {
        case Success(kv, _) => kv
        case other => throw new RuntimeException("invalid key value syntax: " + s)
      }
  }

  def genResources(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    targetDir.mkdirs()

    val sourcePropertiesFile = new File("scalactic-macro/src/main/resources/org/scalactic/ScalacticBundle.properties")
    val lines = Source.fromFile(sourcePropertiesFile).getLines

    val resourcesMethods =
      lines.map { line =>
        val kv = KeyValueParser.parse(line)
        val paramTokens = paramRegex.findAllIn(kv.value)
        val paramCount = if (paramTokens.isEmpty) 0 else paramTokens.map(t => t.substring(1, t.length - 1).toInt).max + 1
        "def " + kv.key + "(" + (for (i <- 0 until paramCount) yield s"param$i: Any").mkString(", ") + "): String = makeString(\"" + kv.key + "\", Array(" + (for (i <- 0 until paramCount) yield s"param$i").mkString(", ") + "))" + "\n\n" +
        "def raw" + kv.key.capitalize + ": String = resourceBundle.getString(\"" + kv.key + "\")"
      }.mkString("\n\n")

    val resourcesFile = new File(targetDir, "Resources.scala")
    val resourcesWriter = new BufferedWriter(new FileWriter(resourcesFile))
    try {
      resourcesWriter.write(resourcesTemplate(resourcesMethods))
    }
    finally {
      resourcesWriter.flush()
      resourcesWriter.close()
      println("Generated " + resourcesFile.getAbsolutePath)
    }

    Vector(resourcesFile)
  }

  def genFailureMessages(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    targetDir.mkdirs()

    val sourcePropertiesFile = new File("scalactic-macro/src/main/resources/org/scalactic/ScalacticBundle.properties")
    val lines = Source.fromFile(sourcePropertiesFile).getLines

    val failureMessagesMethods =
      lines.map { line =>
        val kv = KeyValueParser.parse(line)
        val paramTokens = paramRegex.findAllIn(kv.value)
        val paramCount = if (paramTokens.isEmpty) 0 else paramTokens.map(t => t.substring(1, t.length - 1).toInt).max + 1
        "def " + kv.key + "(" + (for (i <- 0 until paramCount) yield s"param$i: Any").mkString(", ") + "): String = Resources." + kv.key + "(" + (for (i <- 0 until paramCount) yield s"decorateToStringValue(param$i)").mkString(", ") + ")"
      }.mkString("\n\n")

    val failureMessagesFile = new File(targetDir, "FailureMessages.scala")
    val failureMessagesWriter = new BufferedWriter(new FileWriter(failureMessagesFile))
    try {
      failureMessagesWriter.write(failureMessagesTemplate(failureMessagesMethods))
    }
    finally {
      failureMessagesWriter.flush()
      failureMessagesWriter.close()
      println("Generated " + failureMessagesFile.getAbsolutePath)
    }

    Vector(failureMessagesFile)
  }

}