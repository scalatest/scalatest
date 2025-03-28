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
package org.scalatest.events


/**
 * Abstract class for the optional formatter objects that must be passed to the <code>Event</code>s reported
 * during a ScalaTest run.
 */
sealed abstract class Formatter extends Product with Serializable

/**
 * A <code>Formatter</code> that indicates reporters may wish to suppress reporting of an <code>Event</code>.
 * "Suppress" means that the event won't be reported to the user.
 *
 * <p>
 * An example is that specification-style suites, such as <code>FunSpec</code>, generate output that reads
 * more like a specification. One aspect of this is that generally only a single event should be reported
 * for each test, so that output can appear like this:
 * </p>
 *
 * <pre>
 * A Stack (when newly created)
 * - should be empty
 * - should complain when popped
 * </pre>
 *
 * <p>
 * ScalaTest suites should generate two events per test, a <a href="TestStarting.html"><code>TestStarting</code></a> event and either
 * a <a href="TestSucceeded.html"><code>TestSucceeded</code></a> or a <a href="TestFailed.html"><code>TestFailed</code></a> event. The <a href="../FunSpec.html"><code>FunSpec</code></a> trait does report both events,
 * but passes a <code>MotionToSuppress</code> along with the <code>TestStarting</code> event. As a result,
 * The <code>TestStarting</code> events have no effect on the output. Each <code>TestSucceeded</code> or
 * <code>TestFailed</code> event, which is sent with an <a href="IndentedText.html"><code>IndentedText</code></a> formatter instead of
 * a <code>MotionToSuppress</code>, will generate output, such as "<code>- should be empty</code>".
 * </p>
 *
 * <p>
 * Reporters may choose to ignore a <code>MotionToSuppress</code>. For example, an XML reporter may
 * want to report everything about every event that is fired during a concurrent run, so that the
 * events can be reordered later by reading the complete, but unordered, information from an XML file.
 * In this case, the XML reporter would actually report events that were fired with a <code>MotionToSuppress</code>,
 * including indicating that the report included a motion to suppress.
 * </p>
 *
 * @author Bill Venners
 */
final case object MotionToSuppress extends Formatter

/**
 * A <a href="Formatter.html"><code>Formatter</code></a> providing information that enables reporters to create more stylized output.
 *
 * <p>
 * An example is that specification-style suites, such as <a href="../FunSpec.html"><code>FunSpec</code></a>, generate output that reads
 * more like a specification, for instance:
 * </p>
 *
 * <pre>
 * A Stack (when newly created)
 * - should be empty
 * - should complain when popped
 * </pre>
 *
 * <p>
 * This output might be generated by ScalaTest's standard out reporter. Each of these lines would be
 * taken from the <code>IndentedText</code>'s <code>formattedText</code> parameter. Were this same run
 * to be reported in HTML or in a GUI, the output would be based on the <code>rawText</code> and the
 * <code>indentationLevel</code>. Here's what the <code>IndentedText</code> values would be for each event:
 * </p>
 *
 * <ul>
 * <li><a href="InfoProvided.html"><code>InfoProvided</code></a> reported with an:
 * <pre class="stHighlight">
 *   IndentedText(
 *     formattedText = "A Stack (when newly created)",
 *     rawText = "A Stack (when newly created)",
 *     indentationLevel = 0
 *   )
 * </pre>
 * </li>
 * <li><a href="TestSucceeded.html"><code>TestSucceeded</code></a> reported with an:
 * <pre class="stHighlight">
 *   IndentedText(
 *     formattedText = "- should be empty",
 *     rawText = "should be empty",
 *     indentationLevel = 1
 *   )
 * </pre>
 * </li>
 * <li><code>TestSucceeded</code> reported with an:
 * <pre class="stHighlight">
 *   IndentedText(
 *     formattedText = "- should complain when popped",
 *     rawText = "should complain when popped",
 *     indentationLevel = 1
 *   )
 * </pre>
 * </li>
 * </ul>
 *
 * <p>
 * One possible way this information could be presented in HTML, for example, is this:
 * </p>
 *
 * <p>
 * <strong>A Stack (when newly created)</strong>
 * <ul>
 * <li>should be empty</li>
 * <li>should complain when popped</li>
 * </ul>
 * </p>
 *
 * @param formattedText a localized string suitable for presenting to a user by printing it straight to an output stream
 * @param rawText a localized string suitable for presenting to the user after in some way being indented by the
 *        value specified as the <code>indentationLevel</code> parameter
 * @param indentationLevel a zero or positive integer representing an indentation level for the indented text
 *
 * @throws IllegalArgumentException if the specified <code>indentationLevel</code> is less than zero
 */
final case class IndentedText(formattedText: String, rawText: String, indentationLevel: Int) extends Formatter {
  require(indentationLevel >= 0, "indentationLevel was less than zero: " + indentationLevel)
}
