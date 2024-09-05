/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic

import scala.reflect.macros.whitebox.Context

/**
 * Case class that stores the name and value of a variable or expression.
 *
 * <p>
 * See the main documentation for trait <a href="Snapshots.html"><code>Snapshots</code></a> for more information and examples.
 * </p>
 * 
 * @param name the name of the expression
 * @param value the value of the expression
 */
final case class Snapshot(name: String, value: Any) {

  /**
   * Overriden <code>toString</code> to print in {name} = {value} format.
   *
   * @return string in {name} = {value} format
   */
  override def toString: String = Resources.variableWasValue(name, Prettifier.default(value))
}

/**
 * <p>Trait that provides a <code>snap</code> method that takes one or more arguments and results in a
 * <a href="SnapshotSeq.html"><code>SnapshotSeq</code></a>, whose <code>toString</code> lists the names
 * and values of each argument.
 *
 * <p>
 * The intended use case of this trait is to help you write debug and log
 * messages that give a "snapshot" of program state. Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import Snapshots._
 * import Snapshots._
 * 
 * scala&gt; snap(a, b, c, d, e, f)
 * res3: org.scalactic.SnapshotSeq = a was 1, b was 2, c was 3, d was 4, e was null, f was null
 * </pre>
 * 
 * <p><code>SnapshotSeq</code> offers a <code>lines</code> method that places each variable name/value pair on its own line:<p>
 * 
 * <pre class="stREPL">
 * scala&gt; snap(a, b, c, d, e, f).lines
 * res4: String = 
 * a was 1
 * b was 2
 * c was 3
 * d was 4
 * e was null
 * f was null
 * </pre>
 * 
 * <p>
 * Or, because a <code>SnapshotSeq</code> is a <code>IndexedSeq[Snapshot]</code>, you can process it just like any other <code>Seq</code>, for example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; snap(a, b, c, d, e, f).mkString("Wow! ", ", and ", ". That's so awesome!")
 * res6: String = Wow! a was 1, and b was 2, and c was 3, and d was 4, and e was null, and f was null. That's so awesome!
 * </pre>
 */
trait Snapshots {

  import language.experimental.macros

  /**
   * Snap the given expressions.
   *
   * @param expressions expressions to be snapped
   * @return an <code>IndexedSeq</code> of <code>Snapshot</code> for the given expressions.
   */
  def snap(expressions: Any*): SnapshotSeq = macro SnapshotsMacro.snap
}

/**
 * An <code>IndexedSeq[Snapshot]</code> providing <code>toString</code> and <code>lines</code> methods that 
 * can be useful for debug and log messages about program state.
 * 
 * <p>
 * See the main documentation for trait <a href="Snapshots.html"><code>Snapshots</code></a> for more information and examples.
 * </p>
 */
final class SnapshotSeq(underlying: collection.immutable.IndexedSeq[Snapshot]) extends collection.immutable.IndexedSeq[Snapshot] {

  /**
   * Selects an element by its index in the sequence.
   *
   * <p>
   * This method invokes <code>apply</code> on the underlying immutable <code>IndexedSeq[String]</code>, passing in <code>idx</code>, and returns the result.
   * </p>
   *
   * @param idx the index to select
   * @return the element of this sequence at index <code>idx</code>, where 0 indicates the first element
   */
  def apply(idx: Int): Snapshot = underlying.apply(idx)

  /**
   * The length of this sequence.
   *
   * <p>
   * This method invokes <code>length</code> on the underlying immutable <code>IndexedSeq[String]</code> and returns the result.
   * </p>
   *
   * @return the number of elements in this sequence
   */
  def length: Int = underlying.length

  /**
   * Appends a string element to this sequence, if it doesn't already exist in the sequence.
   *
   * <p>
   * If the string element already exists in this sequence, this method returns itself. If not,
   * this method returns a new <code>MultiSelOptionSeq</code> with the passed value appended to the
   * end of the original <code>MultiSelOptionSeq</code>.
   * </p>
   *
   * @param the string element to append to this sequence
   * @return a <code>MultiSelOptionSeq</code> that contains the passed string value
   */
  def +(value: Snapshot): SnapshotSeq = {
    if (!underlying.contains(value))
      new SnapshotSeq(underlying :+ value)
    else
      this
  }

  /**
   * Removes a string element to this sequence, if it already exists in the sequence.
   *
   * <p>
   * If the string element does not already exist in this sequence, this method returns itself. If the element
   * is contained in this sequence, this method returns a new <code>MultiSelOptionSeq</code> with the passed value
   * removed from the the original <code>MultiSelOptionSeq</code>, leaving any other elements in the same order.
   * </p>
   *
   * @param the string element to append to this sequence
   * @return a <code>MultiSelOptionSeq</code> that contains the passed string value
   */
  def -(value: Snapshot): SnapshotSeq = {
    if (underlying.contains(value))
      new SnapshotSeq(underlying.filter(_ != value))
    else
      this
  }

  /**
   * The default way to present the result of the <code>snap</code> method of trait </code>Snapshots</code>. 
   *
   * Here's an example:
   *
   * <pre class="stREPL">
   * scala&gt; snap(a, b, c, d, e, f)
   * res3: org.scalactic.SnapshotSeq = a was 1, b was 2, c was 3, d was 4, e was null, f was null
   * </pre>
   */
  override def toString: String = mkString(", ")

  /**
   * An alternate way to present the result of the <code>snap</code> method of trait </code>Snapshots</code> that
   * puts each variable or expression on its own line.
   *
   * Here's an example:
   *
   * <pre class="stREPL">
   * scala&gt; snap(a, b, c, d, e, f).lines
   * res4: String = 
   * a was 1
   * b was 2
   * c was 3
   * d was 4
   * e was null
   * f was null
   * </pre>
   */
  def lines: String = mkString("\n")
}

object SnapshotSeq {
  def apply(snapshots: Snapshot*): SnapshotSeq = new SnapshotSeq(Vector(snapshots: _*))
}

/**
 * Companion object that facilitates the importing of <code>Snapshots</code> members as
 * an alternative to mixing it in. One use case is to import <code>Snapshots</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala 2.13.6 (OpenJDK 64-Bit Server VM, Java yyy).
 * Type in expressions for evaluation. Or try :help.
 * &nbsp;
 * scala&gt; import org.scalactic.Snapshots._
 * import org.scalatest.Snapshots._
 * &nbsp;
 * scala&gt; val a = 8
 * a: Int = 8
 *&nbsp;
 * scala&gt; snap(a)
 * res0: scala.collection.immutable.Vector[org.scalactic.Snapshot] = Vector(a = 8)
 * </pre>
 */
object Snapshots extends Snapshots

private[scalactic] object SnapshotsMacro {

  def snap(context: Context)(expressions: context.Expr[Any]*): context.Expr[SnapshotSeq] = {
    import context.universe._

    val snapshots =
      expressions.map { expr =>
        Apply(
          Select(
            Select(
              Select(
                Ident(TermName("org")),
                TermName("scalactic")
              ),
              TermName("Snapshot")
            ),
            TermName("apply")
          ),
          List(q"${show(expr.tree)}", expr.tree.duplicate)
        )
      }

    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Ident(TermName("org")),
              TermName("scalactic")
            ),
            TermName("SnapshotSeq")
          ),
          TermName("apply")
        ),
        List(snapshots: _*)
      )
    )
  }
}
