/*
 * Copyright 2001-2011 Artima, Inc.
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
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Calendar

import scala.collection.JavaConversions._

object GenTable {

val scaladocForTableFor1VerbatimString = """
/**
 * A table with 1 column.
 *
 * <p>
 * For an overview of using tables, see the documentation for trait
 * <a href="TableDrivenPropertyChecks.html">TableDrivenPropertyChecks</a>.
 * </p>
 *
 * <p>
 * This table is a sequence of objects, where each object represents one row of the (one-column) table.
 * This table also carries with it a <em>heading</em> tuple that gives a string name to the
 * lone column of the table.
 * </p>
 *
 * <p>
 * A handy way to create a <code>TableFor1</code> is via an <code>apply</code> factory method in the <code>Table</code>
 * singleton object provided by the <code>Tables</code> trait. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val examples =
 *   Table(
 *     "a",
 *       0,
 *       1,
 *       2,
 *       3,
 *       4,
 *       5,
 *       6,
 *       7,
 *       8,
 *       9
 *   )
 * </pre>
 *
 * <p>
 * Because you supplied a list of non-tuple objects, the type you'll get back will be a <code>TableFor1</code>.
 * </p>
 *
 * <p>
 * The table provides an <code>apply</code> method that takes a function with a parameter list that matches
 * the type of the objects contained in this table. The <code>apply</code> method will invoke the
 * function with the object in each row passed as the lone argument, in ascending order by index. (<em>I.e.</em>,
 * the zeroth object is checked first, then the object with index 1, then index 2, and so on until all the rows
 * have been checked (or until a failure occurs). The function represents a property of the code under test
 * that should succeed for every row of the table. If the function returns normally, that indicates the property
 * check succeeded for that row. If the function completes abruptly with an exception, that indicates the
 * property check failed and the <code>apply</code> method will complete abruptly with a
 * <code>TableDrivenPropertyCheckFailedException</code> that wraps the exception thrown by the supplied property function.
 * </p>
 * 
 * <p>
 * The usual way you'd invoke the <code>apply</code> method that checks a property is via a <code>forAll</code> method
 * provided by trait <code>TableDrivenPropertyChecks</code>. The <code>forAll</code> method takes a <code>TableFor1</code> as its
 * first argument, then in a curried argument list takes the property check function. It invokes <code>apply</code> on
 * the <code>TableFor1</code>, passing in the property check function. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (examples) { (a) =>
 *   a should equal (a * 1)
 * }
 * </pre>
 *
 * <p>
 * Because <code>TableFor1</code> is a <code>Seq[(A)]</code>, you can use it as a <code>Seq</code>. For example, here's how
 * you could get a sequence of <a href="../Outcome.html"><code>Outcome</code></a>s for each row of the table, indicating whether a property check succeeded or failed
 * on each row of the table:
 * </p>
 *
 * <pre class="stHighlight">
 * for (row <- examples) yield {
 *   outcomeOf { row._1 should not equal (7) }
 * }
 * </pre>
 *
 * <p>
 * Note: the <code>outcomeOf</code> method, contained in the <code>OutcomeOf</code> trait, will execute the supplied code (a by-name parameter) and
 * transform it to an <code>Outcome</code>. If no exception is thrown by the code, <code>outcomeOf</code> will result in a
 * <a href="../Succeeded\$.html"><code>Succeeded</code></a>, indicating the "property check"
 * succeeded. If the supplied code completes abruptly in an exception that would normally cause a test to fail, <code>outcomeOf</code> will result in
 * in a <a href="../Failed.html"><code>Failed</code></a> instance containing that exception. For example, the previous for expression would give you:
 * </p>
 *
 * <pre class="stHighlight">
 * Vector(Succeeded, Succeeded, Succeeded, Succeeded, Succeeded, Succeeded, Succeeded,
 *     Failed(org.scalatest.TestFailedException: 7 equaled 7), Succeeded, Succeeded)
 * </pre>
 *
 * <p>
 * This shows that all the property checks succeeded, except for the one at index 7.
 * </p>
 *
 * <p>
 * One other way to use a <code>TableFor1</code> is to test subsequent return values
 * of a stateful function. Imagine, for example, you had an object named <code>FiboGen</code>
 * whose <code>next</code> method returned the <em>next</em> fibonacci number, where next
 * means the next number in the series following the number previously returned by <code>next</code>.
 * So the first time <code>next</code> was called, it would return 0. The next time it was called
 * it would return 1. Then 1. Then 2. Then 3, and so on. <code>FiboGen</code> would need to
 * be stateful, because it has to remember where it is in the series. In such a situation,
 * you could create a <code>TableFor1</code> (a table with one column, which you could alternatively
 * think of as one row), in which each row represents
 * the next value you expect.
 * </p>
 *
 * <pre class="stHighlight">
 * val first14FiboNums =
 *   Table("n", 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)
 * </pre>
 *
 * <p>
 * Then in your <code>forAll</code> simply call the function and compare it with the
 * expected return value, like this:
 * </p>
 *
 * <pre class="stHighlight">
 *  forAll (first14FiboNums) { n =>
 *    FiboGen.next should equal (n)
 *  }
 * </pre>
 *
 * @param heading a string name for the lone column of this table
 * @param rows a variable length parameter list of objects containing the data of this table
 *
 * @author Bill Venners 
 */
"""

val copyrightTemplate = """/*
 * Copyright 2001-$year$ Artima, Inc.
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
package org.scalatest
package prop
"""

val importsForTableForNTemplate = """
import scala.collection.mutable.Builder
import scala.collection.mutable.ArrayBuffer
import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import exceptions.StackDepth
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.exceptions.TableDrivenPropertyCheckFailedException
import org.scalatest.enablers.TableAsserting
import org.scalactic._
"""

val tableScaladocTemplate = """
/**
 * A table with $n$ columns.
 *
 * <p>
 * For an introduction to using tables, see the documentation for trait
 * <a href="TableDrivenPropertyChecks.html">TableDrivenPropertyChecks</a>.
 * </p>
 *
 * <p>
 * This table is a sequence of <code>Tuple$n$</code> objects, where each tuple represents one row of the table.
 * The first element of each tuple comprise the first column of the table, the second element of 
 * each tuple comprise the second column, and so on.  This table also carries with it
 * a <em>heading</em> tuple that gives string names to the columns of the table.
 * </p>
 *
 * <p>
 * A handy way to create a <code>TableFor$n$</code> is via an <code>apply</code> factory method in the <code>Table</code>
 * singleton object provided by the <code>Tables</code> trait. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val examples =
 *   Table(
 *     ($argNames$),
$columnsOfIndexes$
 *   )
 * </pre>
 *
 * <p>
 * Because you supplied $n$ members in each tuple, the type you'll get back will be a <code>TableFor$n$</code>.
 * </p>
 *
 * <p>
 * The table provides an <code>apply</code> method that takes a function with a parameter list that matches
 * the types and arity of the tuples contained in this table. The <code>apply</code> method will invoke the
 * function with the members of each row tuple passed as arguments, in ascending order by index. (<em>I.e.</em>,
 * the zeroth tuple is checked first, then the tuple with index 1, then index 2, and so on until all the rows
 * have been checked (or until a failure occurs). The function represents a property of the code under test
 * that should succeed for every row of the table. If the function returns normally, that indicates the property
 * check succeeded for that row. If the function completes abruptly with an exception, that indicates the
 * property check failed and the <code>apply</code> method will complete abruptly with a
 * <code>TableDrivenPropertyCheckFailedException</code> that wraps the exception thrown by the supplied property function.
 * </p>
 * 
 * <p>
 * The usual way you'd invoke the <code>apply</code> method that checks a property is via a <code>forAll</code> method
 * provided by trait <code>TableDrivenPropertyChecks</code>. The <code>forAll</code> method takes a <code>TableFor$n$</code> as its
 * first argument, then in a curried argument list takes the property check function. It invokes <code>apply</code> on
 * the <code>TableFor$n$</code>, passing in the property check function. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (examples) { ($alphaLower$) =>
 *   $sumOfArgs$ should equal (a * $n$)
 * }
 * </pre>
 *
 * <p>
 * Because <code>TableFor$n$</code> is a <code>Seq[($alphaUpper$)]</code>, you can use it as a <code>Seq</code>. For example, here's how
 * you could get a sequence of <a href="../Outcome.html"><code>Outcome</code></a>s for each row of the table, indicating whether a property check succeeded or failed
 * on each row of the table:
 * </p>
 *
 * <pre class="stHighlight">
 * for (row <- examples) yield {
 *   outcomeOf { row._1 should not equal (7) }
 * }
 * </pre>
 *
 * <p>
 * Note: the <code>outcomeOf</code> method, contained in the <code>OutcomeOf</code> trait, will execute the supplied code (a by-name parameter) and
 * transform it to an <code>Outcome</code>. If no exception is thrown by the code, <code>outcomeOf</code> will result in a
 * <a href="../Succeeded\$.html"><code>Succeeded</code></a>, indicating the "property check"
 * succeeded. If the supplied code completes abruptly in an exception that would normally cause a test to fail, <code>outcomeOf</code> will result in
 * in a <a href="../Failed.html"><code>Failed</code></a> instance containing that exception. For example, the previous for expression would give you:
 * </p>
 *
 * <pre class="stHighlight">
 * Vector(Succeeded, Succeeded, Succeeded, Succeeded, Succeeded, Succeeded, Succeeded,
 *     Failed(org.scalatest.TestFailedException: 7 equaled 7), Succeeded, Succeeded)
 * </pre>
 *
 * <p>
 * This shows that all the property checks succeeded, except for the one at index 7.
 * </p>
 *
 * @param heading a tuple containing string names of the columns in this table
 * @param rows a variable length parameter list of <code>Tuple$n$</code>s containing the data of this table
 *
 * @author Bill Venners 
 */
"""

val tableTemplate = """
class TableFor$n$[$alphaUpper$](val heading: ($strings$), rows: ($alphaUpper$)*) extends IndexedSeq[($alphaUpper$)] with IndexedSeqLike[($alphaUpper$), TableFor$n$[$alphaUpper$]] {

  /**
   * Selects a row of data by its index.
   */
  def apply(idx: Int): ($alphaUpper$) = rows(idx)

  /**
   * The number of rows of data in the table. (This does not include the <code>heading</code> tuple)
   */
  def length: Int = rows.length

  /**
   * Creates a new <code>Builder</code> for <code>TableFor$n$</code>s.
   */
  override protected[this] def newBuilder: Builder[($alphaUpper$), TableFor$n$[$alphaUpper$]] =
    new ArrayBuffer mapResult { (buf: Seq[($alphaUpper$)]) =>
      new TableFor$n$(heading, buf: _*)
    }

  /**
   * Applies the passed property check function to each row of this <code>TableFor$n$</code>.
   *
   * <p>
   * If the property checks for all rows succeed (the property check function returns normally when passed
   * the data for each row), this <code>apply</code> method returns normally. If the property check function
   * completes abruptly with an exception for any row, this <code>apply</code> method wraps that exception
   * in a <code>TableDrivenPropertyCheckFailedException</code> and completes abruptly with that exception. Once
   * the property check function throws an exception for a row, this <code>apply</code> method will complete
   * abruptly immediately and subsequent rows will not be checked against the function.
   * </p>
   *
   * @param fun the property check function to apply to each row of this <code>TableFor$n$</code>
   */
  def apply[ASSERTION](fun: ($alphaUpper$) => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forAll(heading, rows: _*)(fun)
  }

  def forEvery[ASSERTION](fun: ($alphaUpper$) => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.forEvery(heading, rows: _*)(fun)
  }

  def exists[ASSERTION](fun: ($alphaUpper$) => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    asserting.exists(heading, rows: _*)(fun)
  }

  /**
   * A string representation of this object, which includes the heading strings as well as the rows of data.
   */
  override def toString: String = stringPrefix + "(" + heading.toString + ", " +  rows.mkString(", ") + ")"
}

/**
 * Companion object for class <code>TableFor$n$</code> that provides an implicit <code>canBuildFrom</code> method
 * that enables higher order functions defined on <code>TableFor$n$</code> to return another <code>TableFor$n$</code>.
 *
 * @author Bill Venners 
 */
object TableFor$n$ {

  /**
   * Implicit method enabling higher order functions of <code>TableFor$n$</code> to return sequences of type <code>TableFor$n$</code>.
   */
  implicit def canBuildFrom[$alphaUpper$]: CanBuildFrom[TableFor$n$[$alphaUpper$], ($alphaUpper$), TableFor$n$[$alphaUpper$]] =
    new CanBuildFrom[TableFor$n$[$alphaUpper$], ($alphaUpper$), TableFor$n$[$alphaUpper$]] {
      def apply(): Builder[($alphaUpper$), TableFor$n$[$alphaUpper$]] =
        new ArrayBuffer mapResult { (buf: Seq[($alphaUpper$)]) =>
          new TableFor$n$(($argsNamedArg$))
        }
      def apply(from: TableFor$n$[$alphaUpper$]): Builder[($alphaUpper$), TableFor$n$[$alphaUpper$]] =
        new ArrayBuffer mapResult { (buf: Seq[($alphaUpper$)]) =>
          new TableFor$n$(from.heading, buf: _*)
        }
    }
}
"""

val tableObjectPreamble = """
/**
 * Trait containing the <code>Table</code> object, which offers one <code>apply</code> factory method for
 * each <code>TableForN</code> class, <code>TableFor1</code> through <code>TableFor22</code>.
 * 
 * <p>
 * For an introduction to using tables, see the documentation for trait
 * <a href="TableDrivenPropertyChecks.html">TableDrivenPropertyChecks</a>.
 * </p>
 *
 * @author Bill Venners
 */
trait Tables {

  /**
   * Object containing one <code>apply</code> factory method for each <code>TableFor&lt;n&gt;</code> class.
   * 
   * <p>
   * For example, you could create a table of 5 rows and 2 colums like this:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.prop.Tables._
   *
   * val examples =
   *   Table(
   *     ("a", "b"),
   *     (  1,   2),
   *     (  2,   4),
     *     (  4,   8),
   *     (  8,  16),
   *     ( 16,  32)
   *   )
   * </pre>
   *
   * <p>
   * Because you supplied 2 members in each tuple, the type you'll get back will be a <code>TableFor2</code>. If
   * you wanted a table with just one column you could write this:
   * </p>
   *
   * <pre class="stHighlight">
   * val moreExamples =
   *   Table(
   *     "powerOfTwo",
   *          1,
   *          2,
   *          4,
   *          8,
   *          16
   *   )
   * </pre>
   *
   * <p>
   * Or if you wanted a table with 10 columns and 10 rows, you could do this:
   * </p>
   *
   * <pre class="stHighlight">
   * val multiplicationTable =
   *   Table(
   *     ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
   *     (  1,   2,   3,   4,   5,   6,   7,   8,   9,  10),
   *     (  2,   4,   6,   8,  10,  12,  14,  16,  18,  20),
   *     (  3,   6,   9,  12,  15,  18,  21,  24,  27,  30),
   *     (  4,   8,  12,  16,  20,  24,  28,  32,  36,  40),
   *     (  5,  10,  15,  20,  25,  30,  35,  40,  45,  50),
   *     (  6,  12,  18,  24,  30,  36,  42,  48,  54,  60),
   *     (  7,  14,  21,  28,  35,  42,  49,  56,  63,  70),
   *     (  8,  16,  24,  32,  40,  48,  56,  64,  72,  80),
   *     (  9,  18,  27,  36,  45,  54,  63,  72,  81,  90),
   *     ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100)
   *   )
   * </pre>
   *
   * <p>
   * The type of <code>multiplicationTable</code> would be <code>TableFor10</code>. You can pass the resulting
   * tables to a <code>forAll</code> method (defined in trait <code>PropertyChecks</code>), to perform a property
   * check with the data in the table. Or, because tables are sequences of tuples, you can treat them as a <code>Seq</code>.
   * </p>
   * 
   * @author Bill Venners
   */
  object Table {
"""

val tableObjectApplyTemplate = """
      /**
       * Factory method for creating a new <code>TableFor$n$</code>.
       *
       * @param heading a tuple containing string names of the columns in this table
       * @param rows a variable length parameter list of <code>Tuple$n$</code>s containing the data of this table
       */
      def apply[$alphaUpper$](heading: ($strings$), rows: ($alphaUpper$)*) =
        new TableFor$n$(heading, rows: _*)
"""

val tablesCompanionObjectVerbatimString = """
/**
 * Companion object that facilitates the importing of <code>Tables</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Tables</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre>
 * Welcome to Scala version 2.8.0.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_22).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala> import org.scalatest.prop.Tables._
 * import org.scalatest.prop.Tables._
 * 
 * scala> val examples =
 *   |   Table(
 *   |     ("a", "b"),
 *   |     (  1,   2),
 *   |     (  3,   4)
 *   |   )
 * examples: org.scalatest.prop.TableFor2[Int,Int] = TableFor2((1,2), (3,4))
 * </pre>
 *
 * @author Bill Venners
 */
object Tables extends Tables
"""

val propertyCheckPreamble = """
import exceptions.StackDepth
import scala.annotation.tailrec
import org.scalatest.enablers.TableAsserting
import org.scalactic._

/**
 * Trait containing methods that faciliate property checks against tables of data.
 *
 * <p>
 * This trait contains one <code>exists</code>, <code>forAll</code>, and <code>forEvery</code> method for each <code>TableForN</code> class, <code>TableFor1</code>
 * through <code>TableFor22</code>, which allow properties to be checked against the rows of a table. It also
 * contains a <code>wherever</code> method that can be used to indicate a property need only hold whenever some
 * condition is true.
 * </p>
 *
 * <p>
 * For an example of trait <code>TableDrivenPropertyChecks</code> in action, imagine you want to test this <code>Fraction</code> class:
 * </p>
 *  
 * <pre class="stHighlight">
 * class Fraction(n: Int, d: Int) {
 *
 *   require(d != 0)
 *   require(d != Integer.MIN_VALUE)
 *   require(n != Integer.MIN_VALUE)
 *
 *   val numer = if (d < 0) -1 * n else n
 *   val denom = d.abs
 *
 *   override def toString = numer + " / " + denom
 * }
 * </pre>
 *
 * <p>
 * <code>TableDrivenPropertyChecks</code> allows you to create tables with
 * between 1 and 22 columns and any number of rows. You create a table by passing
 * tuples to one of the factory methods of object <code>Table</code>. Each tuple must have the 
 * same arity (number of members). The first tuple you pass must all be strings, because
 * it define names for the columns. Subsequent tuples define the data. After the initial tuple
 * that contains string column names, all tuples must have the same type. For example,
 * if the first tuple after the column names contains two <code>Int</code>s, all subsequent
 * tuples must contain two <code>Int</code> (<em>i.e.</em>, have type
 * <code>Tuple2[Int, Int]</code>).
 * </p>
 *
 * <p>
 * To test the behavior of <code>Fraction</code>, you could create a table
 * of numerators and denominators to pass to the constructor of the
 * <code>Fraction</code> class using one of the <code>apply</code> factory methods declared
 * in <code>Table</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.prop.TableDrivenPropertyChecks._
 *
 * val fractions =
 *   Table(
 *     ("n", "d"),  // First tuple defines column names
 *     (  1,   2),  // Subsequent tuples define the data
 *     ( -1,   2),
 *     (  1,  -2),
 *     ( -1,  -2),
 *     (  3,   1),
 *     ( -3,   1),
 *     ( -3,   0),
 *     (  3,  -1),
 *     (  3,  Integer.MIN_VALUE),
 *     (Integer.MIN_VALUE, 3),
 *     ( -3,  -1)
 *   )
 * </pre>
 *
 * <p>
 * You could then check a property against each row of the table using a <code>forAll</code> method, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.Matchers._
 *
 * forAll (fractions) { (n: Int, d: Int) =>
 *
 *   whenever (d != 0 && d != Integer.MIN_VALUE
 *       && n != Integer.MIN_VALUE) {
 *
 *     val f = new Fraction(n, d)
 *
 *     if (n < 0 && d < 0 || n > 0 && d > 0)
 *       f.numer should be > 0
 *     else if (n != 0)
 *       f.numer should be < 0
 *     else
 *       f.numer should be === 0
 *
 *     f.denom should be > 0
 *   }
 * }
 * </pre>
 *
 * <p>
 * Trait <code>TableDrivenPropertyChecks</code> provides 22 overloaded <code>exists</code>, <code>forAll</code>, and <code>forEvery</code> methods
 * that allow you to check properties using the data provided by a table. Each <code>exists</code>, <code>forAll</code>, and <code>forEvery</code>
 * method takes two parameter lists. The first parameter list is a table. The second parameter list
 * is a function whose argument types and number matches that of the tuples in the table. For
 * example, if the tuples in the table supplied to <code>forAll</code> each contain an
 * <code>Int</code>, a <code>String</code>, and a <code>List[Char]</code>, then the function supplied
 * to <code>forAll</code> must take 3 parameters, an <code>Int</code>, a <code>String</code>,
 * and a <code>List[Char]</code>. The <code>forAll</code> method will pass each row of data to
 * the function, and generate a <code>TableDrivenPropertyCheckFailedException</code> if the function
 * completes abruptly for any row of data with any exception that would <a href="../Suite.html#errorHandling">normally cause</a> a test to
 * fail in ScalaTest other than <code>DiscardedEvaluationException</code>. A
 * <code>DiscardedEvaluationException</code>,
 * which is thrown by the <code>whenever</code> method (also defined in this trait) to indicate
 * a condition required by the property function is not met by a row
 * of passed data, will simply cause <code>forAll</code> to skip that row of data.
 * <p>
 *
 * <p>
 * The full list of table methods are:
 * </p>
 *
 * <ul>
 * <li><code>exists</code> - succeeds if the assertion holds true for at least one element</li>
 * <li><code>forAll</code> - succeeds if the assertion holds true for every element</li>
 * <li><code>forEvery</code> - same as <code>forAll</code>, but lists all failing elements if it fails (whereas
 *    <code>forAll</code> just reports the first failing element) and throws <code>TestFailedException</code> with
 *    the first failed check as the cause.</li>
 * </ul>
 *
 * <a name="testingStatefulFunctions"></a><h2>Testing stateful functions</h2>
 *
 * <p>
 * One way to use a table with one column is to test subsequent return values
 * of a stateful function. Imagine, for example, you had an object named <code>FiboGen</code>
 * whose <code>next</code> method returned the <em>next</em> fibonacci number, where next
 * means the next number in the series following the number previously returned by <code>next</code>.
 * So the first time <code>next</code> was called, it would return 0. The next time it was called
 * it would return 1. Then 1. Then 2. Then 3, and so on. <code>FiboGen</code> would need to
 * maintain state, because it has to remember where it is in the series. In such a situation,
 * you could create a <code>TableFor1</code> (a table with one column, which you could alternatively
 * think of as one row), in which each row represents
 * the next value you expect.
 * </p>
 *
 * <pre class="stHighlight">
 * val first14FiboNums =
 *   Table("n", 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233)
 * </pre>
 *
 * <p>
 * Then in your <code>forAll</code> simply call the function and compare it with the
 * expected return value, like this:
 * </p>
 *
 * <pre class="stHighlight">
 *  forAll (first14FiboNums) { n =>
 *    FiboGen.next should equal (n)
 *  }
 * </pre>
 *
 * <a name="testingMutables"></a><h2>Testing mutable objects</h2>
 *
 * <p>
 * If you need to test a mutable object, one way you can use tables is to specify
 * state transitions in a table. For example, imagine you wanted to test this mutable
 * <code>Counter</code> class:
 *
 * <pre class="stHighlight">
      class Counter {
        private var c = 0
        def reset() { c = 0 }
        def click() { c += 1 }
        def enter(n: Int) { c = n }
        def count = c
      }
 * </pre>
 *
 * <p>
 * A <code>Counter</code> keeps track of how many times its <code>click</code> method
 * is called. The count starts out at zero and increments with each <code>click</code>
 * invocation. You can also set the count to a specific value by calling <code>enter</code>
 * and passing the value in. And the <code>reset</code> method returns the count back to
 * zero. You could define the actions that initiate state transitions with case classes, like this:
 * </p>
 *
 * <pre class="stHighlight">
      abstract class Action
      case object Start extends Action
      case object Click extends Action
      case class Enter(n: Int) extends Action
 * </pre>
 *
 * <p>
 * Given these actions, you could define a state-transition table like this:
 * </p>
 *
 * <pre class="stHighlight">
      val stateTransitions =
        Table(
          ("action", "expectedCount"),
          (Start,    0),
          (Click,    1),
          (Click,    2),
          (Click,    3),
          (Enter(5), 5),
          (Click,    6),
          (Enter(1), 1),
          (Click,    2),
          (Click,    3)
        )
 * </pre>
 *
 * <p>
 * To use this in a test, simply do a pattern match inside the function you pass
 * to <code>forAll</code>. Make a pattern for each action, and have the body perform that
 * action when there's a match. Then check that the actual value equals the expected value:
 * </p>
 *
 * <pre class="stHighlight">
      val counter = new Counter
      forAll (stateTransitions) { (action, expectedCount) =>
        action match {
          case Start => counter.reset()
          case Click => counter.click()
          case Enter(n) => counter.enter(n)
        }
        counter.count should equal (expectedCount)
      }
 * </pre>
 *
 * <a name="invalidArgCombos"></a><h2>Testing invalid argument combinations</h2>
 * 
 * <p>
 * A table-driven property check can also be helpful to ensure that the proper exception is thrown when invalid data is
 * passed to a method or constructor. For example, the <code>Fraction</code> constructor shown above should throw <code>IllegalArgumentException</code>
 * if <code>Integer.MIN_VALUE</code> is passed for either the numerator or denominator, or zero is passed for the denominator. This yields the
 * following five combinations of invalid data:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><code>n</code></th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black"><code>d</code></th></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Integer.MIN_VALUE</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Integer.MIN_VALUE</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">a valid value</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Integer.MIN_VALUE</code></td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Integer.MIN_VALUE</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">a valid value</td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Integer.MIN_VALUE</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">zero</td></tr>
 * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">a valid value</td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center">zero</td></tr>
 * </table>
 *
 * <p>
 * You can express these combinations in a table:
 * </p>
 *
 * <pre class="stHighlight">
 * val invalidCombos =
 *   Table(
 *     ("n",               "d"),
 *     (Integer.MIN_VALUE, Integer.MIN_VALUE),
 *     (1,                 Integer.MIN_VALUE),
 *     (Integer.MIN_VALUE, 1),
 *     (Integer.MIN_VALUE, 0),
 *     (1,                 0)
 *   )
 * </pre>
 * 
 * <p>
 * Given this table, you could check that all invalid combinations produce <code>IllegalArgumentException</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * forAll (invalidCombos) { (n: Int, d: Int) =>
 *   evaluating {
 *     new Fraction(n, d)
 *   } should produce [IllegalArgumentException]
 * }
 * </pre>
 *
 * </p>
 * @author Bill Venners
 */
trait TableDrivenPropertyChecks extends Whenever with Tables {

  /*
   * Evaluates the passed code block if the passed boolean condition is true, else throws <code>DiscardedEvaluationException</code>.
   *
   * <p>
   * The <code>whenever</code> method can be used inside property check functions to skip invocations of the function with
   * data for which it is known the property would fail. For example, given the following <code>Fraction</code> class:
   * </p>
   *
   * <pre class="stHighlight">
   * class Fraction(n: Int, d: Int) {
   *
   *   require(d != 0)
   *   require(d != Integer.MIN_VALUE)
   *   require(n != Integer.MIN_VALUE)
   *
   *   val numer = if (d < 0) -1 * n else n
   *   val denom = d.abs
   *
   *   override def toString = numer + " / " + denom
   * }
   * </pre>
   *
   * <p>
   * You could create a table of numerators and denominators to pass to the constructor of the
   * <code>Fraction</code> class like this:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.prop.TableDrivenPropertyChecks._
   *
   * val fractions =
   *   Table(
   *     ("n", "d"),
   *     (  1,   2),
   *     ( -1,   2),
   *     (  1,  -2),
   *     ( -1,  -2),
   *     (  3,   1),
   *     ( -3,   1),
   *     ( -3,   0),
   *     (  3,  -1),
   *     (  3,  Integer.MIN_VALUE),
   *     (Integer.MIN_VALUE, 3),
   *     ( -3,  -1)
   *   )
   * </pre>
   *
   * <p>
   * Imagine you wanted to check a property against this class with data that includes some
   * value that are rejected by the constructor, such as a denominator of zero, which should
   * result in an <code>IllegalArgumentException</code>. You could use <code>whenever</code>
   * to skip any rows in the <code>fraction</code> that represent illegal arguments, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.Matchers._
   *
   * forAll (fractions) { (n: Int, d: Int) =>
   *
   *   whenever (d != 0 && d != Integer.MIN_VALUE
   *       && n != Integer.MIN_VALUE) {
   *
   *     val f = new Fraction(n, d)
   *
   *     if (n < 0 && d < 0 || n > 0 && d > 0)
   *       f.numer should be > 0
   *     else if (n != 0)
   *       f.numer should be < 0
   *     else
   *       f.numer should be === 0
   *
   *     f.denom should be > 0
   *   }
   * }
   * </pre>
   *
   * <p>
   * In this example, rows 6, 8, and 9 have values that would cause a false to be passed 
   * to <code>whenever</code>. (For example, in row 6, <code>d</code> is 0, which means <code>d</code> <code>!=</code> <code>0</code>
   * will be false.) For those rows, <code>whenever</code> will throw <code>DiscardedEvaluationException</code>,
   * which will cause the <code>forAll</code> method to skip that row.
   * </p>
   *
   * @param condition the boolean condition that determines whether <code>whenever</code> will evaluate the 
   *    <code>fun</code> function (<code>condition<code> is true) or throws <code>DiscardedEvaluationException</code> (<code>condition<code> is false)
   * @param fun the function to evaluate if the specified <code>condition</code> is true
   */
/*
  def whenever(condition: Boolean)(fun: => Unit) {
    if (!condition)
      throw new DiscardedEvaluationException
    fun
  }
*/
"""

val propertyCheckForAllTemplate = """
  /**
   * Performs a property check by applying the specified property check function to each row
   * of the specified <code>TableFor$n$</code>.
   *
   * @param table the table of data with which to perform the property check
   * @param fun the property check function to apply to each row of data in the table
   */
  def forAll[$alphaUpper$, ASSERTION](table: TableFor$n$[$alphaUpper$])(fun: ($alphaUpper$) => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    table(fun)
  }
"""

val propertyCheckForEveryTemplateFor1 = """
  /**
   * Performs a property check by applying the specified property check function to each row
   * of the specified <code>TableFor1</code> and reporting every error.
   *
   * <p>
   * The difference between <code>forEvery</code> and <code>forAll</code> is that
   * <code>forEvery</code> will continue to inspect all elements after first failure, and report all failures,
   * whereas <code>forAll</code> will stop on (and only report) the first failure.
   * </p>
   *
   * @param table the table of data with which to perform the property check
   * @param fun the property check function to apply to each row of data in the table
   */
  def forEvery[A, ASSERTION](table: TableFor1[A])(fun: A => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    table.forEvery(fun)
    //asserting.forEvery[Tuple1[A], ASSERTION](table.heading, table.map(Tuple1.apply)){a => fun(a._1)}
  }

"""

val propertyCheckForEveryTemplate = """
  /**
   * Performs a property check by applying the specified property check function to each row
   * of the specified <code>TableFor$n$</code> and reporting every error.
   *
   * <p>
   * The difference between <code>forEvery</code> and <code>forAll</code> is that
   * <code>forEvery</code> will continue to inspect all elements after first failure, and report all failures,
   * whereas <code>forAll</code> will stop on (and only report) the first failure.
   * </p>
   *
   * @param table the table of data with which to perform the property check
   * @param fun the property check function to apply to each row of data in the table
   */
  def forEvery[$alphaUpper$, ASSERTION](table: TableFor$n$[$alphaUpper$])(fun: ($alphaUpper$) => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    table.forEvery(fun)
    //asserting.forEvery[($alphaUpper$), ASSERTION](table.heading, table)(fun.tupled)
  }
"""

  val propertyCheckExistsTemplateFor1 = """
  /**
   * Performs a property check by applying the specified property check function to each row
   * of the specified <code>TableFor1</code> and succeeding if at least one element satisfies the property check.
   *
   * @param table the table of data with which to perform the property check
   * @param fun the property check function to apply to each row of data in the table
   */
  def exists[A, ASSERTION](table: TableFor1[A])(fun: A => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    table.exists(fun)
    //asserting.exists[Tuple1[A], ASSERTION](List(table.heading), table.map(Tuple1.apply), Resources.tableDrivenExistsFailed _, "TableDrivenPropertyChecks.scala", "exists", 3){a => fun(a._1)}
  }

                                          """

  val propertyCheckExistsTemplate = """
  /**
   * Performs a property check by applying the specified property check function to each row
   * of the specified <code>TableFor$n$</code> and succeeding if at least one element satisfies the property check.
   *
   * @param table the table of data with which to perform the property check
   * @param fun the property check function to apply to each row of data in the table
   */
  def exists[$alphaUpper$, ASSERTION](table: TableFor$n$[$alphaUpper$])(fun: ($alphaUpper$) => ASSERTION)(implicit asserting: TableAsserting[ASSERTION], prettifier: Prettifier, pos: source.Position): asserting.Result = {
    table.exists(fun)
    //asserting.exists[($alphaUpper$), ASSERTION](table.heading.productIterator.to[List].map(_.toString), table, Resources.tableDrivenExistsFailed _, "$filename$", "exists", 3)(fun.tupled)
  }
                                      """


val tableDrivenPropertyChecksCompanionObjectVerbatimString = """
/*
 * Companion object that facilitates the importing of <code>TableDrivenPropertyChecks</code> members as 
 * an alternative to mixing it in. One use case is to import <code>TableDrivenPropertyChecks</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre>
 * Welcome to Scala version 2.8.0.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_22).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala> import org.scalatest.prop.TableDrivenPropertyChecks._
 * import org.scalatest.prop.TableDrivenPropertyChecks._
 *
 * scala> val examples =                                       
 *   |   Table(                                             
 *   |     ("a", "b"),                                      
 *   |     (  1,   2),                                      
 *   |     (  3,   4)                                       
 *   |   )
 * examples: org.scalatest.prop.TableFor2[Int,Int] = TableFor2((1,2), (3,4))
 *
 * scala> import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
 *
 * scala> forAll (examples) { (a, b) => a should be < b }
 * 
 * scala> forAll (examples) { (a, b) => a should be > b }
 * org.scalatest.prop.TableDrivenPropertyCheckFailedException: TestFailedException (included as this exception's cause) was thrown during property evaluation.
 * Message: 1 was not greater than 2
 * Location: <console>:13
 * Occurred at table row 0 (zero based, not counting headings), which had values (
 *   a = 1,
 *   b = 2
 * )
 * at org.scalatest.prop.TableFor2$$anonfun$apply$4.apply(Table.scala:355)
 * at org.scalatest.prop.TableFor2$$anonfun$apply$4.apply(Table.scala:346)
 * at scala.collection.mutable.ResizableArray$class.foreach(ResizableArray.scala:57)
 * at scala.collection.mutable.ArrayBuffer.foreach(ArrayBuffer.scala:43)
 * at org.scalatest.prop.TableFor2.apply(Table.scala:346)
 * at org.scalatest.prop.TableDrivenPropertyChecks$class.forAll(TableDrivenPropertyChecks.scala:133)
 * ...
 * </pre>
 *
 * @author Bill Venners
 */
object TableDrivenPropertyChecks extends TableDrivenPropertyChecks
"""

val tableSuitePreamble = """

import org.scalatest.Matchers._
import org.scalatest.exceptions.TableDrivenPropertyCheckFailedException
import org.scalatest.refspec.RefSpec

class TableSuite extends RefSpec with TableDrivenPropertyChecks {
"""

val tableSuiteTemplate = """
  def `table forAll $n$ that succeeds` {

    val examples =
      Table(
        ($argNames$),
$columnsOfOnes$
      )

    forAll (examples) { ($names$) => assert($sumOfArgs$ === ($n$)) }
  }

  def `table forAll $n$, which succeeds even though DiscardedEvaluationException is thrown` {
    val numbers =
      Table(
        ($argNames$),
$columnOfMinusOnes$
$columnsOfOnes$
      )

    forAll (numbers) { ($names$) =>

      whenever (a > 0) {
        assert(a > 0)
      }
    }
  }

  def `table forAll $n$, which fails` {

    val examples =
      Table(
        ($argNames$),
$columnsOfTwos$
      )

    intercept[TableDrivenPropertyCheckFailedException] {
      forAll (examples) { ($names$) => assert($sumOfArgs$ === ($n$)) }
    }
  }

  def `table forEvery $n$ that succeeds` {

    val examples =
      Table(
        ($argNames$),
$columnsOfOnes$
      )

    forEvery (examples) { ($names$) => assert($sumOfArgs$ === ($n$)) }
  }

  def `table forEvery $n$, which succeeds even though DiscardedEvaluationException is thrown` {
    val numbers =
      Table(
        ($argNames$),
$columnOfMinusOnes$
$columnsOfOnes$
      )

    forEvery (numbers) { ($names$) =>
      whenever (a > 0) {
        assert(a > 0)
      }
    }
  }

  def `table forEvery $n$, which fails` {
    val examples =
      Table(
        ($argNames$),
$columnsOfTwos$
      )

    intercept[exceptions.TestFailedException] {
      forEvery (examples) { ($names$) => assert($sumOfArgs$ === ($n$)) }
    }
  }

  def `table exists $n$ that succeeds` {

    val examples =
      Table(
        ($argNames$),
$columnOfMinusOnes$
$columnsOfOnes$
      )

    exists (examples) { ($names$) => assert($sumOfArgs$ === ($n$)) }
  }

  def `table exists $n$, which succeeds even though DiscardedEvaluationException is thrown` {
    val numbers =
      Table(
        ($argNames$),
$columnOfMinusOnes$
$columnsOfOnes$
      )

    exists (numbers) { ($names$) =>
      whenever (a > 0) {
        assert(a > 0)
      }
    }
  }

  def `table exists $n$, which fails` {
    val examples =
      Table(
        ($argNames$),
$columnsOfTwos$
      )

    intercept[exceptions.TestFailedException] {
      exists (examples) { ($names$) => assert($sumOfArgs$ === ($n$)) }
    }
  }

  def `table for $n$ apply, length, and iterator methods work correctly` {

    val examples =
      Table(
        ($argNames$),
$columnsOfIndexes$
      )

    for (i <- 0 to 9) {
      assert(examples(i) === ($listOfIs$))
    }

    assert(examples.length === (10))

    var i = 0
    for (example <- examples.iterator) {
      assert(example === ($listOfIs$))
      i += 1
    }

    assert(examples.iterator.length === (10))
  }
"""

// For some reason that I don't understand, I need to leave off the stars before the <pre> when 
// they are next to ST commands. So I say  "   <pre>" sometimes instead of " * <pre>".

  def transform(content: String): String = {
    var skipMode = false
    content.split("\n").map { line =>
      if (line.trim == "// SKIP-SCALATESTJS-START")
        skipMode = true
      else if (line.trim == "// SKIP-SCALATESTJS-END")
        skipMode = false
      else if (!skipMode) {
        if (line.trim.startsWith("//SCALATESTJS-ONLY "))
          line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19)
        else
          line
      }
      else
        ""
    }.mkString("\n")
  }

  val thisYear = Calendar.getInstance.get(Calendar.YEAR)

  def genTableForNs(targetDir: File, scalaJS: Boolean) {

    val bw = new BufferedWriter(new FileWriter(new File(targetDir, "TableFor1.scala")))
 
    try {
      val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
      st.setAttribute("year", thisYear);
      bw.write(st.toString)
      val imports = new org.antlr.stringtemplate.StringTemplate(importsForTableForNTemplate)
      bw.write(imports.toString)
      val alpha = "abcdefghijklmnopqrstuv"
      for (i <- 1 to 22) {
        val st = new org.antlr.stringtemplate.StringTemplate(
          (if (i == 1) scaladocForTableFor1VerbatimString else tableScaladocTemplate) + tableTemplate
        )
        val alphaLower = alpha.take(i).mkString(", ")
        val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
        val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
        val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
        val strings = List.fill(i)("String").mkString(", ")
        val argsNamedArgSeq =
          for (argsIdx <- 0 until i) yield
            "\"" + "arg" + argsIdx + "\""
        val argsNamedArg = argsNamedArgSeq.mkString(",")                                  
        val sumOfArgs = alpha.take(i).mkString(" + ")
        val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
        val rawRows =                              
          for (idx <- 0 to 9) yield                
            List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
        val columnsOfIndexes = rawRows.mkString(",\n")
        st.setAttribute("n", i)
        st.setAttribute("alphaLower", alphaLower)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("alphaName", alphaName)
        st.setAttribute("strings", strings)
        st.setAttribute("argsNamedArg", argsNamedArg)
        st.setAttribute("namesAndValues", namesAndValues)
        st.setAttribute("sumOfArgs", sumOfArgs)
        st.setAttribute("argNames", argNames)
        st.setAttribute("columnsOfIndexes", columnsOfIndexes)
        if (scalaJS)
          bw.write(transform(st.toString))
        else
          bw.write(st.toString)
      }
    }
    finally {
      bw.close()
    }
  }

  def genPropertyChecks(targetDir: File) {
    val filename = "TableDrivenPropertyChecks.scala"
    val bw = new BufferedWriter(new FileWriter(new File(targetDir, filename)))

    try {
      val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
      st.setAttribute("year", thisYear);
      bw.write(st.toString)
      bw.write(propertyCheckPreamble)
      val alpha = "abcdefghijklmnopqrstuv"
      for (i <- 1 to 22) {
        val st = new org.antlr.stringtemplate.StringTemplate(propertyCheckForAllTemplate)
        val alphaLower = alpha.take(i).mkString(", ")
        val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
        val strings = List.fill(i)("String").mkString(", ")
        st.setAttribute("n", i)
        st.setAttribute("alphaLower", alphaLower)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("strings", strings)
        st.setAttribute("filename", filename)
        bw.write(st.toString)
      }

      for (i <- 1 to 22) {
        val template = if (i == 1) propertyCheckForEveryTemplateFor1 else propertyCheckForEveryTemplate
        val st = new org.antlr.stringtemplate.StringTemplate(template)
        val alphaLower = alpha.take(i).mkString(", ")
        val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
        val strings = List.fill(i)("String").mkString(", ")
        st.setAttribute("n", i)
        st.setAttribute("alphaLower", alphaLower)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("strings", strings)
        st.setAttribute("filename", filename)
        bw.write(st.toString)
      }

      for (i <- 1 to 22) {
        val template = if (i == 1) propertyCheckExistsTemplateFor1 else propertyCheckExistsTemplate
        val st = new org.antlr.stringtemplate.StringTemplate(template)
        val alphaLower = alpha.take(i).mkString(", ")
        val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
        val strings = List.fill(i)("String").mkString(", ")
        st.setAttribute("n", i)
        st.setAttribute("alphaLower", alphaLower)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("strings", strings)
        st.setAttribute("filename", filename)
        bw.write(st.toString)
      }

      bw.write("}\n")
      bw.write(tableDrivenPropertyChecksCompanionObjectVerbatimString)
    }
    finally {
      bw.close()
    }
  }

  def genTables(targetDir: File) {

    val bw = new BufferedWriter(new FileWriter(new File(targetDir, "Tables.scala")))

    try {
      val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
      st.setAttribute("year", thisYear);
      bw.write(st.toString)
      bw.write(tableObjectPreamble)
      val alpha = "abcdefghijklmnopqrstuv"
      for (i <- 1 to 22) {
        val st = new org.antlr.stringtemplate.StringTemplate(tableObjectApplyTemplate)
        val alphaLower = alpha.take(i).mkString(", ")
        val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
        val strings = List.fill(i)("String").mkString(", ")
        st.setAttribute("n", i)
        st.setAttribute("alphaLower", alphaLower)
        st.setAttribute("alphaUpper", alphaUpper)
        st.setAttribute("strings", strings)
        bw.write(st.toString)
      }

      bw.write("  }\n")
      bw.write("}\n")
      bw.write(tablesCompanionObjectVerbatimString)
    }
    finally {
      bw.close()
    }
  }

  def genTableAsserting(targetDir: File, scalaJS: Boolean): Unit = {

    val doForAllMethodTemplate: String =
      """/**
        | * Implementation method for [[org.scalatest.prop.TableDrivenPropertyChecks TableDrivenPropertyChecks]]'s <code>forAll</code> syntax.
        | */
        |def forAll[$alphaUpper$](heading: ($strings$), rows: ($alphaUpper$)*)(fun: ($alphaUpper$) => ASSERTION)(implicit prettifier: Prettifier, pos: source.Position): Result
      """.stripMargin

    def doForAllMethod(i: Int): String = {
      val alpha = "abcdefghijklmnopqrstuv"

      val st = new org.antlr.stringtemplate.StringTemplate(doForAllMethodTemplate)
      val alphaLower = alpha.take(i).mkString(", ")
      val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
      val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
      val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
      val strings = List.fill(i)("String").mkString(", ")
      val argsNamedArgSeq =
        for (argsIdx <- 0 until i) yield
        "\"" + "arg" + argsIdx + "\""
      val argsNamedArg = argsNamedArgSeq.mkString(",")
      val sumOfArgs = alpha.take(i).mkString(" + ")
      val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
      val rawRows =
        for (idx <- 0 to 9) yield
        List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
      val columnsOfIndexes = rawRows.mkString(",\n")
      st.setAttribute("n", i)
      st.setAttribute("alphaLower", alphaLower)
      st.setAttribute("alphaUpper", alphaUpper)
      st.setAttribute("alphaName", alphaName)
      st.setAttribute("strings", strings)
      st.setAttribute("argsNamedArg", argsNamedArg)
      st.setAttribute("namesAndValues", namesAndValues)
      st.setAttribute("sumOfArgs", sumOfArgs)
      st.setAttribute("argNames", argNames)
      st.setAttribute("columnsOfIndexes", columnsOfIndexes)
      if (scalaJS)
        transform(st.toString)
      else
        st.toString
    }

    def doForAllMethodImpl(i: Int): String = {
      val forAllImplTemplate: String =
        doForAllMethodTemplate + """ = {
          |  for ((($alphaLower$), idx) <- rows.zipWithIndex) {
          |    try {
          |      fun($alphaLower$)
          |    }
          |    catch {
          |      case _: DiscardedEvaluationException => // discard this evaluation and move on to the next
          |      case ex: Throwable =>
          |        val ($alphaName$) = heading
          |
          |        // SKIP-SCALATESTJS-START
          |        val stackDepth = 2
          |        // SKIP-SCALATESTJS-END
          |        //SCALATESTJS-ONLY val stackDepth = 1
          |
          |        indicateFailure(
          |          (sde: StackDepthException) => FailureMessages.propertyException(prettifier, UnquotedString(ex.getClass.getSimpleName)) +
          |            ( sde.failedCodeFileNameAndLineNumberString match { case Some(s) => " (" + s + ")"; case None => "" }) + "\n" +
          |            "  " + FailureMessages.thrownExceptionsMessage(prettifier, if (ex.getMessage == null) "None" else UnquotedString(ex.getMessage)) + "\n" +
          |            (
          |              ex match {
          |                case sd: StackDepth if sd.failedCodeFileNameAndLineNumberString.isDefined =>
          |                  "  " + FailureMessages.thrownExceptionsLocation(prettifier, UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + "\n"
          |                case _ => ""
          |              }
          |            ) +
          |            "  " + FailureMessages.occurredAtRow(prettifier, idx) + "\n" +
          |            $namesAndValues$
          |            "  )",
          |          FailureMessages.undecoratedPropertyCheckFailureMessage,
          |          List($alphaLower$),
          |          List($alphaName$),
          |          Some(ex),
          |          None, // Payload
          |          prettifier,
          |          pos,
          |          idx
          |        )
          |      }
          |  }
          |  indicateSuccess(FailureMessages.propertyCheckSucceeded)
          |}
        """.stripMargin

      val alpha = "abcdefghijklmnopqrstuv"

      val st = new org.antlr.stringtemplate.StringTemplate(forAllImplTemplate)
      val alphaLower = alpha.take(i).mkString(", ")
      val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
      val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
      val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
      val strings = List.fill(i)("String").mkString(", ")
      val argsNamedArgSeq =
        for (argsIdx <- 0 until i) yield
        "\"" + "arg" + argsIdx + "\""
      val argsNamedArg = argsNamedArgSeq.mkString(",")
      val sumOfArgs = alpha.take(i).mkString(" + ")
      val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
      val rawRows =
        for (idx <- 0 to 9) yield
        List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
      val columnsOfIndexes = rawRows.mkString(",\n")
      st.setAttribute("n", i)
      st.setAttribute("alphaLower", alphaLower)
      st.setAttribute("alphaUpper", alphaUpper)
      st.setAttribute("alphaName", alphaName)
      st.setAttribute("strings", strings)
      st.setAttribute("argsNamedArg", argsNamedArg)
      st.setAttribute("namesAndValues", namesAndValues)
      st.setAttribute("sumOfArgs", sumOfArgs)
      st.setAttribute("argNames", argNames)
      st.setAttribute("columnsOfIndexes", columnsOfIndexes)
      if (scalaJS)
        transform(st.toString)
      else
        st.toString
    }

    val doForEveryMethodTemplate: String =
      """/**
        | * Implementation method for [[org.scalatest.prop.TableDrivenPropertyChecks TableDrivenPropertyChecks]]'s <code>forEvery</code> syntax.
        | */
        |def forEvery[$alphaUpper$](heading: ($strings$), rows: ($alphaUpper$)*)(fun: ($alphaUpper$) => ASSERTION)(implicit prettifier: Prettifier, pos: source.Position): Result
        |""".stripMargin

    def doForEveryMethod(i: Int): String = {
      val alpha = "abcdefghijklmnopqrstuv"

      val st = new org.antlr.stringtemplate.StringTemplate(doForEveryMethodTemplate)
      val alphaLower = alpha.take(i).mkString(", ")
      val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
      val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
      val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
      val strings = List.fill(i)("String").mkString(", ")
      val argsNamedArgSeq =
        for (argsIdx <- 0 until i) yield
        "\"" + "arg" + argsIdx + "\""
      val argsNamedArg = argsNamedArgSeq.mkString(",")
      val sumOfArgs = alpha.take(i).mkString(" + ")
      val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
      val rawRows =
        for (idx <- 0 to 9) yield
        List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
      val columnsOfIndexes = rawRows.mkString(",\n")
      st.setAttribute("n", i)
      st.setAttribute("alphaLower", alphaLower)
      st.setAttribute("alphaUpper", alphaUpper)
      st.setAttribute("alphaName", alphaName)
      st.setAttribute("strings", strings)
      st.setAttribute("argsNamedArg", argsNamedArg)
      st.setAttribute("namesAndValues", namesAndValues)
      st.setAttribute("sumOfArgs", sumOfArgs)
      st.setAttribute("argNames", argNames)
      st.setAttribute("columnsOfIndexes", columnsOfIndexes)
      if (scalaJS)
        transform(st.toString)
      else
        st.toString
    }

    def doForEveryMethodImpl(i: Int): String = {
      val heading =
        if (i == 1)
          "heading"
        else
          (1 to i).map(x => "heading._" + x).mkString(", ")
      val row = (1 to i).map(x => "row._" + x).mkString(", ")
      val rows =
        if (i == 1)
          "rows.map(Tuple1.apply[A])"
        else
          "rows"
      val forEveryImplTemplate: String =
        doForEveryMethodTemplate + """ = {
                                   |  doForEvery[Tuple$n$[$alphaUpper$]](List($heading$), $rows$, Resources.tableDrivenForEveryFailed _, "TableAsserting.scala", "forEvery", 2, prettifier, pos)((row: $rowType$) => fun($row$))
                                   |}
                                 """.stripMargin

      val alpha = "abcdefghijklmnopqrstuv"

      val st = new org.antlr.stringtemplate.StringTemplate(forEveryImplTemplate)
      val alphaLower = alpha.take(i).mkString(", ")
      val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
      val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
      val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
      val strings = List.fill(i)("String").mkString(", ")
      val argsNamedArgSeq =
        for (argsIdx <- 0 until i) yield
        "\"" + "arg" + argsIdx + "\""
      val argsNamedArg = argsNamedArgSeq.mkString(",")
      val sumOfArgs = alpha.take(i).mkString(" + ")
      val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
      val rawRows =
        for (idx <- 0 to 9) yield
        List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
      val columnsOfIndexes = rawRows.mkString(",\n")
      val rowType = if (i == 1) "Tuple1[A]" else "(" + alphaUpper + ")"
      st.setAttribute("n", i)
      st.setAttribute("alphaLower", alphaLower)
      st.setAttribute("alphaUpper", alphaUpper)
      st.setAttribute("alphaName", alphaName)
      st.setAttribute("strings", strings)
      st.setAttribute("argsNamedArg", argsNamedArg)
      st.setAttribute("namesAndValues", namesAndValues)
      st.setAttribute("sumOfArgs", sumOfArgs)
      st.setAttribute("argNames", argNames)
      st.setAttribute("columnsOfIndexes", columnsOfIndexes)
      st.setAttribute("heading", heading)
      st.setAttribute("rowType", rowType)
      st.setAttribute("row", row)
      st.setAttribute("rows", rows)
      if (scalaJS)
        transform(st.toString)
      else
        st.toString
    }

    val doExistsMethodTemplate: String =
      """/**
        | * Implementation method for [[org.scalatest.prop.TableDrivenPropertyChecks TableDrivenPropertyChecks]]'s <code>exists</code> syntax.
        | */
        |def exists[$alphaUpper$](heading: ($strings$), rows: ($alphaUpper$)*)(fun: ($alphaUpper$) => ASSERTION)(implicit prettifier: Prettifier, pos: source.Position): Result
      """.stripMargin

    def doExistsMethod(i: Int): String = {
      val alpha = "abcdefghijklmnopqrstuv"

      val st = new org.antlr.stringtemplate.StringTemplate(doExistsMethodTemplate)
      val alphaLower = alpha.take(i).mkString(", ")
      val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
      val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
      val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
      val strings = List.fill(i)("String").mkString(", ")
      val argsNamedArgSeq =
        for (argsIdx <- 0 until i) yield
        "\"" + "arg" + argsIdx + "\""
      val argsNamedArg = argsNamedArgSeq.mkString(",")
      val sumOfArgs = alpha.take(i).mkString(" + ")
      val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
      val rawRows =
        for (idx <- 0 to 9) yield
        List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
      val columnsOfIndexes = rawRows.mkString(",\n")
      st.setAttribute("n", i)
      st.setAttribute("alphaLower", alphaLower)
      st.setAttribute("alphaUpper", alphaUpper)
      st.setAttribute("alphaName", alphaName)
      st.setAttribute("strings", strings)
      st.setAttribute("argsNamedArg", argsNamedArg)
      st.setAttribute("namesAndValues", namesAndValues)
      st.setAttribute("sumOfArgs", sumOfArgs)
      st.setAttribute("argNames", argNames)
      st.setAttribute("columnsOfIndexes", columnsOfIndexes)
      if (scalaJS)
        transform(st.toString)
      else
        st.toString
    }

    def doExistsMethodImpl(i: Int): String = {
      val heading =
        if (i == 1)
          "heading"
        else
          (1 to i).map(x => "heading._" + x).mkString(", ")
      val row = (1 to i).map(x => "row._" + x).mkString(", ")
      val rows =
        if (i == 1)
          "rows.map(Tuple1.apply[A])"
        else
          "rows"
      val forEveryImplTemplate: String =
        doExistsMethodTemplate + """ = {
                                     |  doExists[Tuple$n$[$alphaUpper$]](List($heading$), $rows$, Resources.tableDrivenForEveryFailed _, "TableAsserting.scala", "doExists", 2, prettifier, pos)((row: $rowType$) => fun($row$))
                                     |}
                                   """.stripMargin

      val alpha = "abcdefghijklmnopqrstuv"

      val st = new org.antlr.stringtemplate.StringTemplate(forEveryImplTemplate)
      val alphaLower = alpha.take(i).mkString(", ")
      val alphaUpper = alpha.take(i).toUpperCase.mkString(", ")
      val alphaName = alpha.take(i).map(_ + "Name").mkString(", ")
      val namesAndValues = alpha.take(i).map(c => "              \"    \" + " + c + "Name + \" = \" + " + c).mkString("", " + \",\" + \"\\n\" +\n", " + \"\\n\" +\n")
      val strings = List.fill(i)("String").mkString(", ")
      val argsNamedArgSeq =
        for (argsIdx <- 0 until i) yield
        "\"" + "arg" + argsIdx + "\""
      val argsNamedArg = argsNamedArgSeq.mkString(",")
      val sumOfArgs = alpha.take(i).mkString(" + ")
      val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
      val rawRows =
        for (idx <- 0 to 9) yield
        List.fill(i)("  " + idx).mkString(" *     (", ", ", ")")
      val columnsOfIndexes = rawRows.mkString(",\n")
      val rowType = if (i == 1) "Tuple1[A]" else "(" + alphaUpper + ")"
      st.setAttribute("n", i)
      st.setAttribute("alphaLower", alphaLower)
      st.setAttribute("alphaUpper", alphaUpper)
      st.setAttribute("alphaName", alphaName)
      st.setAttribute("strings", strings)
      st.setAttribute("argsNamedArg", argsNamedArg)
      st.setAttribute("namesAndValues", namesAndValues)
      st.setAttribute("sumOfArgs", sumOfArgs)
      st.setAttribute("argNames", argNames)
      st.setAttribute("columnsOfIndexes", columnsOfIndexes)
      st.setAttribute("heading", heading)
      st.setAttribute("rowType", rowType)
      st.setAttribute("row", row)
      st.setAttribute("rows", rows)
      if (scalaJS)
        transform(st.toString)
      else
        st.toString
    }

    val mainTemplate =
      """/*
         | * Copyright 2001-2015 Artima, Inc.
         | *
         | * Licensed under the Apache License, Version 2.0 (the "License");
         | * you may not use this file except in compliance with the License.
         | * You may obtain a copy of the License at
         | *
         | *     http://www.apache.org/licenses/LICENSE-2.0
         | *
         | * Unless required by applicable law or agreed to in writing, software
         | * distributed under the License is distributed on an "AS IS" BASIS,
         | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
         | * See the License for the specific language governing permissions and
         | * limitations under the License.
         | */
         |package org.scalatest.enablers
         |
         |import org.scalatest.Assertion
         |import org.scalatest.Succeeded
         |import org.scalatest.FailureMessages
         |import org.scalatest.UnquotedString
         |import org.scalatest.Resources
         |import org.scalatest.exceptions.StackDepthException
         |import org.scalatest.exceptions.TableDrivenPropertyCheckFailedException
         |import org.scalatest.exceptions.DiscardedEvaluationException
         |import org.scalatest.exceptions.StackDepth
         |import org.scalactic._
         |
         |/**
         | * Supertrait for <code>TableAsserting</code> typeclasses, which are used to implement and determine the result
         | * type of [[org.scalatest.prop.TableDrivenPropertyChecks TableDrivenPropertyChecks]]'s <code>forAll</code>, <code>forEvery</code> and <code>exists</code> method.
         | *
         | * <p>
         | * Currently, an [[org.scalatest.prop.TableDrivenPropertyChecks TableDrivenPropertyChecks]] expression will have result type <code>Assertion</code>, if the function passed has result type <code>Assertion</code>,
         | * else it will have result type <code>Unit</code>.
         | * </p>
         | */
         |trait TableAsserting[ASSERTION] {
         |  /**
         |   * Return type of <code>forAll</code>, <code>forEvery</code> and <code>exists</code> method.
         |   */
         |  type Result
         |  $forAllMethods$
         |  $forEveryMethods$
         |  $existsMethods$
         |}
         |
         |/**
         |  * Class holding lowest priority <code>TableAsserting</code> implicit, which enables [[org.scalatest.prop.TableDrivenPropertyChecks TableDrivenPropertyChecks]] expressions that have result type <code>Unit</code>.
         |  */
         |abstract class UnitTableAsserting {
         |
         |  /**
         |   * Abstract subclass of <code>TableAsserting</code> that provides the bulk of the implementations of <code>TableAsserting</code>'s
         |   * <code>forAll</code>, <code>forEvery</code> and <code>exists</code>.
         |   */
         |  abstract class TableAssertingImpl[ASSERTION] extends TableAsserting[ASSERTION] {
         |
         |    $forAllMethodImpls$
         |
         |    $forEveryMethodImpls$
         |
         |    private[scalatest] case class ForResult[E](passedCount: Int = 0,
         |                          discardedCount: Int = 0,
         |                          messageAcc: IndexedSeq[String] = IndexedSeq.empty,
         |                          passedElements: IndexedSeq[(Int, E)] = IndexedSeq.empty,
         |                          failedElements: IndexedSeq[(Int, E, Throwable)] = IndexedSeq.empty)
         |
         |    private[scalatest] def runAndCollectResult[E <: Product](namesOfArgs: List[String], rows: Seq[E], sourceFileName: String, methodName: String, stackDepthAdjustment: Int, prettifier: Prettifier, pos: source.Position)(fun: E => ASSERTION): ForResult[E] = {
         |      import org.scalatest.InspectorsHelper.{shouldPropagate, indentErrorMessages}
         |
         |      @scala.annotation.tailrec
         |      def innerRunAndCollectResult(itr: Iterator[E], result: ForResult[E], index: Int)(fun: E => ASSERTION): ForResult[E] = {
         |        if (itr.hasNext) {
         |          val head = itr.next
         |          val newResult =
         |            try {
         |              fun(head)
         |              result.copy(passedCount = result.passedCount + 1, passedElements = result.passedElements :+ (index, head))
         |            }
         |            catch {
         |              case _: org.scalatest.exceptions.DiscardedEvaluationException => result.copy(discardedCount = result.discardedCount + 1) // discard this evaluation and move on to the next
         |              case ex if !shouldPropagate(ex) =>
         |                result.copy(failedElements =
         |                  result.failedElements :+ ((index,
         |                    head,
         |                    new org.scalatest.exceptions.TableDrivenPropertyCheckFailedException(
         |                      ((sde: StackDepthException) => FailureMessages.propertyException(prettifier, UnquotedString(ex.getClass.getSimpleName)) +
         |                        (sde.failedCodeFileNameAndLineNumberString match {
         |                          case Some(s) => " (" + s + ")";
         |                          case None => ""
         |                        }) + "\n" +
         |                        "  " + FailureMessages.thrownExceptionsMessage(prettifier, if (ex.getMessage == null) "None" else UnquotedString(ex.getMessage)) + "\n" +
         |                        (
         |                          ex match {
         |                            case sd: StackDepth if sd.failedCodeFileNameAndLineNumberString.isDefined =>
         |                              "  " + FailureMessages.thrownExceptionsLocation(prettifier, UnquotedString(sd.failedCodeFileNameAndLineNumberString.get)) + "\n"
         |                            case _ => ""
         |                          }
         |                          ) +
         |                        "  " + FailureMessages.occurredAtRow(prettifier, index) + "\n" +
         |                        indentErrorMessages(namesOfArgs.zip(head.productIterator.toSeq).map { case (name, value) =>
         |                          name + " = " + value
         |                        }.toIndexedSeq).mkString("\n") +
         |                        "  )"),
         |                      Some(ex),
         |                      pos,
         |                      None,
         |                      FailureMessages.undecoratedPropertyCheckFailureMessage,
         |                      head.productIterator.toList,
         |                      namesOfArgs,
         |                      index
         |                    ))
         |                  )
         |                )
         |            }
         |
         |          innerRunAndCollectResult(itr, newResult, index + 1)(fun)
         |        }
         |        else
         |          result
         |      }
         |      innerRunAndCollectResult(rows.toIterator, ForResult(), 0)(fun)
         |    }
         |
         |    private def doForEvery[E <: Product](namesOfArgs: List[String], rows: Seq[E], messageFun: Any => String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int, prettifier: Prettifier, pos: source.Position)(fun: E => ASSERTION)(implicit asserting: TableAsserting[ASSERTION]): Result = {
         |      import org.scalatest.InspectorsHelper.indentErrorMessages
         |      val result = runAndCollectResult(namesOfArgs, rows, sourceFileName, methodName, stackDepthAdjustment + 2, prettifier, pos)(fun)
         |      val messageList = result.failedElements.map(_._3)
         |      if (messageList.size > 0)
         |        indicateFailure(
         |          messageFun(UnquotedString(indentErrorMessages(messageList.map(_.toString)).mkString(", \n"))),
         |          messageList.headOption,
         |          prettifier,
         |          pos
         |        )
         |      else indicateSuccess(FailureMessages.propertyCheckSucceeded)
         |    }
         |
         |    $existsMethodImpls$
         |
         |    private def doExists[E <: Product](namesOfArgs: List[String], rows: Seq[E], messageFun: Any => String, sourceFileName: String, methodName: String, stackDepthAdjustment: Int, prettifier: Prettifier, pos: source.Position)(fun: E => ASSERTION)(implicit asserting: TableAsserting[ASSERTION]): Result = {
         |      import org.scalatest.InspectorsHelper.indentErrorMessages
         |      val result = runAndCollectResult(namesOfArgs, rows, sourceFileName, methodName, stackDepthAdjustment + 2, prettifier, pos)(fun)
         |      if (result.passedCount == 0) {
         |        val messageList = result.failedElements.map(_._3)
         |        indicateFailure(
         |          messageFun(UnquotedString(indentErrorMessages(messageList.map(_.toString)).mkString(", \n"))),
         |          messageList.headOption,
         |          prettifier,
         |          pos
         |        )
         |      }
         |      else indicateSuccess(FailureMessages.propertyCheckSucceeded)
         |    }
         |
         |    private[scalatest] def indicateSuccess(message: => String): Result
         |
         |    private[scalatest] def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, args: List[Any], namesOfArgs: List[String], optionalCause: Option[Throwable], payload: Option[Any], prettifier: Prettifier, pos: source.Position, idx: Int): Result
         |
         |    private[scalatest] def indicateFailure(message: => String, optionalCause: Option[Throwable], prettifier: Prettifier, pos: source.Position): Result
         |  }
         |
         |  /**
         |   * Provides support of [[org.scalatest.enablers.TableAsserting TableAsserting]] for Unit.  Do nothing when the check succeeds,
         |   * but throw [[org.scalatest.exceptions.TableDrivenPropertyCheckFailedException TableDrivenPropertyCheckFailedException]]
         |   * when check fails.
         |   */
         |  implicit def assertingNatureOfT[T]: TableAsserting[T] { type Result = Unit } = {
         |    new TableAssertingImpl[T] {
         |      type Result = Unit
         |      def indicateSuccess(message: => String): Unit = ()
         |      def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, args: List[Any], namesOfArgs: List[String], optionalCause: Option[Throwable], payload: Option[Any], prettifier: Prettifier, pos: source.Position, idx: Int): Unit =
         |        throw new TableDrivenPropertyCheckFailedException(
         |          messageFun,
         |          optionalCause,
         |          pos,
         |          payload,
         |          undecoratedMessage,
         |          args,
         |          namesOfArgs,
         |          idx
         |        )
         |      def indicateFailure(message: => String, optionalCause: Option[Throwable], prettifier: Prettifier, pos: source.Position): Unit =
         |        throw new org.scalatest.exceptions.TestFailedException(
         |          (_: StackDepthException) => Some(message),
         |          optionalCause,
         |          pos
         |        )
         |    }
         |  }
         |}
         |
         | /**
         |  * Abstract class that in the future will hold an intermediate priority <code>TableAsserting</code> implicit, which will enable inspector expressions
         |  * that have result type <code>Expectation</code>, a more composable form of assertion that returns a result instead of throwing an exception when it fails.
         |  */
         |/*abstract class ExpectationTableAsserting extends UnitTableAsserting {
         |
         |  implicit def assertingNatureOfExpectation: TableAsserting[Expectation] { type Result = Expectation } = {
         |    new TableAsserting[Expectation] {
         |      type Result = Expectation
         |    }
         |  }
         |}*/
         |
         |/**
         | * Companion object to <code>TableAsserting</code> that provides two implicit providers, a higher priority one for passed functions that have result
         | * type <code>Assertion</code>, which also yields result type <code>Assertion</code>, and one for any other type, which yields result type <code>Unit</code>.
         | */
         |object TableAsserting extends UnitTableAsserting /*ExpectationTableAsserting*/ {
         |
         |  /**
         |    * Provides support of [[org.scalatest.enablers.TableAsserting TableAsserting]] for Assertion.  Returns [[org.scalatest.Succeeded Succeeded]] when the check succeeds,
         |    * but throw [[org.scalatest.exceptions.TableDrivenPropertyCheckFailedException TableDrivenPropertyCheckFailedException]]
         |    * when check fails.
         |    */
         |  implicit def assertingNatureOfAssertion: TableAsserting[Assertion] { type Result = Assertion } = {
         |    new TableAssertingImpl[Assertion] {
         |      type Result = Assertion
         |      def indicateSuccess(message: => String): Assertion = Succeeded
         |      def indicateFailure(messageFun: StackDepthException => String, undecoratedMessage: => String, args: List[Any], namesOfArgs: List[String], optionalCause: Option[Throwable], payload: Option[Any], prettifier: Prettifier, pos: source.Position, idx: Int): Assertion =
         |        throw new TableDrivenPropertyCheckFailedException(
         |          messageFun,
         |          optionalCause,
         |          pos,
         |          payload,
         |          undecoratedMessage,
         |          args,
         |          namesOfArgs,
         |          idx
         |        )
         |      def indicateFailure(message: => String, optionalCause: Option[Throwable], prettifier: Prettifier, pos: source.Position): Assertion =
         |        throw new org.scalatest.exceptions.TestFailedException(
         |          (_: StackDepthException) => Some(message),
         |          optionalCause,
         |          pos
         |        )
         |    }
         |  }
         |}
         |
         |
      """.stripMargin

    val bw = new BufferedWriter(new FileWriter(new File(targetDir, "TableAsserting.scala")))

    try {
      val forAllMethods = (for (i <- 1 to 22) yield doForAllMethod(i)).mkString("\n\n")
      val forAllMethodImpls = (for (i <- 1 to 22) yield doForAllMethodImpl(i)).mkString("\n\n")
      val forEveryMethods = (for (i <- 1 to 22) yield doForEveryMethod(i)).mkString("\n\n")
      val forEveryMethodImpls = (for (i <- 1 to 22) yield doForEveryMethodImpl(i)).mkString("\n\n")
      val existsMethods = (for (i <- 1 to 22) yield doExistsMethod(i)).mkString("\n\n")
      val existsMethodImpls = (for (i <- 1 to 22) yield doExistsMethodImpl(i)).mkString("\n\n")
      val st = new org.antlr.stringtemplate.StringTemplate(mainTemplate)
      st.setAttribute("forAllMethods", forAllMethods)
      st.setAttribute("forAllMethodImpls", forAllMethodImpls)
      st.setAttribute("forEveryMethods", forEveryMethods)
      st.setAttribute("forEveryMethodImpls", forEveryMethodImpls)
      st.setAttribute("existsMethods", existsMethods)
      st.setAttribute("existsMethodImpls", existsMethodImpls)
      bw.write(st.toString)
    }
    finally {
      bw.flush()
      bw.close()
    }
  }
 
  def genTableSuite(targetDir: File) {

    val bw = new BufferedWriter(new FileWriter(new File(targetDir, "TableSuite.scala")))
 
    try {
      val st = new org.antlr.stringtemplate.StringTemplate(copyrightTemplate)
      st.setAttribute("year", thisYear);
      bw.write(st.toString)
      bw.write(tableSuitePreamble)
      val alpha = "abcdefghijklmnopqrstuv"
      // for (i <- 1 to 22) {
      for (i <- 1 to 20) { // TODO: To avoid 2.9.0 compiler bug at arities 21 and 22

        val st = new org.antlr.stringtemplate.StringTemplate(tableSuiteTemplate)
        val rowOfMinusOnes = List.fill(i)(" -1").mkString(", ")
        val rowOfOnes = List.fill(i)("  1").mkString(", ")
        val rowOfTwos = List.fill(i)("  2").mkString(", ")
        val listOfIs = List.fill(i)("i").mkString(", ")
        val columnsOfOnes = List.fill(i)("        (" + rowOfOnes + ")").mkString(",\n")
        val columnOfMinusOnes = "        (" + rowOfMinusOnes + "),"
        val columnsOfTwos = List.fill(i)("        (" + rowOfTwos + ")").mkString(",\n")
        val rawRows =
          for (idx <- 0 to 9) yield                
            List.fill(i)("  " + idx).mkString("        (", ", ", ")")
        val columnsOfIndexes = rawRows.mkString(",\n")
        val argNames = alpha.map("\"" + _ + "\"").take(i).mkString(", ")
        val names = alpha.take(i).mkString(", ")
        val sumOfArgs = alpha.take(i).mkString(" + ")
        st.setAttribute("n", i)
        st.setAttribute("columnsOfOnes", columnsOfOnes)
        st.setAttribute("columnOfMinusOnes", columnOfMinusOnes)
        st.setAttribute("columnsOfTwos", columnsOfTwos)
        st.setAttribute("columnsOfIndexes", columnsOfIndexes)
        st.setAttribute("argNames", argNames)
        st.setAttribute("names", names)
        st.setAttribute("sumOfArgs", sumOfArgs)
        st.setAttribute("listOfIs", listOfIs)
        bw.write(st.toString)
      }

      bw.write("}\n")
    }
    finally {
      bw.close()
    }
  }

  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)

    val mainDir = new File(targetDir + "/main/scala/org/scalatest/prop")
    mainDir.mkdirs()
    genMain(mainDir, version, scalaVersion)
    
    val testDir = new File("gentests/" + targetDir + "/test/scala/org/scalatest/prop")
    testDir.mkdirs()
    genTest(testDir, version, scalaVersion)
  }
  
  def genMain(dir: File, version: String, scalaVersion: String) {
    dir.mkdirs()
    genTableForNs(dir, false)
    genPropertyChecks(dir)
    genTables(dir)
    genTableAsserting(dir, false)
  }

  def genMainForScalaJS(dir: File, version: String, scalaVersion: String) {
    dir.mkdirs()
    genTableForNs(dir, true)
    genPropertyChecks(dir)
    genTables(dir)
    genTableAsserting(dir, true)
  }
  
  def genTest(dir: File, version: String, scalaVersion: String) {
    dir.mkdirs()
    genTableSuite(dir)
  }
}

/*
$if (moreThanFour)$
 * <pre>
 * class MySuite extends FunSuite$num$[
 *   $exampleParams$
 * ] {
$else$
*/

/*
IAException was thrown...
Thrown exception's message: 1 did not equal 7
Occurred at row N (zero-based), which had values (
  n = 0,
  d = 1
)
*/
