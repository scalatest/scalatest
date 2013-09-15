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
package org.scalautils

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

/**
 * Represents a value that is one of two possible types, with one type being &ldquo;good&rdquo; and
 * the other &ldquo;bad.&rdquo;
 *
 * <p>
 * An <code>Or</code> will either be a &ldquo;good&rdquo; value wrapped in an instance of
 * <a href="Good.html"><code>Good</code></a> or a &ldquo;bad&rdquo; value wrapped in an instance
 * of <a href="Bad.html"><code>Bad</code></a>.
 * </p>
 *
 * <h2>The motivation for <code>Or</code></h2>
 *
 * <p>
 * <code>Or</code> differs from Scala's <code>Either</code> type in that
 * <code>Either</code> treats both its <code>Left</code> and <code>Right</code> alternatives in an identical manner, whereas
 * <code>Or</code> treats its two alternatives differently: it favors
 * <code>Good</code> over <code>Bad</code>.
 * Because of this, it is more convenient to work with <code>Or</code>s
 * when you prefer one alternative over the other; for example, if one alternative represents a valid result
 * and another represents an error message.
 * </p>
 *
 * <p>
 * To illustrate, imagine you want to create instances this <code>Person</code> class from user input strings:
 * </p>
 *
 * <pre class="stHighlight">
 * case class Person(name: String, age: Int)
 * </pre>
 *
 * <p>
 * You might write a method that parses the name from user input string and returns an
 * <code>Option[String]</code>: <code>None</code> if the string is empty or blank, else the
 * trimmed string wrapped in a <code>Some</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * def parseName(input: String): Option[String] = {
 *   val trimmed = input.trim
 *   if (!trimmed.isEmpty) Some(trimmed) else None
 * }
 * </pre>
 *
 * <p>
 * You might also write a method that parses the age from user input string and returns an
 * <code>Option[Int]</code>: <code>None</code> if either the string is not a valid integer or
 * it is a negative integer, else the string converted to an integer wrapped in a <code>Some</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * def parseAge(input: String): Option[Int] = {
 *   try { 
 *     val age = input.trim.toInt
 *     if (age &gt;= 0) Some(age) else None
 *   }
 *   catch {
 *     case _: NumberFormatException =&gt; None
 *   }
 * }
 * </pre>
 *
 * <p>
 * With these building blocks you could write a method that parses name and age input
 * strings and returns either a <code>Person</code>, wrapped in a <code>Some</code>, or
 * <code>None</code> if either the name or age, or both, was invalid:
 * </p>
 *
 * <pre class="stHighlight">
 * def parsePerson(inputName: String, inputAge: String): Option[Person] =
 *   for {
 *     name &lt;- parseName(inputName)
 *     age &lt;- parseAge(inputAge)
 *   } yield Person(name, age)
 * </pre>
 *
 * <p>
 * Here are some examples of invoking <code>parsePerson</code> in the Scala interpreter:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; parsePerson("Bridget Jones", "29")
 * res4: Option[Person] = Some(Person(Bridget Jones,29))
 *
 * scala&gt; parsePerson("Bridget Jones", "")
 * res5: Option[Person] = None
 *
 * scala&gt; parsePerson("Bridget Jones", "-29")
 * res6: Option[Person] = None
 *
 * scala&gt; parsePerson("", "")
 * res7: Option[Person] = None
 * </pre>
 *
 * <p>
 * Now imagine you want to give an error message back if the user's input is invalid.
 * You might rewrite the parsing methods to return an <code>Either</code> instead. In this
 * case, the desired result is a valid name or age, which by convention should be placed
 * on the right of the <code>Either</code>. The left will be a <code>String</code> error
 * message. Here's the new <code>parseName</code> function, which returns an <code>Either[String, String]</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * def parseName(input: String): Either[String, String] = {
 *   val trimmed = input.trim
 *   if (!trimmed.isEmpty) Right(trimmed) else Left(s""""${input}" is not a valid name""")
 * }
 * </pre>
 *
 * <p>
 * And here's the new <code>parseAge</code> function, which returns an <code>Either[String, Int]</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * def parseAge(input: String): Either[String, Int] = {
 *   try {
 *     val age = input.trim.toInt
 *     if (age &gt;= 0) Right(age) else Left(s""""${age}" is not a valid age""")
 *   }
 *   catch {
 *     case _: NumberFormatException =&gt; Left(s""""${input}" is not a valid integer""")
 *   }
 * }
 * </pre>
 *
 * <p>
 * The new <code>parsePerson</code> method will return an <code>Either[String, Person]</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * def parsePerson(inputName: String, inputAge: String): Either[String, Person] =
 *   for {
 *     name &lt;- parseName(inputName).right
 *     age &lt;- parseAge(inputAge).right
 *   } yield Person(name, age)
 * </pre>
 * 
 * <p>
 * Note that <code>Either</code> requires you to add <code>.right</code>
 * at the end of each generator in the <code>for</code> expression. Although the convention is to place the
 * valid result on the right, you must explicitly (and repetitively) indicate that you've done so by transforming
 * the <code>Either</code> to a <code>RightProjection</code> by invoking <code>.right</code> at each step. 
 * Given this implementation, the <code>parsePerson</code> method will now short-circuit at the first sign
 * of trouble (as it did when we used an <code>Option</code>), but you now get the first error message returned
 * in a <code>Left</code>. Here are some examples in the Scala interpreter:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; parsePerson("Bridget Jones", "29")
 * res4: Either[String,Person] = Right(Person(Bridget Jones,29))
 *
 * scala&gt; parsePerson("Bridget Jones", "")
 * res5: Either[String,Person] = Left("" is not a valid integer)
 *
 * scala&gt; parsePerson("Bridget Jones", "-29")
 * res6: Either[String,Person] = Left("-29" is not a valid age)
 *
 * scala&gt; parsePerson("", "")
 * res7: Either[String,Person] = Left("" is not a valid name)
 * </pre>
 *
 * <h2>An <code>Either</code> with &ldquo;attitude&rdquo;</h2>
 *
 * <p>
 * Because <code>Or</code> declares one alternative to be &ldquo;good&rdquo; and the other &ldquo;bad,&rdquo;
 * it is more convenient than <code>Either</code> in this kind of situation. One difference to note with
 * <code>Or</code> is that the <code>Good</code> alternative is on the left, <code>Bad</code> on the right.
 * The reason is that <code>Or</code> is designed to be written using infix notation, and placing the
 * &ldquo;happy path&rdquo; first is more readable. For example, instead of writing:
 * </p>
 *
 * <pre class="stHighlight">
 * Or[Int, ErrorMessage]
 * </pre>
 *
 * <p>
 * You can write:
 * </p>
 *
 * <pre class="stHighlight">
 * Int Or ErrorMessage
 * </pre>
 *
 * <p>
 * Here's how the <code>parseName</code> method might be written using an <code>Or</code>, where
 * <code>ErrorMessage</code> is a type alias for <code>String</code> declared in the <code>org.scalautils</code>
 * package object:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalautils._
 *
 * def parseName(input: String): String Or ErrorMessage = {
 *   val trimmed = input.trim
 *   if (!trimmed.isEmpty) Good(trimmed) else Bad(s""""${input}" is not a valid name""")
 * }
 * </pre>
 *
 * <p>
 * You can think of the <code>String</code> <code>Or</code> <code>ErrorMessage</code> result
 * type like this: <code>parseName</code> will return an <code>Int</code> or, if the input string
 * is not a valid name, an <code>ErrorMessage</code>.
 * Here's how the <code>parseAge</code> method might be written:
 * </p>
 *
 * <pre class="stHighlight">
 * def parseAge(input: String): Int Or ErrorMessage = {
 *   try {
 *     val age = input.trim.toInt
 *     if (age &gt;= 0) Good(age) else Bad(s""""${age}" is not a valid age""")
 *   }
 *   catch {
 *     case _: NumberFormatException =&gt; Bad(s""""${input}" is not a valid integer""")
 *   }
 * }
 * </pre>
 *
 * <p>
 * Given these implementations, here's how you'd write the <code>parsePerson</code> method:
 * </p>
 *
 * <pre class="stHighlight">
 * def parsePerson(inputName: String, inputAge: String): Person Or ErrorMessage =
 *   for {
 *     name &lt;- parseName(inputName)
 *     age &lt;- parseAge(inputAge)
 *   } yield Person(name, age)
 * </pre>
 *
 * <p>
 * Because of <code>Or</code>'s attitude, you need not write <code>.good</code> at the end of
 * each generator. <code>Or</code> will keep going so long as each step produces a <code>Good</code>,
 * short circuiting at the first sign of a <code>Bad</code>. Here are a few invocations of this
 * <code>parsePerson</code> method in the Scala interpreter: 
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; parsePerson("Bridget Jones", "29")
 * res8: org.scalautils.Or[Person,org.scalautils.ErrorMessage] = Good(Person(Bridget Jones,29))
 *
 * scala&gt; parsePerson("Bridget Jones", "")
 * res9: org.scalautils.Or[Person,org.scalautils.ErrorMessage] = Bad("" is not a valid integer)
 *
 * scala&gt; parsePerson("Bridget Jones", "-29")
 * res10: org.scalautils.Or[Person,org.scalautils.ErrorMessage] = Bad("-29" is not a valid age)
 *
 * scala&gt; parsePerson("", "")
 * res11: org.scalautils.Or[Person,org.scalautils.ErrorMessage] = Bad("" is not a valid name)
 * </pre>
 *
 * <h2>Accumulating errors with <code>Or</code></h2>
 *
 * <p>
 * Another difference between <code>Or</code> and <code>Either</code> is that <code>Or</code> enables
 * you to accumulate errors if the <code>Bad</code> type is an <a href="Every.html"><code>Every</code></a>.
 * An <code>Every</code> is similar to a <code>Seq</code> in that it is contains ordered elements, but
 * different in that it cannot be empty. An <code>Every</code> is either a <a href="One.html"><code>One</code></a>,
 * which contains one and only one element, or a <a href="Many.html"><code>Many</code></a>, which contains two or
 * more elements. Thus an <code>Every</code> contains one or more elements.
 * </p>
 *
 * <p>
 * <em>Note: an <code>Or</code> whose <code>Bad</code> type is an <code>Every</code>, or one of its subtypes,
 * is called an &ldquo;accumulating <code>Or</code>.&rdquo;</em>
 * </p>
 *
 * <p>
 * To rewrite the previous example so that errors can be accumulated, you need first to return an <code>Every</code>
 * as the <code>Bad</code> type. Here's how you'd change the <code>parseName</code> method:
 * </p>
 *
 * <pre class="stHighlight">
 * def parseName(input: String): String Or One[ErrorMessage] = {
 *   val trimmed = input.trim
 *   if (!trimmed.isEmpty) Good(trimmed) else Bad(One(s""""${input}" is not a valid name"""))
 * }
 * </pre>
 *
 * <p>
 * Because <code>parseName</code> will either return a valid name <code>String</code> wrapped in a 
 * <code>Good</code>, or <em>one</em> error message, wrapped in a <code>Bad</code>, you would write the
 * <code>Bad</code> type as <code>One[ErrorMessage]</code>. The same is true for <code>parseAge</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * def parseAge(input: String): Int Or One[ErrorMessage] = {
 *   try {
 *     val age = input.trim.toInt
 *     if (age &gt;= 0) Good(age) else Bad(One(s""""${age}" is not a valid age"""))
 *   }
 *   catch {
 *     case _: NumberFormatException =&gt; Bad(One(s""""${input}" is not a valid integer"""))
 *   }
 * }
 * </pre>
 *
 * <p>
 * Because a <code>for</code> expression short-circuits on the first <code>Bad</code> encountered, you'll
 * need to use a different approach to write the <code>parsePerson</code> method. In this example, the
 * <code>withGood</code> method from trait <a href="Validations.html"><code>Validations</code></a>
 * will do the trick:
 * </p>
 *
 * <pre class="stHighlight">
 * def parsePerson(inputName: String, inputAge: String): Person Or Every[ErrorMessage] = {
 *   val name = parseName(inputName)
 *   val age = parseAge(inputAge)
 *   withGood(name, age) { Person(_, _) }
 * }
 * </pre>
 *
 * <p>
 * Trait <code>Validations</code> offers overloaded <code>withGood</code> methods that take 1 to
 * 22 accumulating <code>Or</code>s, plus a function taking the same number of corresponding
 * <code>Good</code> values.  In this example, if both <code>name</code> and <code>age</code> are
 * <code>Good</code>s, the <code>withGood</code> method will pass the good name <code>String</code>
 * and age <code>Int</code> to the <code>Person(_, _)</code> function, and return the resulting <code>Person</code>
 * object wrapped in a <code>Good</code>. If either <code>name</code> and <code>age</code>, or both,
 * are <code>Bad</code>, <code>withGood</code> will return the accumulated errors in a <code>Bad</code>.
 * </p>
 *
 * <p>
 * The result of <code>parsePerson</code>, if <code>Bad</code>, will therefore contain either one or two
 * error messages, <em>i.e.</em>, the result will either be a <code>One</code> or a <code>Many</code>.
 * As a result, the result type of <code>parsePerson</code> must be <code>Person</code> <code>Or</code>
 * <code>Every[ErrorMessage]</code>. Regardless of whether a <code>Bad</code> result contains one
 * or two error messages, it will contain <em>every</em> error message. Here's some invocations of
 * this accumulating version of <code>parsePerson</code> in the Scala interpreter:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; parsePerson("Bridget Jones", "29")
 * res10: org.scalautils.Or[Person,org.scalautils.Every[org.scalautils.ErrorMessage]] = Good(Person(Bridget Jones,29))
 *
 * scala&gt; parsePerson("Bridget Jones", "")
 * res11: org.scalautils.Or[Person,org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(One("" is not a valid integer))
 *
 * scala&gt; parsePerson("Bridget Jones", "-29")
 * res12: org.scalautils.Or[Person,org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(One("-29" is not a valid age))
 *
 * scala&gt; parsePerson("", "")
 * res13: org.scalautils.Or[Person,org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(Many("" is not a valid name, "" is not a valid integer))
 * </pre>
 *
 * <p>
 * Note that in the last example, the <code>Bad</code> contains an error message for both name and age.
 * </p>
 *
 * <p>
 * The <code>Validations</code> trait also enables other ways of accumulating errors. If you have a collection of
 * accumulating <code>Or</code>s, for example, you can <em>combine</em> them into one <code>Or</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; List(parseAge("29"), parseAge("30"), parseAge("31")).combined
 * res14: org.scalautils.Or[List[Int],org.scalautils.Every[org.scalautils.ErrorMessage]] = Good(List(29, 30, 31))
 *
 * scala&gt; List(parseAge("29"), parseAge("-30"), parseAge("31")).combined
 * res15: org.scalautils.Or[List[Int],org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(One("-30" is not a valid age))
 *
 * scala&gt; List(parseAge("29"), parseAge("-30"), parseAge("-31")).combined
 * res16: org.scalautils.Or[List[Int],org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(Many("-30" is not a valid age, "-31" is not a valid age))
 * </pre>
 *
 * <p>
 * Or if you have a collection of values and a function that transforms that type of value into an accumulating
 * <code>Or</code>s, you can <em>validate</em> the values using the function, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; List("29", "30", "31").validatedBy(parseAge)
 * res17: org.scalautils.Or[List[Int],org.scalautils.Every[org.scalautils.ErrorMessage]] = Good(List(29, 30, 31))
 *
 * scala&gt; List("29", "-30", "31").validatedBy(parseAge)
 * res18: org.scalautils.Or[List[Int],org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(One("-30" is not a valid age))
 *
 * scala&gt; List("29", "-30", "-31").validatedBy(parseAge)
 * res19: org.scalautils.Or[List[Int],org.scalautils.Every[org.scalautils.ErrorMessage]] = Bad(Many("-30" is not a valid age, "-31" is not a valid age))
 * </pre>
 */
sealed abstract class Or[+G,+B] {
  val isGood: Boolean = false
  val isBad: Boolean = false
  def get: G
  def map[H](f: G => H): H Or B
  def foreach(f: G => Unit): Unit
  def flatMap[H, C >: B](f: G => H Or C): H Or C
  def filter(f: G => Boolean): Option[G Or B]
  def exists(f: G => Boolean): Boolean
  def forall(f: G => Boolean): Boolean
  def getOrElse[H >: G](default: => H): H
  def orElse[H >: G, C >: B](alternative: => H Or C): H Or C
  def toOption: Option[G]
  def toSeq: Seq[G]
  def toEither: Either[B, G]
  def accumulating: G Or One[B]
  def toTry(implicit ev: B <:< Throwable): Try[G]
  def swap: B Or G
}

object Or {
  def from[G](theTry: Try[G]): G Or Throwable =
    theTry match {
      case Success(g) => Good(g)
      case Failure(e) => Bad(e)
    }
  def from[B, G](either: Either[B, G]): G Or B =
    either match {
      case Right(g) => Good(g)
      case Left(e) => Bad(e)
    }
}

final case class Good[+G,+B](g: G) extends Or[G,B] {
  override val isGood: Boolean = true
  def asOr: G Or B = this
  def orBad[C](implicit ev: B <:< C): Good[G, C] = this.asInstanceOf[Good[G, C]]
  def get: G = g
  def map[H](f: G => H): Or[H, B] = Good(f(g))
  def foreach(f: G => Unit): Unit = f(g)
  def flatMap[H, C >: B](f: G => H Or C): H Or C = f(g)
  def filter(f: G => Boolean): Option[G Or B] = if (f(g)) Some(this) else None
  def exists(f: G => Boolean): Boolean = f(g)
  def forall(f: G => Boolean): Boolean = f(g)
  def getOrElse[H >: G](default: => H): G = g
  def orElse[H >: G, C >: B](alternative: => H Or C): G Or B = this
  def toOption: Some[G] = Some(g)
  def toSeq: Seq[G] = Seq(g)
  def toEither: Either[B, G] = Right(g)
  def accumulating: G Or One[B] = Good(g)
  def toTry(implicit ev: B <:< Throwable): Success[G] = Success(g)
  def swap: B Or G = Bad(g)
}

object Good {
  class GoodieGoodieGumdrop[G] {
    def orBad[B](b: B): Bad[G, B] = Bad[G, B](b)
    override def toString: String = "GoodieGoodieGumdrop"
  }
  def apply[G]: GoodieGoodieGumdrop[G] = new GoodieGoodieGumdrop[G]
}

final case class Bad[+G,+B](b: B) extends Or[G,B] {
  override val isBad: Boolean = true
  def asOr: G Or B = this
  def get: G = throw new NoSuchElementException("Bad(" + b + ").get")
  def map[H](f: G => H): H Or B = Bad(b)
  def foreach(f: G => Unit): Unit = ()
  def flatMap[H, C >: B](f: G => H Or C): H Or C = Bad(b)
  def filter(f: G => Boolean): None.type = None
  def exists(f: G => Boolean): Boolean = false
  def forall(f: G => Boolean): Boolean = true
  def getOrElse[H >: G](default: => H): H = default
  def orElse[H >: G, C >: B](alternative: => H Or C): H Or C = alternative
  def toOption: None.type = None
  def toSeq: Seq[G] = Seq.empty
  def toEither: Either[B, G] = Left(b)
  def accumulating: G Or One[B] = Bad(One(b))
  def toTry(implicit ev: B <:< Throwable): Failure[G] = Failure(b)
  def swap: B Or G = Good(b)
}

