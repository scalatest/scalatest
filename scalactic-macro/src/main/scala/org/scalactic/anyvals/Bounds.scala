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
package org.scalactic.anyvals

/*
Explain that this is for when you know the minValue and maxValue at compile
time. You explicitly put them in the subtype, which should be an object. Then
that singleton type is used to parameterize the BoundedInts, which are AnyVals.
And the macro can use it for the same trick. The apply method. I think I can 
actually write that. OneToTen(3). Maybe you need to make the macro one too,
but it can just be a subclass. Or can it? Don't you need to do the thing again?
Yes, but I can show how. it is the usual CompileTimeAssertions trick.
*/
abstract class IntBounds(val minValue: Int, val maxValue: Int) {

  def ensuringValid(i: Int): BoundedInt[this.type] =
    if (i >= minValue && i <= maxValue) new BoundedInt(i)
    else throw new AssertionError(s"$i is not between $minValue and $maxValue, inclusive")

/*
  /**
   * The largest value representable as a positive <code>Int</code>, which is <code>PosInt(2147483647)</code>.
   */
  final val MaxValue: PosInt = PosInt.ensuringValid(Int.MaxValue)
  /**
   * The smallest value representable as a positive <code>Int</code>, which is <code>PosInt(1)</code>.
   */
  final val MinValue: PosInt = PosInt.ensuringValid(1) // Can't use the macro here

  /**
   * A factory method that produces an <code>Option[PosInt]</code> given an
   * <code>Int</code> value.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>PosInt</code> representing that value,
   * wrapped in a <code>Some</code>. Otherwise, the passed <code>Int</code>
   * value is 0 or negative, so this method will return <code>None</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas <code>from</code> inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return
   *     wrapped in a <code>Some[PosInt]</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Some[PosInt]</code>, if it is positive, else <code>None</code>.
   */
  def from(value: Int): Option[PosInt] =
    if (PosIntMacro.isValid(value)) Some(new PosInt(value)) else None

  /**
   * A factory/assertion method that produces a <code>PosInt</code> given a
   * valid <code>Int</code> value, or throws <code>AssertionError</code>,
   * if given an invalid <code>Int</code> value.
   *
   * Note: you should use this method only when you are convinced that it will
   * always succeed, i.e., never throw an exception. It is good practice to
   * add a comment near the invocation of this method indicating ''why'' you think
   * it will always succeed to document your reasoning. If you are not sure an
   * `ensuringValid` call will always succeed, you should use one of the other
   * factory or validation methods provided on this object instead: `isValid`, 
   * `tryingValid`, `passOrElse`, `goodOrElse`, or `rightOrElse`.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>PosInt</code> representing that value.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will throw <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time. 
   * It differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises an <code>Int</code> is positive.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return
   *     wrapped in a <code>PosInt</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>PosInt</code>, if it is positive, else throws <code>AssertionError</code>.
   * @throws AssertionError if the passed value is not positive
   */
  def ensuringValid(value: Int): PosInt =
    if (PosIntMacro.isValid(value)) new PosInt(value) else {
      throw new AssertionError(s"$value was not a valid PosInt")
    }

  /**
   * A factory/validation method that produces a <code>PosInt</code>, wrapped
   * in a <code>Success</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an <code>AssertionError</code>, wrapped
   * in a <code>Failure</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>PosInt</code> representing that value, wrapped in a <code>Success</code>.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will return an <code>AssertionError</code>, wrapped in a <code>Failure</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return
   *     wrapped in a <code>Success(PosInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Success(PosInt)</code>, if it is positive, else a <code>Failure(AssertionError)</code>.
   */
  def tryingValid(value: Int): Try[PosInt] =
    if (PosIntMacro.isValid(value))
      Success(new PosInt(value))
    else
      Failure(new AssertionError(s"$value was not a valid PosInt"))

  /**
   * A validation method that produces a <code>Pass</code>
   * given a valid <code>Int</code> value, or
   * an error value of type <code>E</code> produced by passing the
   * given <em>invalid</em> <code>Int</code> value
   * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>Pass</code>.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will return a result of type <code>E</code> obtained by passing
   * the invalid <code>Int</code> value to the given function <code>f</code>,
   * wrapped in a `Fail`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the `Int` to validate that it is positive.
   * @return a `Pass` if the specified `Int` value is positive,
   *   else a `Fail` containing an error value produced by passing the
   *   specified `Int` to the given function `f`.
   */
  def passOrElse[E](value: Int)(f: Int => E): Validation[E] =
    if (PosIntMacro.isValid(value)) Pass else Fail(f(value))

  /**
   * A factory/validation method that produces a <code>PosInt</code>, wrapped
   * in a <code>Good</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an error value of type <code>B</code>
   * produced by passing the given <em>invalid</em> <code>Int</code> value
   * to the given function <code>f</code>, wrapped in a <code>Bad</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>PosInt</code> representing that value, wrapped in a <code>Good</code>.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will return a result of type <code>B</code> obtained by passing
   * the invalid <code>Int</code> value to the given function <code>f</code>,
   * wrapped in a `Bad`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return
   *     wrapped in a <code>Good(PosInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Good(PosInt)</code>, if it is positive, else a <code>Bad(f(value))</code>.
   */
  def goodOrElse[B](value: Int)(f: Int => B): PosInt Or B =
    if (PosIntMacro.isValid(value)) Good(PosInt.ensuringValid(value)) else Bad(f(value))

  /**
   * A factory/validation method that produces a <code>PosInt</code>, wrapped
   * in a <code>Right</code>, given a valid <code>Int</code> value, or if the
   * given <code>Int</code> is invalid, an error value of type <code>L</code>
   * produced by passing the given <em>invalid</em> <code>Int</code> value
   * to the given function <code>f</code>, wrapped in a <code>Left</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0, it will return a <code>PosInt</code> representing that value, wrapped in a <code>Right</code>.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will return a result of type <code>L</code> obtained by passing
   * the invalid <code>Int</code> value to the given function <code>f</code>,
   * wrapped in a `Left`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas this method inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return
   *     wrapped in a <code>Right(PosInt)</code>.
   * @return the specified <code>Int</code> value wrapped
   *     in a <code>Right(PosInt)</code>, if it is positive, else a <code>Left(f(value))</code>.
   */
  def rightOrElse[L](value: Int)(f: Int => L): Either[L, PosInt] =
    if (PosIntMacro.isValid(value)) Right(PosInt.ensuringValid(value)) else Left(f(value))

  /**
   * A predicate method that returns true if a given 
   * <code>Int</code> value is positive.
   *
   * @param value the <code>Int</code> to inspect, and if positive, return true.
   * @return true if the specified <code>Int</code> is positive, else false.
   */
  def isValid(value: Int): Boolean = PosIntMacro.isValid(value)

  /**
   * A factory method that produces a <code>PosInt</code> given a
   * <code>Int</code> value and a default <code>PosInt</code>.
   *
   * <p>
   * This method will inspect the passed <code>Int</code> value and if
   * it is a positive <code>Int</code>, <em>i.e.</em>, a value greater
   * than 0.0, it will return a <code>PosInt</code> representing that value.
   * Otherwise, the passed <code>Int</code> value is 0 or negative, so this
   * method will return the passed <code>default</code> value.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>Int</code> literals at
   * compile time, whereas <code>from</code> inspects
   * <code>Int</code> values at run time.
   * </p>
   *
   * @param value the <code>Int</code> to inspect, and if positive, return.
   * @param default the <code>PosInt</code> to return if the passed
   *     <code>Int</code> value is not positive.
   * @return the specified <code>Int</code> value wrapped in a
   *     <code>PosInt</code>, if it is positive, else the
   *     <code>default</code> <code>PosInt</code> value.
   */
  def fromOrElse(value: Int, default: => PosInt): PosInt =
    if (PosIntMacro.isValid(value)) new PosInt(value) else default

  import language.experimental.macros

  /**
   * A factory method, implemented via a macro, that produces a <code>PosInt</code>
   * if passed a valid <code>Int</code> literal, otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the specified <code>Int</code>
   * expression at compile time. If
   * the expression is a positive <code>Int</code> literal, <em>i.e.</em>, with a
   * value greater than 0, it will return a <code>PosInt</code> representing that value.
   * Otherwise, the passed <code>Int</code> 
   * expression is either a literal that is 0 or negative, or is not a literal, so
   * this method will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code> factory method
   * in that this method is implemented via a macro that inspects
   * <code>Int</code> literals at compile time, whereas <code>from</code> inspects
   * <code>Int</code> values at run time. 
   * </p>
   *
   * @param value the <code>Int</code> literal expression to inspect at compile time,
   *     and if positive, to return wrapped in a <code>PosInt</code> at run time.
   * @return the specified, valid <code>Int</code> literal value wrapped
   *     in a <code>PosInt</code>. (If the specified expression is not a valid
   *     <code>Int</code> literal, the invocation of this method will not
   *     compile.)
   */
  implicit def apply(value: Int): PosInt = macro PosIntMacro.apply
*/
}
