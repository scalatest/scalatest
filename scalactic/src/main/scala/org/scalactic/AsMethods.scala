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
package org.scalactic

/*
      // New policies
      // Both sides Some
      new UncheckedEquality { Some(42) should not equal Some(Complex(42.0, 0.0)) }
      "new CheckedEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Some(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      // Both sides Option
      new UncheckedEquality { Option(42) should not equal Option(Complex(42.0, 0.0)) }
      "new CheckedEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Option(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      // Left side Some, right side Option
      new UncheckedEquality { Some(42) should not equal Option(Complex(42.0, 0.0)) }
      "new CheckedEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Some(42) shouldEqual Option(Complex(42.0, 0.0)) }" shouldNot typeCheck
      // Left side Option, right side Some
      new UncheckedEquality { Option(42) should not equal Some(Complex(42.0, 0.0)) }
      "new CheckedEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      "new EnabledEquality { Option(42) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      import AsMethods._
      new CheckedEquality { Option(42.as[Complex]) shouldEqual Some(Complex(42.0, 0.0)) }
      "new EnabledEquality { Option(42.as[Complex]) shouldEqual Some(Complex(42.0, 0.0)) }" shouldNot typeCheck
      implicit val enableComplexComparisons = EnabledEqualityFor[Complex]
      new EnabledEquality { Option(42.as[Complex]) shouldEqual Some(Complex(42.0, 0.0)) }

actually normally you'd do (Given opt, an Option[Int]):

opt.map(_.as[Complex])

Although you can do this:

(42 : Complex)

You can't do this:

opt.map(_: Complex)

Because the compiler gets confused. You could do this:

opt.map(a => a : Complex)

So as just makes that a bit more obvious maybe:

opt.map(_.as[Complex])

Maybe I won't include this as the former isn't too bad.

*/
trait AsMethods {

  implicit final class Asifier[T](o: T) {
    def as[U](implicit cnv: T => U): U = cnv(o)
  }
} 

/**
 * Companion object for <code>NormMethods</code> enabling its members to be imported as an alternative to mixing them in.
 */
object AsMethods extends AsMethods

