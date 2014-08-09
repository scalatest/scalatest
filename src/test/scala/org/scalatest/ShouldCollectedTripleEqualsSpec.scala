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
package org.scalatest

import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import org.scalactic.TripleEquals
import org.scalactic.TypeCheckedTripleEquals
import org.scalactic.ConversionCheckedTripleEquals
import SharedHelpers._
import Matchers._

class ShouldCollectedTripleEqualsSpec extends Spec with NonImplicitAssertions {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  object `the custom equality should === (operator` {

    object `with TripleEquals` {

      def `should compare anything with anything` {

        new TripleEquals {

          all (List(1, 1, 1)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

          all (List(1, 1, 1)) should === (1L)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }

          all (List(1L, 1L, 1L)) should === (1)
          intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

          all (List("1", "1", "1")) should !== (1)
          intercept[TestFailedException] { all (List("1", "1", "1")) should === (1) }

          all (List(1, 1, 1)) should !== ("1")
          intercept[TestFailedException] { all (List(1, 1, 1)) should === ("1") }

          all (List(super1, super1, super1)) should !== (super2)
          all (List(super1, super1, super1)) should !== (sub2)
          all (List(sub2, sub2, sub2)) should !== (super1)
          all (List(super1, super1, super1)) should === (super1)
          all (List(super1, super1, super1)) should === (sub1)
          all (List(sub1, sub1, sub1)) should === (super1)

          val caught1 = intercept[TestFailedException] { all (List(super1, super1, super1)) should === (null) }
          caught1.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, Super(1) did not equal null (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(Super(1), Super(1), Super(1))"))
          all (List(super1, super1, super1)) should !== (null)

          all (List(nullSuper, nullSuper, nullSuper)) should === (null)
          val caught2 = intercept[TestFailedException] { all (List(nullSuper, nullSuper, nullSuper)) should !== (null) }
          caught2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, null equaled null (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(null, null, null)"))
          val caught3 = intercept[TestFailedException] { all (List(nullSuper, nullSuper, nullSuper)) should === (super1) }
          caught3.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, null did not equal Super(1) (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(null, null, null)"))
          all (List(nullSuper, nullSuper, nullSuper)) should !== (super1)

          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should === (Map("I" -> 1, "II" -> 2))
          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should !== (Map("1" -> 1, "2" -> 2))

          intercept[TestFailedException] { all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should === (7) }
          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should !== (7)

          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should === (Set(1, 2, 3))
          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should !== (Set(2, 3, 4))

          intercept[TestFailedException] { all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should === (7) }
          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should !== (7)

          all (List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) should === (List(1, 2, 3))
          all (List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) should !== (List(2, 3, 4))

          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should === (Array(1, 2, 3))
          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should !== (Array(2, 3, 4))

          all (List(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3))) should === (Array(1, 2, 3))
          all (List(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3))) should !== (Array(2, 3, 4))

          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should === (Seq(1, 2, 3))
          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should !== (Seq(2, 3, 4))

          all (List((), (),())) should === (())
          all (List((), (),())) should !== (7)
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when TripleEquals imported` {

        object O extends TripleEquals
        import O._

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          all (List(1, 1, 1)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

          all (List(ap, ap, ap)) should === (fr)
          all (List(fr, fr, fr)) should === (ap)
          all (List(ap, ap, ap)) should === (cr)
          all (List(cr, cr, cr)) should === (ap)

          all (List(super1, super1, super1)) should !== (super2)
          all (List(super1, super1, super1)) should !== (sub2)
          all (List(sub2, sub2, sub2)) should !== (super1)
          all (List(super1, super1, super1)) should === (super1)
          all (List(super1, super1, super1)) should === (sub1)
          all (List(sub1, sub1, sub1)) should === (super1)

          // The rest should not compile
          // all (List(1, 1, 1)) should === (1L)
          // all (List(1L, 1L, 1L)) should === (1)
          // all (List(1, 1, 1)) should !== (1L)
          // all (List(1L, 1L, 1L)) should !== (1)

          // all (List("1", "1", "1")) should === (1)
          // all (List(1, 1, 1)) should === ("1")
          // all (List("1", "1", "1")) should !== (1)
          // all (List(1, 1, 1)) should !== ("1")

          // all (List(fr, fr, fr)) should === (cr)
          // all (List(cr, cr, cr)) should === (fr)
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when TripleEquals mixed in` {

        object O extends TripleEquals {

          new TypeCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy
  
            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple
  
            all (List(1, 1, 1)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }
  
            all (List(ap, ap, ap)) should === (fr)
            all (List(fr, fr, fr)) should === (ap)
            all (List(ap, ap, ap)) should === (cr)
            all (List(cr, cr, cr)) should === (ap)

            all (List(super1, super1, super1)) should !== (super2)
            all (List(super1, super1, super1)) should !== (sub2)
            all (List(sub2, sub2, sub2)) should !== (super1)
            all (List(super1, super1, super1)) should === (super1)
            all (List(super1, super1, super1)) should === (sub1)
            all (List(sub1, sub1, sub1)) should === (super1)
  
            // The rest should not compile
            // all (List(1, 1, 1)) should === (1L)
            // all (List(1L, 1L, 1L)) should === (1)
            // all (List(1, 1, 1)) should !== (1L)
            // all (List(1L, 1L, 1L)) should !== (1)
  
            // all (List("1", "1", "1")) should === (1)
            // all (List(1, 1, 1)) should === ("1")
            // all (List("1", "1", "1")) should !== (1)
            // all (List(1, 1, 1)) should !== ("1")
  
            // all (List(fr, fr, fr)) should === (cr)
            // all (List(cr, cr, cr)) should === (fr)
          }
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TripleEquals imported` {

        object O extends TripleEquals
        import O._

        new ConversionCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            all (List(1, 1, 1)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

            all (List(ap, ap, ap)) should === (fr)
            all (List(fr, fr, fr)) should === (ap)
            all (List(ap, ap, ap)) should === (cr)
            all (List(cr, cr, cr)) should === (ap)

            all (List(super1, super1, super1)) should !== (super2)
            all (List(super1, super1, super1)) should !== (sub2)
            all (List(sub2, sub2, sub2)) should !== (super1)
            all (List(super1, super1, super1)) should === (super1)
            all (List(super1, super1, super1)) should === (sub1)
            all (List(sub1, sub1, sub1)) should === (super1)

            // These should work with implicit conversions
            all (List(1, 1, 1)) should === (1L)
            all (List(1L, 1L, 1L)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }
            intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

            // The rest should not compile
            // all (List("1", "1", "1")) should === (1)
            // all (List(1, 1, 1)) should === ("1")
            // all (List("1", "1", "1")) should !== (1)
            // all (List(1, 1, 1)) should !== ("1")

            // all (List(fr, fr, fr)) should === (cr)
            // all (List(cr, cr, cr)) should === (fr)
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TripleEquals mixed in` {

        object O extends TripleEquals {

          new ConversionCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            all (List(1, 1, 1)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

            all (List(ap, ap, ap)) should === (fr)
            all (List(fr, fr, fr)) should === (ap)
            all (List(ap, ap, ap)) should === (cr)
            all (List(cr, cr, cr)) should === (ap)

            all (List(super1, super1, super1)) should !== (super2)
            all (List(super1, super1, super1)) should !== (sub2)
            all (List(sub2, sub2, sub2)) should !== (super1)
            all (List(super1, super1, super1)) should === (super1)
            all (List(super1, super1, super1)) should === (sub1)
            all (List(sub1, sub1, sub1)) should === (super1)

            // These should work with implicit conversions
            all (List(1, 1, 1)) should === (1L)
            all (List(1L, 1L, 1L)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }
            intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

            // The rest should not compile
            // all (List("1", "1", "1")) should === (1)
            // all (List(1, 1, 1)) should === ("1")
            // all (List("1", "1", "1")) should !== (1)
            // all (List(1, 1, 1)) should !== ("1")

            // all (List(fr, fr, fr)) should === (cr)
            // all (List(cr, cr, cr)) should === (fr)
          }
        }
      }
    }

    object `with TypeCheckedTripleEquals` {

      def `should compare supertypes with subtypes on either side` {

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          all (List(1, 1, 1)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

          all (List(ap, ap, ap)) should === (fr)
          all (List(fr, fr, fr)) should === (ap)
          all (List(ap, ap, ap)) should === (cr)
          all (List(cr, cr, cr)) should === (ap)

          all (List(super1, super1, super1)) should !== (super2)
          all (List(super1, super1, super1)) should !== (sub2)
          all (List(sub2, sub2, sub2)) should !== (super1)
          all (List(super1, super1, super1)) should === (super1)
          all (List(super1, super1, super1)) should === (sub1)
          all (List(sub1, sub1, sub1)) should === (super1)

          val caught1 = intercept[TestFailedException] { all (List(super1, super1, super1)) should === (null) }
          caught1.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, Super(1) did not equal null (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(Super(1), Super(1), Super(1))"))
          all (List(super1, super1, super1)) should !== (null)

          all (List(nullSuper, nullSuper, nullSuper)) should === (null)
          val caught2 = intercept[TestFailedException] { all (List(nullSuper, nullSuper, nullSuper)) should !== (null) }
          caught2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, null equaled null (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(null, null, null)"))
          val caught3 = intercept[TestFailedException] { all (List(nullSuper, nullSuper, nullSuper)) should === (super1) }
          caught3.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, null did not equal Super(1) (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(null, null, null)"))
          all (List(nullSuper, nullSuper, nullSuper)) should !== (super1)

          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should === (Map("I" -> 1, "II" -> 2))
          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should !== (Map("1" -> 1, "2" -> 2))

          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should === (Set(1, 2, 3))
          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should !== (Set(2, 3, 4))

          all (List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) should === (List(1, 2, 3))
          all (List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) should !== (List(2, 3, 4))

          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should === (Array(1, 2, 3))
          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should !== (Array(2, 3, 4))

          all (List((), (),())) should === (())

          // The rest should not compile
          // all (List((), (),())) should !== (7)

          // all (List(1, 1, 1)) should === (1L)
          // all (List(1L, 1L, 1L)) should === (1)
          // all (List(1, 1, 1)) should !== (1L)
          // all (List(1L, 1L, 1L)) should !== (1)

          // all (List("1", "1", "1")) should === (1)
          // all (List(1, 1, 1)) should === ("1")
          // all (List("1", "1", "1")) should !== (1)
          // all (List(1, 1, 1)) should !== ("1")

          // all (List(fr, fr, fr)) should === (cr)
          // all (List(cr, cr, cr)) should === (fr)

          // all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should === (Seq(1, 2, 3))
          // all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should !== (Seq(2, 3, 4))

          // all (List(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3))) should === (Array(1, 2, 3))
          // all (List(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3))) should !== (Array(2, 3, 4))

          // intercept[TestFailedException] { all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should === (7) }
          // all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should !== (7)

          // intercept[TestFailedException] { all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should === (7) }
          // all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should !== (7)
        }
      }

      def `should be overridable with TripleEquals locally when TypeCheckedTripleEquals imported` {

        object O extends TypeCheckedTripleEquals
        import O._

        new TripleEquals {

          all (List(1, 1, 1)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

          all (List(1, 1, 1)) should === (1L)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }

          all (List(1L, 1L, 1L)) should === (1)
          intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

          all (List("1", "1", "1")) should !== (1)
          intercept[TestFailedException] { all (List("1", "1", "1")) should === (1) }

          all (List(1, 1, 1)) should !== ("1")
          intercept[TestFailedException] { all (List(1, 1, 1)) should === ("1") }

          all (List(super1, super1, super1)) should !== (super2)
          all (List(super1, super1, super1)) should !== (sub2)
          // all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should === (super1)
          all (List(super1, super1, super1)) should === (sub1)
          // all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with TripleEquals locally when TypeCheckedTripleEquals mixed in` {

        object O extends TypeCheckedTripleEquals {

          new TripleEquals {

            all (List(1, 1, 1)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

            all (List(1, 1, 1)) should === (1L)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }

            all (List(1L, 1L, 1L)) should === (1)
            intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

            all (List("1", "1", "1")) should !== (1)
            intercept[TestFailedException] { all (List("1", "1", "1")) should === (1) }

            all (List(1, 1, 1)) should !== ("1")
            intercept[TestFailedException] { all (List(1, 1, 1)) should === ("1") }

            all (List(super1, super1, super1)) should !== (super2)
            all (List(super1, super1, super1)) should !== (sub2)
            // all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should === (super1)
            all (List(super1, super1, super1)) should === (sub1)
            // all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9
          }
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TypeCheckedTripleEquals imported` {

        object O extends TypeCheckedTripleEquals
        import O._

        new ConversionCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          all (List(1, 1, 1)) should === (1) // compiles on 2.10 but not 2.9
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) } // compiles on 2.10 but not 2.9

          all (List(ap, ap, ap)) should === (fr) // compiles on 2.10 but not 2.9
          all (List(fr, fr, fr)) should === (ap) // compiles on 2.10 but not 2.9
          all (List(ap, ap, ap)) should === (cr) // compiles on 2.10 but not 2.9
          all (List(cr, cr, cr)) should === (ap) // compiles on 2.10 but not 2.9

          all (List(super1, super1, super1)) should !== (super2) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should !== (sub2) // compiles on 2.10 but not 2.9
          all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should === (super1) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should === (sub1) // compiles on 2.10 but not 2.9
          all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9

          // These should work with implicit conversions
          all (List(1, 1, 1)) should === (1L)
          all (List(1L, 1L, 1L)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }
          intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

          // The rest should not compile
          // all (List("1", "1", "1")) should === (1)
          // all (List(1, 1, 1)) should === ("1")
          // all (List("1", "1", "1")) should !== (1)
          // all (List(1, 1, 1)) should !== ("1")

          // all (List(fr, fr, fr)) should === (cr)
          // all (List(cr, cr, cr)) should === (fr)
        }
      }

      def `should be overridable with ConversionCheckedTripleEquals locally when TypeCheckedTripleEquals mixed in` {

        object O extends TypeCheckedTripleEquals {

          new ConversionCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            all (List(1, 1, 1)) should === (1) // compiles on 2.10 but not 2.9
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) } // compiles on 2.10 but not 2.9

            all (List(ap, ap, ap)) should === (fr) // compiles on 2.10 but not 2.9
            all (List(fr, fr, fr)) should === (ap) // compiles on 2.10 but not 2.9
            all (List(ap, ap, ap)) should === (cr) // compiles on 2.10 but not 2.9
            all (List(cr, cr, cr)) should === (ap) // compiles on 2.10 but not 2.9

            all (List(super1, super1, super1)) should !== (super2) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should !== (sub2) // compiles on 2.10 but not 2.9
            all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should === (super1) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should === (sub1) // compiles on 2.10 but not 2.9
            all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9

            // These should work with implicit conversions
            all (List(1, 1, 1)) should === (1L)
            all (List(1L, 1L, 1L)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }
            intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

            // The rest should not compile
            // all (List("1", "1", "1")) should === (1)
            // all (List(1, 1, 1)) should === ("1")
            // all (List("1", "1", "1")) should !== (1)
            // all (List(1, 1, 1)) should !== ("1")

            // all (List(fr, fr, fr)) should === (cr)
            // all (List(cr, cr, cr)) should === (fr)
          }
        }
      }
    }

    object `with ConversionCheckedTripleEquals` {

      def `should compare supertypes with subtypes on either side as well as types with implicit conversions in either direction` {

        new ConversionCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          all (List(1, 1, 1)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

          all (List(ap, ap, ap)) should === (fr)
          all (List(fr, fr, fr)) should === (ap)
          all (List(ap, ap, ap)) should === (cr)
          all (List(cr, cr, cr)) should === (ap)

          all (List(super1, super1, super1)) should !== (super2)
          all (List(super1, super1, super1)) should !== (sub2)
          all (List(sub2, sub2, sub2)) should !== (super1)
          all (List(super1, super1, super1)) should === (super1)
          all (List(super1, super1, super1)) should === (sub1)
          all (List(sub1, sub1, sub1)) should === (super1)

          // These should work with implicit conversions
          all (List(1, 1, 1)) should === (1L)
          all (List(1L, 1L, 1L)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) }
          intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

          // Should work sensibly with nulls
          val caught1 = intercept[TestFailedException] { all (List(super1, super1, super1)) should === (null) }
          caught1.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, Super(1) did not equal null (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(Super(1), Super(1), Super(1))"))
          all (List(super1, super1, super1)) should !== (null)

          all (List(nullSuper, nullSuper, nullSuper)) should === (null)
          val caught2 = intercept[TestFailedException] { all (List(nullSuper, nullSuper, nullSuper)) should !== (null) }
          caught2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, null equaled null (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(null, null, null)"))
          val caught3 = intercept[TestFailedException] { all (List(nullSuper, nullSuper, nullSuper)) should === (super1) }
          caught3.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index 0, null did not equal Super(1) (ShouldCollectedTripleEqualsSpec.scala:" + (thisLineNumber - 2) + ") \n" +
                                    "in List(null, null, null)"))
          all (List(nullSuper, nullSuper, nullSuper)) should !== (super1)

          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should === (Map("I" -> 1, "II" -> 2))
          all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should !== (Map("1" -> 1, "2" -> 2))

          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should === (Set(1, 2, 3))
          all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should !== (Set(2, 3, 4))

          all (List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) should === (List(1, 2, 3))
          all (List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) should !== (List(2, 3, 4))

          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should === (Array(1, 2, 3))
          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should !== (Array(2, 3, 4))

          all (List(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3))) should === (Array(1, 2, 3))
          all (List(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3))) should !== (Array(2, 3, 4))

          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should === (Seq(1, 2, 3))
          all (List(Array(1, 2, 3), Array(1, 2, 3), Array(1, 2, 3))) should !== (Seq(2, 3, 4))

          all (List((), (),())) should === (())

          // The rest should not compile
          // all (List((), (),())) should !== (7)

          // all (List("1", "1", "1")) should === (1)
          // all (List(1, 1, 1)) should === ("1")
          // all (List("1", "1", "1")) should !== (1)
          // all (List(1, 1, 1)) should !== ("1")

          // all (List(fr, fr, fr)) should === (cr)
          // all (List(cr, cr, cr)) should === (fr)

          // intercept[TestFailedException] { all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should === (7) }
          // all (List(Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2), Map("I" -> 1, "II" -> 2))) should !== (7)

          // intercept[TestFailedException] { all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should === (7) }
          // all (List(Set(1, 2, 3), Set(1, 2, 3), Set(1, 2, 3))) should !== (7)
        }
      }

      def `should be overridable with TripleEquals locally when ConversionCheckedTripleEquals imported` {

        object O extends ConversionCheckedTripleEquals
        import O._

        new TripleEquals {

          all (List(1, 1, 1)) should === (1)
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

          all (List(1, 1, 1)) should === (1L) // compiles on 2.10 but not 2.9
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) } // compiles on 2.10 but not 2.9

          all (List(1L, 1L, 1L)) should === (1)
          intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

          all (List("1", "1", "1")) should !== (1)
          intercept[TestFailedException] { all (List("1", "1", "1")) should === (1) }

          all (List(1, 1, 1)) should !== ("1")
          intercept[TestFailedException] { all (List(1, 1, 1)) should === ("1") }

          all (List(super1, super1, super1)) should !== (super2)
          all (List(super1, super1, super1)) should !== (sub2)
          all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should === (super1)
          all (List(super1, super1, super1)) should === (sub1)
          all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with TripleEquals locally when ConversionCheckedTripleEquals mixed in` {

        object O extends ConversionCheckedTripleEquals {

          new TripleEquals {

            all (List(1, 1, 1)) should === (1)
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) }

            all (List(1, 1, 1)) should === (1L) // compiles on 2.10 but not 2.9
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1L) } // compiles on 2.10 but not 2.9

            all (List(1L, 1L, 1L)) should === (1)
            intercept[TestFailedException] { all (List(1L, 1L, 1L)) should !== (1) }

            all (List("1", "1", "1")) should !== (1)
            intercept[TestFailedException] { all (List("1", "1", "1")) should === (1) }

            all (List(1, 1, 1)) should !== ("1")
            intercept[TestFailedException] { all (List(1, 1, 1)) should === ("1") }

            all (List(super1, super1, super1)) should !== (super2)
            all (List(super1, super1, super1)) should !== (sub2)
            all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should === (super1)
            all (List(super1, super1, super1)) should === (sub1)
            all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9
          }
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when ConversionCheckedTripleEquals imported` {

        object O extends ConversionCheckedTripleEquals
        import O._

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          all (List(1, 1, 1)) should === (1) // compiles on 2.10 but not 2.9
          intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) } // compiles on 2.10 but not 2.9

          all (List(ap, ap, ap)) should === (fr) // compiles on 2.10 but not 2.9
          all (List(fr, fr, fr)) should === (ap) // compiles on 2.10 but not 2.9
          all (List(ap, ap, ap)) should === (cr) // compiles on 2.10 but not 2.9
          all (List(cr, cr, cr)) should === (ap) // compiles on 2.10 but not 2.9

          all (List(super1, super1, super1)) should !== (super2) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should !== (sub2) // compiles on 2.10 but not 2.9
          all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should === (super1) // compiles on 2.10 but not 2.9
          all (List(super1, super1, super1)) should === (sub1) // compiles on 2.10 but not 2.9
          all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9

          // The rest should not compile
          // all (List(1, 1, 1)) should === (1L)
          // all (List(1L, 1L, 1L)) should === (1)
          // all (List(1, 1, 1)) should !== (1L)
          // all (List(1L, 1L, 1L)) should !== (1)

          // all (List("1", "1", "1")) should === (1)
          // all (List(1, 1, 1)) should === ("1")
          // all (List("1", "1", "1")) should !== (1)
          // all (List(1, 1, 1)) should !== ("1")

          // all (List(fr, fr, fr)) should === (cr)
          // all (List(cr, cr, cr)) should === (fr)
        }
      }

      def `should be overridable with TypeCheckedTripleEquals locally when ConversionCheckedTripleEquals mixed in` {

        object O extends ConversionCheckedTripleEquals {

          new TypeCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy

            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple

            all (List(1, 1, 1)) should === (1) // compiles on 2.10 but not 2.9
            intercept[TestFailedException] { all (List(1, 1, 1)) should !== (1) } // compiles on 2.10 but not 2.9

            all (List(ap, ap, ap)) should === (fr) // compiles on 2.10 but not 2.9
            all (List(fr, fr, fr)) should === (ap) // compiles on 2.10 but not 2.9
            all (List(ap, ap, ap)) should === (cr) // compiles on 2.10 but not 2.9
            all (List(cr, cr, cr)) should === (ap) // compiles on 2.10 but not 2.9

            all (List(super1, super1, super1)) should !== (super2) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should !== (sub2) // compiles on 2.10 but not 2.9
            all (List(sub2, sub2, sub2)) should !== (super1) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should === (super1) // compiles on 2.10 but not 2.9
            all (List(super1, super1, super1)) should === (sub1) // compiles on 2.10 but not 2.9
            all (List(sub1, sub1, sub1)) should === (super1) // compiles on 2.10 but not 2.9

            // The rest should not compile
            // all (List(1, 1, 1)) should === (1L)
            // all (List(1L, 1L, 1L)) should === (1)
            // all (List(1, 1, 1)) should !== (1L)
            // all (List(1L, 1L, 1L)) should !== (1)

            // all (List("1", "1", "1")) should === (1)
            // all (List(1, 1, 1)) should === ("1")
            // all (List("1", "1", "1")) should !== (1)
            // all (List(1, 1, 1)) should !== ("1")

            // all (List(fr, fr, fr)) should === (cr)
            // all (List(cr, cr, cr)) should === (fr)
          }
        }
      }
    }
  }
}

