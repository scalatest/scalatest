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

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class TripleEqualsSpec extends Spec with NonImplicitAssertions {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  object `the custom equality === operator` {

    object `with TripleEquals` {

      def `should compare anything with anything` {

        new TripleEquals {

          assert(1 === 1)
          assert(!(1 !== 1))

          assert(1 === 1L)
          assert(!(1 !== 1L))

          assert(1L === 1)
          assert(!(1L !== 1))

          assert("1" !== 1)
          assert(!("1" === 1))

          assert(1 !== "1")
          assert(!(1 === "1"))

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          assert(!(super1 === null))
          assert(super1 !== null)

          assert(nullSuper === null)
          assert(!(nullSuper !== null))
          assert(!(nullSuper === super1))
          assert(nullSuper !== super1)
        }
      }

/*
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

          assert(1 === 1)
          assert(!(1 !== 1))

          assert(ap === fr)
          assert(fr === ap)
          assert(ap === cr)
          assert(cr === ap)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          // The rest should not compile
          // assert(1 === 1L)
          // assert(1L === 1)
          // assert(1 !== 1L)
          // assert(1L !== 1)

          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
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
  
            assert(1 === 1)
            assert(!(1 !== 1))
  
            assert(ap === fr)
            assert(fr === ap)
            assert(ap === cr)
            assert(cr === ap)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            assert(sub2 !== super1)
            assert(super1 === super1)
            assert(super1 === sub1)
            assert(sub1 === super1)
  
            // The rest should not compile
            // assert(1 === 1L)
            // assert(1L === 1)
            // assert(1 !== 1L)
            // assert(1L !== 1)
  
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")
  
            // assert(fr === cr)
            // assert(cr === fr)
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

            assert(1 === 1)
            assert(!(1 !== 1))

            assert(ap === fr)
            assert(fr === ap)
            assert(ap === cr)
            assert(cr === ap)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            assert(sub2 !== super1)
            assert(super1 === super1)
            assert(super1 === sub1)
            assert(sub1 === super1)

            // These should work with implicit conversions
            assert(1 === 1L)
            assert(1L === 1)
            assert(!(1 !== 1L))
            assert(!(1L !== 1))

            // The rest should not compile
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
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

            assert(1 === 1)
            assert(!(1 !== 1))

            assert(ap === fr)
            assert(fr === ap)
            assert(ap === cr)
            assert(cr === ap)

            assert(super1 !== super2)
            assert(super1 !== sub2)
            assert(sub2 !== super1)
            assert(super1 === super1)
            assert(super1 === sub1)
            assert(sub1 === super1)

            // These should work with implicit conversions
            assert(1 === 1L)
            assert(1L === 1)
            assert(!(1 !== 1L))
            assert(!(1L !== 1))

            // The rest should not compile
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }
*/
    }

    object `with TypeCheckedTripleEquals` {

/*
      def `should compare supertypes with subtypes on either side` {

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          assert(1 === 1)
          assert(!(1 !== 1))

          assert(ap === fr)
          assert(fr === ap)
          assert(ap === cr)
          assert(cr === ap)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          assert(!(super1 === null))
          assert(super1 !== null)

          assert(nullSuper === null)
          assert(!(nullSuper !== null))
          assert(!(nullSuper === super1))
          assert(nullSuper !== super1)

          // The rest should not compile
          // assert(1 === 1L)
          // assert(1L === 1)
          // assert(1 !== 1L)
          // assert(1L !== 1)

          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

      def `should be overridable with TripleEquals locally when TypeCheckedTripleEquals imported` {

        object O extends TypeCheckedTripleEquals
        import O._

        new TripleEquals {

          assert(1 === 1)
          assert(!(1 !== 1))

          assert(1 === 1L)
          assert(!(1 !== 1L))

          assert(1L === 1)
          assert(!(1L !== 1))

          assert("1" !== 1)
          assert(!("1" === 1))

          assert(1 !== "1")
          assert(!(1 === "1"))

          assert(super1 !== super2)
          assert(super1 !== sub2)
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          assert(super1 === super1)
          assert(super1 === sub1)
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with TripleEquals locally when TypeCheckedTripleEquals mixed in` {

        object O extends TypeCheckedTripleEquals {

          new TripleEquals {

            assert(1 === 1)
            assert(!(1 !== 1))

            assert(1 === 1L)
            assert(!(1 !== 1L))

            assert(1L === 1)
            assert(!(1L !== 1))

            assert("1" !== 1)
            assert(!("1" === 1))

            assert(1 !== "1")
            assert(!(1 === "1"))

            assert(super1 !== super2)
            assert(super1 !== sub2)
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            assert(super1 === super1)
            assert(super1 === sub1)
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9
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

          // assert(1 === 1) // compiles on 2.10 but not 2.9
          // assert(!(1 !== 1)) // compiles on 2.10 but not 2.9

          // assert(ap === fr) // compiles on 2.10 but not 2.9
          // compiles on 2.10 but not 2.9/ assert(fr === ap) // compiles on 2.10 but not 2.9
          // assert(ap === cr) // compiles on 2.10 but not 2.9
          // assert(cr === ap) // compiles on 2.10 but not 2.9

          // assert(super1 !== super2) // compiles on 2.10 but not 2.9
          // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          // assert(super1 === super1) // compiles on 2.10 but not 2.9
          // assert(super1 === sub1) // compiles on 2.10 but not 2.9
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9

          // These should work with implicit conversions
          assert(1 === 1L)
          assert(1L === 1)
          assert(!(1 !== 1L))
          assert(!(1L !== 1))

          // The rest should not compile
          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
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

            // assert(1 === 1) // compiles on 2.10 but not 2.9
            // assert(!(1 !== 1)) // compiles on 2.10 but not 2.9

            // assert(ap === fr) // compiles on 2.10 but not 2.9
            // assert(fr === ap) // compiles on 2.10 but not 2.9
            // assert(ap === cr) // compiles on 2.10 but not 2.9
            // assert(cr === ap) // compiles on 2.10 but not 2.9

            // assert(super1 !== super2) // compiles on 2.10 but not 2.9
            // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            // assert(super1 === super1) // compiles on 2.10 but not 2.9
            // assert(super1 === sub1) // compiles on 2.10 but not 2.9
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9

            // These should work with implicit conversions
            assert(1 === 1L)
            assert(1L === 1)
            assert(!(1 !== 1L))
            assert(!(1L !== 1))

            // The rest should not compile
            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }
*/
    }

    object `with ConversionCheckedTripleEquals` {

/*
      def `should compare supertypes with subtypes on either side as well as types with implicit conversions in either direction` {

        new ConversionCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          assert(1 === 1)
          assert(!(1 !== 1))

          assert(ap === fr)
          assert(fr === ap)
          assert(ap === cr)
          assert(cr === ap)

          assert(super1 !== super2)
          assert(super1 !== sub2)
          assert(sub2 !== super1)
          assert(super1 === super1)
          assert(super1 === sub1)
          assert(sub1 === super1)

          // These should work with implicit conversions
          assert(1 === 1L)
          assert(1L === 1)
          assert(!(1 !== 1L))
          assert(!(1L !== 1))

          // Should work sensibly with nulls
          assert(!(super1 === null)) 
          assert(super1 !== null)

          assert(nullSuper === null)
          assert(!(nullSuper !== null))
          assert(!(nullSuper === super1))
          assert(nullSuper !== super1)

          // The rest should not compile
          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
        }
      }

      def `should be overridable with TripleEquals locally when ConversionCheckedTripleEquals imported` {

        object O extends ConversionCheckedTripleEquals
        import O._

        new TripleEquals {

          assert(1 === 1)
          assert(!(1 !== 1))

          // assert(1 === 1L) // compiles on 2.10 but not 2.9
          // assert(!(1 !== 1L)) // compiles on 2.10 but not 2.9

          assert(1L === 1)
          assert(!(1L !== 1))

          assert("1" !== 1)
          assert(!("1" === 1))

          assert(1 !== "1")
          assert(!(1 === "1"))

          assert(super1 !== super2)
          assert(super1 !== sub2)
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          assert(super1 === super1)
          assert(super1 === sub1)
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9
        }
      }

      def `should be overridable with TripleEquals locally when ConversionCheckedTripleEquals mixed in` {

        object O extends ConversionCheckedTripleEquals {

          new TripleEquals {

            assert(1 === 1)
            assert(!(1 !== 1))

            // assert(1 === 1L) // compiles on 2.10 but not 2.9
            // assert(!(1 !== 1L)) // compiles on 2.10 but not 2.9

            assert(1L === 1)
            assert(!(1L !== 1))

            assert("1" !== 1)
            assert(!("1" === 1))

            assert(1 !== "1")
            assert(!(1 === "1"))

            assert(super1 !== super2)
            assert(super1 !== sub2)
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            assert(super1 === super1)
            assert(super1 === sub1)
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9
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

          // assert(1 === 1) // compiles on 2.10 but not 2.9
          // assert(!(1 !== 1)) // compiles on 2.10 but not 2.9

          // assert(ap === fr) // compiles on 2.10 but not 2.9
          // assert(fr === ap) // compiles on 2.10 but not 2.9
          // assert(ap === cr) // compiles on 2.10 but not 2.9
          // assert(cr === ap) // compiles on 2.10 but not 2.9

          // assert(super1 !== super2) // compiles on 2.10 but not 2.9
          // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
          // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
          // assert(super1 === super1) // compiles on 2.10 but not 2.9
          // assert(super1 === sub1) // compiles on 2.10 but not 2.9
          // assert(sub1 === super1) // compiles on 2.10 but not 2.9

          // The rest should not compile
          // assert(1 === 1L)
          // assert(1L === 1)
          // assert(1 !== 1L)
          // assert(1L !== 1)

          // assert("1" === 1)
          // assert(1 === "1")
          // assert("1" !== 1)
          // assert(1 !== "1")

          // assert(fr === cr)
          // assert(cr === fr)
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

            // assert(1 === 1) // compiles on 2.10 but not 2.9
            // assert(!(1 !== 1)) // compiles on 2.10 but not 2.9

            // assert(ap === fr) // compiles on 2.10 but not 2.9
            // assert(fr === ap) // compiles on 2.10 but not 2.9
            // assert(ap === cr) // compiles on 2.10 but not 2.9
            // assert(cr === ap) // compiles on 2.10 but not 2.9

            // assert(super1 !== super2) // compiles on 2.10 but not 2.9
            // assert(super1 !== sub2) // compiles on 2.10 but not 2.9
            // assert(sub2 !== super1) // compiles on 2.10 but not 2.9
            // assert(super1 === super1) // compiles on 2.10 but not 2.9
            // assert(super1 === sub1) // compiles on 2.10 but not 2.9
            // assert(sub1 === super1) // compiles on 2.10 but not 2.9

            // The rest should not compile
            // assert(1 === 1L)
            // assert(1L === 1)
            // assert(1 !== 1L)
            // assert(1L !== 1)

            // assert("1" === 1)
            // assert(1 === "1")
            // assert("1" !== 1)
            // assert(1 !== "1")

            // assert(fr === cr)
            // assert(cr === fr)
          }
        }
      }
*/
    }
  }
}

