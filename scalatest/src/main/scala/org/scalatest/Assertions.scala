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

import org.scalactic._
import Requirements._
import scala.reflect.ClassTag
import Assertions.NormalResult
import Assertions.areEqualComparingArraysStructurally
import exceptions.StackDepthException
import exceptions.StackDepthException.toExceptionFunction
import exceptions.TestFailedException
import exceptions.TestPendingException

trait Assertions extends GenericAssertions {

  type RESULT = Assertion

}

/**
 * Companion object that facilitates the importing of <code>Assertions</code> members as 
 * an alternative to mixing it in. One use case is to import <code>Assertions</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.7.3.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala&gt; import org.scalatest.Assertions._
 * import org.scalatest.Assertions._
 * &nbsp;
 * scala&gt; assert(1 === 2)
 * org.scalatest.TestFailedException: 1 did not equal 2
 *      at org.scalatest.Assertions$class.assert(Assertions.scala:211)
 *      at org.scalatest.Assertions$.assert(Assertions.scala:511)
 *      at .&lt;init&gt;(&lt;console&gt;:7)
 *      at .&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$.&lt;init&gt;(&lt;console&gt;:3)
 *      at RequestResult$.&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$result(&lt;console&gt;)
 *      at sun.reflect.NativeMethodAccessorImpl.invoke...
 *&nbsp;
 * scala&gt; assertResult(3) { 1 + 3 }
 * org.scalatest.TestFailedException: Expected 3, but got 4
 *      at org.scalatest.Assertions$class.expect(Assertions.scala:447)
 *      at org.scalatest.Assertions$.expect(Assertions.scala:511)
 *      at .&lt;init&gt;(&lt;console&gt;:7)
 *      at .&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$.&lt;init&gt;(&lt;console&gt;:3)
 *      at RequestResult$.&lt;clinit&gt;(&lt;console&gt;)
 *      at RequestResult$result(&lt;console&gt;)
 *      at sun.reflect.NativeMethodAccessorImpl.in...
 *&nbsp;
 * scala&gt; val caught = intercept[StringIndexOutOfBoundsException] { "hi".charAt(-1) }
 * caught: StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException: String index out of range: -1
 * </pre>
 *
 * @author Bill Venners
 */
object Assertions extends Assertions {

  @deprecated("The trap method is no longer needed for demos in the REPL, which now abreviates stack traces, so NormalResult will be removed in a future version of ScalaTest")
  case class NormalResult(result: Any) extends Throwable {
    override def toString = if (result == ()) Resources.noExceptionWasThrown else Resources.resultWas(Prettifier.default(result))
  }

  private[scalatest] def areEqualComparingArraysStructurally(left: Any, right: Any): Boolean = {
    // Prior to 2.0 this only called .deep if both sides were arrays. Loosened it
    // when nearing 2.0.M6 to call .deep if either left or right side is an array.
    // TODO: this is the same algo as in scalactic.DefaultEquality. Put that one in
    // a singleton and use it in both places.
    left match {
      case leftArray: Array[_] =>
        right match {
          case rightArray: Array[_] => leftArray.deep == rightArray.deep
          case _ => leftArray.deep == right
        }
      case _ => {
        right match {
          case rightArray: Array[_] => left == rightArray.deep
          case _ => left == right
        }
      }
    }
  }
}
