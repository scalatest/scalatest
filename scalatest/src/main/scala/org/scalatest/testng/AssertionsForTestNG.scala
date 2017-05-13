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
package org.scalatest.testng

import org.scalatest._

/**
  * Trait that contains ScalaTest's basic assertion methods, suitable for use with TestNG that requires its test method to return <code>void</code> in <code>Java</code> (<code>Unit</code> in <code>Scala</code>).
  *
  * <p>
  * For details on the importing approach, see the documentation
  * for the <a href="AssertionsForTestNG$.html"><code>AssertionsForTestNG</code> companion object</a>.
  * For the details on the <code>AssertionsForTestNG</code> syntax, see the Scaladoc documentation for
  * <a href="../GenericAssertions.html"><code>org.scalatest.GenericAssertions</code></a>
  * </p>
  *
  * @author Bill Venners
  * @author Chee Seng
  */
trait AssertionsForTestNG extends GenericAssertions {

  type RESULT = Unit

}

/**
  * Companion object that facilitates the importing of <code>AssertionsForTestNG</code> members as
  * an alternative to mixing it in. One use case is to import <code>AssertionsForTestNG</code> members so you can use
  * them in the Scala interpreter:
  *
  * <pre>
  * $ scala -cp testng.jar:../target/jar_contents
  * Welcome to Scala version 2.7.5.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
  * Type in expressions to have them evaluated.
  * Type :help for more information.
  *
  * scala> import org.scalatest.junit.AssertionsForTestNG._
  * import org.scalatest.junit.AssertionsForTestNG._
  *
  * scala> assert(1 === 2)
  * junit.framework.AssertionFailedError: 1 did not equal 2
  * 	at org.scalatest.junit.AssertionsForJUnit$class.assert(AssertionsForJUnit.scala:353)
  * 	at org.scalatest.junit.AssertionsForJUnit$.assert(AssertionsForJUnit.scala:672)
  * 	at .<init>(<console>:7)
  * 	at .<clinit>(<console>)
  * 	at RequestResult$.<init>(<console>:3)
  * 	at RequestResult$.<clinit>(<console>)
  * 	at RequestResult$result(<consol...
  * scala> expect(3) { 1 + 3 }
  * junit.framework.AssertionFailedError: Expected 3, but got 4
  * 	at org.scalatest.junit.AssertionsForJUnit$class.expect(AssertionsForJUnit.scala:563)
  * 	at org.scalatest.junit.AssertionsForJUnit$.expect(AssertionsForJUnit.scala:672)
  * 	at .<init>(<console>:7)
  * 	at .<clinit>(<console>)
  * 	at RequestResult$.<init>(<console>:3)
  * 	at RequestResult$.<clinit>(<console>)
  * 	at RequestResult$result(<co...
  * scala> val caught = intercept[StringIndexOutOfBoundsException] { "hi".charAt(-1) }
  * caught: StringIndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException: String index out of range: -1
  * </pre>
  *
  * @author Bill Venners
  */
object AssertionsForTestNG extends AssertionsForTestNG
