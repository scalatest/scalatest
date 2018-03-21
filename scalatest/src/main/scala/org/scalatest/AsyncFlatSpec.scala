/*
 * Copyright 2001-2018 Artima, Inc.
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

/**
  * This class is deprecated and will be removed in future version of ScalaTest, please use org.scalatest.flatspec.AsyncFlatSpec instead.
  */
@deprecated("Please use org.scalatest.flatspec.AsyncFlatSpec instead")
abstract class AsyncFlatSpec extends AsyncFlatSpecLike {

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, this)
}
/*
May take from this later:

In an AsyncFlatSpec, threads that transform and perform callbacks for Futures
are taken from the implicit execution context. On Scala.js, you need not
worry about thread synchronization, because JavaScript is essentially
single-threaded. On the JVM, however, multiple transformations and callbacks of Futures
that operate on mutable state will need to be synchronized, because
they may be performed by different thrads.

The best way to avoid this issue is to avoid mutable state in the first place-i.e.,
to use a functional style. Use Futures only to transform immutable objects without any
side effects. When that is not practical, one option on the JVM is to block, either using
scala.concurrent.Await or the methods of ScalaFutures.  Blocking will also allow you
to use a synchronous style, such as FlatSpec, which doesn't take threads fron
an execution context. Synchronous styles are designed so that you need not worry
about synchronizing access to mutable state shared between tests, because only
one thread executes each test. Even if you ix in PTE, you need not synchronize access
to instance variables because each test is executed in its own instance of the test
class, so each test has its own copy of the instance variables. 

On Scala.js, you can't block, so using Await or ScalaFutures will not work if you
Futures are truly asynchronous (involve calling APIs that work outside of the JS thread)
On Scala.js, therefore, you must use async style tests to test asynchrounous code,
but you need not worry about synchronoization. ON the JVM, you can block,
so you have the option to use synchronous style tests to test asynchronous code.

If you choose to write async-style tests on the JVM, you must ensure that the objects you 
manipulate through Futures are thread-safe (and the easiest way to do that is make them
immutable).
*/
