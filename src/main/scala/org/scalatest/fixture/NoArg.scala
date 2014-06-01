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
package org.scalatest.fixture

import org.scalatest._

/**
 * A function that takes no parameters (<em>i.e.</em>, a <code>Function0</code> or "no-arg" function) and results in <code>Unit</code>, which when
 * invoked executes the body of the constructor of the class into which this trait is mixed.
 *
 * <p>
 * This trait extends <code>DelayedInit</code> and defines a <code>delayedInit</code> method that
 * saves the body of the constructor (passed to <code>delayedInit</code>) for later execution when <code>apply</code> is invoked.
 * </p>
 *
 * <p>
 * This trait is somewhat magical and therefore may be challenging for your collegues to understand, so please use it as a last resort only when the
 * simpler options described in the "<a href="../FlatSpec.html#sharedFixtures">shared fixtures</a>" section of your chosen style trait won't do
 * the job. <code>NoArg</code> is
 * intended to address a specific use case that will likely be rare, and is unlikely to be useful outside of its intended use case, but
 * it is quite handy for its intended use case (described in the next paragraph).
 * One potential gotcha, for example, is that a subclass's constructor body could in theory be executed multiple times by simply invoking <code>apply</code> multiple
 * times. In the intended use case for this trait, however, the body will be executed only once.
 * </p>
 *
 * <p>
 * The intended use case for this method is (relatively rare) situations in which you want to extend a different instance of the same class
 * for each test, with the body of the test inheriting the members of that class, and with code executed before and/or after
 * the body of the test.
 * </p>
 *
 * <p>
 * For example, Akka's <code>TestKit</code> class takes an <code>ActorSystem</code>,
 * which must have a unique name. To run a suite of tests in parallel, each test must get its own <code>ActorSystem</code>, to
 * ensure the tests run in isolation. At the end of each test, the <code>ActorSystem</code> must be shutdown. With <code>NoArg</code>,
 * you can achieve this by first defining a class that extends <code>TestKit</code> and mixes in <code>NoArg</code>.
 * Here's an example taken with permission from the book <a href="http://www.artima.com/shop/akka_concurrency"><em>Akka Concurrency</em></a>, by Derek Wyatt:
 * </p>
 *
 * <pre class="stHighlight">
 * import akka.actor.ActorSystem
 * import akka.testkit.{TestKit, ImplicitSender}
 * import java.util.concurrent.atomic.AtomicInteger
 * import org.scalatest.fixture.NoArg
 *
 * object ActorSys {
 *   val uniqueId = new AtomicInteger(0)
 * }
 *
 * class ActorSys(name: String) extends
 *         TestKit(ActorSystem(name))
 *         with ImplicitSender
 *         with NoArg {
 *
 *   def this() = this(
 *     "TestSystem%05d".format(
 *        ActorSys.uniqueId.getAndIncrement()))
 *
 *   def shutdown(): Unit = system.shutdown()
 *
 *   override def apply() {
 *     try super.apply()
 *     finally shutdown()
 *   }
 * }
 * </pre>
 *
 * <p>
 * Given this implementation of <code>ActorSys</code>, which will invoke <code>shutdown</code> after the constructor code
 * is executed, you can run each test in a suite in a subclass of <code>TestKit</code>, giving each test's <code>TestKit</code>
 * an <code>ActorSystem</code> with a unique name, allowing you to safely run those tests in parallel. Here's an example
 * from <em>Akka Concurrency</em>:
 * </p>
 *
 * <pre class="stHighlight">
 * class MyActorSpec extends fixture.WordSpec
 *         with Matchers
 *         with UnitFixture
 *         with ParallelTestExecution {
 * 
 *   def makeActor(): ActorRef =
 *     system.actorOf(Props[MyActor], "MyActor")
 * 
 *   "My Actor" should {
 *     "throw when made with the wrong name" in new ActorSys {
 *       evaluating {
 *         // use a generated name
 *         val a = system.actorOf(Props[MyActor])
 *       } should produce [Exception]
 *     }
 *     "construct without exception" in new ActorSys {
 *       val a = makeActor()
 *       // The throw will cause the test to fail
 *     }
 *     "respond with a Pong to a Ping" in new ActorSys {
 *       val a = makeActor()
 *       a ! Ping
 *       expectMsg(Pong)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * <a href="UnitFixture.html"><code>UnitFixture</code></a> is used in this example, because in this case, the <code>fixture.WordSpec</code> feature enabling tests to be defined as
 * functions from fixture objects of type <code>FixtureParam</code> to <code>Unit</code> is not being used. Rather, only the secondary feature that enables
 * tests to be defined as functions from <em>no parameters</em> to <code>Unit</code> is being used. This secondary feature is described in the second-to-last
 * paragraph on the main Scaladoc documentation of <a href="WordSpec.html"><code>fixture.WordSpec</code></a>, which says:
 * </p>
 *
 * <blockquote>
 * If a test doesn't need the fixture, you can indicate that by providing a no-arg instead of a one-arg function, ...
 * In other words, instead of starting your function literal
 * with something like &ldquo;<code>db =&gt;</code>&rdquo;, you'd start it with &ldquo;<code>() =&gt;</code>&rdquo;. For such tests, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
 * </blockquote>
 *
 * <p>
 * Since <code>FixtureParam</code> is unused in this use case, it could 
 * be anything. Making it <code>Unit</code> will hopefully help readers more easily recognize that it is not being used.
 * </p>
 *
 * <p>
 * Note: As of Scala 2.11, <code>DelayedInit</code> (which is used by <code>NoArg</code>) has been deprecated, to indicate it is buggy and should be avoided
 * if possible. Those in charge of the Scala compiler and standard library have promised that <code>DelayedInit</code> will not be removed from Scala
 * unless an alternate way to achieve the same goal is provided. Thus it <em>should</em> be safe to use <code>NoArg</code>, but if you'd rather
 * not you can achieve the same effect with a bit more boilerplate by extending (<code>() =&gt; Unit</code>) instead of <code>NoArg</code> and placing
 * your code in an explicit <code>body</code> method. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import akka.actor.ActorSystem
 * import akka.testkit.{TestKit, ImplicitSender}
 * import java.util.concurrent.atomic.AtomicInteger
 * import org.scalatest.fixture.NoArg
 *
 * object ActorSys {
 *   val uniqueId = new AtomicInteger(0)
 * }
 *
 * class ActorSys(name: String) extends
 *         TestKit(ActorSystem(name))
 *         with ImplicitSender
 *         with (() =&gt; Unit) {
 *
 *   def this() = this(
 *     "TestSystem%05d".format(
 *        ActorSys.uniqueId.getAndIncrement()))
 *
 *   def shutdown(): Unit = system.shutdown()
 *   def body(): Unit
 *
 *   override def apply() = {
 *     try body()
 *     finally shutdown()
 *   }
 * }
 * </pre>
 *
 * <p>
 * Using this version of <code>ActorSys</code> will require an explicit
 * <code>body</code> method in the tests:
 *
 * </p>
 *
 * <pre class="stHighlight">
 * class MyActorSpec extends fixture.WordSpec
 *         with Matchers
 *         with UnitFixture
 *         with ParallelTestExecution {
 * 
 *   def makeActor(): ActorRef =
 *     system.actorOf(Props[MyActor], "MyActor")
 * 
 *   "My Actor" should {
 *     "throw when made with the wrong name" in new ActorSys {
 *       def body() = 
 *         evaluating {
 *           // use a generated name
 *           val a = system.actorOf(Props[MyActor])
 *         } should produce [Exception]
 *     }
 *     "construct without exception" in new ActorSys {
 *       def body() = {
 *         val a = makeActor()
 *         // The throw will cause the test to fail
 *       }
 *     }
 *     "respond with a Pong to a Ping" in new ActorSys {
 *       def body() = {
 *         val a = makeActor()
 *         a ! Ping
 *         expectMsg(Pong)
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 */
trait NoArg extends DelayedInit with (() => Unit) {

  private var theBody: () => Unit = _

  /**
   * Saves the body of the constructor, passed as <code>body</code>, for later execution by <code>apply</code>.
   */
  final def delayedInit(body: => Unit): Unit = {
    synchronized { theBody = (() => body) }
  }

  /**
   * Executes the body of the constructor that was passed to <code>delayedInit</code>.
   */
  def apply(): Unit = synchronized { if (theBody != null) theBody() }

  /**
   * This method exists to cause a compile-time type error if someone accidentally 
   * tries to mix this trait into a <code>Suite</code>.
   *
   * <p>
   * This trait is intended to be mixed
   * into classes that are constructed within the body (or as the body) of tests, not mixed into <code>Suite</code>s themselves. For an example,
   * the the main Scaladoc comment for this trait.
   * </p>
   */
  final val styleName: Int = 0 // So can't mix into Suite
}

