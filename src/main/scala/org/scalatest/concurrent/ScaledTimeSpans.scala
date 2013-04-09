/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest.time.Span
import org.scalatest.tools.Runner

/*
I have checked in the PoC at the following branch:-

https://scalatest.googlecode.com/svn/branches/akka-span-factor 

The overriding is pretty straight forward (though it did take some time for me to read through akka's doc to figure out how to get the akka.test.timefactor correctly):-

import org.scalatest.concurrent.Eventually
import akka.actor.ActorSystem
import akka.testkit.TestKitExtension

object AkkaEventually extends Eventually {
  override def spanScaleFactor: Double = TestKitExtension.get(ActorSystem()).TestTimeFactor
}

and the TestTimeFactor is defined in application.conf:-

akka {
  test {
    timefactor = 2.0
  }
}

I have checked in AkkaEventuallySpec that checks the behavior, you can open and run the project using eclipse, with dependency to jars (couldn't find akka maven configuration in their download page) I attached and scalatest 1.8 RC2 that built from to-release-as-1.8 branch.

Hope this helps.

Thanks!
*/
/*
import org.scalatest.concurrent.SpanScaleFactor
import akka.actor.ActorSystem
import akka.testkit.TestKitExtension

trait AkkaSpanScaleFactor extends SpanScaleFactor {
  override def spanScaleFactor: Double = TestKitExtension.get(ActorSystem()).TestTimeFactor
}

and the TestTimeFactor is defined in application.conf:-

akka {
  test {
    timefactor = 2.0
  }
}

class MySpec extends FunSpec with Eventually with AkkaSpanScaleFactor {
  // ..
}
*/
/**
 * Trait providing a <code>scaled</code> method that can be used to scale time
 * <code>Span</code>s used during the testing of asynchronous operations.
 *
 * <p>
 * The <code>scaled</code> method allows tests of asynchronous operations to be tuned
 * according to need.  For example, <code>Span</code>s can be scaled larger when running
 * tests on slower continuous integration servers or smaller when running on faster
 * development machines.
 * </p>
 *
 * <p>
 * The <code>Double</code> factor by which to scale the <code>Span</code>s passed to
 * <code>scaled</code> is obtained from the <code>spanScaleFactor</code> method, also declared
 * in this trait. By default this method returns 1.0, but can be configured to return
 * a different value by passing a <code>-F</code> argument to <code>Runner</code> (or
 * an equivalent mechanism in an ant, sbt, or Maven build file).
 * </p>
 *
 * <p>
 * The default timeouts and intervals defined for for traits <code>Eventually</code> and
 * <code>AsyncAssertions</code> invoke <code>scaled</code>, so those defaults 
 * will be scaled automatically. Other than such defaults, however, to get a <code>Span</code>
 * to scale you'll need to explicitly pass it to <code>scaled</code>.
 * For example, here's how you would scale a <code>Span</code> you supply to 
 * the <code>failAfter</code> method from trait <code>Timeouts</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * failAfter(scaled(150 millis)) {
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * The reason <code>Span</code>s are not scaled automatically in the general case is
 * to make code obvious. If a reader sees <code>failAfter(1 second)</code>, it will
 * mean exactly that: fail after one second. And if a <code>Span</code> will be scaled,
 * the reader will clearly see that as well: <code>failAfter(scaled(1 second))</code>.
 * </p>
 *
 * <h2>Overriding <code>spanScaleFactor</code></h2>
 * 
 * </p>
 * You can override the <code>spanScaleFactor</code> method to configure the factor by a
 * different means. For example, to configure the factor from Akka
 * TestKit's test time factor you might create a trait like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.concurrent.SpanScaleFactor
 * import akka.actor.ActorSystem
 * import akka.testkit.TestKitExtension
 *
 * trait AkkaSpanScaleFactor extends SpanScaleFactor {
 *   override def spanScaleFactor: Double =
 *       TestKitExtension.get(ActorSystem()).TestTimeFactor
 * }
 * </pre>
 *
 * <p>
 * This trait overrides <code>spanScaleFactor</code> so that it takes its
 * scale factor from Akka's <code>application.conf</code> file.
 * You could then scale <code>Span</code>s tenfold in Akka's configuration file
 * like this:
 * </p>
 *
 * <pre>
 * akka {
 *   test {
 *     timefactor = 10.0
 *   }
 * }
 * </pre>
 *
 * <p>
 * Armed with this trait and configuration file, you can simply mix trait
 * <code>AkkaSpanScaleFactor</code> into any test class whose <code>Span</code>s
 * you want to scale, like this:
 * 
 * <pre class="stHighlight">
 * class MySpec extends FunSpec with Eventually with AkkaSpanScaleFactor {
 *   // ..
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait ScaledTimeSpans {

  /**
   * Scales the passed <code>Span</code> by the <code>Double</code> factor returned
   * by <code>spanScaleFactor</code>.
   *
   * <p>
   * The <code>Span</code> is scaled by invoking its <code>scaledBy</code> method,
   * thus this method has the same behavior:
   * The value returned by <code>spanScaleFactor</code> can be any positive number or zero,
   * including a fractional number. A number greater than one will scale the <code>Span</code>
   * up to a larger value. A fractional number will scale it down to a smaller value. A
   * factor of 1.0 will cause the exact same <code>Span</code> to be returned. A
   * factor of zero will cause <code>Span.ZeroLength</code> to be returned.
   * If overflow occurs, <code>Span.Max</code> will be returned. If underflow occurs,
   * <code>Span.ZeroLength</code> will be returned.
   * </p>
   *
   * @throws IllegalArgumentException if the value returned from <code>spanScaleFactor</code>
   *           is less than zero
   */
  final def scaled(span: Span): Span = span scaledBy spanScaleFactor

  /**
   * The factor by which the <code>scaled</code> method will scale <code>Span</code>s.
   *
   * <p>
   * The default implementation of this method will return the <em>span scale factor</em> that 
   * was specified for the run, or 1.0 if no factor was specified. For example, you can specify a span scale factor when invoking ScalaTest
   * via the command line by passing a <a href="../tools/Runner$.html#scalingTimeSpans"><code>-F</code> argument</a> to <a href="../tools/Runner$.html"><code>Runner</code></a>.
   * </p>
   */
  def spanScaleFactor: Double = Runner.spanScaleFactor
}

