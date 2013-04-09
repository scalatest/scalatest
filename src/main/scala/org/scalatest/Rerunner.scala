/*
 * Copyright 2001-2008 Artima, Inc.
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
 * Trait whose instances can rerun tests or other entities (such as suites). An object extending
 * this trait can be passed to a <code>Reporter</code> as part of a <code>Report</code>. The
 * test or other entity about which the report is made can then be rerun by invoking the
 * <code>rerun</code> method on the <code>Rerunnable</code>.
 *
 * @author Bill Venners
 */
@deprecated("We are considering removing Rerunner in ScalaTest 2.0 and would like to know if anyone is using it. If you are, please email scalatest-users@googlegroups.com or and describe your use case. Thanks!")
trait Rerunner /* extends ((Reporter, Stopper, Filter, ConfigMap, Option[Distributor], Tracker, ClassLoader) => Unit) */ {

  /**
   * Rerun a test or other entity (such as a suite), reporting results to the specified <code>Reporter</code>.
   *
   * @param reporter the <code>Reporter</code> to which results will be reported
   * @param stopper the <code>Stopper</code> that will be consulted to determine whether to stop execution early.
   * @param filter a <code>Filter</code> with which to filter tests based on their tags
   * @param configMap a <code>Map</code> of key-value pairs that can be used by the suite or test being rerun
   * @param distributor an optional <code>Distributor</code>, into which to put nested <code>Suite</code>s, if any, to be executed
   *              by another entity, such as concurrently by a pool of threads. If <code>None</code>, nested <code>Suite</code>s will be executed sequentially.
   * @param loader the <code>ClassLoader</code> from which to load classes needed to rerun
   *     the test or suite.
   *
   * @throws NullPointerException if any of the passed values are <code>null</code>.
   */
  def apply(reporter: Reporter, stopper: Stopper, filter: Filter,
            configMap: ConfigMap, distributor: Option[Distributor], tracker: Tracker, loader: ClassLoader): Unit
}

/**
 * TODO: Document in 2.0 release notes that I removed this deprecated implicit.
 * Companion object to Rerunner that holds a deprecated implicit conversion.
 */
private[scalatest] object Rerunner {

  /*
   * Converts a <code>Rerunner</code> to a function type that prior to the ScalaTest 1.5 release the
   * <code>Rerunner</code> extended.
   *
   * <p>
   * Prior to ScalaTest 1.5, <code>Rerunner</code> extended function type <code>(Reporter, Stopper, Filter, Map[String, Any], Option[Distributor], Tracker, ClassLoader) => Unit</code>.
   * This inheritance relationship was severed in 1.5 to make it possible to implement <code>Rerunner</code>s in Java, a request by an IDE
   * vendor to isolate their ScalaTest integration from binary incompatibility between different Scala/ScalaTest releases.
   * To make a trait easily implementable in Java, it needs to have no concrete methods. <code>Rerunner</code> itself does not declare
   * any concrete methods, but <code>(Reporter, Stopper, Filter, Map[String, Any], Option[Distributor], Tracker, ClassLoader) => Unit</code> does.
   * </p>
   *
   * <p>
   * This implicit conversion was added in ScalaTest 1.5 to avoid breaking any source code that was actually using
   * <code>Rerunner</code> as an <code>(Reporter, Stopper, Filter, Map[String, Any], Option[Distributor], Tracker, ClassLoader) => Unit</code> function. It is unlikely anyone was actually doing that, but if you were
   * and now get the deprecation warning, please email scalatest-users@googlegroups.com if you believe this implicit conversion should
   * be retained. If no one steps forward with a compelling justification, it will be removed in a future version of ScalaTest.
   * </p>
   */
/*
  @deprecated("See the documentation for Rerunner.convertRerunnerToFunction for information")
  implicit def convertRerunnerToFunction(rerunner: Rerunner): (Reporter, Stopper, Filter, Map[String, Any], Option[Distributor], Tracker, ClassLoader) => Unit =
  (reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: Map[String, Any], distributor: Option[Distributor], tracker: Tracker, loader: ClassLoader) => rerunner(reporter,
          stopper, filter, configMap, distributor, tracker, loader)
*/
}
  /* *
   * Converts a <code>Rerunner</code> to a function type that prior to the ScalaTest 1.5 release the
   * <code>Rerunner</code> extended.
   *
   * <p>
   * Prior to ScalaTest 1.5, <code>Rerunner</code> extended function type <code>FFF</code>.
   * This inheritance relationship was severed in 1.5 to make it possible to implement <code>Rerunner</code>s in Java, a request by an IDE
   * vendor to isolate their ScalaTest integration from binary incompatibility between different Scala/ScalaTest releases.
   * To make a trait easily implementable in Java, it needs to have no concrete methods. <code>Rerunner</code> itself does not declare
   * any concrete methods, but <code>FFF</code> does.
   * </p>
   *
   * <p>
   * This implicit conversion was added in ScalaTest 1.5 to avoid breaking any source code that was actually using
   * <code>Rerunner</code> as an <code>FFF</code> function. It is unlikely anyone was actually doing that, but if you were
   * and now get the deprecation warning, please email scalatest-users@googlegroups.com if you believe this implicit conversion should
   * be retained. If no one steps forward with a compelling justification, it will be removed in a future version of ScalaTest.
   * </p>
   */
