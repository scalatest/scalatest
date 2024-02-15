package org.scalatest

import org.scalactic.Requirements.requireNonNull

/**
 * Contains information and a handle to a discovered <a href="Suite.html"><code>Suite</code></a> that will be ran or is already running in the current test run.
 *
 * @param className the name of the class of the suite
 * @param lazyHandle the lazy handle to a (usually singleton) instance of the suite.
 *                   If the suite hasn't been instantiated yet, calling this function will instantiate it.
 * @param isSingleton indicates whether calling <code>lazyHandle</code> will return a singleton instance of the Suite.
 *                    Always <code>true</code> on the JVM.
 *                    However on Scala.js and Scala Native, when tests run using a worker (<code>org.scalatest.tools.SlaveRunner</code>),
 *                    singleton-ness cannot be guaranteed anymore.
 *                    As of the time of writing, tests on Scala Native nearly always run in separate worker processes,
 *                    making singleton guarantees impossible.
 *                    If this is <code>false</code>, running <code>lazyHandle</code> will duplicate the Suite.
 *
 * @throws NullArgumentException if any passed parameter is <code>null</code>.
 */
case class RunningSuite(
  className: String,
  lazyHandle: () => Suite,
  isSingleton: Boolean
) {
    requireNonNull(className, lazyHandle, isSingleton)
}
