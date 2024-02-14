package org.scalatest

import org.scalactic.Requirements.requireNonNull

/**
 * Contains information and a handle to a discovered <a href="Suite.html"><code>Suite</code></a> that will be ran or is already running in the current test run.
 *
 * @param className the name of the class of the suite
 * @param lazyHandle the lazy handle to a singleton instance of the suite. If the suite hasn't been instantiated yet, calling this function will instantiate it.
 *
 * @throws NullArgumentException if any passed parameter is <code>null</code>.
 */
case class RunningSuite(
  className: String,
  lazyHandle: () => Suite
) {
    requireNonNull(className, lazyHandle)
}
