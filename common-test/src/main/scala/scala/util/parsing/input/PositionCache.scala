package scala.util.parsing.input

import java.lang.{CharSequence, ThreadLocal}
import java.util.WeakHashMap

/**
 * @author Tomáš Janoušek
 */
private[input] trait PositionCache {
  private lazy val indexCacheTL =
    // not DynamicVariable as that would share the map from parent to child :-(
    new ThreadLocal[java.util.Map[CharSequence, Array[Int]]] {
      override def initialValue = new WeakHashMap[CharSequence, Array[Int]]
    }

  private[input] def indexCache = indexCacheTL.get
}
