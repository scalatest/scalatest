package scala.util.parsing.input

import java.lang.CharSequence
import java.util.{AbstractMap, Collections}

private[input] trait PositionCache {
  private[input] lazy val indexCache: java.util.Map[CharSequence,Array[Int]] = new AbstractMap[CharSequence, Array[Int]] {

    override def entrySet() = Collections.emptySet()

    // the /dev/null of Maps
    override def put(ch: CharSequence, a: Array[Int]) = null
  }
}
