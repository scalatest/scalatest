package org.scalactic

trait Pretty[T] {
  def apply(o: T): String
}

object Pretty {
  def apply[T](f: T => String): Pretty[T] =
    new Pretty[T] {
      def apply(o: T): String = f(o)
    }
}

