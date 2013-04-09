package org.scalatest

import collection.GenTraversable
import matchers.ShouldMatchers.TraversableShouldWrapper

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
 * the matchers DSL.
 *
 * You should only use this trait when you are not using ShouldMatchers, if you use ShouldMatchers, there's no need to use this trait anymore as 
 * ShouldMatchers already support .loneElement syntax.
 */
trait LoneElement {

  final class LoneElementTraversableWrapper[T](xs: GenTraversable[T]) {
      /**
       * This method enables syntax such as the following:
       *
       * <pre class="stHighlight">
       * xs.loneElement should be &gt; 9
       *    ^
       * </pre>
       */
      def loneElement: T = {
        if (xs.size == 1)
          xs.head
        else
          throw new exceptions.TestFailedException(
            Some(FailureMessages(
                   "notLoneElement",
                   xs,
                   xs.size)), 
            None, 
            1
          )
      }
  }

  /**
   * This implicit conversion method enables ScalaTest matchers expressions like the following:
   *
   * <pre class="stHighlight">
   * xs.loneElement should be &gt; 9
   *    ^
   * </pre>
   */
  implicit def convertToTraversableLoneElementWrapper[T](xs: GenTraversable[T]): LoneElementTraversableWrapper[T] = new LoneElementTraversableWrapper[T](xs)
}

