package org.scalatest

import enablers.Collecting

/**
 * Trait that provides an implicit conversion that adds a <code>loneElement</code> method
 * to <code>GenTraversable</code>, which will return the value of the lone element if the collection does
 * indeed contain one and only one element, or throw <code>TestFailedException</code> if not.
 *
 * <p>
 * This construct allows you to express in one statement that a collection should contain one and only one element
 * and that the element value should meet some expectation. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * set.loneElement should be &gt; 9
 * </pre>
 *
 * <p>
 * Or, using an assertion instead of a matcher expression:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(set.loneElement &gt; 9)
 * </pre>
 */
trait LoneElement {

  /**
   * Wrapper class that adds a <code>loneElement</code> method to <code>GenTraversable</code>.
   *
   * <p>
   * Through the implicit conversion provided by trait <code>LoneElement</code>, this class allows you to make statements like:
   * </p>
   *
   * <pre class="stHighlight">
   * trav.loneElement should be &gt; 9
   * </pre>
   *
   * @param trav A <code>GenTraversable</code> to wrap in a <code>LoneElementCollectionWrapper</code>, which provides the <code>loneElement</code> method.
   */
  final class LoneElementCollectionWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]]) {

    /**
     * Returns the value contained in the wrapped <code>GenTraversable</code>, if it contains one and only one element, else throws <code>TestFailedException</code> with
     * a detail message describing the problem.
     *
     * <p>
     * This method enables syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * trav.loneElement should be &gt; 9
     *      ^
     * </pre>
     */
    def loneElement: E = {
      collecting.loneElementOf(collection) match {
        case Some(ele) => ele
        case None =>
          throw new exceptions.TestFailedException(
            Some(FailureMessages(
                 "notLoneElement",
                 collection,
                 collecting.sizeOf(collection))), 
            None, 
            1
          )
      }
    }
  }

  /**
   * Implicit conversion that adds a <code>loneElement</code> method to <code>GenTraversable</code>.
   *
   *
   * @param trav the <code>GenTraversable</code> on which to add the <code>loneElement</code> method
   */
// C = CC[E] collection is collected elements (CTC is Collection Type Constructor")
  implicit def convertToCollectionLoneElementWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]]): LoneElementCollectionWrapper[E, CTC] = new LoneElementCollectionWrapper[E, CTC](collection)
}

/**
 * Companion object that facilitates the importing of <code>LoneElement</code> members as 
 * an alternative to mixing it in. One use case is to import <code>LoneElement</code>'s members so you can use
 * <code>loneElement</code> in the Scala interpreter.
 */
object LoneElement extends LoneElement

