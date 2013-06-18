package org.scalatest

import enablers.Collecting

/**
 * Trait that provides an implicit conversion that adds to collection types a <code>loneElement</code> method, which
 * will return the value of the lone element if the collection does
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
 *
 * <p>
 * The <code>loneElement</code> syntax can be used with any collection type <code>C</code> for which an
 * implicit <a href="enablers/Collecting.html"><code>Collecting[C]</code></a> is available. ScalaTest provides
 * implicit <code>Collecting</code> instances for <code>scala.collection.GenTraversable</code>, <code>Array</code>,
 * <code>java.util.Collection</code>, and <code>java.util.Map</code>. You can enable the <code>loneElement</code>
 * syntax on other collection types by defining an implicit <code>Collecting</code> instances for those types.
 * </p>
 *
 * @author Bill Venners
 */
trait LoneElement {

  /**
   * Wrapper class that adds a <code>loneElement</code> method to any collection type <code>C</code> for which 
   * an implicit <code>Collection[C]</code> is available.
   *
   * <p>
   * Through the implicit conversion provided by trait <code>LoneElement</code>, this class allows you to make statements like:
   * </p>
   *
   * <pre class="stHighlight">
   * trav.loneElement should be &gt; 9
   * </pre>
   *
   * @tparam E the element type of the collection on which to add the <code>loneElement</code> method
   * @tparam CTC the "collection type constructor" for the collection on which to add the <code>loneElement</code> method
   * @param collection a collection to wrap in a <code>LoneElementCollectionWrapper</code>, which provides the <code>loneElement</code> method.
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  final class LoneElementCollectionWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]]) {

    /**
     * Returns the value contained in the wrapped collection, if it contains one and only one element, else throws <code>TestFailedException</code> with
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
  
  final class LoneElementJavaMapWrapper[K, V, JMAP[K, V]](jmap: JMAP[K, V])(implicit collecting: Collecting[(K, V), JMAP[K, V]]) {
    
    def loneElement: (K, V) = {
      collecting.loneElementOf(jmap) match {
        case Some(ele) => ele
        case None =>
          throw new exceptions.TestFailedException(
            Some(FailureMessages(
                 "notLoneElement",
                 jmap,
                 collecting.sizeOf(jmap))), 
            None, 
            1
          )
      }
    }
    
  }

  /**
   * Implicit conversion that adds a <code>loneElement</code> method to any collection type <code>C</code> for which an
   * implicit <code>Collection[C]</code> is available.
   *
   * @tparam E the element type of the collection on which to add the <code>loneElement</code> method
   * @tparam CTC the "collection type constructor" for the collection on which to add the <code>loneElement</code> method
   * @param collection the collection on which to add the <code>loneElement</code> method
   * @param collecting a typeclass that enables the <code>loneElement</code> syntax
   */
  implicit def convertToCollectionLoneElementWrapper[E, CTC[_]](collection: CTC[E])(implicit collecting: Collecting[E, CTC[E]]): LoneElementCollectionWrapper[E, CTC] = new LoneElementCollectionWrapper[E, CTC](collection)
  
  // Needed for Java Map to work, any better solution?
  implicit def convertJavaMapToCollectionLoneElementWrapper[K, V](jmap: java.util.Map[K, V])(implicit collecting: Collecting[(K, V), java.util.Map[K, V]]): LoneElementJavaMapWrapper[K, V, java.util.Map] = {
    new LoneElementJavaMapWrapper(jmap)(collecting)
  }
}

/**
 * Companion object that facilitates the importing of <code>LoneElement</code> members as 
 * an alternative to mixing it in. One use case is to import <code>LoneElement</code>'s members so you can use
 * <code>loneElement</code> in the Scala interpreter.
 */
object LoneElement extends LoneElement

