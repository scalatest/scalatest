package org.scalatest;

import java.lang.annotation.*; 

/**
 * Annotation used to indicate that an otherwise discoverable test class should not be discovered.
 *
 * <p>
 * <em>Note: This is actually an annotation defined in Java, not a Scala trait. It must be defined in Java instead of Scala so it will be accessible
 * at runtime. It has been inserted into Scaladoc by pretending it is a trait.</em>
 * </p>
 *
 * <p>
 * ScalaTest will discover any class that either extends <a href="Suite.html"><code>Suite</code></a> and has a public, no-arg constructor, or is annotated with
 * a valid <a href="WrapWith.html"><code>WrapWith</code></a> annotation. If you wish to prevent a class from being discovered, simply annotated it
 * with <code>DoNotDiscover</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * 
 * @DoNotDiscover
 * class SetSpec extends FlatSpec {
 * 
 *   "An empty Set" should "have size 0" in {
 *     assert(Set.empty.size === 0)
 *   }
 * 
 *   it should "produce NoSuchElementException when head is invoked" in {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 * 
 * <p>
 * ScalaTest will run classes annotated with <code>DoNotDiscover</code> if asked to explicitly, it just won't discover them. 
 * </p>
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface DoNotDiscover {

}
