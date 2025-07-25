/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic

import scala.util.Try
import scala.util.control.{ControlThrowable, NonFatal}
import UsingCompat.Releasable

/**
  * This class is ported from the Scala standard library (https://github.com/scala/scala/blob/v2.13.16/src/library/scala/util/Using.scala), and is used to manage resources for Scala 2.11 and 2.12.
  * 
  *A utility for performing automatic resource management. It can be used to perform an
  * operation using resources, after which it releases the resources in reverse order
  * of their creation.
  *
  * ==Usage==
  *
  * There are multiple ways to automatically manage resources with `Using`. If you only need
  * to manage a single resource, the [[Using.apply `apply`]] method is easiest; it wraps the
  * resource opening, operation, and resource releasing in a `Try`.
  *
  * Example:
  * {{{
  * import java.io.{BufferedReader, FileReader}
  * import scala.util.{Try, Using}
  *
  * val lines: Try[Seq[String]] =
  *   Using(new BufferedReader(new FileReader("file.txt"))) { reader =>
  *     Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
  *   }
  * }}}
  *
  * If you need to manage multiple resources, [[Using.Manager$.apply `Using.Manager`]] should
  * be used. It allows the managing of arbitrarily many resources, whose creation, use, and
  * release are all wrapped in a `Try`.
  *
  * Example:
  * {{{
  * import java.io.{BufferedReader, FileReader}
  * import scala.util.{Try, Using}
  *
  * val files = List("file1.txt", "file2.txt", "file3.txt", "file4.txt")
  * val lines: Try[Seq[String]] = Using.Manager { use =>
  *   // acquire resources
  *   def mkreader(filename: String) = use(new BufferedReader(new FileReader(filename)))
  *
  *   // use your resources here
  *   def lines(reader: BufferedReader): Iterator[String] =
  *     Iterator.continually(reader.readLine()).takeWhile(_ != null)
  *
  *   files.map(mkreader).flatMap(lines)
  * }
  * }}}
  *
  * Composed or "wrapped" resources may be acquired in order of construction,
  * if "underlying" resources are not closed. Although redundant in this case,
  * here is the previous example with a wrapped call to `use`:
  * {{{
  *   def mkreader(filename: String) = use(new BufferedReader(use(new FileReader(filename))))
  * }}}
  *
  * Custom resources can be registered on construction by requiring an implicit `Manager`.
  * This ensures they will be released even if composition fails:
  * {{{
  * import scala.util.Using
  *
  * case class X(x: String)(implicit mgr: Using.Manager) extends AutoCloseable {
  *   override def close() = println(s"CLOSE $x")
  *   mgr.acquire(this)
  * }
  * case class Y(y: String)(x: String)(implicit mgr: Using.Manager) extends AutoCloseable {
  *   val xres = X(x)
  *   override def close() = println(s"CLOSE $y")
  *   // an error during construction releases previously acquired resources
  *   require(y != null, "y is null")
  *   mgr.acquire(this)
  * }
  *
  * Using.Manager { implicit mgr =>
  *   val y = Y("Y")("X")
  *   println(s"USE $y")
  * }
  * println {
  *   Using.Manager { implicit mgr =>
  *     Y(null)("X")
  *   }
  * } // Failure(java.lang.IllegalArgumentException: requirement failed: y is null)
  * }}}
  *
  * If you wish to avoid wrapping management and operations in a `Try`, you can use
  * [[Using.resource `Using.resource`]], which throws any exceptions that occur.
  *
  * Example:
  * {{{
  * import java.io.{BufferedReader, FileReader}
  * import scala.util.Using
  *
  * val lines: Seq[String] =
  *   Using.resource(new BufferedReader(new FileReader("file.txt"))) { reader =>
  *     Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
  *   }
  * }}}
  *
  * ==Suppression Behavior==
  *
  * If two exceptions are thrown (e.g., by an operation and closing a resource),
  * one of them is re-thrown, and the other is
  * [[java.lang.Throwable#addSuppressed added to it as a suppressed exception]].
  * If the two exceptions are of different 'severities' (see below), the one of a higher
  * severity is re-thrown, and the one of a lower severity is added to it as a suppressed
  * exception. If the two exceptions are of the same severity, the one thrown first is
  * re-thrown, and the one thrown second is added to it as a suppressed exception.
  * If an exception is a [[scala.util.control.ControlThrowable `ControlThrowable`]], or
  * if it does not support suppression (see
  * [[java.lang.Throwable `Throwable`'s constructor with an `enableSuppression` parameter]]),
  * an exception that would have been suppressed is instead discarded.
  *
  * Exceptions are ranked from highest to lowest severity as follows:
  *   - `java.lang.VirtualMachineError`
  *   - `java.lang.LinkageError`
  *   - `java.lang.InterruptedException` and `java.lang.ThreadDeath`
  *   - [[scala.util.control.NonFatal fatal exceptions]], excluding `scala.util.control.ControlThrowable`
  *   - `scala.util.control.ControlThrowable`
  *   - all other exceptions
  *
  * When more than two exceptions are thrown, the first two are combined and
  * re-thrown as described above, and each successive exception thrown is combined
  * as it is thrown.
  *
  * @define suppressionBehavior See the main doc for [[Using `Using`]] for full details of
  *                             suppression behavior.
  */
object Using {
  /** Performs an operation using a resource, and then releases the resource,
    * even if the operation throws an exception.
    *
    * $suppressionBehavior
    *
    * @return a [[Try]] containing an exception if one or more were thrown,
    *         or the result of the operation if no exceptions were thrown
    */
  def apply[R: Releasable, A](resource: => R)(f: R => A): Try[A] = Try { Using.resource(resource)(f) }

  /** A resource manager.
    *
    * Resources can be registered with the manager by calling [[acquire `acquire`]];
    * such resources will be released in reverse order of their acquisition
    * when the manager is closed, regardless of any exceptions thrown
    * during use.
    *
    * $suppressionBehavior
    *
    * @note It is recommended for API designers to require an implicit `Manager`
    *       for the creation of custom resources, and to call `acquire` during those
    *       resources' construction. Doing so guarantees that the resource ''must'' be
    *       automatically managed, and makes it impossible to forget to do so.
    *
    *
    *       Example:
    *       {{{
    *       class SafeFileReader(file: File)(implicit manager: Using.Manager)
    *         extends BufferedReader(new FileReader(file)) {
    *
    *         def this(fileName: String)(implicit manager: Using.Manager) = this(new File(fileName))
    *
    *         manager.acquire(this)
    *       }
    *       }}}
    */
  final class Manager {
    import Manager._

    private var closed = false
    private[this] var resources: List[Resource[_]] = Nil

    /** Registers the specified resource with this manager, so that
      * the resource is released when the manager is closed, and then
      * returns the (unmodified) resource.
      */
    def apply[R: Releasable](resource: R) = {
      acquire(resource)
      resource
    }

    /** Registers the specified resource with this manager, so that
      * the resource is released when the manager is closed.
      */
    def acquire[R: Releasable](resource: R): Unit = {
      if (resource == null) throw new NullPointerException("null resource")
      if (closed) throw new IllegalStateException("Manager has already been closed")
      resources = new Resource(resource) :: resources
    }

    private def manage[A](op: Manager => A): A = {
      var toThrow: Throwable = null
      try {
        op(this)
      } catch {
        case t: Throwable =>
          toThrow = t
          null.asInstanceOf[A] // compiler doesn't know `finally` will throw
      } finally {
        releaseAll(toThrow)
      }
    }

    private def releaseAll(passedIn: Throwable): Unit = {
      if (closed) throw new IllegalStateException("Manager has already been closed")
      closed = true
      var rs = resources
      resources = null // allow GC, in case something is holding a reference to `this`
      var toThrow: Throwable = passedIn
      while (rs.nonEmpty) {
        val resource = rs.head
        rs = rs.tail
        try resource.release()
        catch { case t: Throwable => 
          if (toThrow == null) toThrow = t
          else toThrow = preferentiallySuppress(toThrow, t)
        }
      }
      if (toThrow != null) throw toThrow
    }
    /** Closes this manager, releasing all resources registered with it. */
    def close(): Unit = releaseAll(null)
  }

  object Manager {
    /** Performs an operation using a `Manager`, then closes the `Manager`,
      * releasing its resources (in reverse order of acquisition).
      *
      * Example:
      * {{{
      * val lines = Using.Manager { use =>
      *   use(new BufferedReader(new FileReader("file.txt"))).lines()
      * }
      * }}}
      *
      * If using resources which require an implicit `Manager` as a parameter,
      * this method should be invoked with an `implicit` modifier before the function
      * parameter:
      *
      * Example:
      * {{{
      * val lines = Using.Manager { implicit use =>
      *   new SafeFileReader("file.txt").lines()
      * }
      * }}}
      *
      * See the main doc for [[Using `Using`]] for full details of suppression behavior.
      *
      * @param op the operation to perform using the manager
      * @tparam A the return type of the operation
      * @return a [[Try]] containing an exception if one or more were thrown,
      *         or the result of the operation if no exceptions were thrown
      */
    def apply[A](op: Manager => A): Try[A] = Try { (new Manager).manage(op) }

    private final class Resource[R](resource: R)(implicit releasable: Releasable[R]) {
      def release(): Unit = releasable.release(resource)
    }
  }

  private def preferentiallySuppress(primary: Throwable, secondary: Throwable): Throwable = {
    def score(t: Throwable): Int = t match {
      case _: VirtualMachineError                   => 4
      case _: LinkageError                          => 3
      case _: InterruptedException | _: ThreadDeath => 2
      case _: ControlThrowable                      => 0
      case e if !NonFatal(e)                        => 1 // in case this method gets out of sync with NonFatal
      case _                                        => -1
    }
    @inline def suppress(t: Throwable, suppressed: Throwable): Throwable = { t.addSuppressed(suppressed); t }

    if (score(secondary) > score(primary)) suppress(secondary, primary)
    else suppress(primary, secondary)
  }

  /** Performs an operation using a resource, and then releases the resource,
    * even if the operation throws an exception. This method behaves similarly
    * to Java's try-with-resources.
    *
    * $suppressionBehavior
    *
    * @param resource the resource
    * @param body     the operation to perform with the resource
    * @tparam R the type of the resource
    * @tparam A the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resource throws
    */
  def resource[R, A](resource: R)(body: R => A)(implicit releasable: Releasable[R]): A = {
    if (resource == null) throw new NullPointerException("null resource")

    var toThrow: Throwable = null
    try {
      body(resource)
    } catch {
      case t: Throwable =>
        toThrow = t
        null.asInstanceOf[A] // compiler doesn't know `finally` will throw
    } finally {
      if (toThrow eq null) releasable.release(resource)
      else {
        try releasable.release(resource)
        catch { case other: Throwable => toThrow = preferentiallySuppress(toThrow, other) }
        finally throw toThrow
      }
    }
  }

  /** Performs an operation using two resources, and then releases the resources
    * in reverse order, even if the operation throws an exception. This method
    * behaves similarly to Java's try-with-resources.
    *
    * $suppressionBehavior
    *
    * @param resource1 the first resource
    * @param resource2 the second resource
    * @param body      the operation to perform using the resources
    * @tparam R1 the type of the first resource
    * @tparam R2 the type of the second resource
    * @tparam A  the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resources throws
    */
  def resources[R1: Releasable, R2: Releasable, A](
      resource1: R1,
      resource2: => R2
    )(body: (R1, R2) => A
  ): A =
    resource(resource1) { r1 =>
      resource(resource2) { r2 =>
        body(r1, r2)
      }
    }

  /** Performs an operation using three resources, and then releases the resources
    * in reverse order, even if the operation throws an exception. This method
    * behaves similarly to Java's try-with-resources.
    *
    * $suppressionBehavior
    *
    * @param resource1 the first resource
    * @param resource2 the second resource
    * @param resource3 the third resource
    * @param body      the operation to perform using the resources
    * @tparam R1 the type of the first resource
    * @tparam R2 the type of the second resource
    * @tparam R3 the type of the third resource
    * @tparam A  the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resources throws
    */
  def resources[R1: Releasable, R2: Releasable, R3: Releasable, A](
      resource1: R1,
      resource2: => R2,
      resource3: => R3
    )(body: (R1, R2, R3) => A
  ): A =
    resource(resource1) { r1 =>
      resource(resource2) { r2 =>
        resource(resource3) { r3 =>
          body(r1, r2, r3)
        }
      }
    }

  /** Performs an operation using four resources, and then releases the resources
    * in reverse order, even if the operation throws an exception. This method
    * behaves similarly to Java's try-with-resources.
    *
    * $suppressionBehavior
    *
    * @param resource1 the first resource
    * @param resource2 the second resource
    * @param resource3 the third resource
    * @param resource4 the fourth resource
    * @param body      the operation to perform using the resources
    * @tparam R1 the type of the first resource
    * @tparam R2 the type of the second resource
    * @tparam R3 the type of the third resource
    * @tparam R4 the type of the fourth resource
    * @tparam A  the return type of the operation
    * @return the result of the operation, if neither the operation nor
    *         releasing the resources throws
    */
  def resources[R1: Releasable, R2: Releasable, R3: Releasable, R4: Releasable, A](
      resource1: R1,
      resource2: => R2,
      resource3: => R3,
      resource4: => R4
    )(body: (R1, R2, R3, R4) => A
  ): A =
    resource(resource1) { r1 =>
      resource(resource2) { r2 =>
        resource(resource3) { r3 =>
          resource(resource4) { r4 =>
            body(r1, r2, r3, r4)
          }
        }
      }
    }
}