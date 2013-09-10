package org

import scala.util.control.NonFatal

package object scalautils {

  type ErrorMessage = String

  def attempt[R](f: => R): R Or Throwable =
    try Good(f)
    catch {
      case e: Throwable if NonFatal(e) => Bad(e)
    }
}
