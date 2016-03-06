
package org.scalactic

trait DefaultPrettifier {
  implicit def prettifier: Prettifier = Prettifier.default
}

