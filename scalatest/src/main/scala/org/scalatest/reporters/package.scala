package org.scalatest

/**
  * This package should contain only reporters which are parallel friendly.
  *
  * Many reporters live outside of this package, because changing theirs fully qualified name (package) would make
  * it incompatible. There are many tests running by specifying fully qualified name of reporter, thus changing package
  * would break things.
  */
package object reporters {
}
