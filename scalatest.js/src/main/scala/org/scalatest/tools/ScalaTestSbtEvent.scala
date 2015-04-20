package org.scalatest.tools

import sbt.testing._

private[tools] case class ScalaTestSbtEvent(
  fullyQualifiedName: String,
  fingerprint: Fingerprint,
  selector: Selector,
  status: Status,
  throwable: OptionalThrowable,
  duration: Long) extends Event