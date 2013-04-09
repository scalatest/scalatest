package org.scalatest.tools

import org.scalatest.Suite
import org.scalatest.DoNotDiscover

@DoNotDiscover
class SomeApiClassRunner(someApi: Class[_ <: SomeApiClass]) extends Suite
