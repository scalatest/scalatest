package org.scalatest
package prop

trait GetImplicitConfig { self: Configuration =>
  def getImplicitConfig()(implicit config: PropertyCheckConfiguration): PropertyCheckConfiguration = config
}

class ConfigImplicitOverrideInClassTest extends FunSuite with Matchers with Configuration with GetImplicitConfig {
  implicit val classImplicit = PropertyCheckConfig(minSuccessful = 42)
  test("config implicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}

class ConfigImplicitOverrideInTestTest extends FunSuite with Matchers with Configuration with GetImplicitConfig {
  /*
  // Fails to compile:
  //  could not find implicit value for parameter config: ConfigImplicitOverrideInTestTest.this.PropertyCheckConfiguration
  test("config implicit override in test") {
    implicit val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 42)
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
  */
}

class ConfigExplicitOverrideInClass extends FunSuite with Matchers with Configuration with GetImplicitConfig {
  // Note: Needs PropertyCheckConfiguration type ascription to compile, otherwise compile failure:
  //    could not find implicit value for parameter config: ConfigExplicitOverrideInClass.this.PropertyCheckConfiguration
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfig(minSuccessful = 42)
  test("config explicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}

class ConfigurationImplicitOverrideInClassTest extends FunSuite with Matchers with Configuration with GetImplicitConfig {
  implicit val classImplicit = PropertyCheckConfiguration(minSuccessful = 42)
  test("configuration implicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}

class ConfigurationImplicitOverrideInTestTest extends FunSuite with Matchers with Configuration with GetImplicitConfig {
  test("configuration implicit override in test") {
    implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 42)
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}


class ConfigurationExplicitOverrideInClass extends FunSuite with Matchers with Configuration with GetImplicitConfig {
  val classExplicit = PropertyCheckConfiguration(minSuccessful = 42)
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = classExplicit
  test("configuration explicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}