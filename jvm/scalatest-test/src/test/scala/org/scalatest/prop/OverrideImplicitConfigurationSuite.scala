package org.scalatest.prop

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import Configuration.PropertyCheckConfiguration

trait GetImplicitConfig { self: Configuration =>
  def getImplicitConfig()(implicit config: PropertyCheckConfiguration): PropertyCheckConfiguration = config
}

class ConfigExplicitOverrideInClass extends AnyFunSuite with Matchers with Configuration with GetImplicitConfig {
  // Note: Needs PropertyCheckConfiguration type ascription to compile, otherwise compile failure:
  //    could not find implicit value for parameter config: ConfigExplicitOverrideInClass.this.PropertyCheckConfiguration
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 42)
  test("config explicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}

class ConfigurationImplicitOverrideInClassTest extends AnyFunSuite with Matchers with Configuration with GetImplicitConfig {
  implicit val classImplicit: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 42)
  test("configuration implicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}

class ConfigurationImplicitOverrideInTestTest extends AnyFunSuite with Matchers with Configuration with GetImplicitConfig {
  test("configuration implicit override in test") {
    implicit val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 42)
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}


class ConfigurationExplicitOverrideInClass extends AnyFunSuite with Matchers with Configuration with GetImplicitConfig {
  val classExplicit = PropertyCheckConfiguration(minSuccessful = 42)
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration = classExplicit
  test("configuration explicit override in class") {
    assert(getImplicitConfig().minSuccessful.value == 42)
  }
}