package org.scalatest.matchers

import org.scalatest.{FlatSpec, Matchers}

class TypeMatcherMacroSpec extends FlatSpec with Matchers {

  case class Organization(name: String)
  val org = Organization("acme")

  "assertTypeImpl" should "work when a val called 'org' is in scope" in {
    org.name shouldBe a[String]
    org.name shouldBe an[String]
  }

  "aTypeMatcherImpl" should "work when a val called 'org' is in scope" in {
    org.name should be(a[String])
  }

  "anTypeMatcherImpl" should "work when a val called 'org' is in scope" in {
    org.name should be(an[String])
  }

  "notATypeMatcher" should "work when a val called 'org' is in scope" in {
    org.name should (not be a [Int] and not be a [Double])
  }

  "notAnTypeMatcher" should "work when a val called 'org' is in scope" in {
    org.name should (not be an[Int] and not be an[Double])
  }

  "andNotATypeMatcher" should "work when a val called 'org' is in scope" in {
    org.name should (be(a[String]) and not be a[Int])
  }

  "andNotAnTypeMatcher" should "work when a val called 'org' is in scope" in {
    org.name should (be(an[String]) and not be an[Int])
  }

  "orNotATypeMatcher" should "work when a val called 'org' is in scope" in {
    org.name should (be(a[Double]) or not be a[Int])
  }

  "orNotAnTypeMatcher" should "work when a val called 'org' is in scope" in {
    org.name should (be(an[Double]) or not be an[Int])
  }

  "assertATypeShouldBeTrueImpl" should "work when a val called 'org' is in scope" in {
    org.name should not be a[Int]
  }

  "assertAnTypeShouldBeTrueImpl" should "work when a val called 'org' is in scope" in {
    org.name should not be an[Int]
  }
}
