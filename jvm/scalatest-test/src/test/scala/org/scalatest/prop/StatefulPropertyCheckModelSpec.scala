package org.scalatest.prop

import org.scalatest.{Assertion, OptionValues, Expectation}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.exceptions.TestFailedException

class StaffManagementSystem(initStaffs: Map[String, String]) {
  
  private var staffs: Map[String, String] = initStaffs
  
  def addStaff(id: String, name: String): Unit = 
    staffs = staffs + (id -> name)
  
  def searchStaff(id: String): Option[String] = 
    staffs.get(id)

  def getStaffs: Map[String, String] = staffs
}

trait StaffManagementSystemModel[T] extends StatefulPropertyCheckModel[T] {

  sealed trait Command
  case class AddStaff(id: String, name: String) extends Command
  case class SearchStaff(id: String) extends Command

  type TCommand = Command
  type TState = Map[String, String]

  def initialize: (Map[String, String], Generator[Command], Randomizer) = {
    val gen = 
      for {
        r <- Generator.intGenerator
        id <- Generator.stringGenerator
        name <- Generator.stringGenerator
      } yield {
        if (r % 2 == 0)
          SearchStaff(id)
        else
          AddStaff(id, name)
      }
    (Map.empty, gen, Randomizer.default)
  }

  protected def createStaffManagementSystem(initState: Map[String, String]): StaffManagementSystem = new StaffManagementSystem(initState)

  def createSystemUnderTest(initState: Map[String, String]): SystemUnderTest = 
    new SystemUnderTest {
      val sut = createStaffManagementSystem(initState)
      def nextState(state: Map[String, String], command: Command): Map[String, String] = 
        command match {
          case AddStaff(id, name) => 
            sut.addStaff(id, name)
            sut.getStaffs

          case SearchStaff(id) => 
            sut.searchStaff(id)
            sut.getStaffs
        }
      def state(): Map[String, String] = sut.getStaffs  
    }

  def nextState(state: Map[String, String], command: Command): Map[String, String] = 
    command match {
      case AddStaff(id, name) => state + (id -> name)
      case SearchStaff(id) => state
    }

  def preCondition(state: Map[String, String], command: Command, accCmd: IndexedSeq[Command], accRes: IndexedSeq[Map[String, String]]): Boolean = true

  def postCondition(oldState: Map[String, String], newState: Map[String, String], command: Command, accCmd: IndexedSeq[Command], accRes: IndexedSeq[Map[String, String]]): Boolean = true

}

class FaulthyStaffManagementSystem(initStaffs: Map[String, String]) extends StaffManagementSystem(initStaffs) {
  override def addStaff(id: String, name: String): Unit = 
    if (getStaffs.size < 10)
      super.addStaff(id, name)
}

trait FaulthyStaffManagementSystemModel[T] extends StaffManagementSystemModel[T] {

  override protected def createStaffManagementSystem(initState: Map[String, String]): StaffManagementSystem = new FaulthyStaffManagementSystem(initState)

}

class AssertingStaffManagementSystemModel extends StaffManagementSystemModel[Assertion] with AssertiongStatefulPropertyCheckModel

class AssertingFaulthyStaffManagementSystemModel extends FaulthyStaffManagementSystemModel[Assertion] with AssertiongStatefulPropertyCheckModel

class AssertingStatefulPropertyCheckModelSpec extends AnyFunSuite with OptionValues {

  test("AssertingStaffManagementSystemModel should be able to add staff and search staff") {
    val model = new AssertingStaffManagementSystemModel()
    model.check(SizeParam(0, 100, 100))
  }

  test("AssertingFaulthyStaffManagementSystemModel should be able to shrink init state and commands to find the bug") {
    val model = new AssertingFaulthyStaffManagementSystemModel()
    val e = intercept[TestFailedException] {
      model.check(SizeParam(0, 100, 100))
    }
    assert(e.payload.isDefined)
    val payload = e.payload.value
    assert(payload.isInstanceOf[Tuple2[_, _]])
    val (initStat: Map[String, String], initRnd: Randomizer) = payload.asInstanceOf[Tuple2[_, _]]
    assert(initStat.size == 10)
  }

}

class ExpectationStaffManagementSystemModel extends StaffManagementSystemModel[Expectation] with ExpectationStatefulPropertyCheckModel

class ExpectationFaulthyStaffManagementSystemModel extends FaulthyStaffManagementSystemModel[Expectation] with ExpectationStatefulPropertyCheckModel

class ExpectationStatefulPropertyCheckModelSpec extends AnyFunSuite with OptionValues {

  test("ExpectationStaffManagementSystemModel should be able to add staff and search staff") {
    val model = new ExpectationStaffManagementSystemModel()
    val fact = model.check(SizeParam(0, 100, 100))
    assert(fact.isYes)
  }

  test("ExpectationFaulthyStaffManagementSystemModel should be able to shrink init state and commands to find the bug") {
    val model = new ExpectationFaulthyStaffManagementSystemModel()
    val fact = model.check(SizeParam(0, 100, 100))
    assert(fact.isNo)
    assert(fact.factMessage.contains("SUT returned different state after executing commands:"))
  }

}