package org.scalatest.prop

import org.scalatest.{Assertion, OptionValues, Expectation}
import org.scalatest.funsuite.{AnyFunSuite, AsyncFunSuite}
import org.scalatest.exceptions.TestFailedException

import scala.concurrent.Future

class StaffManagementSystem(initStaffs: Map[String, String]) {
  
  private var staffs: Map[String, String] = initStaffs
  
  def addStaff(id: String, name: String): Unit = 
    staffs = staffs + (id -> name)
  
  def searchStaff(id: String): Option[String] = 
    staffs.get(id)

  def getStaffs: Map[String, String] = staffs
}

sealed trait Command
case class AddStaff(id: String, name: String) extends Command
case class SearchStaff(id: String) extends Command

class BlockingStaffManagementSystem(initState: Map[String, String]) extends SystemUnderTest[Command, Map[String, String]] {
  val sut = new StaffManagementSystem(initState)
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

trait StaffManagementSystemModel[Sut, T] extends StatefulPropertyCheckModel[Command, Map[String, String], Sut, T] {

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

class BlockingFaulthyStaffManagementSystem(initState: Map[String, String]) extends BlockingStaffManagementSystem(initState) {
  override val sut = new FaulthyStaffManagementSystem(initState)
}

class AssertingStaffManagementSystemModel extends StaffManagementSystemModel[SystemUnderTest[Command, Map[String, String]], Assertion] {
  def createSystemUnderTest(initState: Map[String, String]): SystemUnderTest[Command, Map[String, String]] = 
    new BlockingStaffManagementSystem(initState)
}

class AssertingFaulthyStaffManagementSystemModel extends StaffManagementSystemModel[SystemUnderTest[Command, Map[String, String]], Assertion] {
  def createSystemUnderTest(initState: Map[String, String]): SystemUnderTest[Command, Map[String, String]] = 
    new BlockingFaulthyStaffManagementSystem(initState)
}

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

class ExpectationStaffManagementSystemModel extends StaffManagementSystemModel[SystemUnderTest[Command, Map[String, String]], Expectation] {
  def createSystemUnderTest(initState: Map[String, String]): SystemUnderTest[Command, Map[String, String]] = 
    new BlockingStaffManagementSystem(initState)
}

class ExpectationFaulthyStaffManagementSystemModel extends StaffManagementSystemModel[SystemUnderTest[Command, Map[String, String]], Expectation] {
  def createSystemUnderTest(initState: Map[String, String]): SystemUnderTest[Command, Map[String, String]] = 
    new BlockingFaulthyStaffManagementSystem(initState)
}

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

class AsyncStaffManagementSystem(initState: Map[String, String])(implicit execCtx: scala.concurrent.ExecutionContext) extends AsyncSystemUnderTest[Command, Map[String, String]] {
  val sut = new StaffManagementSystem(initState)
  def nextState(state: Map[String, String], command: Command): Future[Map[String, String]] = 
    Future {
      command match {
        case AddStaff(id, name) => 
          sut.addStaff(id, name)
          sut.getStaffs

        case SearchStaff(id) => 
          sut.searchStaff(id)
          sut.getStaffs
      }
    }
    
  def state(): Future[Map[String, String]] = Future { sut.getStaffs }
}

class AsyncFaulthyStaffManagementSystem(initState: Map[String, String])(implicit exeCtx: scala.concurrent.ExecutionContext) extends AsyncStaffManagementSystem(initState) {
  override val sut = new FaulthyStaffManagementSystem(initState)
}

class AsyncStaffManagementSystemModel(implicit exeCtx: scala.concurrent.ExecutionContext) extends StaffManagementSystemModel[AsyncSystemUnderTest[Command, Map[String, String]], Future[Assertion]] {
  def createSystemUnderTest(initState: Map[String, String]): AsyncSystemUnderTest[Command, Map[String, String]] = 
    new AsyncStaffManagementSystem(initState)
}

class AsyncFaulthyStaffManagementSystemModel(implicit exeCtx: scala.concurrent.ExecutionContext) extends StaffManagementSystemModel[AsyncSystemUnderTest[Command, Map[String, String]], Future[Assertion]] {
  def createSystemUnderTest(initState: Map[String, String]): AsyncSystemUnderTest[Command, Map[String, String]] = 
    new AsyncFaulthyStaffManagementSystem(initState)
}

class AsyncStatefulPropertyCheckModelSpec extends AsyncFunSuite with OptionValues {

  test("AsyncStaffManagementSystemModel should be able to add staff and search staff") {
    val model = new AsyncStaffManagementSystemModel()
    model.check(SizeParam(0, 100, 100))
  }

  test("AsyncFaulthyStaffManagementSystemModel should be able to shrink init state and commands to find the bug") {
    val model = new AsyncFaulthyStaffManagementSystemModel()
    recoverToExceptionIf[TestFailedException] {
      model.check(SizeParam(0, 100, 100))
    }.map { e =>
      assert(e.payload.isDefined)
      val payload = e.payload.value
      assert(payload.isInstanceOf[Tuple2[_, _]])
      val (initStat: Map[String, String], initRnd: Randomizer) = payload.asInstanceOf[Tuple2[_, _]]
      assert(initStat.size == 10)
    }
  }

}