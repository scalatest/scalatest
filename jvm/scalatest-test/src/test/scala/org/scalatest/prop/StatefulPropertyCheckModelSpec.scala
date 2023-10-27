package org.scalatest.prop

import org.scalatest.funsuite.AnyFunSuite

class StaffManagementSystem {
  
    private var staffs: Map[String, String] = Map.empty
  
    def addStaff(id: String, name: String): Unit = 
      staffs = staffs + (id -> name)
  
    def searchStaff(id: String): Option[String] = 
      staffs.get(id)

    def getStaffs: Map[String, String] = staffs
}

class StaffManagementSystemModel extends AssertiongStatefulPropertyCheckModel {

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

  def createSystemUnderTest(initState: Map[String, String]): SystemUnderTest = 
    new SystemUnderTest {
      val sut = new StaffManagementSystem
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

  def command(state: Map[String, String], gen: Generator[Command], rnd: Randomizer): (Command, Randomizer) = {
    val (nextV, _, newRnd) = gen.next(SizeParam(1, 10, 10), List.empty, rnd)
    (nextV.value, newRnd)
  }

  def preCondition(state: Map[String, String], command: Command, accCmd: IndexedSeq[Command], accRes: IndexedSeq[Map[String, String]]): Boolean = true

  def postCondition(oldState: Map[String, String], newState: Map[String, String], command: Command, accCmd: IndexedSeq[Command], accRes: IndexedSeq[Map[String, String]]): Boolean = true

}

class StatefulPropertyCheckModelSpec extends AnyFunSuite {

  test("StaffManagementSystemModel should be able to add staff and search staff") {
    val model = new StaffManagementSystemModel()
    model.check(SizeParam(0, 100, 100))
  }

}