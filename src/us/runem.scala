
import org.scalatest.tools.Runner

object runem extends App {
  for (i <- 1 to 135) {
    Runner.run(
      Array("-p", ".", "-oD", "-d", "dashboard", "-s", "UnitedStates")
    )
    Thread.sleep(1000)
  }
}

