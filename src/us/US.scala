import org.scalatest._
import scala.collection.mutable.Stack
import scala.collection.mutable
import org.scalatest.events._
import scala.util.Random
import scala.reflect.NameTransformer
import org.scalatest.exceptions.TestCanceledException

class UnitedStates extends Suite {

  import UnitedStates.allStates
  import UnitedStates.nestedSuiteCount

  override def nestedSuites: collection.immutable.IndexedSeq[Suite] = allStates.take(nestedSuiteCount).toIndexedSeq

  override def run(testName: Option[String], args: Args): org.scalatest.Status = {

    if (nestedSuiteCount < allStates.length)
      nestedSuiteCount += 1

    super.run(testName, args)
  }
}

trait StateSuite extends Suite {

  import StateSuite.allTestNames
  import StateSuite.testCounts
  import StateSuite.testStatuses

  private def anInitialDuration = Random.nextInt(20)

  private val simpleName = getClass.getSimpleName.replaceAll("\\$", "")

  override def testNames: Set[String] = allTestNames.take(testCounts(simpleName)).toSet

  override def tags: Map[String, Set[String]] = Map()

  override def run(testName: Option[String], args: Args): org.scalatest.Status = {

    val testCount = testCounts(simpleName)

    if (testCount < allTestNames.length) {
      testCounts(simpleName) += 1

    // The new test is at testCount; Decide if it is pending, and if so, how many times it will stay pending
    // whether or not it will be pending should be a 50/50 choice
    val nameOfNewTest = allTestNames(testCount)
    val isPending = Random.nextInt(2) == 0

    testStatuses(simpleName)(nameOfNewTest) =
      if (isPending) Pending(anInitialDuration, Random.nextInt(30)) // Maximum of 30 times pending before going to some other status
      else Succeeded(anInitialDuration)
    }

    val alterSucceededTests = Random.nextInt(3) == 0

    if (alterSucceededTests) {
      val nameOfTestToAlter = allTestNames(Random.nextInt(testCounts(simpleName)) )
      testStatuses(simpleName)(nameOfTestToAlter) match {
        case Succeeded(duration) =>
          val isIgnored = Random.nextInt(2) == 0
          val isCanceled = !isIgnored && (Random.nextInt(2) == 0) // If not ignored or canceled, then make it failed
          val remaining = Random.nextInt(if (isIgnored) 20 else 15)
          testStatuses(simpleName)(nameOfTestToAlter) =
            if (isIgnored) Ignored(duration, remaining) else if (isCanceled) Canceled(duration, remaining) else Failed(duration, remaining)
        case _ =>
      }
    }

    val shouldSlowATest = Random.nextInt(50) == 0
    if (shouldSlowATest) {
      val nameOfTestToSlow = allTestNames(Random.nextInt(testCounts(simpleName)))
      val slowFactor = Random.nextInt(25)
      val slowerStatus =
        testStatuses(simpleName)(nameOfTestToSlow) match {
          case Succeeded(d) => Succeeded(d * slowFactor)
          case Pending(d, r) => Pending(d * slowFactor, r)
          case Canceled(d, r) => Canceled(d * slowFactor, r)
          case Ignored(d, r) => Ignored(d * slowFactor, r)
          case Failed(d, r) => Failed(d * slowFactor, r)
        }
      testStatuses(simpleName)(nameOfTestToSlow) = slowerStatus
    }
    super.run(testName, args)
  }

  private def reportTestStarting(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, suiteRerunner: Option[String], location: Option[Location]) {
    report(TestStarting(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Some(MotionToSuppress),
      location, suiteRerunner))
  }

 private def reportTestFailed(theSuite: Suite, report: Reporter, throwable: Throwable, testName: String, testText: String, 
      suiteRerunner: Option[String], tracker: Tracker, duration: Long, level: Int, includeIcon: Boolean, location: Option[Location]) {

    val message = getMessageForException(throwable)
    val formatter = getIndentedText(testText, level, includeIcon)
    report(TestFailed(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Vector.empty, Some(throwable), Some(duration), Some(formatter), location, suiteRerunner))
  }

  private def reportTestCanceled(theSuite: Suite, report: Reporter, throwable: Throwable, testName: String, testText: String,
      rerunnable: Option[Rerunner], tracker: Tracker, duration: Long, level: Int, includeIcon: Boolean, location: Option[Location]) {

    val message = getMessageForException(throwable)
    val formatter = getIndentedText(testText, level, includeIcon)
    report(TestCanceled(tracker.nextOrdinal(), message, theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Vector.empty, Some(throwable), Some(duration), Some(formatter), location, rerunnable))
  }

  private def reportTestSucceeded(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, duration: Long, formatter: Formatter, suiteRerunner: Option[String], location: Option[Location]) {
    report(TestSucceeded(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Vector.empty, Some(duration), Some(formatter),
      location, suiteRerunner))
  }

  private def reportTestPending(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, duration: Long, formatter: Formatter, location: Option[Location]) {
    report(TestPending(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Vector.empty, Some(duration), Some(formatter),
      location))
  }

  private def getMessageForException(e: Throwable): String =
    if (e.getMessage != null)
      e.getMessage
    else
      e.getClass.getName + " was thrown"

  private def getIndentedText(testText: String, level: Int, includeIcon: Boolean) = {
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = "-"
        ("  " * (if (level == 0) 0 else (level - 1))) + testSucceededIcon + " " + testText
      }
      else {
        ("  " * level) + testText
      }
    IndentedText(formattedText, testText, level)
  }

  def indentation(level: Int) = "  " * level

  private def reportTestIgnored(report: Reporter, tracker: Tracker, testName: String, testText: String, level: Int) {
    val testSucceededIcon = "-"
    val formattedText = indentation(level - 1) + (testSucceededIcon + " " + testText)
    report(TestIgnored(tracker.nextOrdinal(), suiteName, suiteId, Some(getClass.getName), testName, testText, Some(IndentedText(formattedText, testText, level)),
      None))

  }

  private def handleFailedTest(throwable: Throwable, testName: String, 
      report: Reporter, tracker: Tracker, duration: Long, location: Option[Location]) {

    val message = getMessageForException(throwable)
    val formatter = getIndentedText(testName, 1, true)
    report(TestFailed(tracker.nextOrdinal(), message, suiteName, suiteId, Some(getClass.getName), testName, testName, Vector.empty, Some(throwable), Some(duration), Some(formatter), location, None))
  }
  override def runTest(testName: String, args: Args): org.scalatest.Status = {

    if (!testStatuses(simpleName)(testName).isInstanceOf[Ignored])
      reportTestStarting(this, args.reporter, args.tracker, testName, testName, None, None)

    val formatter = getIndentedText(testName, 1, true)

    testStatuses(simpleName)(testName) match {
      case Pending(duration, remaining) =>
        if (remaining > 1)
          testStatuses(simpleName)(testName) = Pending(duration, remaining - 1)
        else
          testStatuses(simpleName)(testName) = Succeeded(duration)
        reportTestPending(this, args.reporter, args.tracker, testName, testName, duration, formatter, None)
        org.scalatest.SucceededStatus
      case Ignored(duration, remaining) =>
        if (remaining > 1)
          testStatuses(simpleName)(testName) = Ignored(duration, remaining - 1)
        else
          testStatuses(simpleName)(testName) = Succeeded(duration)
        reportTestIgnored(args.reporter, args.tracker, testName, testName, 1)
        org.scalatest.SucceededStatus
      case Canceled(duration, remaining) =>
        if (remaining > 1)
          testStatuses(simpleName)(testName) = Canceled(duration, remaining - 1)
        else
          testStatuses(simpleName)(testName) = Succeeded(duration)
          val e = intercept[TestCanceledException] { cancel("Because of rain") }
          val message = getMessageForException(e)
          val formatter = getIndentedText(testName, 1, true)
          args.reporter(TestCanceled(args.tracker.nextOrdinal(), message, suiteName, suiteId, Some(getClass.getName), testName, testName, Vector.empty, Some(e), Some(duration), Some(formatter), None, None))
          org.scalatest.SucceededStatus
      case Failed(duration, remaining) =>
        if (remaining > 1)
          testStatuses(simpleName)(testName) = Failed(duration, remaining - 1)
        else
          testStatuses(simpleName)(testName) = Succeeded(duration)
        val e = intercept[TestFailedException] { fail("1 + 1 did not equal 3, even for very large values of 1") }
        handleFailedTest(e, testName, args.reporter, args.tracker, duration, None)
        org.scalatest.FailedStatus
      case Succeeded(duration) => 
        reportTestSucceeded(this, args.reporter, args.tracker, testName, testName, duration, formatter, None, None)
        org.scalatest.SucceededStatus
    }
  }
}

sealed abstract class Status {
  val duration: Int
}
case class Pending(duration: Int, remaining: Int) extends Status
case class Succeeded(duration: Int) extends Status
case class Canceled(duration: Int, remaining: Int) extends Status
case class Ignored(duration: Int, remaining: Int) extends Status
case class Failed(duration: Int, remaining: Int) extends Status

object StateSuite {

  val testCounts =
    mutable.Map(
      "Alabama" -> 0,
      "Alaska" -> 0,
      "Arizona" -> 0,
      "Arkansas" -> 0,
      "California" -> 0,
      "Colorado" -> 0,
      "Connecticut" -> 0,
      "Delaware" -> 0,
      "Florida" -> 0,
      "Georgia" -> 0,
      "Hawaii" -> 0,
      "Idaho" -> 0,
      "Illinois" -> 0,
      "Indiana" -> 0,
      "Iowa" -> 0,
      "Kansas" -> 0,
      "Kentucky" -> 0,
      "Louisiana" -> 0,
      "Maine" -> 0,
      "Maryland" -> 0,
      "Massachusetts" -> 0,
      "Michigan" -> 0,
      "Minnesota" -> 0,
      "Mississippi" -> 0,
      "Missouri" -> 0,
      "Montana" -> 0,
      "Nebraska" -> 0,
      "Nevada" -> 0,
      "NewHampshire" -> 0,
      "NewJersey" -> 0,
      "NewMexico" -> 0,
      "NewYork" -> 0,
      "NorthCarolina" -> 0,
      "NorthDakota" -> 0,
      "Ohio" -> 0,
      "Oklahoma" -> 0,
      "Oregon" -> 0,
      "Pennsylvania" -> 0,
      "RhodeIsland" -> 0,
      "SouthCarolina" -> 0,
      "SouthDakota" -> 0,
      "Tennessee" -> 0,
      "Texas" -> 0,
      "Utah" -> 0,
      "Vermont" -> 0,
      "Virginia" -> 0,
      "Washington" -> 0,
      "WestVirginia" -> 0,
      "Wisconsin" -> 0,
      "Wyoming" -> 0
    )

  val testStatuses =
    mutable.Map(
      "Alabama" -> mutable.Map.empty[String, Status],
      "Alaska" -> mutable.Map.empty[String, Status],
      "Arizona" -> mutable.Map.empty[String, Status],
      "Arkansas" -> mutable.Map.empty[String, Status],
      "California" -> mutable.Map.empty[String, Status],
      "Colorado" -> mutable.Map.empty[String, Status],
      "Connecticut" -> mutable.Map.empty[String, Status],
      "Delaware" -> mutable.Map.empty[String, Status],
      "Florida" -> mutable.Map.empty[String, Status],
      "Georgia" -> mutable.Map.empty[String, Status],
      "Hawaii" -> mutable.Map.empty[String, Status],
      "Idaho" -> mutable.Map.empty[String, Status],
      "Illinois" -> mutable.Map.empty[String, Status],
      "Indiana" -> mutable.Map.empty[String, Status],
      "Iowa" -> mutable.Map.empty[String, Status],
      "Kansas" -> mutable.Map.empty[String, Status],
      "Kentucky" -> mutable.Map.empty[String, Status],
      "Louisiana" -> mutable.Map.empty[String, Status],
      "Maine" -> mutable.Map.empty[String, Status],
      "Maryland" -> mutable.Map.empty[String, Status],
      "Massachusetts" -> mutable.Map.empty[String, Status],
      "Michigan" -> mutable.Map.empty[String, Status],
      "Minnesota" -> mutable.Map.empty[String, Status],
      "Mississippi" -> mutable.Map.empty[String, Status],
      "Missouri" -> mutable.Map.empty[String, Status],
      "Montana" -> mutable.Map.empty[String, Status],
      "Nebraska" -> mutable.Map.empty[String, Status],
      "Nevada" -> mutable.Map.empty[String, Status],
      "NewHampshire" -> mutable.Map.empty[String, Status],
      "NewJersey" -> mutable.Map.empty[String, Status],
      "NewMexico" -> mutable.Map.empty[String, Status],
      "NewYork" -> mutable.Map.empty[String, Status],
      "NorthCarolina" -> mutable.Map.empty[String, Status],
      "NorthDakota" -> mutable.Map.empty[String, Status],
      "Ohio" -> mutable.Map.empty[String, Status],
      "Oklahoma" -> mutable.Map.empty[String, Status],
      "Oregon" -> mutable.Map.empty[String, Status],
      "Pennsylvania" -> mutable.Map.empty[String, Status],
      "RhodeIsland" -> mutable.Map.empty[String, Status],
      "SouthCarolina" -> mutable.Map.empty[String, Status],
      "SouthDakota" -> mutable.Map.empty[String, Status],
      "Tennessee" -> mutable.Map.empty[String, Status],
      "Texas" -> mutable.Map.empty[String, Status],
      "Utah" -> mutable.Map.empty[String, Status],
      "Vermont" -> mutable.Map.empty[String, Status],
      "Virginia" -> mutable.Map.empty[String, Status],
      "Washington" -> mutable.Map.empty[String, Status],
      "WestVirginia" -> mutable.Map.empty[String, Status],
      "Wisconsin" -> mutable.Map.empty[String, Status],
      "Wyoming" -> mutable.Map.empty[String, Status]
    )
    
  val allTestNames =
    Vector(
      "When in the Course of human events",
      "it becomes necessary for one people to dissolve the political bands which have connected them with another",
      "and to assume among the powers of the earth",
      "the separate and equal station to which the Laws of Nature and of Nature's God entitle them",
      "a decent respect to the opinions of mankind requires that they should declare the causes which impel them to the separation",
      "We hold these truths to be self-evident",
      "that all men are created equal",
      "that they are endowed by their Creator with certain unalienable Rights",
      "that among these are Life, Liberty and the pursuit of Happiness.",
      "That to secure these rights",
      "Governments are instituted among Men",
      "deriving their just powers from the consent of the governed",
      "That whenever any Form of Government becomes destructive of these ends",
      "it is the Right of the People to alter or to abolish it",
      "and to institute new Government",
      "laying its foundation on such principles and organizing its powers in such form",
      "as to them shall seem most likely to effect their Safety and Happiness.",
      "Prudence, indeed, will dictate that Governments long established should not be changed for light and transient causes",
      "and accordingly all experience hath shewn",
      "that mankind are more disposed to suffer",
      "while evils are sufferable",
      "than to right themselves by abolishing the forms to which they are accustomed",
      "But when a long train of abuses and usurpations",
      "pursuing invariably the same Object evinces a design to reduce them under absolute Despotism",
      "it is their right",
      "it is their duty",
      "to throw off such Government",
      "and to provide new Guards for their future security",
      "Such has been the patient sufferance of these Colonies",
      "and such is now the necessity which constrains them to alter their former Systems of Government",
      "The history of the present King of Great Britain is a history of repeated injuries and usurpations",
      "all having in direct object the establishment of an absolute Tyranny over these States",
      "To prove this, let Facts be submitted to a candid world.",
      "He has refused his Assent to Laws, the most wholesome and necessary for the public good",
      "He has forbidden his Governors to pass Laws of immediate and pressing importance",
      "unless suspended in their operation till his Assent should be obtained",
      "and when so suspended, he has utterly neglected to attend to them",
      "He has refused to pass other Laws for the accommodation of large districts of people",
      "unless those people would relinquish the right of Representation in the Legislature",
      "a right inestimable to them and formidable to tyrants only ",
      "He has called together legislative bodies at places unusual, uncomfortable, and distant from the depository of their public Records",
      "for the sole purpose of fatiguing them into compliance with his measures ",
      "He has dissolved Representative Houses repeatedly",
      "for opposing with manly firmness his invasions on the rights of the people.",
      "He has refused for a long time, after such dissolutions, to cause others to be elected",
      "whereby the Legislative powers, incapable of Annihilation, have returned to the People at large for their exercise",
      "the State remaining in the mean time exposed to all the dangers of invasion from without, and convulsions within.",
      "He has endeavoured to prevent the population of these States",
      "for that purpose obstructing the Laws for Naturalization of Foreigners",
      "refusing to pass others to encourage their migrations hither",
      "and raising the conditions of new Appropriations of Lands",
      "He has obstructed the Administration of Justice",
      "by refusing his Assent to Laws for establishing Judiciary powers.",
      "He has made Judges dependent on his Will alone",
      "for the tenure of their offices",
      "and the amount and payment of their salaries.",
      "He has erected a multitude of New Offices",
      "and sent hither swarms of Officers to harrass our people, and eat out their substance.",
      "He has kept among us, in times of peace, Standing Armies without the Consent of our legislatures",
      "He has affected to render the Military independent of and superior to the Civil power.",
      "He has combined with others to subject us to a jurisdiction foreign to our constitution, and unacknowledged by our laws",
      "giving his Assent to their Acts of pretended Legislation:",
      "For Quartering large bodies of armed troops among us",
      "For protecting them, by a mock Trial, from punishment for any Murders which they should commit on the Inhabitants of these States",
      "For cutting off our Trade with all parts of the world",
      "For imposing Taxes on us without our Consent:",
      "For depriving us in many cases, of the benefits of Trial by Jury",
      "For transporting us beyond Seas to be tried for pretended offences",
      "For abolishing the free System of English Laws in a neighbouring Province",
      "establishing therein an Arbitrary government",
      "and enlarging its Boundaries so as to render it at once an example and fit instrument for introducing the same absolute rule into these Colonies",
      "For taking away our Charters, abolishing our most valuable Laws, and altering fundamentally the Forms of our Governments",
      "For suspending our own Legislatures, and declaring themselves invested with power to legislate for us in all cases whatsoever",
      "He has abdicated Government here, by declaring us out of his Protection and waging War against us",
      "He has plundered our seas, ravaged our Coasts, burnt our towns, and destroyed the lives of our people",
      "He is at this time transporting large Armies of foreign Mercenaries to compleat the works of death, desolation and tyranny",
      "already begun with circumstances of Cruelty & perfidy scarcely paralleled in the most barbarous ages",
      "and totally unworthy the Head of a civilized nation.",
      "He has constrained our fellow Citizens taken Captive on the high Seas to bear Arms against their Country",
      "to become the executioners of their friends and Brethren",
      "or to fall themselves by their Hands",
      "He has excited domestic insurrections amongst us",
      "and has endeavoured to bring on the inhabitants of our frontiers",
      "the merciless Indian Savages, whose known rule of warfare",
      "is an undistinguished destruction of all ages, sexes and conditions.",
      "In every stage of these Oppressions We have Petitioned for Redress in the most humble terms",
      "Our repeated Petitions have been answered only by repeated injury",
      "A Prince whose character is thus marked by every act which may define a Tyrant",
      "is unfit to be the ruler of a free people.",
      "Nor have We been wanting in attentions to our Brittish brethren",
      "We have warned them from time to time of attempts by their legislature to extend an unwarrantable jurisdiction over us",
      "We have reminded them of the circumstances of our emigration and settlement here",
      "We have appealed to their native justice and magnanimity",
      "and we have conjured them by the ties of our common kindred to disavow these usurpations",
      "which, would inevitably interrupt our connections and correspondence",
      "They too have been deaf to the voice of justice and of consanguinity",
      "We must, therefore, acquiesce in the necessity, which denounces our Separation",
      "and hold them, as we hold the rest of mankind, Enemies in War, in Peace Friends",
      "We, therefore, the Representatives of the united States of America",
      "in General Congress, Assembled, appealing to the Supreme Judge of the world for the rectitude of our intentions",
      "do, in the Name, and by Authority of the good People of these Colonies",
      "solemnly publish and declare, That these United Colonies are",
      "and of Right ought to be Free and Independent States",
      "that they are Absolved from all Allegiance to the British Crown",
      "and that all political connection between them and the State of Great Britain",
      "is and ought to be totally dissolved",
      "and that as Free and Independent States",
      "they have full Power to levy War",
      "conclude Peace",
      "contract Alliances",
      "establish Commerce",
      "and to do all other Acts and Things which Independent States may of right do",
      "And for the support of this Declaration",
      "with a firm reliance on the protection of divine Providence",
      "we mutually pledge to each other our Lives",
      "our Fortunes and our sacred Honor"
    )
}

object UnitedStates {

  private var nestedSuiteCount = 0

  val allStates =
    Vector(
      Alabama,
      Alaska,
      Arizona,
      Arkansas,
      California,
      Colorado,
      Connecticut,
      Delaware,
      Florida,
      Georgia,
      Hawaii,
      Idaho,
      Illinois,
      Indiana,
      Iowa,
      Kansas,
      Kentucky,
      Louisiana,
      Maine,
      Maryland,
      Massachusetts,
      Michigan,
      Minnesota,
      Mississippi,
      Missouri,
      Montana,
      Nebraska,
      Nevada,
      NewHampshire,
      NewJersey,
      NewMexico,
      NewYork,
      NorthCarolina,
      NorthDakota,
      Ohio,
      Oklahoma,
      Oregon,
      Pennsylvania,
      RhodeIsland,
      SouthCarolina,
      SouthDakota,
      Tennessee,
      Texas,
      Utah,
      Vermont,
      Virginia,
      Washington,
      WestVirginia,
      Wisconsin,
      Wyoming
    )

}

object Alabama extends StateSuite
object Alaska extends StateSuite
object Arizona extends StateSuite
object Arkansas extends StateSuite
object California extends StateSuite
object Colorado extends StateSuite
object Connecticut extends StateSuite
object Delaware extends StateSuite
object Florida extends StateSuite
object Georgia extends StateSuite
object Hawaii extends StateSuite
object Idaho extends StateSuite
object Illinois extends StateSuite
object Indiana extends StateSuite
object Iowa extends StateSuite
object Kansas extends StateSuite
object Kentucky extends StateSuite
object Louisiana extends StateSuite
object Maine extends StateSuite
object Maryland extends StateSuite
object Massachusetts extends StateSuite
object Michigan extends StateSuite
object Minnesota extends StateSuite
object Mississippi extends StateSuite
object Missouri extends StateSuite
object Montana extends StateSuite
object Nebraska extends StateSuite
object Nevada extends StateSuite
object NewHampshire extends StateSuite
object NewJersey extends StateSuite
object NewMexico extends StateSuite
object NewYork extends StateSuite
object NorthCarolina extends StateSuite
object NorthDakota extends StateSuite
object Ohio extends StateSuite
object Oklahoma extends StateSuite
object Oregon extends StateSuite
object Pennsylvania extends StateSuite
object RhodeIsland extends StateSuite
object SouthCarolina extends StateSuite
object SouthDakota extends StateSuite
object Tennessee extends StateSuite
object Texas extends StateSuite
object Utah extends StateSuite
object Vermont extends StateSuite
object Virginia extends StateSuite
object Washington extends StateSuite
object WestVirginia extends StateSuite
object Wisconsin extends StateSuite
object Wyoming extends StateSuite

