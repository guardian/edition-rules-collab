// empty file
import java.time.LocalDate

object WeekDay extends Enumeration(0) {
  type WeekDay = Value
  val Mon, Tues, Wed, Thurs, Fri, Sat, Sun = Value
  implicit def WeekDayToInt(weekDay: WeekDay): Int = weekDay.id
  implicit def IntToWeekDay(int: Int): WeekDay = WeekDay(int)
  def LocalDateToWeekday: WeekDay = ???
}

import WeekDay._

case class PresentationStuff()

case class CapiQuery(query: String) extends AnyVal

case class CollectionTemplate(
    prefill: CapiQuery,
    name: String,
    hidden: Boolean,
    presentationStuff: PresentationStuff
)

case class FrontTemplate(
    collections: List[CollectionTemplate],
    name: String,
    hidden: Boolean,
    presentationStuff: PresentationStuff
)

case class IssueTemplate(
    publicationDate: LocalDate,
    name: S
    frontsGeneratorVersion: String,
    fronts: List[FrontTemplate],
    presentationStuff: PresentationStuff
)

object Generator {
  def generateIssueTemplate(date: LocalDate): IssueTemplate = {
    return IssueTemplate(
      date,
      "v1",
      generateFronts(date),
      PresentationStuff()
    )
  }

  def generateFronts(date: LocalDate): List[FrontTemplate] = {
    val day = WeekDay.LocalDateToWeekday(date)
    val fronts = day match {
      case Sat => DailyEdition.GuardianSaturday
      case Sun => DailyEdition.Observer
      case _   => DailyEdition.GuardianWeekday
    }
    selectFronts(fronts)
  }

  def generateCollections(front: Front, date: LocalDate)

  def selectFronts(
      fronts: List[(Front, Frequency)],
      date: LocalDate
  ): List[Front] = {
    fronts
      .filter { case (front, freq) => freq.validOn(date) }
      .map { case (front, _) => front }
  }
}

trait Frequency {
  def validOn(date: LocalDate): Boolean
}
case object Daily extends Frequency
case object EveryThursday extends Frequency

object DailyEdition {
  val ukNews: Front = Front("UK news", List())
  val sports: Front = Front(
    "Sport",
    List(
      Sport.rollerDerby -> EveryFriday,
      Sport.football,
      Sport.cricket,
      Sport.rugby
    )
  )
  val opinion: Front = Front("Opinion")
  val technology: Front = Front("Technology")
  val foodMonthly: Front = Front("Food Monthly")

  val GuardianWeekday = SetOfFronts(
    List(
      ukNews -> Daily,
      opinion -> Daily,
      technology -> EveryThursday,
      sports -> Daily
    )
  )

  val GuardianSaturday = SetOfFronts(
    List(
      sports -> Daily,
      opinion -> Daily,
      ukNews -> Daily
    )
  )

  val Observer = SetOfFronts(
    List(
      ukNews -> always,
      foodMonthly -> thirdSunday
    )
  )

  // alternative way of creating the above???
  case class SetOfFronts(fronts: List[(Fronts, Periodicity)])

  val allPossibleFronts: List[???] = List(
    )

}

object Sport {
  val football: Collection = new Collection("Football", CapiQuery("???"))
  val cricket: Collection = ???
  val rugby: Collection = ???
  val rollerDerby: Collection = ???
}

class Front(name: String, collections: List[Collection]) {
  def isValidOn(date: LocalDate): Boolean
}

class Collection(name: String, capiQuery: CapiQuery) {
  def isValidOn(date: LocalDate): Boolean
}
