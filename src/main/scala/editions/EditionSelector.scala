import com.github.nscala_time.time.Imports._

object WeekDay extends Enumeration(0) {
  type WeekDay = Value
  val Mon, Tues, Wed, Thurs, Fri, Sat, Sun = Value
  implicit def WeekDayToInt(weekDay: WeekDay): Int = weekDay.id
  implicit def IntToWeekDay(int: Int): WeekDay = WeekDay(int)
}

import WeekDay._

case class Collection(val `type`: String) {}
case class Front(val collections: List[Collection]) {}

object Front {
  def default = Front(List(Collection("default")))
}

case class Template(val fronts: List[Front]) {}

// So this can be JSONified
object Template {
  def default = Template(List(Front.default))
}

trait Edition {
  def templateByWeekDay(weekDay: WeekDay): Option[Template]
  def templateByDate(date: DateTime) = templateByWeekDay(date.getDayOfWeek())
}

// Some type of edition edition
case class Observer(val region: String) extends Edition {
  val food = Template.default
  val default = Template.default

  def templateByWeekDay(weekDay: WeekDay, monthWeek: Int): Option[Template] = weekDay match {
    case Sun => monthWeek match {
      case 3 => Some(food)
      case _ => Some(default)
    }
    case _ => None
  }
}

object Main {
  def getDailyEditionForToday() = Observer("en").templateByDate(new DateTime())
}
