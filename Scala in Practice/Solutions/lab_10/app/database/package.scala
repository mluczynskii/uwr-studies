import slick.jdbc.PostgresProfile.api._

package object database {
  val students = TableQuery[Students]
  val lectures = TableQuery[Lectures]
  val enrollments = TableQuery[Enrollments]
}