package database 

import slick.jdbc.PostgresProfile.api._

case class Student(index: Int, name: String, year: Short)

class Students(tag: Tag) extends Table[Student](tag, "students"){
  def index = column[Int]("index", O.PrimaryKey)
  def name  = column[String]("name")
  def year  = column[Short]("year")
  def *     = (index, name, year) <> (Student.tupled, Student.unapply)
}

case class Lecture(id: String, name: String)

class Lectures(tag: Tag) extends Table[Lecture](tag, "lectures"){
  def id    = column[String]("id", O.PrimaryKey)
  def name  = column[String]("name")
  def *     = (id, name) <> (Lecture.tupled, Lecture.unapply)
}

case class Enrollment(lecture_id: String, student_id: Int)

class Enrollments(tag: Tag) extends Table[Enrollment](tag, "enrollments"){
  def lecture_id = column[String]("lecture_id")
  def student_id = column[Int]("student_id")
  def pk         = primaryKey("pk_enrollments", (lecture_id, student_id))
  def *          = (lecture_id, student_id) <> (Enrollment.tupled, Enrollment.unapply)
}

