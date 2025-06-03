package database 

import slick.jdbc.PostgresProfile.api._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Queries {
  lazy val db = Database.forConfig("postgres")

  def studentNames(): Future[Seq[String]] = {
    val query = students.map(_.name)
    db.run(query.result)
  }
    
  def studentInfo(index: Int): Future[Option[Student]] = {
    val query = students.filter(_.index === index)
    db.run(query.result).map {
      case Vector() => None 
      case x +: xs => Some(x) 
    }
  }

  def lectureNames(): Future[Seq[String]] = {
    val query = lectures.map(_.name)
    db.run(query.result)
  }

  def lectureInfo(id: String): Future[(Option[Lecture], Seq[String])] = {
    val lectureQuery = lectures.filter(_.id === id)
    val enrollmentQuery = for {
      (e, s) <- enrollments join students on (_.student_id === _.index)
      if e.lecture_id === id
    } yield s.name
    for {
      lecture <- db.run(lectureQuery.result).map {
        case Vector() => None 
        case x +: xs => Some(x)
      }
      enrollmentSeq <- db.run(enrollmentQuery.result)
    } yield (lecture, enrollmentSeq)
  }
}