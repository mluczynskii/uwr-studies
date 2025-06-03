package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import database._
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

@Singleton
class ZapisyController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  def students() = Action.async {
    Queries.studentNames().map {
      xs => Ok(views.html.students(xs))
    }.recover {
      case ex: Throwable =>
        InternalServerError(s"db fetch error: $ex")
    }
  }

  def student(index: Int) = Action.async {
    Queries.studentInfo(index).map {
      case Some(student) => Ok(views.html.student(student))
      case None => NotFound(s"Student with index=$index does not exist")
    }.recover {
      case ex: Throwable =>
        InternalServerError(s"db fetch error: $ex")
    }
  }

  def lectures() = Action.async {
    Queries.lectureNames().map {
      xs => Ok(views.html.lectures(xs))
    }.recover {
      case ex: Throwable =>
        InternalServerError(s"db fetch error: $ex")
    }
  }

  def lecture(id: String) = Action.async {
    Queries.lectureInfo(id).map {
      case (Some(lecture), xs) => Ok(views.html.enrollments(lecture, xs))
      case (None, _) => NotFound(s"Lecture with id=$id does not exist")
    }.recover {
      case ex: Throwable =>
        InternalServerError(s"db fetch error: $ex")
    }
  }

}