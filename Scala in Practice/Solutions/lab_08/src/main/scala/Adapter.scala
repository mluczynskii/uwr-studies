package adapter 

import com.restfb.{DefaultFacebookClient, Version, Parameter}
import com.restfb.types.User
import scala.concurrent.{Future, Await}
import java.io.{File, FileWriter}
import java.time.LocalDateTime
import scala.util.{Success, Failure}

object Utility {
  def log(logFile: String, msg: String): Future[Unit] = Future {
    try {
      val fileWriter = new FileWriter(new File(logFile), true)
      fileWriter.write(msg + "\n")
      fileWriter.close()
    } catch {
      case e: Exception => println(s"failed to log message: $msg to $logFile")
    }
  }(scala.concurrent.ExecutionContext.global)
}

object FacebookAdapter {
  private val myAppSecret = 
    sys.env.getOrElse("API_KEY", throw new Exception("no API key provided"))

  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  class MyFacebookClient(currentAccessToken: String) 
    extends DefaultFacebookClient(currentAccessToken, myAppSecret, Version.LATEST) {

    private def getUser(id: String): Future[User] = Future { 
      this.fetchObject(id, classOf[User], Parameter.withFields("name,birthday"))
    }

    /* Graph API privacy changes make this unusable */
    def compareLikes(logFile: String, id1: String, id2: String): Unit = { 
      Utility.log(logFile, s"[compareLikes] ${LocalDateTime.now()} $id1 $id2")
      (this.getUser(id1) zip this.getUser(id2)).onComplete{
        case Success((user1, user2)) =>
          println(s"${user1.getName}, likes: ${user1.getLikes.getTotalCount} vs. \n" +
            s"${user2.getName}, likes: ${user2.getLikes.getTotalCount}")
        case Failure(exception) => 
          println(s"Failed to fetch like counts: ${exception.getMessage}")
      }
    }

    /* Alternative method to show that the API works */
    def getInfo(logFile: String, id: String): Unit = {
      Utility.log(logFile, s"[getInfo] ${LocalDateTime.now()} $id")
      this.getUser(id).onComplete{
        case Success(user) =>
          println(s"name: ${user.getName}, birthday: ${user.getBirthday}")
        case Failure(exception) => 
          println(s"Failed to fetch information: ${exception.getMessage}")
      }
    }
  }
}