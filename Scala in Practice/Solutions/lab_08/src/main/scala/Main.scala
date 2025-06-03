import adapter.FacebookAdapter

object Main {
  def main(args: Array[String]): Unit = {
    val accessToken = sys.env.getOrElse(
      "ACCESS_TOKEN", 
      throw new Exception("no access token provided")
    )
    val client = new FacebookAdapter.MyFacebookClient(accessToken)
    client.getInfo("journal.log", "me")
  }
}