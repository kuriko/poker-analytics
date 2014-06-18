import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.http.HeaderNames
import play.api.libs.json.Json
import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication{
      route(FakeRequest(GET, "/bomb")) must beNone
    }

    "render the index page" in new WithApplication{
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/plain")
      contentAsString(home) must contain ("It works!")
    }

    "getOdds" in new WithApplication{

      val jsBody = Json.parse{
        """
        {

          "players": [
            {
              "id": 0,
              "hand": ["Jh", "Qh"],
              "isActive": true
            },
            {
              "id": 1,
              "hand": ["3c", "5d"],
              "isActive": true
            }
          ]
        }
        """
      }
      val home = route(FakeRequest("POST", "/odds").withHeaders(HeaderNames.CONTENT_TYPE -> "application/json").withJsonBody(jsBody)).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "application/json")
    }
  }

}
