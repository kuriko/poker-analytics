package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.JsValue
import play.api.libs.json.Json.toJson

object Application extends Controller {

  def index = Action {
    Ok("it's running...")
  }

  def getOdds = Action { request =>
    request.body.asJson.map { json =>
      (json \ "name").asOpt[String].map { name =>
        Ok("Hello " + name)
      }.getOrElse {
        BadRequest("Missing parameter [name]")
      }
    }.getOrElse {
      BadRequest("Expecting Json data")
    }

    Ok(result)
  }

}