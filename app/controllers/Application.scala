package controllers

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.json.Json.toJson
import model.{Card, Player}

object Application extends Controller {

  def index = Action {
    Ok("It works!")
  }

  def getOdds = Action { request =>

    val jsBody = request.body.asJson
    System.out.println(jsBody)
    jsBody.map { json =>
      val players = (json \ "players").as[List[JsValue]].map { player =>
        new Player(
          (player \ "id").as[Int],
          (player \ "hand").as[List[JsValue]].map( card => Card(card.as[String]) ),
          (player \ "isActive").as[Boolean])
      }
      val board = (json \ "board").asOpt[List[JsValue]].getOrElse(Nil).map( card => Card(card.as[String]))
      val odds = OddsCalculator.getOdds(players,board)

      val jsResponse = toJson(Map("players" ->
        toJson(odds.map{ po =>
          toJson(Map(
            "id" -> toJson(po._1.id),
            "win" -> toJson(po._2.win_par),
            "tie" -> toJson(po._2.tie_par)))
        })))
      System.out.println(jsResponse)
      Ok(jsResponse)
    }.getOrElse {
      BadRequest("Expecting Json data")
    }
  }
}