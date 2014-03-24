package model

/**
 * プレイヤー
 */
case class Player(id: Int, holes: Seq[Card], isActive: Boolean){
  var score_win: Int = 0
  var score_tie: Int = 0
  var score_lose: Int = 0
}