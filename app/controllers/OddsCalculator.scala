package controllers

import model.{Card, Player}
import scala.util.Random

object OddsCalculator {

  /**
   * 勝率
   */
  case class Odds(win: Double, tie: Double)

  /**
   * 勝率取得
   */
  def getOdds(players: Seq[Player], board: Seq[Card]): Map[Player, Odds] = {
    //見えているカードをすべて取り除いたデッキを作成
    val deck = Card.createDeck
      .filterNot(card => players.exists( player => player.holes.contains(card)))
      .filterNot(board.contains(_))
    //ボード枚数によって計算処理を分岐
    board.size match {
      case 5 => calcOdds(players, Seq(board)) //単純判定
      case 4 => calcOdds(players, deck.map( card => Seq(card) ++ board )) //総当たり
      case 3 => calcOdds(players, deck.combinations(2).map( cards => cards ++ board ).toSeq)  //組み合わせ総当たり
      case _ => calcOdds(players, (1 to 10000).map(i => Random.shuffle(deck).take(5))) //モンテカルロ
    }
  }

  /**
   * 勝率計算
   */
  private def calcOdds(players: Seq[Player], boards: Seq[Seq[Card]]): Map[Player, Odds] = {
    val (folder,survivor) = players.partition(_.isFolded)
    val folderOdds = folder.map(me => me -> Odds(0, 0))
    //生存者のみでboardsの数だけ勝率判定
    val survivorOdds = survivor.map{ me =>
      val results: Map[String, Int] = Map("win" -> 0, "tie" -> 0, "lose" -> 0)
      boards.foreach{ board =>
        Judge.getWinner(survivor,board) match {
          case winners if !winners.keysIterator.contains(me) => results("lose") += 1
          case winners if winners.size > 1 => results("win") += 1 ; results("tie") += 1
          case _ => results("win") += 1
        }
      }
      me -> Odds(results("win") / boards.size, results("tie") / boards.size)
    }
    (folderOdds ++ survivorOdds).toMap
  }
}
