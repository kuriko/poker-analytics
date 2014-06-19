package controllers

import model.{Card, Player}
import scala.util.Random

object OddsCalculator {

  /**
   * 勝率
   */
  case class Odds(win: Double, tie: Double){
    val win_par = (Math.round(win * 10000) / 100.0).toString + "%"
    val tie_par = (Math.round(tie * 10000) / 100.0).toString + "%"
  }

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
      case _ => calcOdds(players, (1 to 1000).map(i => Random.shuffle(deck).take(5))) //モンテカルロ
    }
  }

  /**
   * 勝率計算
   */
  private def calcOdds(players: Seq[Player], boards: Seq[Seq[Card]]): Map[Player, Odds] = {
    val (actives, inactives) = players.partition(_.isActive)
    val folderOdds = inactives.map(me => me -> Odds(0.0, 0.0))
    //スコア判定
    boards.foreach{ board =>
      val winners = Judge.getWinner(actives,board)
      actives.map{ me =>
        if(winners.keySet.contains(me)) {
          if(winners.size > 1) {
            me.score_tie += 1
          } else {
            me.score_win += 1
          }
        } else {
          me.score_lose += 1
        }
      }
    }
    val survivorOdds = actives.map{ me =>
      me -> Odds(me.score_win.toDouble / boards.size, me.score_tie.toDouble / boards.size)
    }
    (folderOdds ++ survivorOdds).toMap
  }
}
