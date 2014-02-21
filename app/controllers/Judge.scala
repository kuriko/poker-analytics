package controllers

import model.{Player, Card, PokerHand}

object Judge {

  /**
   * 勝利ハンド
   */
  case class WinningHand(hand: PokerHand, kicker: Option[Card])

  /**
   * 勝者選出
   */
  def getWinner(players: Seq[Player], board: Seq[Card]): Map[Player, WinningHand] = {
    case class PlayerHands(player: Player, hand: PokerHand)
    val playerHands = players.map( player => PlayerHands(player, PokerHandFactory.createPokerHand(player.holes ++ board)))

    //ハンド＋キッカーの強さを数値化してグルーピング -> ソート
    val groupedPHs = playerHands.groupBy(_.hand.strength).toSeq
    val sortedPHs = groupedPHs.sortBy(_._1).reverse.map(_._2)

    //勝敗によって戻り値を変える
    sortedPHs.toList match {
      case phs::_ if phs.size > 1 => //Chop
        phs.map(ph => ph.player -> WinningHand(ph.hand, Some(ph.hand.cards.last))).toMap

      case (ph1::_)::(ph2::_)::_ =>
        val kicker = getKickerNum(ph1.hand, ph2.hand).map( num => ph1.hand.cards(num))
        Map(ph1.player -> WinningHand(ph1.hand, kicker))

      case _ => throw new RuntimeException
    }
  }

  /**
   * キッカー勝負になる場合 -> Some(キッカーの場所）
   */
  private def getKickerNum(hand1: PokerHand, hand2: PokerHand): Option[Int] = {
    val differ = (Math.log10(Math.abs(hand1.strength - hand2.strength)) / 2).toInt
    if(differ < 5 - hand1.using) Some(differ) else None
  }

}
