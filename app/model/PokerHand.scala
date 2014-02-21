package model

/**
 * ポーカーハンド
 */
sealed abstract class PokerHand(val rank: Int, val cards: Seq[Card], val using: Int, val name: String){
  val strength = {
    val pokerHandValue = rank * Math.pow(100,5)
    val cardsValue = (0 to 4).map(i => cards(i).rank.code * Math.pow(100,4 - i)).sum
    (pokerHandValue + cardsValue).toLong
  }
}

class HighCard           (override val cards: Seq[Card]) extends PokerHand(0, cards, 0, "ハイカード"){ override def toString = s"$name"}
class OnePair            (override val cards: Seq[Card]) extends PokerHand(1, cards, 2, "ワンペア"){ override def toString = s"${cards(0).rank.name} $name"}
class TwoPair            (override val cards: Seq[Card]) extends PokerHand(2, cards, 4, "ツーペア"){ override def toString = s"${cards(0).rank.name} & ${cards(2).rank.name} $name"}
class ThreeOfAKind       (override val cards: Seq[Card]) extends PokerHand(3, cards, 3, "スリーカード"){ override def toString = s"${cards(0).rank.name} $name"}
class Straight           (override val cards: Seq[Card]) extends PokerHand(4, cards, 5, "ストレート"){ override def toString = s"${cards.map(_.rank.name).mkString(",")} $name"}
class Flush              (override val cards: Seq[Card]) extends PokerHand(5, cards, 5, "フラッシュ"){ override def toString = s"${cards.map(_.rank.name).mkString(",")} $name"}
class FullHouse          (override val cards: Seq[Card]) extends PokerHand(6, cards, 5, "フルハウス"){ override def toString = s"${cards(0).rank.name} & ${cards(3).rank.name} $name"}
class FourOfAKind        (override val cards: Seq[Card]) extends PokerHand(7, cards, 4, "フォーカード"){ override def toString = s"${cards(0).rank.name} $name"}
class StraightFlush      (override val cards: Seq[Card]) extends PokerHand(8, cards, 5, "ストレートフラッシュ"){ override def toString = s"${cards.map(_.rank.name).mkString(",")} $name"}
class RoyalStraightFlush (override val cards: Seq[Card]) extends PokerHand(9, cards, 5, "ロイヤルストレートフラッシュ"){ override def toString = s"$name"}