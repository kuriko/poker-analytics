package model

import play.api.libs.json.Json._
import scala.util.Random

/**
 * カード
 * @param suit スーツ
 * @param rank ランク
 */
class Card (val suit: Suit, val rank: Rank){
  def toJsValue = toJson(suit.mark + rank.name)
}
object Card {
  /**
   * 降順ソート
   */
  def rankDescSort(cards: Seq[Card]): Seq[Card] = cards.sortBy(_.rank.code).reverse

  def createDeck: Seq[Card] = Random.shuffle(for(i <- 0 to Suit.values.size * Rank.size - 1) yield new Card(Suit(i / Rank.size),Rank((i % Rank.size) + 2)))
}

/**
 * スート
 */
sealed abstract class Suit(val code: String, val mark: String)
object Suit {
  case object Spade   extends Suit("s", "♠")
  case object Hart    extends Suit("h", "♥")
  case object Diamond extends Suit("d", "♦")
  case object Club    extends Suit("c", "♣")
  val values = Seq(Spade, Hart, Diamond, Club)
  def apply(code: Int) = values.find(_.code == code).get
}

/**
 * ランク
 */
sealed abstract class Rank(val code: Int, val name: String){
  override def toString = name
}
object Rank {
  case object _1 extends Rank(1, "1") //ストレート判定時のみ使用
  case object _2 extends Rank(2, "2")
  case object _3 extends Rank(3, "3")
  case object _4 extends Rank(4, "4")
  case object _5 extends Rank(5, "5")
  case object _6 extends Rank(6, "6")
  case object _7 extends Rank(7, "7")
  case object _8 extends Rank(8, "8")
  case object _9 extends Rank(9, "9")
  case object _T extends Rank(10, "T")
  case object _J extends Rank(11, "J")
  case object _Q extends Rank(12, "Q")
  case object _K extends Rank(13, "K")
  case object _A extends Rank(14, "A")
  val values = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _T, _J, _Q, _K, _A)
  val size = values.size - 1 //ダミーの１は除外
  def apply(code: Int) = values.find(_.code == code).get
}