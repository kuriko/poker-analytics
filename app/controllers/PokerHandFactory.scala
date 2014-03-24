package controllers

import scala.collection.mutable.ListBuffer
import model._

object PokerHandFactory {

  /**
   * カード群（7枚）からハンド（最適な組み合わせ）を取得
   */
  def createPokerHand(cards: Seq[Card]): PokerHand ={
    require(cards.size.equals(7))
    val hands: ListBuffer[PokerHand] = new ListBuffer[PokerHand]()

    //5連続以上 -> ストレート
    //同スートが５枚以上 -> フラッシュ
    //同スートの中にストレートがある -> ストフラ
    //ストフラの5枚目がTである -> ロイヤル
    val s = getStraightCandidate(cards)
    val h = getFlushCandidate(cards)
    val sh = if(h.isDefined) getStraightCandidate(h.get) else None
    val rsh = if(sh.isDefined && (sh.get.last.rank == Rank._T)) sh else None

    if(rsh.isDefined)               hands += new RoyalStraightFlush(rsh.get)
    if(rsh.isEmpty && sh.isDefined) hands += new StraightFlush(desc5(sh.get))
    if(sh.isEmpty && h.isDefined)   hands += new Flush(desc5(h.get))
    if(h.isEmpty && s.isDefined)    hands += new Straight(desc5(s.get))

    //４枚組がある -> クアッズ
    //３枚組がある且つ２枚組以上がある -> フルハウス
    //３枚組がある且つ２枚組以上がない -> トリップス
    //２枚組が２つある -> ツーペア
    //２枚組が２つない -> ワンペア判定
    //２枚組がない -> ハイカード
    getRankHash(cards) match {
      case x1::x2::_ if x1.size >= 4 =>                         hands += new FourOfAKind(List.concat(x1,x2.take(1)))
      case x1::x2::_ if x1.size == 3 && x2.size >= 2 =>         hands += new FullHouse(List.concat(x1,x2.take(2)))
      case x1::x2::x3::_ if x1.size == 3 && x2.size == 1 =>     hands += new ThreeOfAKind(List.concat(x1,x2,x3))
      case x1::x2::x3::_ if x1.size == 2 && x2.size == 2 =>     hands += new TwoPair(List.concat(x1,x2,x3))
      case x1::x2::x3::x4::_ if x1.size == 2 && x2.size == 1 => hands += new OnePair(List.concat(x1,x2,x3,x4))
      case x1::_ if x1.size == 1 =>                             hands += new HighCard(desc5(cards))
      case _ =>
    }

    //最上位のハンドのみを返却
    hands.sortBy(_.rank).last
  }

  /**
   * 枚数降順＆ランク降順のランク別にまとめる
   * 例：(1,1,3,5,5,5,6) -> ((5,5,5),(1,1),6,3)
   */
  private def getRankHash(cards: Seq[Card]): Seq[Seq[Card]] = {
    val rankHash = cards.groupBy( card => card.rank )
    val rankSortedHash = rankHash.toSeq.sortBy( s => s._2.size * 100 + s._1.code).reverse
    val result = rankSortedHash.map(cards => cards._2.toList).toList
    result
  }

  /**
   * フラッシュ候補取得
   *   同スートが５枚以上ある場合にそのスートのSeqを返却する。
   *   無い場合はNoneを返却する。
   *   前提：cards.size < 10
   */
  private def getFlushCandidate(cards: Seq[Card]): Option[Seq[Card]] = {
    require(cards.size <= 7)
    val suitDividedCards = Suit.values.map(suit => cards.filter(card => card.suit == suit))
    suitDividedCards.find( cards => cards.size >= 5)
  }

  /**
   * ストレート候補取得
   *   連続したカードが５枚以上ある場合に連続したカード群のSeqを返却する。
   *   無い場合はNoneを返却する。
   *   前提：cards.size < 10
   */
  private def getStraightCandidate(cards: Seq[Card]): Option[Seq[Card]] = {
    require(cards.size <= 7)
    val rank1Cards = cards.filter( _.rank == Rank._A ).map(a => new Card(a.suit,Rank._1)) //仮想の1(A)カードを追加
    straightFilter(Card.rankDescSort(rank1Cards ++ cards))
  }

  /**
   * 非５連続カードの除外 （再帰）
   * 前提：cards.size < 10
   */
  private def straightFilter(cards: Seq[Card], hands: ListBuffer[Card] = ListBuffer[Card]()): Option[Seq[Card]] = {
    Card.rankDescSort(cards) match {
      case Nil => if(hands.size >= 5) Some(hands) else None //基底部
      case x::xs if hands.isEmpty => straightFilter(xs,hands += x) //初期化
      case x::xs if hands.last.rank == x.rank => straightFilter(xs,hands) //同ランクは無視
      case x::xs if hands.last.rank == Rank(x.rank.code+1) => straightFilter(xs,hands += x) //連続している場合はhands先頭に追加
      case x::xs if hands.size >= 5 => Some(hands) //不連続且つストレートが完成している場合は終了
      case x::xs => straightFilter(xs,ListBuffer[Card]() += x) //不連続且つストレートが完成していない場合はリセット
    }
  }

  /**
   * カードの降順TOP5を返却
   */
  private def desc5(cards: Seq[Card]): Seq[Card] = Card.rankDescSort(cards).take(5).toList
}
