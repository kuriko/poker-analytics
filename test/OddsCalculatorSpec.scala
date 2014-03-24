import model._
import controllers.OddsCalculator
import org.specs2.mutable.Specification

class OddsCalculatorSpec extends Specification {

  "success" should {
    "getOdds" in {
      val player1 = Player(0, Seq(new Card(Suit.Club, Rank._8), new Card(Suit.Club, Rank._K)), isActive = true)
      val player2 = Player(1, Seq(new Card(Suit.Spade, Rank._7), new Card(Suit.Diamond, Rank._Q)), isActive = true)
      val player3 = Player(2, Seq(new Card(Suit.Hart, Rank._2), new Card(Suit.Club, Rank._T)), isActive = true)
      val player4 = Player(3, Seq(new Card(Suit.Diamond, Rank._3), new Card(Suit.Club, Rank._J)), isActive = true)
      val player5 = Player(4, Seq(new Card(Suit.Club, Rank._4), new Card(Suit.Club, Rank._A)), isActive = false)
      val player6 = Player(5, Seq(new Card(Suit.Spade, Rank._T), new Card(Suit.Spade, Rank._4)), isActive = true)
      val player7 = Player(6, Seq(new Card(Suit.Spade, Rank._2), new Card(Suit.Diamond, Rank._2)), isActive = true)
      val player8 = Player(7, Seq(new Card(Suit.Diamond, Rank._6), new Card(Suit.Spade, Rank._5)), isActive = true)
      val board = Seq()
      val odds = OddsCalculator.getOdds(
        Seq(
          player1,
          player2,
          player3,
          player4,
          player5,
          player6,
          player7,
          player8), board)
      //System.out.println(odds.map(o => "[" +o._1.name + ":" + Math.round(o._2.win * 10000) / 100.0 + "%, " + Math.round(o._2.tie * 10000) / 100.0 + "%]"))
      1 === 1
    }
  }
}
