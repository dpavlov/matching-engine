package services

import currency.Currency
import domain.{Client, ClientCode}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks._

/**
  * Created by dpavlov on 26/10/2017.
  */
class ClientFactorySpec extends FlatSpec with Matchers {

  val validCases =
    Table(
      ("name", "balance", "a", "b", "c", "d"),
      ("C1", 1000, 130L, 240L, 760L, 320L),
      ("C2", 4350, 370L, 120L, 950L, 560L),
      ("C3", 2760, 0L,	0L,	0L,	0L),
      ("C4", 560,	450L,	540L,	480L,	950L),
      ("C5", 1500,	0L,	0L,	400L,	100L),
      ("C6", 1300,	890L,	320L,	100L,	0L),
      ("C7", 750,	20L,	0L,	790L,	0L),
      ("C8", 7000,	90L,	190L,	0L,	0L),
      ("C9", 7250,	190L,	190L,	0L,	280L)
    )

  "A Factory" should "create an client instance from tab separated line" in {
    forAll(validCases) { (name: String, balance: Int, a: Long, b: Long, c: Long, d:Long) =>
      val expectedStocksBalance = Map("A" -> a, "B" -> b, "C" -> c, "D" -> d).filter { case (_, b) => b > 0 }

      ClientFactory.fromTabSeparatedLine(s"${name}\t$balance\t$a\t$b\t$c\t$d") should be(
        Some(Client(ClientCode(name), Currency(balance, "USD"), expectedStocksBalance))
      )
    }
  }

  it should "return non in case if client name is missing" in {
    ClientFactory.fromTabSeparatedLine(s"\t0\t0\t0\t0\t0") should be(None)
  }
}
