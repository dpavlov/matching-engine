package services

import currency.Currency
import domain.{ClientCode, Operation, Order}
import org.scalatest.prop.PropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dpavlov on 26/10/2017.
  */
class OrderFactorySpec extends FlatSpec with Matchers {

  val validCases =
    Table(
      ("name", "operation", "stockSymbol", "price", "qty"),
      ("C8", "b", "C", 15,	4L),
      ("C2", "s", "C", 14,	5L),
      ("C2", "s", "C", 13,	2L),
      ("C9", "b", "B", 6,	4L),
      ("C4", "b", "D", 5,	4L)
    )

  "A Factory" should "create an order instance from tab separated line" in {
    forAll(validCases) { (name: String, op: String, symbol: String, price: Int, qty: Long) =>
      OrderFactory.fromTabSeparatedLine(s"${name}\t$op\t$symbol\t$price\t$qty").map(_.copy(ts = 0)) should be(
        Some(Order(ClientCode(name), Operation(op), symbol, Currency(price, "USD"), qty, 0))
      )
    }
  }

  it should "return none in case if client name is missing" in {
    OrderFactory.fromTabSeparatedLine(s"\tb\tC\t12\t3") should be(None)
  }

  it should "return none in case if operation code is wrong" in {
    OrderFactory.fromTabSeparatedLine(s"\tf\tC\t12\t3") should be(None)
  }
}
