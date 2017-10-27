package services

import currency.Currency
import domain.{Buy, ClientCode, Order, Sell}

import scala.util.Try

/**
  * Created by dpavlov on 26/10/2017.
  */
object OrderFactory {
  def fromTabSeparatedLine(line: String): Option[Order] = {
    line.split("\\t").toList match {
      case code :: "s" :: stockSymbol :: price :: qty :: Nil if code.nonEmpty && stockSymbol.nonEmpty => {
        val priceInUsd = Currency(Try(price.toInt).getOrElse(0), "USD")
        val quantity =  Try(qty.toLong).getOrElse(0L)
        Some(Order(ClientCode(code), Sell, stockSymbol, priceInUsd, quantity))
      }
      case code :: "b" :: stockSymbol :: price :: qty :: Nil if code.nonEmpty && stockSymbol.nonEmpty => {
        val priceInUsd = Currency(Try(price.toInt).getOrElse(0), "USD")
        val quantity =  Try(qty.toLong).getOrElse(0L)
        Some(Order(ClientCode(code), Buy, stockSymbol, priceInUsd, quantity))
      }
      case _ => None
    }
  }
}
