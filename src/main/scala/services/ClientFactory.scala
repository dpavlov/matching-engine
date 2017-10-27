package services

import currency.Currency
import domain.{Client, ClientCode}

import scala.util.Try

/**
  * Created by dpavlov on 26/10/2017.
  */
object ClientFactory {
  def fromTabSeparatedLine(line: String): Option[Client] = {
    line.split("\\t").toList match {
      case code :: balance :: stocks if code.nonEmpty => {
        val balanceInUsd = Currency(Try(balance.toInt).getOrElse(0), "USD")
        val stockBalances = stocks
          .view
          .map(balance => Try(balance.toLong).getOrElse(0L))
          .zip('A' to 'Z')
          .filter { case (balance, _) => balance > 0 }
          .map { case (balance, symbol) => symbol.toString -> balance }
          .toMap

        Some(Client(ClientCode(code), balanceInUsd, stocks = stockBalances))
      }
      case _ => None
    }
  }
}
