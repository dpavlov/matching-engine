import currency.Currency

/**
  * Created by dpavlov on 26/10/2017.
  */
package object domain {
  case class ClientCode(code: String) extends AnyVal
  type StockSymbol = String

  case class Client(code: ClientCode, balance: Currency, stocks: Map[StockSymbol, Long])


  sealed trait Operation
  case object Sell extends Operation
  case object Buy extends Operation

  object Operation {
    def apply(code: String): Operation = code match {
      case "b" => Buy
      case _ => Sell
    }
  }

  case class Order(clientCode: ClientCode, operation: Operation, stockSymbol: StockSymbol, price: Currency, qty: Long, ts: Long = System.nanoTime())
}
