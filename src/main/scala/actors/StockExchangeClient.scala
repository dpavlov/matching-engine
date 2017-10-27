package actors

import akka.actor.{Actor, ActorRef}
import currency.Currency
import domain._

/**
  * Created by dpavlov on 26/10/2017.
  */
class StockExchangeClient(client: Client) extends Actor {

  import actors.StockExchangeClient._

  private var balance = client.balance
  private var stocks = client.stocks

  private var balanceSubscribers = Set.empty[ActorRef]

  override def receive: Receive = {
    case order: Order => order.operation match {
      case Sell => {
        balance = balance + (order.price * order.qty)
        val newStockQty = stocks.getOrElse(order.stockSymbol, 0L) - order.qty
        stocks = stocks.updated(order.stockSymbol, newStockQty)
        notifySubscribers()
      }
      case Buy => {
        balance = balance - (order.price * order.qty)
        val newStockQty = stocks.getOrElse(order.stockSymbol, 0L) + order.qty
        stocks = stocks.updated(order.stockSymbol, newStockQty)
        notifySubscribers()
      }
    }
    case GetClientBalance => {
      sender() ! ClientBalance(client.code, this.balance, this.stocks)
    }

    case Subscribe(listener: ActorRef) => balanceSubscribers = balanceSubscribers + listener
    case UnSubscribe(listener: ActorRef) => balanceSubscribers = balanceSubscribers - listener

  }

  private[this] def notifySubscribers(): Unit = balanceSubscribers.foreach(_ ! ClientBalance(client.code, this.balance, this.stocks))
}

object StockExchangeClient {
  case object GetClientBalance
  case class ClientBalance(clientCode: ClientCode, balance: Currency, stocks: Map[StockSymbol, Long])

  case class Subscribe(listener: ActorRef)
  case class UnSubscribe(listener: ActorRef)
}
