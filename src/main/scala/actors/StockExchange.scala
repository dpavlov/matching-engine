package actors

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.pattern.pipe
import akka.util.Timeout
import currency.Currency
import domain.{Client, ClientCode, Order, StockSymbol}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * Created by dpavlov on 26/10/2017.
  */
class StockExchange extends Actor {

  import StockExchange._

  implicit val ex: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout = Timeout(100 milliseconds)

  var orderBooks = Map.empty[StockSymbol, ActorRef]
  var clients = Map.empty[ClientCode, ActorRef]

  var filledOrdersCount: Long = 0L
  var acknowledgedOrdersCount: Long = 0L
  var canceledOrdersCount: Long = 0L
  var rejectedOrdersCount: Long = 0L

  override def receive: Receive = {
    case order: Order => {
      (orderBooks.getOrElse(order.stockSymbol, createNewOrderBookFor(order.stockSymbol)) ? order).pipeTo(sender)
    }
    case client: Client => sender() ! clients.getOrElse(client.code, createNewClient(client))
    case OrderBook.Filled(_, _, orders, _) => {
      clients.get(orders(0).clientCode).foreach(client => client ! orders(0))
      clients.get(orders(1).clientCode).foreach(client => client ! orders(1))
    }
    case request: GetClientBalance => {
      val balances = Future.sequence(
        request.clientCodes
          .flatMap(clients.get)
          .map(ref => (ref ? StockExchangeClient.GetClientBalance).mapTo[StockExchangeClient.ClientBalance])
      )
      sendClientBalances(sender(), balances)
    }

    case GetClientBalances => {
      val balances = Future.sequence(
        clients.values
          .map(ref => (ref ? StockExchangeClient.GetClientBalance).mapTo[StockExchangeClient.ClientBalance])
      )
      sendClientBalances(sender(), balances)
    }
    case request: SubscribeForClientBalanceChange => {
      request.clientCodes.flatMap(clients.get).foreach(_ ! StockExchangeClient.Subscribe(request.listener))
    }
    case request: UnSubscribeForClientBalanceChange => {
      request.clientCodes.flatMap(clients.get).foreach(_ ! StockExchangeClient.UnSubscribe(request.listener))
    }
  }

  private def sendClientBalances(replyTo: ActorRef, balances: Future[Iterable[StockExchangeClient.ClientBalance]]): Unit = {
    balances.map(all =>
      StockExchange.ClientBalances(
        all.map(b => b.clientCode -> StockExchange.ClientBalance(b.balance, b.stocks)).toMap
      )
    ).pipeTo(replyTo)
  }

  private[this] def createNewOrderBookFor(symbol: StockSymbol): ActorRef = {
    val orderBook: ActorRef = context.actorOf(Props(new OrderBook), s"order-book-$symbol")
    orderBooks = orderBooks.updated(symbol, orderBook)
    orderBook
  }

  private[this] def createNewClient(client: Client): ActorRef = {
    val clientRef: ActorRef = context.actorOf(Props(new StockExchangeClient(client)), client.code.code)
    clients = clients.updated(client.code, clientRef)
    clientRef
  }
}

object StockExchange {
  case class GetClientBalance(clientCodes: ClientCode*)
  case object GetClientBalances
  case class ClientBalances(clients: Map[ClientCode, ClientBalance] = Map.empty)
  case class ClientBalance(balance: Currency, stocks: Map[StockSymbol, Long])

  case class SubscribeForClientBalanceChange(listener: ActorRef, clientCodes: ClientCode*)
  case class UnSubscribeForClientBalanceChange(listener: ActorRef, clientCodes: ClientCode*)
}
