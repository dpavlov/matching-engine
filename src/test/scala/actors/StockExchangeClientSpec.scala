package actors

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import currency.Currency
import domain._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
  * Created by dm.pavlov@inbox.ru on 27.10.17.
  */
class StockExchangeClientSpec extends TestKit(ActorSystem("StockExchangeClientSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "StockExchangeClient" must {
    "be created with initial state" in {

      val clientInfo = Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      val client = system.actorOf(Props(new StockExchangeClient(clientInfo)))

      client ! StockExchangeClient.GetClientBalance

      expectMsg(StockExchangeClient.ClientBalance(clientInfo.code, clientInfo.balance, clientInfo.stocks))
    }

    "update balance according to executed sell order" in {

      val clientInfo = Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      val client = system.actorOf(Props(new StockExchangeClient(clientInfo)))

      val totalStocksToSell = 5L
      val stockPrice = Currency(10, "USD")

      client ! Order(ClientCode("C1"), Sell, "A", stockPrice, totalStocksToSell)

      client ! StockExchangeClient.GetClientBalance

      val expectedStocks = clientInfo.stocks.updated("A", clientInfo.stocks.getOrElse("A", 0L) - totalStocksToSell)
      val expectedBalance = clientInfo.balance + (stockPrice * totalStocksToSell)

      expectMsg(StockExchangeClient.ClientBalance(clientInfo.code, expectedBalance, expectedStocks))
    }

    "update balance according to executed buy order" in {

      val clientInfo = Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      val client = system.actorOf(Props(new StockExchangeClient(clientInfo)))

      val totalStocksToBuy = 5L
      val stockPrice = Currency(10, "USD")

      client ! Order(ClientCode("C1"), Buy, "A", stockPrice, totalStocksToBuy)

      client ! StockExchangeClient.GetClientBalance

      val expectedStocks = clientInfo.stocks.updated("A", clientInfo.stocks.getOrElse("A", 0L) + totalStocksToBuy)
      val expectedBalance = clientInfo.balance - (stockPrice * totalStocksToBuy)

      expectMsg(StockExchangeClient.ClientBalance(clientInfo.code, expectedBalance, expectedStocks))
    }
  }

}
