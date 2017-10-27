package actors

import actors.OrderBook.Acknowledged
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import currency.Currency
import domain._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
  * Created by dm.pavlov@inbox.ru on 27.10.17.
  */
class StockExchangeSpec extends TestKit(ActorSystem("StockExchangeSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "StockExchange" must {
    "return correct client balances after registration" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.GetClientBalance(ClientCode("C1"), ClientCode("C2"))

      val messages = receiveN(3).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balance = messages(0).asInstanceOf[StockExchange.ClientBalances]

      balance should  be(StockExchange.ClientBalances(
        Map(
          ClientCode("C1") -> StockExchange.ClientBalance(Currency(1000, "USD"), Map("A" -> 10, "B" -> 5)),
          ClientCode("C2") -> StockExchange.ClientBalance(Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))
        )
      ))
    }

    "C1 sell 5 stocks of type A and C2 buy of 5 stocks of type A" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.SubscribeForClientBalanceChange(self, ClientCode("C1"), ClientCode("C2"))

      stockExchange ! Order(ClientCode("C1"), Sell, "A", Currency(10, "USD"), 5)
      stockExchange ! Order(ClientCode("C2"), Buy,  "A", Currency(10, "USD"), 5)

      val messages = receiveN(6).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balances = messages.map(_.asInstanceOf[StockExchangeClient.ClientBalance])

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C1"), Currency(1050, "USD"), Map("A" -> 5, "B" -> 5))
      )

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C2"), Currency(950, "USD"), Map("A" -> 10, "B" -> 10))
      )

    }

    "C1 sell 6 stocks of type A and C2 buy of 3 stocks of type A" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.SubscribeForClientBalanceChange(self, ClientCode("C1"), ClientCode("C2"))

      stockExchange ! Order(ClientCode("C1"), Sell, "A", Currency(12, "USD"), 3)
      stockExchange ! Order(ClientCode("C1"), Sell, "A", Currency(8, "USD"), 3)
      stockExchange ! Order(ClientCode("C2"), Buy,  "A", Currency(10, "USD"), 3)

      val messages = receiveN(7).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balances = messages.map(_.asInstanceOf[StockExchangeClient.ClientBalance])

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C1"), Currency(1024, "USD"), Map("A" -> 7, "B" -> 5))
      )

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C2"), Currency(976, "USD"), Map("A" -> 8, "B" -> 10))
      )

    }

    "C1 sell 7 stocks of type A and C2 buy of 3 stocks of type A" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.SubscribeForClientBalanceChange(self, ClientCode("C1"), ClientCode("C2"))

      stockExchange ! Order(ClientCode("C1"), Sell, "A", Currency(12, "USD"), 3)
      stockExchange ! Order(ClientCode("C1"), Sell, "A", Currency(8, "USD"), 4)
      stockExchange ! Order(ClientCode("C2"), Buy,  "A", Currency(10, "USD"), 3)

      val messages = receiveN(7).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balances = messages.map(_.asInstanceOf[StockExchangeClient.ClientBalance])

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C1"), Currency(1024, "USD"), Map("A" -> 7, "B" -> 5))
      )

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C2"), Currency(976, "USD"), Map("A" -> 8, "B" -> 10))
      )

    }


    "C1 buy 6 stocks of type A and C2 cell of 3 stocks of type A" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.SubscribeForClientBalanceChange(self, ClientCode("C1"), ClientCode("C2"))

      stockExchange ! Order(ClientCode("C1"), Buy, "A", Currency(12, "USD"), 3)
      stockExchange ! Order(ClientCode("C1"), Buy, "A", Currency(8, "USD"), 3)
      stockExchange ! Order(ClientCode("C2"), Sell,  "A", Currency(10, "USD"), 3)

      val messages = receiveN(7).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balances = messages.map(_.asInstanceOf[StockExchangeClient.ClientBalance])

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C1"), Currency(970, "USD"), Map("A" -> 13, "B" -> 5))
      )

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C2"), Currency(1030, "USD"), Map("A" -> 2, "B" -> 10))
      )

    }

    "C1 buy 7 stocks of type A and C2 cell of 3 stocks of type A" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.SubscribeForClientBalanceChange(self, ClientCode("C1"), ClientCode("C2"))

      stockExchange ! Order(ClientCode("C1"), Buy, "A", Currency(12, "USD"), 3)
      stockExchange ! Order(ClientCode("C1"), Buy, "A", Currency(8, "USD"), 3)
      stockExchange ! Order(ClientCode("C2"), Sell,  "A", Currency(10, "USD"), 4)

      val messages = receiveN(7).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balances = messages.map(_.asInstanceOf[StockExchangeClient.ClientBalance])

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C1"), Currency(970, "USD"), Map("A" -> 13, "B" -> 5))
      )

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C2"), Currency(1030, "USD"), Map("A" -> 2, "B" -> 10))
      )

    }



    "C1 sell 5 stocks of type A and buy 3 stocks of type B and C2 buy of 5 stocks of type A and sell 7 stocks of type B" in {
      val stockExchange = system.actorOf(Props(new StockExchange))

      stockExchange ! Client(ClientCode("C1"), Currency(1000, "USD"), Map("A" -> 10, "B" -> 5))
      stockExchange ! Client(ClientCode("C2"), Currency(1000, "USD"), Map("A" -> 5, "B" -> 10))

      stockExchange ! StockExchange.SubscribeForClientBalanceChange(self, ClientCode("C1"), ClientCode("C2"))

      stockExchange ! Order(ClientCode("C1"), Sell, "A", Currency(10, "USD"), 5)
      stockExchange ! Order(ClientCode("C2"), Buy,  "A", Currency(10, "USD"), 5)
      stockExchange ! Order(ClientCode("C1"), Buy, "B", Currency(10, "USD"), 3)
      stockExchange ! Order(ClientCode("C2"), Sell,  "B", Currency(10, "USD"), 7)

      val messages = receiveN(10).filter {
        case _: ActorRef => false
        case _: Acknowledged => false
        case _ => true
      }

      val balances = messages.map(_.asInstanceOf[StockExchangeClient.ClientBalance])

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C1"), Currency(1020, "USD"), Map("A" -> 5, "B" -> 8))
      )

      balances should contain(
        StockExchangeClient.ClientBalance(ClientCode("C2"), Currency(980, "USD"), Map("A" -> 10, "B" -> 7))
      )

    }
  }

}
