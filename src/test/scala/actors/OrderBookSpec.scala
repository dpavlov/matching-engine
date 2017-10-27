package actors

import actors.OrderBook.Acknowledged
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import currency.Currency
import domain.{Buy, ClientCode, Order, Sell}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

/**
  * Created by dm.pavlov@inbox.ru on 27.10.17.
  */
class OrderBookSpec extends TestKit(ActorSystem("OrderBookSpec")) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "OrderBook" must {
    "be created with clean state" in {
      val orderBook = system.actorOf(Props(new OrderBook))

      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState())
    }

    "return correct bids count and best bid" in {
      val orderBook = system.actorOf(Props(new OrderBook))

      val _ = sendOrder(orderBook, Order(ClientCode("C1"), Buy, "A", Currency(10, "USD"), 5))
      val o2 = sendOrder(orderBook, Order(ClientCode("C2"), Buy, "A", Currency(12, "USD"), 10))
      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState(bidsCount =  2, bestBid = Some(o2)))
    }

    "return correct offers count and best offer" in {
      val orderBook = system.actorOf(Props(new OrderBook))

      val _ = sendOrder(orderBook, Order(ClientCode("C1"), Sell, "A", Currency(12, "USD"), 5))
      val o2 = sendOrder(orderBook, Order(ClientCode("C2"), Sell, "A", Currency(10, "USD"), 10))
      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState(offerCount =  2, bestOffer = Some(o2)))
    }

    "execute order if bid and offer are matched" in {
      val orderBook = system.actorOf(Props(new OrderBook))

      val o1 = sendOrder(orderBook, Order(ClientCode("C1"), Sell, "A", Currency(10, "USD"), 5))
      val _ = sendOrder(orderBook, Order(ClientCode("C2"), Buy,  "A", Currency(10, "USD"), 5))
      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState(volume = 5, bestOffer = Some(o1)))
    }

    "execute order if offer and bid are matched" in {
      val orderBook = system.actorOf(Props(new OrderBook))

      val o1 = sendOrder(orderBook, Order(ClientCode("C1"), Buy, "A", Currency(10, "USD"), 5))
      val _ = sendOrder(orderBook, Order(ClientCode("C2"), Sell,  "A", Currency(10, "USD"), 5))
      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState(volume = 5, bestBid = Some(o1)))
    }

    "execute order if bid and offer are partial matched" in {
      val orderBook = system.actorOf(Props(new OrderBook))
      val o1 = sendOrder(orderBook, Order(ClientCode("C1"), Sell, "A", Currency(10, "USD"), 10))
      val _ = sendOrder(orderBook, Order(ClientCode("C2"), Buy,  "A", Currency(10, "USD"), 5))
      orderBook ! OrderBook.GetOrderBookState

      val expectedBestOffer = o1.copy(qty = 5)
      expectMsg(OrderBook.OrderBookState(offerCount = 1, volume = 5, bestOffer = Some(expectedBestOffer)))
    }

    "execute order if offer and bid are partial matched" in {
      val orderBook = system.actorOf(Props(new OrderBook))
      val o1 = sendOrder(orderBook, Order(ClientCode("C1"), Buy, "A", Currency(10, "USD"), 10))
      val _ = sendOrder(orderBook, Order(ClientCode("C2"), Sell,  "A", Currency(10, "USD"), 5))
      orderBook ! OrderBook.GetOrderBookState

      val expectedBestBid = o1.copy(qty = 5)
      expectMsg(OrderBook.OrderBookState(bidsCount = 1, volume = 5, bestBid = Some(expectedBestBid)))
    }

    "execute scenario with following orders list buy, buy, sell" in {
      val orderBook = system.actorOf(Props(new OrderBook))
      val o1 = sendOrder(orderBook, Order(ClientCode("C1"), Buy, "A", Currency(10, "USD"), 10))
      val o2 = sendOrder(orderBook, Order(ClientCode("C2"), Buy, "A", Currency(10, "USD"), 10))
      val o3 = sendOrder(orderBook, Order(ClientCode("C3"), Sell,  "A", Currency(10, "USD"), 20))
      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState(volume = 20, bestBid = Some(o1)))
    }

    "execute scenario with following orders list sell, buy, buy, sell" in {
      val orderBook = system.actorOf(Props(new OrderBook))

      val o1 = sendOrder(orderBook, Order(ClientCode("C1"), Sell,  "A", Currency(10, "USD"), 10))
      val o2 = sendOrder(orderBook, Order(ClientCode("C2"), Buy, "A", Currency(10, "USD"), 10))
      val o3 = sendOrder(orderBook, Order(ClientCode("C3"), Buy, "A", Currency(10, "USD"), 10))
      val o4 = sendOrder(orderBook, Order(ClientCode("C4"), Sell,  "A", Currency(10, "USD"), 10))

      orderBook ! OrderBook.GetOrderBookState

      expectMsg(OrderBook.OrderBookState(volume = 20, bestBid = Some(o3), bestOffer = Some(o1)))
    }
  }

  def sendOrder(orderBook: ActorRef, order: Order): Order = {
    orderBook ! order
    expectMsgClass(classOf[Acknowledged])
    order
  }
}
