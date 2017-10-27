import java.io.{File, PrintWriter}

import actors.{OrderBook, StockExchange}
import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import services.{ClientFactory, OrderFactory}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Created by dpavlov on 25/10/2017.
  */
object MainApp extends App {

  implicit val timeout: Timeout = Timeout(100 milliseconds)
  implicit val system = ActorSystem("StockExchangeSystem")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val stockExchange = system.actorOf(Props(new StockExchange), "StockExchange")

  val clientsInputFile = getClass.getResourceAsStream("clients.txt")
  val ordersInputFile = getClass.getResourceAsStream("orders.txt")

  val clientsFuture = Future.successful(scala.io.Source.fromInputStream(clientsInputFile)
    .getLines()
    .flatMap(ClientFactory.fromTabSeparatedLine)
    .map(client => (stockExchange ? client).mapTo[ActorRef]).toList)

  val result = clientsFuture.flatMap(_ => {

    val ordersSource: Source[String, NotUsed] = Source.fromIterator(() => scala.io.Source.fromInputStream(ordersInputFile).getLines())

    val orders = ordersSource.map(OrderFactory.fromTabSeparatedLine).filter(_.isDefined).map(_.get)

    orders.mapAsync(10)(order => (stockExchange ? order).mapTo[OrderBook.OrderBookResponse]).runWith(Sink.ignore)

  } ).flatMap(_ => {
    (stockExchange ? StockExchange.GetClientBalances).mapTo[StockExchange.ClientBalances]
  })

  result.onComplete {
    case Success(balances) => {
      val writer = new PrintWriter(new File("result.txt"))
      balances.clients.keys.toList.sortBy(_.code).foreach { case clientCode => {
        val balance = balances.clients(clientCode)
        val a = balance.stocks.getOrElse("A", 0L)
        val b = balance.stocks.getOrElse("B", 0L)
        val c = balance.stocks.getOrElse("C", 0L)
        val d = balance.stocks.getOrElse("D", 0L)
        writer.append(s"${clientCode.code}\t${balance.balance.amount}\t$a\t$b\t$c\t$d\n")
      } }
      writer.close()
      system.terminate()
    }
    case Failure(ex) =>  {
      ex.printStackTrace()
      system.terminate()
    }
  }

}
