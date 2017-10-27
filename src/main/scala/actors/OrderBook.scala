package actors

import java.util.PriorityQueue

import akka.actor.Actor
import currency.Currency
import domain.{Buy, Order, Sell}
import utils.Comparators

/**
  * Created by dpavlov on 26/10/2017.
  */
class OrderBook extends Actor {

  import OrderBook._

  val initialQueueCapacity = 1000

  val bidsQueue = new PriorityQueue[Order](initialQueueCapacity, Comparators.bidComparator)
  val offersQueue = new PriorityQueue[Order](initialQueueCapacity, Comparators.offerComparator)

  var volume: Long = 0

  var bestBid: Option[Order] = None
  var bestOffer: Option[Order] = None

  override def receive: Receive = {
    case order: Order => {
      val ts = System.currentTimeMillis()
      val (isOk, message) = validateOrder(order)
      if (!isOk) {
        sender() ! Rejected(order, message, ts)
      } else {
        sender() ! Acknowledged(order, ts)
        processOrder(order)
      }
    }
    case GetOrderBookState => {
      sender() ! OrderBookState(bidsQueue.size(), offersQueue.size(), volume, bestBid, bestOffer)
    }

  }

  private[this] def validateOrder(order: Order): (Boolean, String) = {
    if (order.qty < 0) (false, "qty < 0") else (true, "")
  }

  private[this] def processOrder(order: Order): Unit = {
    val (ordersQueue, oppositeQueue) = order.operation match {
      case Sell => (offersQueue, bidsQueue)
      case Buy => (bidsQueue, offersQueue)
    }
    if (oppositeQueue.size() > 0 && isOrderExecutable(order, oppositeQueue.peek)) {
      matchOrder(order, ordersQueue, oppositeQueue)
    }
    else {
      ordersQueue.add(order)
      updateBestBidAndOffer()
    }
  }

  private[this] def matchOrder(order: Order, orderQueue: PriorityQueue[Order], oppositeQueue: PriorityQueue[Order]): Unit = {
    val ts = System.currentTimeMillis()
    val oppositeOrder = oppositeQueue.poll
    if (order.qty < oppositeOrder.qty) {
      val updatedOppositeOrder = oppositeOrder.copy(qty = oppositeOrder.qty - order.qty)
      oppositeQueue.add(updatedOppositeOrder)

      this.volume += order.qty
      order.operation match {
        case Sell => context.parent ! Filled(order.price, order.qty, Array(order, oppositeOrder.copy(price = order.price, qty = order.qty)), ts)
        case Buy => context.parent ! Filled(updatedOppositeOrder.price, order.qty, Array(order.copy(price = oppositeOrder.price), oppositeOrder.copy(qty = order.qty)), ts)
      }

    } else if (order.qty > oppositeOrder.qty) {
      val reducedQty = order.qty - oppositeOrder.qty
      val updatedOrder = order.copy(qty = reducedQty)

      this.volume += oppositeOrder.qty
      order.operation match {
        case Sell => context.parent ! Filled(updatedOrder.price, oppositeOrder.qty, Array(updatedOrder.copy(qty = oppositeOrder.qty), oppositeOrder.copy(price = order.price)), ts)
        case Buy => context.parent ! Filled(oppositeOrder.price, oppositeOrder.qty, Array(updatedOrder.copy(qty = oppositeOrder.qty, price = oppositeOrder.price), oppositeOrder), ts)
      }
      processOrder(updatedOrder)
    } else {
      this.volume += order.qty
      order.operation match {
        case Sell => context.parent ! Filled(order.price, order.qty, Array(order, oppositeOrder.copy(price = order.price)), ts)
        case Buy => context.parent ! Filled(oppositeOrder.price, order.qty, Array(order.copy(price = oppositeOrder.price), oppositeOrder), ts)
      }
    }
    updateBestBidAndOffer()
  }

  private[this] def isOrderExecutable(order: Order, oppositeOrder: Order): Boolean = order.operation match {
    case Sell => order.price <= oppositeOrder.price
    case Buy => order.price >= oppositeOrder.price
  }

  private def updateBestBidAndOffer() = {
    val bidHead = Option(bidsQueue.peek)
    val offerHead = Option(offersQueue.peek)
    if(bidHead.isDefined && bidHead != bestBid) bestBid = bidHead
    if(offerHead.isDefined && offerHead != bestOffer) bestOffer = offerHead
  }

}

object OrderBook {
  sealed trait OrderBookResponse
  case class Filled(price: Currency, qty: Long, order: Array[Order], ts: Long) extends OrderBookResponse
  case class Acknowledged(order: Order, ts: Long) extends OrderBookResponse
  case class Rejected(order: Order, error: String, ts: Long) extends OrderBookResponse
  case class Canceled(order: Order, reason: String) extends OrderBookResponse

  case object GetOrderBookState
  case class OrderBookState(bidsCount: Long = 0, offerCount: Long = 0, volume: Long = 0, bestBid: Option[Order] = None, bestOffer: Option[Order] = None)
}
