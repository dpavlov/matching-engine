package utils

import java.util.Comparator

import domain.Order

/**
  * Created by dpavlov on 26/10/2017.
  */
object Comparators {

  private val offerOrdering = Ordering.by { order: Order => order.price }
  private val bidOrdering = offerOrdering.reverse

  val bidComparator = new Comparator[Order]{
    def compare(o1: Order, o2: Order): Int = bidOrdering.compare(o1, o2)
  }
  val offerComparator = new Comparator[Order]{
    def compare(o1: Order, o2: Order): Int = offerOrdering.compare(o1, o2)
  }
}
