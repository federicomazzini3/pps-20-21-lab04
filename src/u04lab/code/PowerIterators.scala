package u04lab.code

import Optionals._
import Lists._
import Streams._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]

  def allSoFar(): List[A]

  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]

  def fromList[A](list: List[A])

  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  private case class PowerIteratorImpl[A](private var stream: Stream[A]) extends PowerIterator[A] {

    private var list: List[A] = List.Nil()

    override def next(): Option[A] = {
      val next = Stream.first(stream)

      next match {
        case Option.Some(a) => {
          list = List.append(list, List.Cons(a, List.Nil()))
          stream = Stream.tail(stream)
          next
        }
      }
      next
    }

    override def allSoFar(): List[A] = list

    override def reversed(): PowerIterator[A] = {
      val reverseList: List[A] = List.reverse(list)
      new PowerIteratorImpl[A](Stream.fromList(reverseList))
    }
  }

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] =
    PowerIteratorImpl(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): Unit = PowerIteratorImpl(Stream.fromList(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIteratorImpl(Stream.take(Stream.generate(Random.nextBoolean))(size))
}
