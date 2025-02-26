package u04lab.code

import Optionals._
import Lists._
import Streams._
import u04lab.code.Optionals.Option.None
import u04lab.code.PowerIterator.PowerIteratorImpl

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]

  def allSoFar(): List[A]

  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]

  def fromList[A](list: List[A]): PowerIterator[A]

  def randomBooleans(size: Int): PowerIterator[Boolean]
}

/*
 * companion object per l'implementazione di PowerIterator
 */
object PowerIterator{

  def apply[A](stream: Stream[A]): PowerIterator[A] = PowerIteratorImpl(stream)

  private case class PowerIteratorImpl[A](private var stream:Stream[A]) extends PowerIterator[A]{
    private var list: List[A] = List.Nil()

    override def next(): Option[A] = {
      val next = Stream.first(stream)

      next match {
        case Option.Some(a) => {
          list = List.append(list, List.Cons(a, List.Nil()))
          stream = Stream.tail(stream)
          next
        }
        case Option.None() => next
      }
    }

    override def allSoFar(): List[A] = list

    override def reversed(): PowerIterator[A] = {
      val reverseList: List[A] = List.reverse(list)
      new PowerIteratorImpl[A](Stream.fromList(reverseList))
    }
  }
}

/*
 * implementazione PowerIteratorFactory come class 
 */
class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] =
    PowerIterator(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = PowerIterator(Stream.fromList(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIterator(Stream.take(Stream.generate(Random.nextBoolean))(size))
}
