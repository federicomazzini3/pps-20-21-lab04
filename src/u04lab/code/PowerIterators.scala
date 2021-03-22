package u04lab.code

import Optionals._
import Lists._
import Streams._
import u04lab.code.Streams.Stream.{empty, tail}

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

/*
/*
  con companion object, un object con stesso nome del trait che va a rappresentare
 */
object PowerIteratorsFactory {

  case class PowerIteratorsFactoryImpl() extends PowerIteratorsFactory {

    private case class PowerIteratorImpl[A](streams: Stream[A]) extends PowerIterator[A] {

      private var list = List.nil[Int]

      override def next(): Option[A] = {
        ???
      }

      override def allSoFar(): List[A] = ???

      override def reversed(): PowerIterator[A] = ???
    }

    override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] =
      PowerIteratorImpl(Stream.cons(start, Stream.generate(successive)))

    override def fromList[A](list: List[A]): Unit = ???

    override def randomBooleans(size: Int): PowerIterator[Boolean] = ???


  }
 */
