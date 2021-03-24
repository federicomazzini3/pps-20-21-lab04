package u04lab.code

import Optionals._
import Lists._
import Lists.List._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5,_+2); // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next());
    assertEquals(Option.of(7), pi.next());
    assertEquals(Option.of(9), pi.next());
    assertEquals(Option.of(11), pi.next());
    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11,List.Nil())))), pi.allSoFar()); // elementi già prodotti
    for (i <- 0 until 10) {
      pi.next(); // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()); // sono arrivato a 33
  }

  @Test
  def testRandomBooleans(): Unit ={
    val pi = factory.randomBooleans(5);
    val b1 = pi.next(); //valori booleani random
    val b2 = pi.next();
    val b3 = pi.next();
    val b4 = pi.next();
    val b5 = pi.next();
    val b6 = pi.next(); //sarà un optional vuoto
    println(b1, b2, b3, b4, b5, b6)
  }

  @Test
  def fromList(): Unit ={
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
    val pi = factory.fromList(list)
    assertEquals(Option.of(1), pi.next())
    assertEquals(Option.of(2), pi.next())
    assertEquals(Option.of(3), pi.next())
    assertEquals(Option.of(4), pi.next())
  }

  @Test
  def reversed(): Unit ={
    val pi = factory.incremental(1, _ + 1)
    assertEquals(Option.of(1), pi.next())
    assertEquals(Option.of(2), pi.next())
    val piReverse = pi.reversed()
    assertEquals(Option.of(2), piReverse.next())
    assertEquals(Option.of(1), piReverse.next())
  }
}

