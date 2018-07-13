package fpinscala.testing

//import fpinscala.laziness.Stream
//import fpinscala.parallelism._
//import fpinscala.parallelism.Par.Par

import State._
import RNG._
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}
import Stream._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases,RNG) => Result) {
  // to add a tagging functionality
  def addTag(s : String) = Prop {
    (n,rng) => run(n,rng) match {
      case Falsified(msg,i) => Falsified(s+"\n"+msg,i)
      case Passed => Passed
    }
  }

  // Ex 8.9
  // lazy and
  def &&(p: Prop) = Prop {
    (n,rng) =>run(n,rng) match {
        case r1 : Falsified => r1
        case _ => p.run(n,rng) 
      }
  }
  
  // lazy or => defaults to r1 failure in case of failure
  def ||(p: Prop) = Prop {
    (n,rng) => run(n,rng) match {
        case Passed => Passed
        case Falsified(msg,j) => p.run(n,rng) match {
          case Passed => Passed
          case Falsified(msg2,i) => Falsified(msg+"\n"+msg2,if(i<j) i else j)
        }
    }
  }
        
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
    successes: SuccessCount) extends Result {
    def isFalsified = true
  }


  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString , i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }

    }.find(_.isFalsified).getOrElse(Passed)
      
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object Gen {
  // Ex 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = choose(0,2).map(_ == 0)
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  // Ex 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen(State(RNG.nonNegativeInt).map( n=> n % (stopExclusive-start)+start))

  // Ex 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if(_) g1 else g2)

  // Ex 8.8
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = Gen(State(RNG.double)).
    flatMap( x => if(x > g1._2/(g1._2+g2._2)) g2._1 else g1._1)
}

case class Gen[A](sample: State[RNG,A]) {
  //trait Gen[A] {

  // Ex 8.6
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.
    flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_,this))
}

trait SGen[+A] {

}

