package fpinscala.testing

//import fpinscala.laziness.Stream
//import fpinscala.parallelism._
//import fpinscala.parallelism.Par.Par

import State._
import RNG._
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: TestCases => Result) {
// trait Prop {
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

