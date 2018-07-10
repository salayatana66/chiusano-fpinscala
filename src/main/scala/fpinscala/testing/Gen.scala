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

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  // Ex 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // Ex 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen(State(RNG.nonNegativeInt).map( n=> n % (stopExclusive-start)+start))

}

case class Gen[A](sample: State[RNG,A]) {
  //trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

