package com.nebtrx.tagless


import cats._
import cats.implicits._
//import cats.instances.string._
//import cats.instances.option._
//import cats.instances.all._
//import cats.syntax.semigroup._
//import cats.syntax.all._



object Main extends App {
  trait KVStore[F[_]] {
    def get(key: String): F[Option[String]]
    def put(key: String, a: String): F[Unit]
  }

  def program[M[_]: FlatMap, F[_]](a: String)(K: KVStore[M])(implicit P: Parallel[M, F]) =
    for {
      _ <- K.put("A", a)
      x <- (K.get("B"), K.get("C")).parMapN(_ |+| _)
      _ <- K.put("X", x.getOrElse("-"))
    } yield x

  println("Hello " |+| "Cats!")
}
