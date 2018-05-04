package com.nebtrx.tagless

import cats._
import cats.data.Const
import cats.implicits._
////import cats.instances.option._
//import cats.instances.string._
////import cats.syntax.parallel._
////import cats.syntax.semigroup._
//import cats.Apply
////import cats.syntax.all._


object Simple extends App {
  trait KVStore[F[_]] {
    def get(key: String): F[Option[String]]
    def put(key: String, a: String): F[Unit]
  }

  def program[F[_]: Apply](F: KVStore[F]): F[List[String]] =
    (F.get("Cats"), F.get("Dogs"), F.put("Mice", "42"), F.get("Cats"))
      .mapN((f, s, _, t) => List(f, s, t).flatten)

  def optimizedProgram[F[_]: Applicative](F: KVStore[F]): F[List[String]] = {
    val (gets, puts) = program(analysisInterpreter).getConst

    puts.toList.traverse { case (k, v) => F.put(k, v) }  *> gets.toList.traverse(F.get).map(_.flatten)
  }

  val analysisInterpreter: KVStore[Const[(Set[String], Map[String, String]), ?]] =
    new KVStore[Const[(Set[String], Map[String, String]), ?]] {
      def get(key: String) = Const((Set(key), Map.empty))
      def put(key: String, a: String) = Const((Set.empty, Map(key -> a)))
    }

  val result = program(analysisInterpreter).getConst

  println(s"PROGRAM RESULT: $result")
}
