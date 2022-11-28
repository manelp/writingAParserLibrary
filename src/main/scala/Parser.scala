package com.agilogy.wapl

import scala.annotation.tailrec

type Parser[A] = (String, Int) => Either[ParseError, (Int, A)]

def string(token: String): Parser[Unit] = (s, position) =>
  if (s.startsWith(token, position)) Right((position + token.length, ())) else Left(ParseError(s, position, List(token)))

def many[A](parser: Parser[A]): Parser[List[A]] = {
  @tailrec
  def go(acc: List[A], parser: Parser[A], s: String, position: Int): (Int, List[A])= parser(s, position) match {
    case Right((newPos, value)) => go(acc :+ value, parser, s, newPos)
    case Left(_) => (position, acc)
  }
  (s, position) => Right(go(List.empty, parser, s, position))
}

extension[A](self: Parser[A])
  def map[B](f: A => B): Parser[B] = (s, position) =>
    self(s, position).map((pos, a) => (pos,f(a)))
  def as[B](v: B): Parser[B] = map(_ => v)
  def |(other: Parser[A]): Parser[A] = (s, position) => self(s, position) match
    case ok @ Right(_) => ok
    case Left(error) => other(s, position) match
      case ok @ Right(_) => ok
      case Left(secondError) => Left(ParseError(s, position, error.expected ++ secondError.expected))

  def **[B](other: Parser[B]): Parser[(A,B)] = (s, position) =>
    for
      ia <- self(s, position)
      ib <- other(s, ia._1)
    yield (ib._1, (ia._2, ib._2))

  def *>[B](other: Parser[B]): Parser[B] = (self ** other).map(_._2)
  def <*[B](other: Parser[B]): Parser[A] = (self ** other).map(_._1)

  def * : Parser[List[A]] = many(self)