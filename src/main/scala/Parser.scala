package com.agilogy.wapl

type Parser[A] = (String, Int) => Either[ParseError, A]

implicit class ParserOps[A](self: Parser[A]):
  def map[B](f: A => B): Parser[B] = (s, position) =>
    self(s, position).map(f)

implicit class IntParserOps(self: Parser[Int]):
  infix def sequence(other: Parser[Int]): Parser[Int] = (s, position) =>
    for
      ia <- self(s, position)
      ib <- other(s, ia)
    yield ib
  def **(other: Parser[Int]): Parser[Int] = sequence(other)