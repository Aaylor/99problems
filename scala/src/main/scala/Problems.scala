package org.problems

/* Problem 001 */
object Problem001 {

  def last[A](l : List[A]) : A = l match {
    case Nil       => throw new NoSuchElementException
    case x :: Nil  => x
    case _ :: tail => last(tail)
  }

}


/* Problem 002 */
object Problem002 {

  def lastButOne[A](l : List[A]) : A = l match {
    case x :: _ :: Nil => x
    case _ :: tail     => lastButOne(tail)
    case _             => throw new NoSuchElementException
  }

}


/* Problem 003 */
object Problem003 {

  def nth[A](l : List[A], n : Int) : A = l match {
    case x :: _ if n == 0 => x
    case _ :: tail        => nth(tail, n - 1)
    case _                => throw new NoSuchElementException
  }

}


/* Problem 004 */
object Problem004 {

  def length[A](l : List[A]) : Int = l match {
    case Nil => 0
    case _ :: tail => 1 + length(tail)
  }

}


/* Problem 005 */
object Problem005 {

  def rev[A](l : List[A]) = {
    def aux[A](acc : List[A], l : List[A]) : List[A] = l match {
      case Nil => acc
      case x :: tail => aux(x :: acc, tail)
    }
    aux(Nil, l)
  }

}


/* Problem 006 */
object Problem006 {

  def palindrome[A](l : List[A]) = {
    l == Problem005.rev(l)
  }

}


/* Problem 007 */
object Problem007 {

  def flatten(l : List[Any]) : List[Any] = l match {
    case Nil => Nil
    case (x : List[_]) :: tail => flatten(x) ::: flatten(tail)
    case x :: tail => x :: flatten(tail)
  }

}


/* Problem 008 */
object Problem008 {

  def compress[A](l : List[A]) : List[A] = {
    def aux[A](acc : List[A], l : List[A]) : List[A] =
      l match {
        case Nil         => acc.reverse
        case (x :: tail) => aux(x :: acc, tail dropWhile (_ == x))
      }
    aux(Nil, l)
  }

}


/* Problem 009 */
object Problem009 {

  def pack[A](l : List[A]) : List[List[A]] = l match {
    case Nil => Nil
    case x :: tail =>
      val (xs, tail) = l span (_ == x)
      xs :: pack(tail)
  }

}


/* Problem 010 */
object Problem010 {

  def dropWhileNb[A](l : List[A])(p : A => Boolean) = {
    def aux(count : Int, l : List[A]) : Tuple2[Int,List[A]] = l match {
      case Nil => (count, Nil)
      case x :: _ if !p(x) => (count, l)
      case x :: tail => aux(count + 1, tail)
    }
    aux(0, l)
  }

  def pack[A](l : List[A]) : List[(Int, A)] = l match {
    case Nil => Nil
    case x :: tail =>
      val (count, tail) = dropWhileNb(l)(_ == x)
      (count, x) :: pack(tail)
  }

}


/* Problem 011 */
object Problem011 {

  def pack(l : List[Any]) : List[Any] = l match {
    case Nil => Nil
    case x :: tail =>
      val (count, tail) = Problem010.dropWhileNb(l)(_ == x)
      (if (count == 1) x else (count, x)) :: pack(tail)
  }
}


/* Problem 012 */
object Problem012 {

  def decode[A](l : List[Tuple2[Int, A]]) : List[A] = l match {
    case Nil => Nil
    case (count, v) :: tail if count == 1 => v :: decode(tail)
    case (count, v) :: tail => v :: decode((count - 1, v) :: tail)
  }

}


/* Problem 013 */
object Problem013 {

  def encodeDirect[A](l : List[A]) : List[Tuple2[Int, A]] = l match {
    case Nil => Nil
    case v :: tail =>
      encodeDirect(tail) match {
        case Nil => List((1, v))
        case (code, value) :: xs if value == v => (code + 1, value) :: xs
        case l => (1, v) :: l
      }
      // val encodedList = encodeDirect(tail)

  }

}


/* Problem 014 */
object Problem014 {

  def duplicate[A](l : List[A]) : List[A] = l match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

}


/* Problem 015 */
object Problem015 {

  def duplicateN[A](n : Int, l : List[A]) : List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      def f(m : Int) : List[A] =
        if (m == 0) duplicateN(n, xs) else x :: f(m - 1)
      f(n)
  }

}


/* Problem 016 */
object Problem016 {

  def drop[A](n : Int, l : List[A]) : List[A] = {
    def aux(current : Int, l : List[A]) : List[A] = l match {
      case Nil => Nil
      case _ :: xs if current == 1 => aux(n, xs)
      case x :: xs => x :: aux(current - 1, xs)
    }
    aux(n, l)
  }

}


/* Problem 017 */
object Problem017 {

  def split[A](n : Int, l : List[A]) = {
    def aux(current : Int, acc : List[A], l : List[A])
        : Tuple2[List[A], List[A]] = l match {
      case Nil => (acc.reverse, Nil)
      case l if current == 0 => (acc.reverse, l)
      case x :: xs => aux(current - 1, x :: acc, xs)
    }
    aux(n, Nil, l)
  }

}


/* Problem 018 */
object Problem018 {

  def slice[A](min : Int, max : Int, l : List[A]) : List[A] = {
    for ((x, cnt) <- l.zipWithIndex if (cnt >= min && cnt < max)) yield x
  }

}


/* Problem 019 */
object Problem019 {

  def rotate[A](n : Int, l : List[A]) : List[A] = {
    val where = if (n < 0) l.length + n else n
    val (head, tail) = l splitAt where
    tail ::: head
  }

}


/* Problem 020 */
object Problem020 {

  def removeAt[A](where : Int, l : List[A]) : Tuple2[List[A], A] = l match {
    case Nil => throw new NoSuchElementException
    case x :: tail if where == 0 => (tail, x)
    case x :: tail =>
      val (l, ret) = removeAt(where - 1, tail)
      (x :: l, ret)
  }

}


/* Problem 021 */
object Problem021 {

  def insertAt[A](where : Int, what : A, l : List[A]) : List[A] = l match {
    case Nil if where > 0 => throw new NoSuchElementException
    case Nil => List(what)
    case xs if where == 0 => what :: xs
    case x :: xs => x :: insertAt(where - 1, what, xs)
  }

}


/* Problem 022 */
object Problem022 {

  def range(min : Int, max : Int) : List[Int] =
    if (min > max) Nil else min :: range(min + 1, max)

}


/* Problem 023 */
object Problem023 {

  def randomSelect[A](nb : Int, l : List[A]) = {
    def aux(nbLeft : Int, l : List[A], length : Int,
      generator : java.util.Random) : List[A] = {
      if (nbLeft == 0) Nil
      else {
        val (ls, x) = Problem020.removeAt(generator nextInt length, l)
        x :: aux(nbLeft - 1, ls, length - 1, generator)
      }
    }
    aux(nb, l, l.length, new java.util.Random)
  }

}


/* Problem 024 */
object Problem024 {

  def lotto(nb : Int, max : Int) =
    Problem023.randomSelect(nb, Problem022.range(1, max))

}


/* Problem 025 */
object Problem025 {

  def randomPermut[A](l : List[A]) = {
    Problem023.randomSelect(l.length, l)
  }

}


/* Problem 026 */
object Problem026 {

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }

}


/* Problem 027 */
object Problem027 {

  /* write here */

}


/* Problem 028 */
object Problem028 {

  /* write here */

}


/* Problem 029 */
object Problem029 {

  /* write here */

}


/* Problem 030 */
object Problem030 {

  /* write here */

}


/* Problem 031 */
object Problem031 {

  /* write here */

}


/* Problem 032 */
object Problem032 {

  /* write here */

}


/* Problem 033 */
object Problem033 {

  /* write here */

}


/* Problem 034 */
object Problem034 {

  /* write here */

}


/* Problem 035 */
object Problem035 {

  /* write here */

}


/* Problem 036 */
object Problem036 {

  /* write here */

}


/* Problem 037 */
object Problem037 {

  /* write here */

}


/* Problem 038 */
object Problem038 {

  /* write here */

}


/* Problem 039 */
object Problem039 {

  /* write here */

}


/* Problem 040 */
object Problem040 {

  /* write here */

}


/* Problem 041 */
object Problem041 {

  /* write here */

}


/* Problem 042 */
object Problem042 {

  /* write here */

}


/* Problem 043 */
object Problem043 {

  /* write here */

}


/* Problem 044 */
object Problem044 {

  /* write here */

}


/* Problem 045 */
object Problem045 {

  /* write here */

}


/* Problem 046 */
object Problem046 {

  def not(a : Boolean) = a match {
    case true  => false
    case false => true
  }

  def and(a : Boolean, b : Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(a : Boolean, b : Boolean) = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def nand(a : Boolean, b : Boolean) = not(and(a, b))

  def nor(a : Boolean, b : Boolean) = not(or(a, b))

  def xor(a : Boolean, b : Boolean) = not(equ(a, b))

  def impl(a : Boolean, b : Boolean) = or(not(a), b)

  def equ(a : Boolean, b : Boolean) = or(and(a, b), and(not(a), not(b)))

  def table2(f : (Boolean, Boolean) => Boolean) = {
    println("  A     B   Result")
    for {
      a <- List(false, true);
      b <- List(false, true)
    } {
      printf("%-6b%-6b%-6b\n", a, b, f(a, b))
    }
  }

}


/* Problem 047 */
object Problem047 {

  def not(a : Boolean) = a match {
    case true  => false
    case false => true
  }

  class Logic(a : Boolean) {
    import Logic._

    def and(b : Boolean) = (a, b) match {
      case (true, true) => true
      case _ => false
    }

    def or(b : Boolean) = (a, b) match {
      case (false, false) => false
      case _ => true
    }

    def nand(b : Boolean) = not(a and b)
    def nor(b : Boolean)  = not(a or b)
    def xor(b : Boolean)  = not(a equ b)
    def impl(b : Boolean) = not(a) or b
    def equ(b : Boolean)  = (a and b) or (not(a) and not(b))
  }

  object Logic {
    import scala.language.implicitConversions
    implicit def boolean2Logic(b : Boolean) : Logic = new Logic(b)
  }

  def table2(f : (Boolean, Boolean) => Boolean) = {
    println("  A     B   Result")
    for {
      a <- List(false, true);
      b <- List(false, true)
    } {
      printf("%-6b%-6b%-6b\n", a, b, f(a, b))
    }
  }

}


/* Problem 048 */
object Problem048 {

  /* write here */

}


/* Problem 049 */
object Problem049 {

  /* write here */

}


/* Problem 050 */
object Problem050 {

  /* write here */

}


/* Problem 051 */
object Problem051 {

  /* write here */

}


/* Problem 052 */
object Problem052 {

  /* write here */

}


/* Problem 053 */
object Problem053 {

  /* write here */

}


/* Problem 054 */
object Problem054 {

  /* write here */

}


/* Problem 055 */
object Problem055 {

  /* write here */

}


/* Problem 056 */
object Problem056 {

  /* write here */

}


/* Problem 057 */
object Problem057 {

  /* write here */

}


/* Problem 058 */
object Problem058 {

  /* write here */

}


/* Problem 059 */
object Problem059 {

  /* write here */

}


/* Problem 060 */
object Problem060 {

  /* write here */

}


/* Problem 061 */
object Problem061 {

  /* write here */

}


/* Problem 062 */
object Problem062 {

  /* write here */

}


/* Problem 063 */
object Problem063 {

  /* write here */

}


/* Problem 064 */
object Problem064 {

  /* write here */

}


/* Problem 065 */
object Problem065 {

  /* write here */

}


/* Problem 066 */
object Problem066 {

  /* write here */

}


/* Problem 067 */
object Problem067 {

  /* write here */

}


/* Problem 068 */
object Problem068 {

  /* write here */

}


/* Problem 069 */
object Problem069 {

  /* write here */

}


/* Problem 070 */
object Problem070 {

  /* write here */

}


/* Problem 071 */
object Problem071 {

  /* write here */

}


/* Problem 072 */
object Problem072 {

  /* write here */

}


/* Problem 073 */
object Problem073 {

  /* write here */

}


/* Problem 074 */
object Problem074 {

  /* write here */

}


/* Problem 075 */
object Problem075 {

  /* write here */

}


/* Problem 076 */
object Problem076 {

  /* write here */

}


/* Problem 077 */
object Problem077 {

  /* write here */

}


/* Problem 078 */
object Problem078 {

  /* write here */

}


/* Problem 079 */
object Problem079 {

  /* write here */

}


/* Problem 080 */
object Problem080 {

  /* write here */

}


/* Problem 081 */
object Problem081 {

  /* write here */

}


/* Problem 082 */
object Problem082 {

  /* write here */

}


/* Problem 083 */
object Problem083 {

  /* write here */

}


/* Problem 084 */
object Problem084 {

  /* write here */

}


/* Problem 085 */
object Problem085 {

  /* write here */

}


/* Problem 086 */
object Problem086 {

  /* write here */

}


/* Problem 087 */
object Problem087 {

  /* write here */

}


/* Problem 088 */
object Problem088 {

  /* write here */

}


/* Problem 089 */
object Problem089 {

  /* write here */

}


/* Problem 090 */
object Problem090 {

  /* write here */

}


/* Problem 091 */
object Problem091 {

  /* write here */

}


/* Problem 092 */
object Problem092 {

  /* write here */

}


/* Problem 093 */
object Problem093 {

  /* write here */

}


/* Problem 094 */
object Problem094 {

  /* write here */

}


/* Problem 095 */
object Problem095 {

  /* write here */

}


/* Problem 096 */
object Problem096 {

  /* write here */

}


/* Problem 097 */
object Problem097 {

  /* write here */

}


/* Problem 098 */
object Problem098 {

  /* write here */

}


/* Problem 099 */
object Problem099 {

  /* write here */

}
