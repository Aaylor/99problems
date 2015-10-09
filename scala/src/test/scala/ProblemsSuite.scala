package org.problems

import org.scalatest._


/* Problem 001 Specification */
class Problem001Spec extends FlatSpec {

  "last" should "return the last element on one length list" in {
    assert(Problem001.last(List(42)) === 42)
  }

  it should "return the last element on a n-size list (n > 0)" in {
    assert(Problem001.last(List(1, 2, 3, 4, 5, 39, 40, 41, 42)) === 42)
  }

  it should "throw NoSuchElementException when empty list" in {
    intercept[NoSuchElementException] {
      Problem001.last(List())
    }
  }

}


/* Problem 002 Specification */
class Problem002Spec extends FlatSpec {

  "lastButOne" should "return the last but one on two length list" in {
    assert(Problem002.lastButOne(List(41, 42)) === 41)
  }

  it should "return the last but one on n-size list (n > 1)" in {
    assert(Problem002.lastButOne(List(1, 2, 3, 4, 5, 39, 40, 41, 42)) === 41)
  }

  it should "throw NoSuchElementexception when list of size 1" in {
    intercept[NoSuchElementException] {
      Problem002.lastButOne(List(42))
    }
  }

  it should "throw NoSuchElementexception when empty list" in {
    intercept[NoSuchElementException] {
      Problem002.lastButOne(List())
    }
  }

}


/* Problem 003 Specification */
class Problem003Spec extends FlatSpec {

  "nth" should "throw NoSuchElementexception when n < 0 || n > length(list)" in {
    intercept[NoSuchElementException] {
      Problem003.nth(List(1, 2, 3), -42)
    }

    intercept[NoSuchElementException] {
      Problem003.nth(List(1, 2, 3), 4)
    }
  }

  it should "returns the third element" in {
    assert(Problem003.nth(List(1, 2, 3, 4), 2) === 3)
  }

}


/* Problem 004 Specification */
class Problem004Spec extends FlatSpec {

  "length" should "returns 0 on empty list" in {
    assert(Problem004.length(Nil) === 0)
  }

  it should "return 4 in four-element list" in {
    assert(Problem004.length(List(1, 2, 3, 4)) === 4)
  }

}


/* Problem 005 Specification */
class Problem005Spec extends FlatSpec {

  "rev" should "returns the empty list on empty list" in {
    assert(Problem005.rev(Nil) === Nil)
  }

  it should "returns the reversed list" in {
    assert(Problem005.rev(List(1, 2, 3, 4, 5)) === List(5, 4, 3, 2, 1))
  }

}


/* Problem 006 Specification */
class Problem006Spec extends FlatSpec {

  "palindrome" should "returns true on empty list" in {
    assert(Problem006.palindrome(Nil))
  }

  it should "returns false on non palindrome" in {
    assert(!Problem006.palindrome(List(1, 2, 3, 2, 4)))
  }

  it should "returns true on palindrome" in {
    assert(Problem006.palindrome(List(1, 2, 3, 2, 1)))
  }

}


/* Problem 007 Specification */
class Problem007Spec extends FlatSpec {

  "flatten" should "returns empty list on empty list" in {
    assert(Problem007.flatten(Nil) === Nil)
  }

  it should "returns flattened list" in {
    val fl = Problem007.flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    assert (fl === List(1, 1, 2, 3, 5, 8))
  }

}


/* Problem 008 Specification */
class Problem008Spec extends FlatSpec {

  "compress" should "returns empty list on empty list" in {
    assert(Problem008.compress(Nil) === Nil)
  }

  it should "returns the compressed list" in {
    val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(Problem008.compress(l) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  it should "returns a compressed list" in {
    val l = List('a, 'b, 'c, 'a, 'd, 'e)
    assert(Problem008.compress(l) === l)
  }

}


/* Problem 009 Specification */
class Problem009Spec extends FlatSpec {

  "pack" should "returns empty list on empty list" in {
    assert(Problem009.pack(Nil) === Nil)
  }

  it should "returns the packed list" in {
    val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(Problem009.pack(l) ===
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
        List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

}


/* Problem 010 Specification */
class Problem010Spec extends FlatSpec {

  "pack" should "returns empty list on empty list" in {
    assert(Problem010.pack(Nil) === Nil)
  }

  it should "returns the packed list with correct number" in {
    val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(Problem010.pack(l) ===
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

}


/* Problem 011 Specification */
class Problem011Spec extends FlatSpec {

  "pack" should "returns empty list on empty list" in {
    assert(Problem011.pack(Nil) === Nil)
  }

  it should "returns the packed list with correct number" in {
    val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(Problem011.pack(l) ===
      List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

}


/* Problem 012 Specification */
class Problem012Spec extends FlatSpec {

  "decode" should "returns empty list on empty list" in {
    assert(Problem012.decode(Nil) === Nil)
  }

  it should "returns the unpacked list" in {
    val l = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    assert(Problem012.decode(l) ===
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

}


/* Problem 013 Specification */
class Problem013Spec extends FlatSpec {

  "encodeDirect" should "returns empty list on empty list" in {
    assert(Problem013.encodeDirect(Nil) === Nil)
  }

  it should "returns the packed list" in {
    val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    assert(Problem013.encodeDirect(l) ===
      List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

}


/* Problem 014 Specification */
class Problem014Spec extends FlatSpec {

  "duplicate" should "returns an empty list on empty list" in {
    assert(Problem014.duplicate(Nil) === Nil)
  }

  it should "returns the list with all duplicated elements" in {
    val l = List('a, 'b, 'c, 'd)
    assert(Problem014.duplicate(l) === List('a, 'a, 'b, 'b, 'c, 'c, 'd, 'd))
  }

}


/* Problem 015 Specification */
class Problem015Spec extends FlatSpec {

  "duplicateN" should "returns an empty list on empty list" in {
    assert(Problem015.duplicateN(3, Nil) === Nil)
  }

  it should "returns the list with all duplicated elements" in {
    val l = List('a, 'b, 'c, 'd)
    assert(Problem015.duplicateN(3, l) ===
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'd, 'd, 'd))
  }

}


/* Problem 016 Specification */
class Problem016Spec extends FlatSpec {

  "drop" should "returns an empty list on empty list" in {
    assert(Problem016.drop(3, Nil) === Nil)
  }

  it should "returns the list with all duplicated elements" in {
    val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    assert(Problem016.drop(3, l) === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

}


/* Problem 017 Specification */
class Problem017Spec extends FlatSpec {

  "split" should "returns an empty list on empty list" in {
    assert(Problem017.split(3, Nil) === (Nil, Nil))
  }

  it should "returns the list with all duplicated elements" in {
    val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    assert(Problem017.split(3, l) ===
      (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

}


/* Problem 018 Specification */
class Problem018Spec extends FlatSpec {

  "slice" should "returns empty list on empty list" in {
    assert(Problem018.slice(21, 42, Nil) === Nil)
  }

  it should "returns empty list on invalid values" in {
    assert(Problem018.slice(5, 10, List('a, 'b, 'c)) === Nil)
  }

  it should "returns the sliced list" in {
    val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
    assert(Problem018.slice(3, 7, l) === List('d, 'e, 'f, 'g))
  }

}


/* Problem 019 Specification */
class Problem019Spec extends FlatSpec {

  "rotate" should "do the rotation with positive number" in {
    assert(
      Problem019.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  it should "do the rotation with negative number" in {
    assert(
      Problem019.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

}


/* Problem 020 Specification */
class Problem020Spec extends FlatSpec {

  "removeAt" should "fail on empty list" in {
    intercept[NoSuchElementException] {
      Problem020.removeAt(0, List())
    }
  }

  it should "returns the list without the first value" in {
    assert(Problem020.removeAt(0, List('a, 'b, 'c)) === (List('b, 'c), 'a))
  }

  it should "returns the list without the second value" in {
    assert(Problem020.removeAt(1, List('a, 'b, 'c)) === (List('a, 'c), 'b))
  }

}


/* Problem 021 Specification */
class Problem021Spec extends FlatSpec {

  "insertAt" should "fail on empty list" in {
    intercept[NoSuchElementException] {
      Problem021.insertAt(10, 'new, List())
    }
  }

  it should "returns the list with the new value for head" in {
    assert(Problem021.insertAt(0, 'new, List('a, 'b, 'c)) ===
      List('new, 'a, 'b, 'c))
  }

  it should "returns the list with the new value at the second pos" in {
    assert(Problem021.insertAt(1, 'new, List('a, 'b, 'c)) ===
      List('a, 'new, 'b, 'c))
  }

}


/* Problem 022 Specification */
class Problem022Spec extends FlatSpec {

  "range" should "returns empty list when min > max" in {
    assert(Problem022.range(1, 0) == Nil)
  }

  it should "returns the range between min and max inclusive" in {
    assert(Problem022.range(4, 9) == List(4, 5, 6, 7, 8, 9))
  }

}


/* Problem 023 Specification */
class Problem023Spec extends FlatSpec {

  "randomSelect" should "returns value existing on the list" in {
    val l = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)
    val select = Problem023.randomSelect(3, l)
    assert(select forall (l contains _))
    assert(select.length === 3)
  }

}


/* Problem 024 Specification */
class Problem024Spec extends FlatSpec {

  "lotto" should "returns n unique values between the range 1 max" in {
    val lotto = Problem024.lotto(6, 49)
    assert(lotto forall (x => x >= 1 && x <= 49))
    assert(lotto.length === 6)
    assert(lotto.distinct === lotto)
  }

}


/* Problem 025 Specification */
class Problem025Spec extends FlatSpec {

  "randomPermut" should "returns a permutation of the list" in {
    val l = List('a, 'b, 'c, 'd, 'e, 'f)
    val lperm = Problem025.randomPermut(l)
    assert(lperm forall (l contains _))
    assert(l.length === lperm.length)
  }

}


/* Problem 026 Specification */
class Problem026Spec extends FlatSpec {

  /* write here */

}


/* Problem 027 Specification */
class Problem027Spec extends FlatSpec {

  /* write here */

}


/* Problem 028 Specification */
class Problem028Spec extends FlatSpec {

  /* write here */

}


/* Problem 029 Specification */
class Problem029Spec extends FlatSpec {

  /* write here */

}


/* Problem 030 Specification */
class Problem030Spec extends FlatSpec {

  /* write here */

}


/* Problem 031 Specification */
class Problem031Spec extends FlatSpec {

  /* write here */

}


/* Problem 032 Specification */
class Problem032Spec extends FlatSpec {

  /* write here */

}


/* Problem 033 Specification */
class Problem033Spec extends FlatSpec {

  /* write here */

}


/* Problem 034 Specification */
class Problem034Spec extends FlatSpec {

  /* write here */

}


/* Problem 035 Specification */
class Problem035Spec extends FlatSpec {

  /* write here */

}


/* Problem 036 Specification */
class Problem036Spec extends FlatSpec {

  /* write here */

}


/* Problem 037 Specification */
class Problem037Spec extends FlatSpec {

  /* write here */

}


/* Problem 038 Specification */
class Problem038Spec extends FlatSpec {

  /* write here */

}


/* Problem 039 Specification */
class Problem039Spec extends FlatSpec {

  /* write here */

}


/* Problem 040 Specification */
class Problem040Spec extends FlatSpec {

  /* write here */

}


/* Problem 041 Specification */
class Problem041Spec extends FlatSpec {

  /* write here */

}


/* Problem 042 Specification */
class Problem042Spec extends FlatSpec {

  /* write here */

}


/* Problem 043 Specification */
class Problem043Spec extends FlatSpec {

  /* write here */

}


/* Problem 044 Specification */
class Problem044Spec extends FlatSpec {

  /* write here */

}


/* Problem 045 Specification */
class Problem045Spec extends FlatSpec {

  /* write here */

}


/* Problem 046 Specification */
class Problem046Spec extends FlatSpec {

  /* write here */

}


/* Problem 047 Specification */
class Problem047Spec extends FlatSpec {

  /* write here */

}


/* Problem 048 Specification */
class Problem048Spec extends FlatSpec {

  /* write here */

}


/* Problem 049 Specification */
class Problem049Spec extends FlatSpec {

  /* write here */

}


/* Problem 050 Specification */
class Problem050Spec extends FlatSpec {

  /* write here */

}


/* Problem 051 Specification */
class Problem051Spec extends FlatSpec {

  /* write here */

}


/* Problem 052 Specification */
class Problem052Spec extends FlatSpec {

  /* write here */

}


/* Problem 053 Specification */
class Problem053Spec extends FlatSpec {

  /* write here */

}


/* Problem 054 Specification */
class Problem054Spec extends FlatSpec {

  /* write here */

}


/* Problem 055 Specification */
class Problem055Spec extends FlatSpec {

  /* write here */

}


/* Problem 056 Specification */
class Problem056Spec extends FlatSpec {

  /* write here */

}


/* Problem 057 Specification */
class Problem057Spec extends FlatSpec {

  /* write here */

}


/* Problem 058 Specification */
class Problem058Spec extends FlatSpec {

  /* write here */

}


/* Problem 059 Specification */
class Problem059Spec extends FlatSpec {

  /* write here */

}


/* Problem 060 Specification */
class Problem060Spec extends FlatSpec {

  /* write here */

}


/* Problem 061 Specification */
class Problem061Spec extends FlatSpec {

  /* write here */

}


/* Problem 062 Specification */
class Problem062Spec extends FlatSpec {

  /* write here */

}


/* Problem 063 Specification */
class Problem063Spec extends FlatSpec {

  /* write here */

}


/* Problem 064 Specification */
class Problem064Spec extends FlatSpec {

  /* write here */

}


/* Problem 065 Specification */
class Problem065Spec extends FlatSpec {

  /* write here */

}


/* Problem 066 Specification */
class Problem066Spec extends FlatSpec {

  /* write here */

}


/* Problem 067 Specification */
class Problem067Spec extends FlatSpec {

  /* write here */

}


/* Problem 068 Specification */
class Problem068Spec extends FlatSpec {

  /* write here */

}


/* Problem 069 Specification */
class Problem069Spec extends FlatSpec {

  /* write here */

}


/* Problem 070 Specification */
class Problem070Spec extends FlatSpec {

  /* write here */

}


/* Problem 071 Specification */
class Problem071Spec extends FlatSpec {

  /* write here */

}


/* Problem 072 Specification */
class Problem072Spec extends FlatSpec {

  /* write here */

}


/* Problem 073 Specification */
class Problem073Spec extends FlatSpec {

  /* write here */

}


/* Problem 074 Specification */
class Problem074Spec extends FlatSpec {

  /* write here */

}


/* Problem 075 Specification */
class Problem075Spec extends FlatSpec {

  /* write here */

}


/* Problem 076 Specification */
class Problem076Spec extends FlatSpec {

  /* write here */

}


/* Problem 077 Specification */
class Problem077Spec extends FlatSpec {

  /* write here */

}


/* Problem 078 Specification */
class Problem078Spec extends FlatSpec {

  /* write here */

}


/* Problem 079 Specification */
class Problem079Spec extends FlatSpec {

  /* write here */

}


/* Problem 080 Specification */
class Problem080Spec extends FlatSpec {

  /* write here */

}


/* Problem 081 Specification */
class Problem081Spec extends FlatSpec {

  /* write here */

}


/* Problem 082 Specification */
class Problem082Spec extends FlatSpec {

  /* write here */

}


/* Problem 083 Specification */
class Problem083Spec extends FlatSpec {

  /* write here */

}


/* Problem 084 Specification */
class Problem084Spec extends FlatSpec {

  /* write here */

}


/* Problem 085 Specification */
class Problem085Spec extends FlatSpec {

  /* write here */

}


/* Problem 086 Specification */
class Problem086Spec extends FlatSpec {

  /* write here */

}


/* Problem 087 Specification */
class Problem087Spec extends FlatSpec {

  /* write here */

}


/* Problem 088 Specification */
class Problem088Spec extends FlatSpec {

  /* write here */

}


/* Problem 089 Specification */
class Problem089Spec extends FlatSpec {

  /* write here */

}


/* Problem 090 Specification */
class Problem090Spec extends FlatSpec {

  /* write here */

}


/* Problem 091 Specification */
class Problem091Spec extends FlatSpec {

  /* write here */

}


/* Problem 092 Specification */
class Problem092Spec extends FlatSpec {

  /* write here */

}


/* Problem 093 Specification */
class Problem093Spec extends FlatSpec {

  /* write here */

}


/* Problem 094 Specification */
class Problem094Spec extends FlatSpec {

  /* write here */

}


/* Problem 095 Specification */
class Problem095Spec extends FlatSpec {

  /* write here */

}


/* Problem 096 Specification */
class Problem096Spec extends FlatSpec {

  /* write here */

}


/* Problem 097 Specification */
class Problem097Spec extends FlatSpec {

  /* write here */

}


/* Problem 098 Specification */
class Problem098Spec extends FlatSpec {

  /* write here */

}


/* Problem 099 Specification */
class Problem099Spec extends FlatSpec {

  /* write here */

}
