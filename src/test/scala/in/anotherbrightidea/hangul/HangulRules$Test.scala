package in.anotherbrightidea.hangul

import org.scalatest.FunSuite

/**
 *
 * @author Patryk Dworznik
 *         5/19/13 8:13 PM
 *
 */

class HangulRules$Test extends FunSuite {

  test("Resyllabification 1") {
    val (s1, s2) = (Hangul.char('글'), Hangul.char('은'))
    val (r1, r2) = HangulRules.resyllabification(s1, s2)
    println(r1, r2)
    assert(r1 === '그')
    assert(r2 === '른')

  }

  test("Resyllabification 2") {
    val (s1, s2) = (Hangul.char('책'), Hangul.char('이'))
    val (r1, r2) = HangulRules.resyllabification(s1, s2)
    println(r1, r2)
    assert(r1 === '채')
    assert(r2 === '기')

  }

  test("Syllable-final closure") {
    val (s1, s2) = (Hangul.char('잎'), Hangul.char('거'))
    val (r1, r2) = HangulRules.syllableFinalClosure(s1, s2)
    println(r1, r2)
    assert(r1 === '입')
    assert(r2 === '거')
  }

  test("Nasal assimilation") {
    val (s1, s2) = (Hangul.char('입'), Hangul.char('만'))
    val (r1, r2) = HangulRules.nasalAssimilation(s1, s2)
    println(r1, r2)
    assert(r1 === '임')
    assert(r2 === '만')
  }

  test("ㄴ to ㄹ assimilation 1") {
    val (s1, s2) = (Hangul.char('진'), Hangul.char('리'))
    val (r1, r2) = HangulRules.nToLAssimilation(s1, s2)
    println(r1, r2)
    assert(r1 === '질')
    assert(r2 === '리')
  }

  test("ㄴ to ㄹ assimilation 2") {
    val (s1, s2) = (Hangul.char('달'), Hangul.char('님'))
    val (r1, r2) = HangulRules.nToLAssimilation(s1, s2)
    println(r1, r2)
    assert(r1 === '달')
    assert(r2 === '림')
  }

  test("Tensification 1") {
    val (s1, s2) = (Hangul.char('학'), Hangul.char('생'))
    val (r1, r2) = HangulRules.tensification(s1, s2)
    println(r1, r2)
    assert(r1 === '학')
    assert(r2 === '씽')
  }

  test("Tensification 2") {
    val (s1, s2) = (Hangul.char('몇'), Hangul.char('번'))
    val (r1, r2) = HangulRules.tensification(s1, s2)
    println(r1, r2)
    assert(r1 === '멷')
    assert(r2 === '뻔')
  }

  test("Aspiration 1") {
    val (s1, s2) = (Hangul.char('좋'), Hangul.char('다'))
    val (r1, r2) = HangulRules.aspiration(s1, s2)
    println(r1, r2)
    assert(r1 === '조')
    assert(r2 === '타')
  }

  test("Aspiration 2") {
    val (s1, s2) = (Hangul.char('좋'), Hangul.char('고'))
    val (r1, r2) = HangulRules.aspiration(s1, s2)
    println(r1, r2)
    assert(r1 === '조')
    assert(r2 === '코')
  }

  test("Double consonant reduction") {
    val (s1, s2) = (Hangul.char('없'), Hangul.char('다'))
    val (r1, r2) = HangulRules.doubleConsonantReduction(s1, s2)
    println(r1, r2)
    assert(r1 === '업')
    assert(r2 === '따')
  }

  test("Palatalization") {
    val (s1, s2) = (Hangul.char('닫'), Hangul.char('혀'))
    val (r1, r2) = HangulRules.palatalization(s1, s2)
    println(r1, r2)
    assert(r1 === '다')
    assert(r2 === '쳐')
  }

  test("Place assimilation") {
    val (s1, s2) = (Hangul.char('닫'), Hangul.char('혀'))
    val (r1, r2) = HangulRules.palatalization(s1, s2)
    println(r1, r2)
    assert(r1 === '다')
    assert(r2 === '쳐')
  }


  test("All rules") {
    val (s1, s2) = (Hangul.char('글'), Hangul.char('은'))
    val (r1, r2) = HangulRules.apply(s1, s2)
    println(r1, r2)
    assert(r1 === '그')
    assert(r2 === '른')
  }
}