package in.anotherbrightidea.hangul

import org.scalatest.FunSuite

/**
 *
 * @author Patryk Dworznik
 *         5/18/13 12:34 AM
 *
 */

class HangulStringTest extends FunSuite {



  test("iteration") {
    val str = new HangulString("하지   마!")
    println(str)
    for (ch <- str.hangulAndOther) println(ch)
  }

  test("read hangul") {
    val str = new HangulString("학안")
    println(str.pronunciation)
  }

  test("resyllabification") {
    assert(HangulString("한글은").pronunciation === HangulString("한그른"))
//    assert(HangulString("읽어요").pronunciation === HangulString("일거요"))
    assert(HangulString("천만에요").pronunciation === HangulString("천마네요"))
    assert(HangulString("질문이 었어요").pronunciation === HangulString("질무니 어써요"))

  }
  //
  //  test("syllable-final closure") {
  //    assert(HangulString("꽃").pronunciation === HangulString("꼳"))
  //    assert(HangulString("꽃은").pronunciation === HangulString("꼬츤"))
  //    assert(HangulString("꽃또").pronunciation === HangulString("꼳또"))
  //
  //  }
  //
  //  test("nasal assimilation") {
  //    assert(HangulString("임만").pronunciation === HangulString("임만"))
  //    assert(HangulString("없나요").pronunciation === HangulString("엄나요"))
  //    assert(HangulString("있는데").pronunciation === HangulString("인는데"))
  //    assert(HangulString("낳는다").pronunciation === HangulString("난는다"))
  //    assert(HangulString("앞문").pronunciation === HangulString("암문"))
  //    assert(HangulString("받는다").pronunciation === HangulString("반는다"))
  //    assert(HangulString("몇 년").pronunciation === HangulString("면 년"))
  //    assert(HangulString("일학년").pronunciation === HangulString("일항년"))
  //  }
  //
  //  test("ㄴ and ㄹ assimilation") {
  //    assert(HangulString("찬련").pronunciation ===  HangulString("찰련"))
  //  }
  //
  //  test("tensification") {
  //
  //  }
  //
  //  test("aspiration") {
  //
  //  }
  //
  //  test("double consonant reduction") {
  //
  //  }
  //
  //
  //  test("palatalization") {
  //
  //  }
  //
  //
  //  test("place assimilation") {
  //
  //
  //  }
}