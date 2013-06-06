package in.anotherbrightidea.hangul

import org.scalatest.FunSuite

/**
 *
 * @author Patryk Dworznik
 *         5/17/13 5:09 PM
 *
 */

class HangulCharTest extends FunSuite {


  test("Jamo constructors") {
    val l = new Lead('ᄇ')
    assert(l.isInstanceOf[Jamo])
    assert(l.isInstanceOf[Lead])
    val v = new Vowel('ᅡ')
    assert(v.isInstanceOf[Jamo])
    assert(v.isInstanceOf[Vowel])
    val t = new Tail('ᆩ')
    assert(t.isInstanceOf[Jamo])
    assert(t.isInstanceOf[Tail])
  }

  test("Jamo char function") {
    val l = Hangul.jamo('ᄇ')
    assert(l.isInstanceOf[Jamo])
    assert(l.isInstanceOf[Lead])
    val v = Hangul.jamo('ᅡ')
    assert(v.isInstanceOf[Jamo])
    assert(v.isInstanceOf[Vowel])
    val t = Hangul.jamo('ᆩ')
    assert(t.isInstanceOf[Jamo])
    assert(t.isInstanceOf[Tail])
  }


  test("Hangul decomposition") {
    val h: HangulChar = Hangul.char('꿈').asInstanceOf[HangulChar]
    assert(h.lead === 'ᄁ')
    assert(h.vowel === 'ᅮ')
    assert(!h.tail.isEmpty)
    assert(h.tail.get === 'ᆷ')
  }

  test("Hangul pattern matching 1") {
    val h: Hangul = Hangul.char('꿈')
    h match {
      case HangulChar(lead, vowel, tail) => {
        assert(lead === 'ᄁ')
        assert(vowel === 'ᅮ')
        assert(!tail.isEmpty)
        assert(tail.get === 'ᆷ')
      }
      case _ => fail
    }

  }

  test("Hangul pattern matching 2") {
    val h: Hangul = Hangul.char('나')
    h match {
      case HangulChar(lead, vowel, tail) => {
        assert(lead === 'ᄂ')
        assert(vowel === 'ᅡ')
        assert(tail.isEmpty)
      }
      case _ => fail
    }

  }

  test("Hangul pattern matching 3") {
    val h: Hangul = Hangul.char('응')
    h match {
      case HangulChar(lead, vowel, tail) => {
        assert(lead === 'ᄋ')
        assert(vowel === 'ᅳ')
        assert(!tail.isEmpty)
        assert(tail.get === 'ᆼ')
      }
      case _ => fail
    }

  }

  test("Hangul lead Jamo") {
    val h: Hangul = Hangul.char('ᄂ')
    h match {
      case Lead(ch) => {
        assert(ch === 'ᄂ')
      }
      case _ => fail
    }
  }

  test("Hangul vowel Jamo") {
    val h: Hangul = Hangul.char('ᅳ')
    h match {
      case Vowel(ch) => {
        assert(ch === 'ᅳ')
      }
      case _ => fail
    }
  }

  test("Hangul tail Jamo") {
    val h: Hangul = Hangul.char('ᆼ')
    h match {
      case Tail(ch) => {
        assert(ch === 'ᆼ')
      }
      case _ => fail
    }
  }

  test("Hangul compatibility Jamo") {
    val h: Hangul = Hangul.char('ㄹ')
    h match {
      case CompJamo(ch) => {
        assert(ch === 'ㄹ')
      }
      case _ => fail
    }
  }

  test("Hangul other") {
    val h: Hangul = Hangul.char('!')
    h match {
      case OtherChar(ch) => {
        assert(ch === '!')
      }
      case _ => fail
    }
  }
  test("Hangul double Jamo") {
    val h: Hangul = Hangul.char('ᆹ')
    h match {
      case DoubleTail(Tail(ch1), Tail(ch2)) => {
        ch1 === 'ᆸ
        ch2 === 'ᆺ'
      }
      case _ => fail
    }
  }


  test("trait") {

    trait O {
      val a: Int
      val b: Int = 0
    }

    case class A(a: Int) extends O

    case class B(a: Int, override val b: Int) extends O


    val a: O = B(2,3)

    a match {
      case A(a) => println("A: " + a)
      case B(a, b) => println("B: " + a, b)
    }

  }


}