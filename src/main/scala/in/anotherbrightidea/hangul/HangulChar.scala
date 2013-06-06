package in.anotherbrightidea.hangul

import scala.math._
import scala.Some

/**
 *
 * @author Patryk Dworznik
 *         5/17/13 5:07 PM
 *
 */

object Hangul {

  def isJamo(ch: Char): Boolean = isCompatibilityJamo(ch) || isLead(ch) || isVowel(ch) || isTail(ch)

  def isLead(ch: Char): Boolean = ch >= 0x1100 && ch <= 0x1112

  def isVowel(ch: Char): Boolean = ch >= 0x1161 && ch <= 0x1175

  def isTail(ch: Char): Boolean = ch >= 0x11A8 && ch <= 0x11C2

  def isDoubleTail(ch: Char): Boolean = doubleJamos.contains(ch)

  def isCompatibilityJamo(ch: Char): Boolean = ch >= 0x3130 && ch <= 0x318e

  def isHangul(ch: Char) = ch >= 0xAC00 && ch <= 0xD7A3

  def calculateHangulCodepoint(lead: Lead, vowel: Vowel, tail: Option[Tail]): Char = {
    val leadOffset = lead.toInt - 0x1100 + 1
    val vowelOffset = vowel.toInt - 0x1161 + 1
    val tailOffset = tail match {
      case None => 0
      case Some(jamo) => jamo.toInt - 0x11A8 + 1
    }
    (tailOffset + (vowelOffset - 1) * 28 + (leadOffset - 1) * 588 + 44032).toChar
  }

  val doubleJamos = Map(
    0x11A9 ->(0x11A8, 0x11A8), //ᆩ
    0x11AA ->(0x11A8, 0x11BA), //ᆪ
    0x11AC ->(0x11AB, 0x11BD), //ᆬ
    0x11AD ->(0x11AB, 0x11C2), //ᆭ
    0x11B0 ->(0x11AF, 0x11A8), //ㄺ
    0x11B1 ->(0x11AF, 0x11B7), //ᆱ
    0x11B2 ->(0x11AF, 0x11B8), //ᆲ
    0x11B3 ->(0x11AF, 0x11BA), //ᆳ
    0x11B4 ->(0x11AF, 0x11C0), //ᆴ
    0x11B5 ->(0x11AF, 0x11C1), //ᆵ
    0x11B6 ->(0x11AF, 0x11C2), //ᆶ
    0x11B9 ->(0x11B8, 0x11BA), //ᆹ
    0x11BB ->(0x11BA, 0x11BA) //ᆻ
  ) map {
    case (k, (v1, v2)) => (k.toChar, (v1.toChar, v2.toChar))
  }

  val reverseDoubleJamos = doubleJamos.map(_.swap)

  def jamo(ch: Char) = ch match {
    case ch if Hangul.isLead(ch) => Lead(ch)
    case ch if Hangul.isVowel(ch) => Vowel(ch)
    case ch if Hangul.isTail(ch) => Tail(ch)
    case ch if Hangul.isCompatibilityJamo(ch) => CompJamo(ch)
    case _ => throw new IllegalArgumentException("Invalid Jamo character: " + ch)
  }

  def lead(ch: Char): Lead = Lead((0x1100 + floor(((ch - 44032) / 588))).toChar)

  def vowel(ch: Char): Vowel = {
    val t: Option[Tail] = tail(ch)
    val tailOffset = t match {
      case None => -1
      case Some(tch) => tch.toInt - 0x11A8 + 1
    }
    Vowel((floor(((ch - 44032 - tailOffset) % 588) / 28) + 0x1161).toChar)
  }

  def tail(ch: Char): Option[Tail] = {
    val code = (((ch - 44032) % 28) + 0x11A8 - 1)
    if (code == 4519) None
    else if (doubleJamos.contains(code.toChar))
    //      Some(doubleJamos(ch) match {
    //        case (ch1, ch2) => DoubleTail(Tail(ch1), Tail(ch2))
    //      })
      Some(Tail(code.toChar))

    else
      Some(Tail(code.toChar))
  }

  def char(lead: Lead, vowel: Vowel, tail: Tail) = HangulChar(lead, vowel, Some(tail))

  def char(lead: Lead, vowel: Vowel) = HangulChar(lead, vowel, None)

  def char(lead: Jamo, vowel: Jamo, tail: Jamo): HangulChar = (lead, vowel, tail) match {
    case (l: Lead, v: Vowel, t: Tail) => char(l, v, t)
    //    case (lead: Tail, vowel: Vowel, tail: Tail) => char(tailToLead(lead), vowel, tail)
    case jamos => throw new IllegalArgumentException("Invalid Jamo sequence" + jamos)
  }

  def char(lead: Jamo, vowel: Jamo): HangulChar = (lead, vowel) match {
    case (lead: Lead, vowel: Vowel) => char(lead, vowel)
    case (lead: Tail, vowel: Vowel) => ???
    case jamos => throw new IllegalArgumentException("Invalid Jamo sequence" + jamos)
  }


  def char(jamos: Seq[Jamo]): HangulChar = jamos match {
    case lead :: (vowel :: (tail :: _)) => char(lead, vowel, tail)
    case lead :: (vowel :: _) => char(lead, vowel)
    case _ => throw new IllegalArgumentException("Invalid Jamo sequence: " + jamos)
  }

  def apply(ch: Char): Hangul = {
    if (isHangul(ch))
      HangulChar(lead(ch), vowel(ch), tail(ch))
    else if (isJamo(ch))
      jamo(ch)
    else
      OtherChar(ch)
  }

  def char(ch: Char): HangulChar = HangulChar(lead(ch), vowel(ch), tail(ch))


}

abstract class Hangul extends Proxy with Ordered[Char]

case class HangulChar(val lead: Lead, val vowel: Vowel, val tail: Option[Tail]) extends Hangul {
  def ch: Char = Hangul.calculateHangulCodepoint(lead, vowel, tail)

  def self: Any = ch

  def compare(that: Char): Int = if (ch < that) -1 else if (ch > that) 1 else 0
}

abstract class Jamo(ch: Char) extends Hangul {
  def self: Any = ch

  require(Hangul.isJamo(ch), "Invalid Jamo character: " + self)

  def compare(that: Char): Int = if (ch < that) -1 else if (ch > that) 1 else 0

  def toInt = ch.toInt
}

case class CompJamo(comp: Char) extends Jamo(comp) {
  require(Hangul.isCompatibilityJamo(comp), "Invalid Jamo character: " + comp)
}

case class Lead(lead: Char) extends Jamo(lead) {
  require(Hangul.isLead(lead), "Invalid Jamo character: " + lead)
}

case class Vowel(vowel: Char) extends Jamo(vowel) {
  require(Hangul.isVowel(vowel), "Invalid Jamo character: " + vowel)
}


case class Tail(tail: Char) extends Jamo(tail) {
  require(Hangul.isTail(tail), "Invalid Jamo character: " + tail)
}

object DoubleTail {

  def apply(tail1: Tail, tail2: Tail) {
  }

  def unapply(tail: Tail) = tail match {
    case Tail(ch) if Hangul.doubleJamos.contains(ch) => Hangul.doubleJamos.get(ch) match {
      case Some((ch1, ch2)) => Some((Tail(ch1), Tail(ch2)))
    }
    case _ => None
  }
}

case class OtherChar(val char: Char) extends Hangul {
  def self: Any = char

  def compare(that: Char): Int = if (char < that) -1 else if (char > that) 1 else 0

}
