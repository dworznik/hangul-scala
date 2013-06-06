package in.anotherbrightidea.hangul

import scala.annotation.tailrec


/**
 *
 * @author Patryk Dworznik
 *         5/17/13 5:20 PM
 *
 */


object HangulString {

  def apply(str: String): HangulString = {
    new HangulString(str)
  }

}

class HangulString(val self: String) extends Proxy with Ordered[String] {

  lazy val all: List[Hangul] = (for (ch <- self) yield Hangul(ch)).toList

  def hangul = all filter (ch => ch.isInstanceOf[HangulChar] || ch.isInstanceOf[Jamo])

  def other = all filter (_.isInstanceOf[OtherChar])

  def hangulAndOther: List[(HangulChar, List[Hangul])] = {
    @tailrec
    def process(input: List[Hangul], acc: List[(HangulChar, List[Hangul])]): List[(HangulChar, List[Hangul])] = {
      if (input.isEmpty)
        acc.reverse
      else {
        val ch = input.head
        ch match {
          case HangulChar(_, _, _) => process(input.tail, (input.head.asInstanceOf[HangulChar], List()) :: acc)
          case _ if ch.isInstanceOf[OtherChar] || ch.isInstanceOf[Jamo] => {
            val last = acc.head
            process(input.tail, (last._1, last._2 :+ input.head) :: acc.tail)
          }
        }
      }

    }
    process(all, List())

  }

  def pronunciation: HangulString = {

    def applicable(sep: List[Hangul]) = sep.size < 2

    def applyRules(s1: (HangulChar, List[Hangul]), s2: (HangulChar, List[Hangul])) = (s1, s2) match {
      case ((ch1, l1), (ch2, l2)) if(applicable(l1)) => HangulRules.apply(ch1, ch2) match {
        case (r1, r2) => ((r1, l1), (r2, l2))
      }
      case _ => (s1, s2)
    }

    @tailrec
    def process(hanguls: List[(HangulChar, List[Hangul])], output: List[(HangulChar, List[Hangul])]): List[(HangulChar, List[Hangul])] = hanguls match {
      case one :: (two :: rest) => {
        val (n1, n2) = applyRules(one, two)
        process(n2 :: rest, output :+ n1)
      }
      case (one :: rest) => output :+ one
      case _ => output
    }

    val ret = process(hangulAndOther, List[(HangulChar, List[Hangul])]())
    HangulString((for (el <- ret) yield el._1 + el._2.mkString).mkString)
  }


  type HangulRule = ((HangulChar, List[Hangul]), (HangulChar, List[Hangul])) => ((HangulChar, List[Hangul]), (HangulChar, List[Hangul]))

  def compare(that: String): Int = self compareTo that
}