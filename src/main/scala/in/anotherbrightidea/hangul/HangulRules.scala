package in.anotherbrightidea.hangul

/**
 *
 * @author Patryk Dworznik
 *         5/19/13 8:12 PM
 *
 */


object HangulRules {

  private
  val tailToLead = Map(
    0x11A8 -> 0x1100, //ᆨ
    0x11A9 -> 0x1101, //ᆩ
    0x11AB -> 0x1102, //ᆫ
    0x11AE -> 0x1103, //ᆮ
    0x11AF -> 0x1105, //ᆯ
    0x11B7 -> 0x1106, //ᆷ
    0x11B8 -> 0x1107, //ᆸ
    0x11BA -> 0x1109, //ᆺ
    0x11BB -> 0x110A, //ᆻ
    0x11BD -> 0x110C, //ᆽ
    0x11BE -> 0x110E, //ᆾ
    0x11BF -> 0x110F, //ᆿ
    0x11C0 -> 0x1110, //ᇀ
    0x11C1 -> 0x1111, //ᇁ
    0x11C2 -> 0x1112 //ᇂ
  ).map {
    case (k, v) => (Tail(k.toChar), Lead(v.toChar))
  }


  private
  val syllableFinalChanges: Map[Tail, Tail] = Map(
    'ᆸ' -> 'ᆸ',
    'ᇁ' -> 'ᆸ',
    'ᆮ' -> 'ᆮ',
    'ᇀ' -> 'ᆮ',
    'ᆺ' -> 'ᆮ',
    'ᆻ' -> 'ᆮ',
    'ᆽ' -> 'ᆮ',
    'ᆾ' -> 'ᆮ',
    'ᆨ' -> 'ᆨ',
    'ᆿ' -> 'ᆨ',
    'ᆩ' -> 'ᆨ'
  ).map {
    case (k, v) => (Tail(k.toChar), Tail(v.toChar))
  }

  private
  val nasalChange: Map[Tail, Tail] = Map(
    'ᆸ' -> 'ᆷ',
    'ᇁ' -> 'ᆷ',
    'ᆮ' -> 'ᆫ',
    'ᇀ' -> 'ᆫ',
    'ᆺ' -> 'ᆫ',
    'ᆻ' -> 'ᆫ',
    'ᆽ' -> 'ᆫ',
    'ᆾ' -> 'ᆫ',
    'ᇂ' -> 'ᆫ',
    'ᆨ' -> 'ᆼ',
    'ᆿ' -> 'ᆼ',
    'ᆩ' -> 'ᆼ'
  ).map {
    case (k, v) => (Tail(k.toChar), Tail(v.toChar))
  }


  type HangulRule = (HangulChar, HangulChar) => (HangulChar, HangulChar)

  def nasal(ch: Jamo): Boolean = if (ch.isInstanceOf[Tail]) ch == 'ᆫ' || ch == 'ᆷ'
  else if (ch.isInstanceOf[Lead]) ch == 'ᄂ' || ch == 'ᄆ'
  else false


  val resyllabification: HangulRule = (s1, s2) => (s1, s2) match {
    case (HangulChar(lead1, vowel1, Some(tail1)), HangulChar(Lead('ᄋ'), vowel2, tail2)) if tailToLead.contains(tail1) => (Hangul.char(lead1, vowel1), HangulChar(tailToLead(tail1), vowel2, tail2))
    case _ => (s1, s2)
  }

  val syllableFinalClosure: HangulRule = (s1, s2) => (s1, s2) match {
    case (HangulChar(lead1, vowel1, Some(tail1)), hs2@HangulChar(lead2, vowel2, _)) if lead2 != 'ᄋ' =>
      (Hangul.char(lead1, vowel1, syllableFinalChanges.get(tail1).getOrElse(tail1)), hs2)
    case _ => (s1, s2)
  }

  val nasalAssimilation: HangulRule = (s1, s2) => (s1, s2) match {
    case (HangulChar(lead1, vowel1, Some(tail1)), hs2@HangulChar(lead2, vowel2, _)) if nasal(lead2) && nasalChange.contains(tail1) =>
      (Hangul.char(lead1, vowel1, nasalChange(tail1)), hs2)
    case _ => (s1, s2)
  }

  val nToLAssimilation: HangulRule = (s1, s2) => (s1, s2) match {
    case (HangulChar(lead1, vowel1, Some(Tail('ᆫ'))), HangulChar(Lead('ᄅ'), vowel2, tail2)) =>
      (Hangul.char(lead1, vowel1, Tail('ᆯ')), HangulChar(Lead('ᄅ'), vowel2, tail2))
    case (HangulChar(lead1, vowel1, Some(Tail('ᆯ'))), HangulChar(Lead('ᄂ'), vowel2, tail2)) =>
      (Hangul.char(lead1, vowel1, Tail('ᆯ')), HangulChar(Lead('ᄅ'), vowel2, tail2))
    case _ => (s1, s2)
  }

  val rules: List[HangulRule] = List(resyllabification, syllableFinalClosure, nasalAssimilation)

  val apply: HangulRule = (s1, s2) => rules.foldLeft((s1, s2))((ret, f) => f(ret._1, ret._2))

  val tensification: HangulRule = (s1, s2) => (s1, s2)

  val aspiration: HangulRule = (s1, s2) => (s1, s2)

  val doubleConsonantReduction: HangulRule = (s1, s2) => (s1, s2)

  val palatalization: HangulRule = (s1, s2) => (s1, s2)

  val placeAssimilation: HangulRule = (s1, s2) => (s1, s2)

}
