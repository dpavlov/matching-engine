/*
 * Copyright (c) 2009 Thomas Knierim
 * http://www.thomasknierim.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * @author Thomas Knierim
 * @version 1.0
 *
 */
package currency

object VerbaliseNumber {

/** Verbalises a <code>long</code> number in the given language.
  *
  * @param value the number to verbalise.
  * @param language lowercase two-letter ISO-639 language code, e.g.
  * "en", "de", etc.
  * @return a String containing the number as words.
  * @throws IllegalArgumentException if <code>language</code> is not a supported
  * language. Currently <code>en, de, fr, es</code> are supported.
  */
  def toWords(value: Long, language: String): String =
    language match {
      case "en" => toWords_en(value)
      case "de" => toWords_de(value)
      case "fr" => toWords_fr(value)
      case "es" => toWords_es(value)
      case _ => throw new IllegalArgumentException("VerbaliseNumber: language not supported")
    }

 /** Verbalises a <code>long</code> number in English words.
   * Uses the échelle courte e.g. "billion" for 1,000,000,000
   * and returns empty string for zero.
   *
   * @param value the number to verbalise.
   * @return the verbalised number.
   */
  private def toWords_en(value: Long): String = {

    val zeroToNineteen = Array("",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
      "ten",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      "seventeen",
      "eighteen",
      "nineteen" )

    val multiplesOfTen = Array("",
      "ten",
      "twenty",
      "thirty",
      "forty",
      "fifty",
      "sixty",
      "seventy",
      "eighty",
      "ninety" )

    val powersOfThousand = Array("",
      "thousand",
      "million",
      "billion",
      "trillion",
      "quadrillion",
      "quintillion",
      "sextillion",
      "septillion",
      "octillion",
      "nonillion"
    )

    val absValue = Math.abs(value)
    var result: List[String] = List()
    var powerOfTen: Long = 1
    var n = 0
    while (absValue / powerOfTen > 0) {
      val triplet = (absValue / powerOfTen) % 1000
      if (triplet > 0)
        result = powersOfThousand(n) :: result
      n += 1
      val lastTwoDigits = (triplet % 100).toInt
      if (lastTwoDigits < 20)
        result = zeroToNineteen(lastTwoDigits) :: result
      else {
        result = zeroToNineteen(lastTwoDigits % 10) :: result
        result = multiplesOfTen(lastTwoDigits / 10) :: result
      }
      val firstDigit = (triplet / 100).toInt
      if (firstDigit > 0) {
        result = "hundred" :: result
        result = zeroToNineteen(firstDigit) :: result
      }
      powerOfTen *= 1000
    }
    if (value < 0)
      result = "minus" :: result
    result.filter(!_.isEmpty).mkString(" ")
  }

  /** Verbalises a <code>long</code> number in German.
   *  Uses the échelle longue (Peletier), e.g. "Milliarde" for 1,000,000,000
   *  and returns empty string for zero.
   *
   *  @param value the number to verbalise.
   *  @return the verbalised number.
   */
  private def toWords_de(value: Long): String = {

    val zeroToNineteen = Array("",
      "ein",
      "zwei",
      "drei",
      "vier",
      "fünf",
      "sechs",
      "sieben",
      "acht",
      "neun",
      "zehn",
      "elf",
      "zwölf",
      "dreizehn",
      "vierzehn",
      "fünfzehn",
      "sechszehn",
      "siebzehn",
      "achtzehn",
      "neunzehn" )

    val multiplesOfTen = Array("",
      "zehn",
      "zwanzig",
      "dreissig",
      "vierzig",
      "fünfzig",
      "sechszig",
      "siebzig",
      "achtzig",
      "neunzig" )

    val powersOfThousand = Array("",
      "tausend",
      "millionen",
      "milliarden",
      "billionen",
      "billiarden",
      "trillionen",
      "trilliarden",
      "quadrillionen",
      "quadrilliarden",
      "quintillionen"
    )

    val absValue = Math.abs(value)
    var powerOfTen: Long = 1
    var result: List[String] = List()
    var n = 0
    while (absValue / powerOfTen > 0) {
      val triplet = (absValue / powerOfTen) % 1000
      if (triplet > 0)
        result = powersOfThousand(n) :: result
      n += 1
      val lastTwoDigits = (triplet % 100).toInt
      if (lastTwoDigits < 20) {
        if (n > 2 && lastTwoDigits == 1) {
          var mill = result.head
          mill = if (mill.endsWith("den")) mill.take(mill.length-1)
                 else mill.take(mill.length-2)
          result = mill :: result.tail
          result = "eine" :: result
        }
        else
          result = zeroToNineteen(lastTwoDigits) :: result
      }
      else {
        result = multiplesOfTen(lastTwoDigits / 10) :: result
        result = "und" :: result
        result = zeroToNineteen(lastTwoDigits % 10) :: result
      }
      val firstDigit = (triplet / 100).toInt
      if (firstDigit > 0) {
        result = "hundert" :: result
        result = zeroToNineteen(firstDigit) :: result
      }
      powerOfTen *= 1000
    }
    var stringResult = result.mkString
    if (stringResult.endsWith("ein"))
      stringResult += "s"
    stringResult = stringResult.capitalize
    if (value < 0)
      stringResult = "minus " + stringResult
    stringResult
  }

  /** Verbalises a <code>long</code> number in Spanish.
   *  Uses the échelle longue, e.g. "mil milliones" for 1,000,000,000
   *  and returns empty string for zero.
   *
   *  @param value the number to verbalise.
   *  @return the verbalised number.
   */
  private def toWords_es(value: Long): String = {

    val zeroToTwentyNine = Array("",
      "uno",
      "dos",
      "tres",
      "cuatro",
      "cinco",
      "seis",
      "siete",
      "ocho",
      "nueve",
      "diez",
      "once",
      "doce",
      "trece",
      "catorce",
      "quince",
      "dieziséis",
      "diezisiete",
      "dieziocho",
      "diezinueve",
      "veinte",
      "veintiuno",
      "veintidos",
      "veintitrés",
      "veinticuatro",
      "veinticinco",
      "veintiséis",
      "veintisiete",
      "veintiocho",
      "veintinueve"
    )

    val multiplesOfTen = Array("",
      "diez",
      "veinte",
      "treinta",
      "cuarenta",
      "cincuenta",
      "sesenta",
      "setenta",
      "ochenta",
      "noventa" )

    val powersOfThousand = Array("",
      "mil",
      "millón",
      "mil millones",
      "billón",
      "mil billones",
      "trillón",
      "mil trillones",
      "cuatrillón",
      "mil cuatrillones",
      "quintillón"
    )

    val absValue = Math.abs(value)
    var result: List[String] = List()
    var powerOfTen: Long = 1
    var n = 0
    while (absValue / powerOfTen > 0) {
      val triplet = (absValue / powerOfTen) % 1000
      if (triplet > 0)
        result = powersOfThousand(n) :: result
      n += 1
      if (n > 1 && triplet > 1) {
        var mill = result.head
        if (mill.endsWith("ón")) {
          mill = mill.take(mill.length-2) + "ones"
          result = mill :: result.tail
        }
      }
      val lastTwoDigits = (triplet % 100).toInt
      if (lastTwoDigits < 30)
        result = zeroToTwentyNine(lastTwoDigits) :: result
      else {
        val ones = lastTwoDigits % 10
        result = zeroToTwentyNine(ones) :: result
        if (ones > 0)
          result = "y" :: result
        result = multiplesOfTen(lastTwoDigits / 10) :: result
      }
      val firstDigit = (triplet / 100).toInt
      if (firstDigit > 0) {
        if (firstDigit == 1) {
          if (lastTwoDigits == 0)
            result = "cien" :: result
          else
            result = "ciento" :: result
        }
        else
          result = zeroToTwentyNine(firstDigit) + "cientos" :: result
      }
      powerOfTen *= 1000
    }
    if (result.head == "uno") {
      if (result.tail.head == "mil")
        result = result.tail
      else
        result = "un" :: result.tail
    }
    if (value < 0)
      result = "menos" :: result
    result.filter(!_.isEmpty).mkString(" ")
  }

  /** Verbalises a <code>long</code> number in French.
   *  Uses the échelle courte, e.g. "milliarde" for 1,000,000,000
   *  and returns empty string for zero.
   *
   *  @param value the number to verbalise.
   *  @return the verbalised number.
   */
  private def toWords_fr(value: Long): String = {

    val zeroToNineteen = Array("",
      "un",
      "deux",
      "trois",
      "quatre",
      "cinq",
      "six",
      "sept",
      "huit",
      "neuf",
      "dix",
      "onze",
      "douze",
      "treize",
      "quatorze",
      "quinze",
      "seize",
      "dix-sept",
      "dix-huit",
      "dix-neuf"
    )

    val multiplesOfTen = Array("",
      "dix",
      "vingt",
      "trente",
      "quarante",
      "cinquante",
      "soixante",
      "soixante-dix",
      "quatre-vingts",
      "quatre-vingt-dix" )

    val powersOfThousand = Array("",
      "mille",
      "million",
      "milliard",
      "billion",
      "billiard",
      "trillion",
      "trilliard",
      "quatrillion",
      "quatrilliard",
      "quintillion"
    )

    val absValue = Math.abs(value)
    var result: List[String] = List()
    var powerOfTen: Long = 1
    var n = 0
    while (absValue / powerOfTen > 0) {
      val triplet = (absValue / powerOfTen) % 1000
      if (triplet > 0) {
        var thousands = powersOfThousand(n)
        if (triplet > 1 && n > 1)
          thousands += "s"
        result =  thousands :: result
      }
      n += 1
      val lastTwoDigits = (triplet % 100).toInt
      if (lastTwoDigits < 20) {
        if (!(triplet == 1 && n == 2))
          result = zeroToNineteen(lastTwoDigits) :: result
      }
      else if (lastTwoDigits > 80)
        result = "quatre-vingt-" + zeroToNineteen(lastTwoDigits-80) :: result
      else if (lastTwoDigits == 80)
        result = "quatre-vingts" :: result
      else if (lastTwoDigits == 71)
        result = "soixante et onze" :: result
      else if (lastTwoDigits > 61)
        result = "soixante-" + zeroToNineteen(lastTwoDigits-60) :: result
      else {
        val ones = lastTwoDigits % 10
        if (ones == 0)
          result = multiplesOfTen(lastTwoDigits / 10) :: result
        else if (ones == 1) {
          result = "et un" :: result
          result = multiplesOfTen(lastTwoDigits / 10) :: result
        }
        else
          result = multiplesOfTen(lastTwoDigits / 10) + "-" + zeroToNineteen(ones) :: result
      }
      val firstDigit = (triplet / 100).toInt
      if (firstDigit > 0) {
        if (firstDigit == 1)
          result = "cent" :: result
        else {
          result = "cents" :: result
          result = zeroToNineteen(firstDigit) :: result
        }
      }
      powerOfTen *= 1000
    }
    if (value < 0)
      result = "moins" :: result
    result.filter(!_.isEmpty).mkString(" ")
  }
}
