package advent.of.code.twentytwentyone

import scala.collection.mutable

object Advent2021_14_1 {



  val polymerTemplate = "OOFNFCBHCKBBVNHBNVCP"
  
  val pairInsertionRules = Map[String, String]("PH" -> "V","OK" -> "S","KK" -> "O","BV" -> "K","CV" -> "S","SV" -> "C","CK" -> "O","PC" -> "F","SC" -> "O","KC" -> "S","KF" -> "N","SN" -> "C","SF" -> "P","OS" -> "O","OP" -> "N","FS" -> "P","FV" -> "N","CP" -> "S","VS" -> "P","PB" -> "P","HP" -> "P","PK" -> "S","FC" -> "F","SB" -> "K","NC" -> "V","PP" -> "B","PN" -> "N","VN" -> "C","NV" -> "O","OV" -> "O","BS" -> "K","FP" -> "V","NK" -> "K","PO" -> "B","HF" -> "H","VK" -> "S","ON" -> "C","KH" -> "F","HO" -> "P","OO" -> "H","BC" -> "V","CS" -> "O","OC" -> "B","VB" -> "N","OF" -> "P","FK" -> "H","OH" -> "H","CF" -> "K","CC" -> "V","BK" -> "O","BH" -> "F","VV" -> "N","KS" -> "V","FO" -> "F","SH" -> "F","OB" -> "O","VH" -> "F","HH" -> "P","PF" -> "C","NF" -> "V","VP" -> "S","CN" -> "V","SK" -> "O","FB" -> "S","FN" -> "S","BF" -> "H","FF" -> "V","CB" -> "P","NN" -> "O","VC" -> "F","HK" -> "F","BO" -> "H","KO" -> "C","CH" -> "N","KP" -> "C","HS" -> "P","NP" -> "O","NS" -> "V","NB" -> "H","HN" -> "O","BP" -> "C","VF" -> "S","KN" -> "P","HC" -> "C","PS" -> "K","BB" -> "O","NO" -> "N","NH" -> "F","BN" -> "F","KV" -> "V","SS" -> "K","CO" -> "H","KB" -> "P","FH" -> "C","SP" -> "C","SO" -> "V","PV" -> "S","VO" -> "O","HV" -> "N","HB" -> "V")

  def main(args: Array[String]): Unit = {
    var polymer = polymerTemplate
    for (i <- 0 until 10) {
      polymer = step(polymer)
    }

    val occurencesMap = createOccurencesMap(polymer)
    val occurencesMapValues = occurencesMap.values
    val min = occurencesMapValues.min
    val max = occurencesMapValues.max
    println(max - min)
  }

  private def step(polymer: String): String = {
    val lastVal = polymer.takeRight(1)
    polymer.sliding(2, 1).toList.flatMap {
      pair =>
        val insertionVal = pairInsertionRules.getOrElse(pair, "")
        pair.take(1).concat(insertionVal)
    }.mkString("").concat(lastVal)
  }

  private def createOccurencesMap(s: String): mutable.Map[String, Int] = {
    val occurencesMap = mutable.Map[String, Int]()

    s.split("").foreach {
      c => {
        occurencesMap.put(c, 1 + occurencesMap.getOrElse(c, 0))
      }
    }

    occurencesMap
  }
  

}
