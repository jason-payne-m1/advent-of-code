package advent.of.code.twentytwentyone

import scala.collection.mutable

object Advent2021_14_2 {

  val polymerTemplate = "OOFNFCBHCKBBVNHBNVCP"
  
  val pairInsertionRules: Map[String, String] = Map[String, String]("PH" -> "V","OK" -> "S","KK" -> "O","BV" -> "K","CV" -> "S","SV" -> "C","CK" -> "O","PC" -> "F","SC" -> "O","KC" -> "S","KF" -> "N","SN" -> "C","SF" -> "P","OS" -> "O","OP" -> "N","FS" -> "P","FV" -> "N","CP" -> "S","VS" -> "P","PB" -> "P","HP" -> "P","PK" -> "S","FC" -> "F","SB" -> "K","NC" -> "V","PP" -> "B","PN" -> "N","VN" -> "C","NV" -> "O","OV" -> "O","BS" -> "K","FP" -> "V","NK" -> "K","PO" -> "B","HF" -> "H","VK" -> "S","ON" -> "C","KH" -> "F","HO" -> "P","OO" -> "H","BC" -> "V","CS" -> "O","OC" -> "B","VB" -> "N","OF" -> "P","FK" -> "H","OH" -> "H","CF" -> "K","CC" -> "V","BK" -> "O","BH" -> "F","VV" -> "N","KS" -> "V","FO" -> "F","SH" -> "F","OB" -> "O","VH" -> "F","HH" -> "P","PF" -> "C","NF" -> "V","VP" -> "S","CN" -> "V","SK" -> "O","FB" -> "S","FN" -> "S","BF" -> "H","FF" -> "V","CB" -> "P","NN" -> "O","VC" -> "F","HK" -> "F","BO" -> "H","KO" -> "C","CH" -> "N","KP" -> "C","HS" -> "P","NP" -> "O","NS" -> "V","NB" -> "H","HN" -> "O","BP" -> "C","VF" -> "S","KN" -> "P","HC" -> "C","PS" -> "K","BB" -> "O","NO" -> "N","NH" -> "F","BN" -> "F","KV" -> "V","SS" -> "K","CO" -> "H","KB" -> "P","FH" -> "C","SP" -> "C","SO" -> "V","PV" -> "S","VO" -> "O","HV" -> "N","HB" -> "V")

  val pairsMap: mutable.Map[String, Long] = mutable.Map[String, Long]()
  polymerTemplate.sliding(2).toSeq.foreach(pair => pairsMap.put(pair, 1L))
  val countMap: mutable.Map[String, Long] = mutable.Map[String, Long]()
  polymerTemplate.split("").foreach(s => countMap.put(s, 1L + countMap.getOrElse(s, 0L)))

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 40) {
      step(i)
    }

    val min = countMap.values.min
    val max = countMap.values.max
    println(max - min)
  }

  private def step(step: Int): Unit = {
    println(s"Step: $step")
    val pairsForStep = Map.from(pairsMap)
    pairsForStep.foreach {
      case (pair, count) => {
        val insertionVal = pairInsertionRules.get(pair).head
        pairsMap.put(pair, pairsMap.getOrElse(pair, 1L) - count)
        pairsMap.put(pair.take(1).concat(insertionVal), pairsMap.getOrElse(pair.take(1).concat(insertionVal), 0L) + count)
        pairsMap.put(insertionVal.concat(pair.takeRight(1)), pairsMap.getOrElse(insertionVal.concat(pair.takeRight(1)), 0L) + count)
        countMap.put(insertionVal, countMap.getOrElse(insertionVal, 0L) + count)
      }
    }
  }
}
