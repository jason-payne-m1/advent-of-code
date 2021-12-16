package advent.of.code.twentytwentyone

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Advent2021_11_2 {

//  val input = Seq("5483143223","2745854711","5264556173","6141336146","6357385478","4167524645","2176841721","6882881134","4846848554","5283751526")

  val input = Seq("6788383436","5526827441","4582435866","5152547273","3746433621","2465145365","6324887128","8537558745","4718427562","2283324746")

  val octopi = input.map(_.split("").map(_.toInt)).toArray

  var counter = 0

  def main(args: Array[String]): Unit = {
    var steps = 1
    breakable {
      while (true) {
        try {
          step(steps)
          steps += 1
        } catch {
          case e: Exception => break
        }
      }
    }
  }

  private def step(step: Int): Unit = {
    val hasFlashed = mutable.Map[(Int, Int), Boolean]()
    increaseEnergy(1)

    while(anyWillFlash(hasFlashed)) {
      for (i <- octopi(0).indices) {
        for (j <- octopi.indices) {
          if(octopi(j)(i) > 9) {
            if(!hasFlashed.contains((j, i))) {
              hasFlashed.put((j, i), true)
              counter += 1
              flash(i, j)
            }
          }
        }
      }
    }
    resetFlash
    allFlashed(hasFlashed, step)
  }

  private def flash(x: Int, y: Int): Unit = {
    octopi(y)(x) += 1

    val directions = Seq[(Int, Int)]((-1, -1), (-1, 0), (-1, 1), (0, 1), (0, -1), (1, 0), (1, -1), (1, 1))

    directions.foreach(direction => {
      try {
        octopi(y + direction._1)(x + direction._2) += 1
      } catch {
        case _ =>
      }
    })
  }

  private def anyWillFlash(hasFlashed: mutable.Map[(Int, Int), Boolean]): Boolean = {
    var anyFlash = false
    for (i <- octopi(0).indices) {
      for (j <- octopi.indices) {
        if (octopi(j)(i) > 9 && !hasFlashed.contains((j, i))) {
          anyFlash = true
        }
      }
    }
    anyFlash
  }

  private def allFlashed(hasFlashed: mutable.Map[(Int, Int), Boolean], step: Int): Unit = {
    if(hasFlashed.keys.size == 100) {
      viewOctopi()
      println()
      println(s"ALL FLASHED AT STEP: $step")
      throw new Exception("FOUND IT")
    }
//    if(octopi.map(_.sum).sum) {}
  }

  private def increaseEnergy(amount: Int): Unit = {
    for (i <- octopi(0).indices) {
      for (j <- octopi.indices) {
        octopi(j)(i) += 1
      }
    }
  }

  private def resetFlash: Unit = {
    for (i <- octopi(0).indices) {
      for (j <- octopi.indices) {
        if(octopi(j)(i) > 9) {
          octopi(j)(i) = 0
        }
      }
    }
  }

  private def viewOctopi(): Unit = {
    for (elem <- octopi) {
      for (elem <- elem) {
        print(elem)
      }
      println()
    }
  }

}
