package analysis

import common._
import scala.io.Source

object Analysis {
  val filename = "/Users/Alex/Documents/EPFL/BA4/Parallelism_Concurrency/Test2/src/main/resources/logs.log"
  private final val CORE_NUMBER = 8 // Number of core in my computer

  def parAnalysis(arr: Array[String], threshold: Int, data: String): Int = {
    // Parallel analysis
    def analysis(from: Int, to: Int): Int = {
      if (to - from <= threshold) seqAnalysis(from, to)
      else {
        val q = from + (to - from) / 2
        val (c1, c2) = parallel(analysis(from, q), analysis(q, to))
        c1 + c2
      }
    }

    // Sequential analysis
    def seqAnalysis(from: Int, to: Int): Int = {
      def loop(curr: Int, acc: Int): Int = {
        if (curr == to) acc
        else if (arr(curr).contains(data)) loop(curr + 1, acc + 1)
        else loop(curr + 1, acc)
      }
      loop(from, 0)
    }

    analysis(0, arr.length)
  }

  def findData(toFind: String): Unit = {
    try {
      // val errors = Source.fromFile(filename).getLines().count(l => l.contains(criticalMsg))
      val data = Source.fromFile(filename).getLines().toArray
      val threshold = data.length / CORE_NUMBER
      println(toFind + " : " + parAnalysis(data, threshold, toFind))
    } catch {
      case ex: Exception => println(ex.getMessage)
    }
  }
}

object main {
  def main(args: Array[String]): Unit = {
    Analysis.findData(DataName.Error.toString)
  }
}