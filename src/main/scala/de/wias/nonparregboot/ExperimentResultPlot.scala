package de.wias.nonparregboot

import java.io.File

import cats._
import cats.data._
import cats.implicits._

import breeze.linalg.linspace
import breeze.numerics.log2
import breeze.plot.PaintScale.{black, blue, cyan, green, magenta, red, white, yellow}
import breeze.plot.{Figure, plot}
import de.wias.nonparregboot.ToDV._

case class Result(t: Int, P: Int, rmse: Double, coverage: Double, interval: (Double, Double))

object ExperimentResultPlot extends App {
  def getInt(s: String): Int = {
    s.split('=')(1).toInt
  }

  def getDouble(s: String): Double = {
    s.split('=')(1).toDouble
  }

  def getInterval(s: String): (Double, Double) = {
    val l :: u :: Nil = s.tail.init.split(',').map(_.toDouble).toList
    (l, u)
  }

  def parse(s: String) = {
    val split = s.split("\\s+")
    val t = getInt(split(1))
    val P = getInt(split(2))
    val rmse = getDouble(split(3))
    val coveragee = getDouble(split(4))

    Result(t, P, rmse, coveragee, getInterval(split.last))
  }

  def plotline(results: List[Result], color: Char, t: Int) = {
    val ps = log2(results.map(_.P.toDouble).toDV)
    val probs = results.map(_.coverage).toDV
    plot(ps, probs, colorcode = "" + color, name = t.toString)
  }

  val results = io.Source.fromFile(new File("results")).getLines().toList.map(parse)
  val groupedByT = results.groupBy(_.t).toList.sortBy(_._1).map{ case(t, rs) => (t, rs.sortBy(_.P)) }
  groupedByT.unzip._2.map(_.map(_.coverage).mkString(", ")).map(s => "[" + s+"]").foreach(println)

  implicit val intervalshow = new Show[(Double, Double)] {
    override def show(t: (Double, Double)): String = f"$$(${t._1}%.2f,${t._2}%.2f)$$"
  }

  groupedByT.map {
    case(ts, results) =>  "$2^" + log2(ts).toInt + "$" + " & " + results.sortBy(_.P).map(_.interval.show).mkString(" & ") + " \\\\ " }.foreach(println)

}
