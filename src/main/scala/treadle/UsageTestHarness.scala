/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package treadle

import java.io.File
import java.nio.file.{Files, Paths}

import firrtl.{AnnotationSeq, FileUtils}
import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}

import scala.util.Random

object WireUsageTest {
  val NoAnnotations: AnnotationSeq = Seq()
  val ReportUsageOnly: AnnotationSeq = Seq(ReportUsageAnnotation)
  val VcdOnly: AnnotationSeq = Seq(
    WriteVcdAnnotation,
    TargetDirAnnotation("usage_vcds"),
    OutputFileAnnotation("usage_out.vcd")
  )
  val AllAnnotations: AnnotationSeq = ReportUsageOnly ++ VcdOnly

  def testWithPermutedAnnotations(firrtlSrc: String, testFn: TreadleTester => Unit) {
    Seq(NoAnnotations, ReportUsageOnly, VcdOnly, AllAnnotations).foreach {
      annotations: AnnotationSeq =>
        val s = s"=== Testing with annotations: ${Console.GREEN +
          annotations.map { _.serialize }.mkString(", ") + Console.RESET} ==="
        println(s) // scalastyle:ignore
        val tester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtlSrc)) ++ annotations)
        testFn(tester)
    }
  }

  /**
    * Compares the results of mark and sweep with the usage analyzer vs mark and sweep without.
    *
    * @param firrtlSrc the string containing the firrtl source code
    * @param cycles the number of cycles for which to simulate
    * @param testFn the test function
    */
  def compareWithStatic(firrtlSrc: String, cycles: Int, testFn: TreadleTester => Unit): Unit = {
    println("=== Commencing test comparing with static ===") // scalastyle:ignore
    val noUsageTester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtlSrc)))
    val usageTester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtlSrc)) ++ ReportUsageOnly)
    val wireNames = getInterestingWireNames(noUsageTester) // same for both
    val wireNamesStr = if (wireNames.size < 10) wireNames.mkString(", ") else s"<${wireNames.size} wires>"
    println(s"=== Found wires: $wireNamesStr ===") // scalastyle:ignore
    testFn(noUsageTester)
    testFn(usageTester)
    // 3-tuples of total (usage), found (usage/nousage), lastTen (usage/nousage)
    val resultsTuple = wireNames.map { wireName =>
      val n = noUsageTester.findDependenciesOf(wireName, cycles)
      val u = usageTester.findDependenciesOf(wireName, cycles)
      (u.totalWires, u.foundWires.toDouble / n.foundWires.toDouble, u.lastTenCycles.toDouble / n.lastTenCycles.toDouble)
    }
    val tots = resultsTuple.foldRight((0.0, 0.0))((v, acc) => (v._2 + acc._1, v._3 + acc._2))
    val avgs = (tots._1 / wireNames.length, tots._2 / wireNames.length)
    println(s"=== ${Console.GREEN}total: ${resultsTuple(0)._1}, avg found % diff: ${avgs._1}, avg ten cycle found % diff: ${avgs._2}")
  }

  /**
    * Runs the test and performs mark and sweep with different root sets.
    * The tester is only run once.
    * The different root sets are each singleton sets of "interesting" wires: see [getInterestingWireNames].
    *
    * @param firrtlSrc the string containing the firrtl source code
    * @param cycles the number of cycles for which to simulate
    * @param testFn the test function
    */
  def testWithInterestingWires(firrtlSrc: String, cycles: Int, testFn: TreadleTester => Unit) {
    println("=== Commencing test with permuted root sets ===") // scalastyle:ignore
    // No VCD annotation in the interest of time
    val tester = TreadleTester(Seq(FirrtlSourceAnnotation(firrtlSrc)) ++ ReportUsageOnly)
    val wireNames = getInterestingWireNames(tester)
    val wireNamesStr = if (wireNames.size < 10) wireNames.mkString(", ") else s"<${wireNames.size} wires>"
    println(s"=== Found wires: $wireNamesStr ===") // scalastyle:ignore
    testFn(tester)
    val results = wireNames.map { wireName => tester.findDependenciesOf(wireName, cycles) }
    val (min, avg, max) = MarkAndSweepResult.min_avg_max(results)
    println(s"=== ${Console.GREEN}max: $max, min: $min, avg: $avg ===${Console.RESET}") // scalastyle:ignore
  }

  /**
    * Returns the names of all outputs, named wires, and registers in the circuit. This excludes inputs, register/in
    * wires, clocks, and GEN and _T wires.
    *
    * @param tester the tester containing the symbol table to examine
    * @return a sequence of wire names
    */
  def getInterestingWireNames(tester: TreadleTester): Seq[String] = {
    val symbolTable = tester.engine.symbolTable
    val wireNames = symbolTable.nameToSymbol
      .keysIterator
      .filterNot(
        name =>
          symbolTable.instanceNames.contains(name) ||
            name.contains("GEN") ||
            name.contains("_T_") ||
            name.contains("T_") ||
            name.endsWith("/in") ||
            name.contains("clk") ||
            name.contains("clock")
      )
    val inputNames = symbolTable.inputPortsNames
    wireNames.toSet.diff(inputNames).toSeq
  }
}

object UsageTestHarness extends App {
  def gcdFirrtl(width: Int): String =
    s"""
       |circuit GCD :
       |  module GCD :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    input io_a : UInt<$width>
       |    input io_b : UInt<$width>
       |    input io_e : UInt<1>
       |    output io_z : UInt<$width>
       |    output io_v : UInt<1>
       |    reg x : UInt<$width>, clock with :
       |      reset => (UInt<1>("h0"), x)
       |    reg y : UInt<$width>, clock with :
       |      reset => (UInt<1>("h0"), y)
       |    node T_13 = gt(x, y)
       |    node T_14 = sub(x, y)
       |    node T_15 = tail(T_14, 1)
       |    node T_17 = eq(T_13, UInt<1>("h0"))
       |    node T_18 = sub(y, x)
       |    node T_19 = tail(T_18, 1)
       |    node T_21 = eq(y, UInt<1>("h0"))
       |    node GEN_0 = mux(T_13, T_15, x)
       |    x <= mux(io_e, io_a, GEN_0)
       |    node GEN_1 = mux(T_17, T_19, y)
       |    y <= mux(io_e, io_b, GEN_1)
       |    io_z <= x
       |    io_v <= T_21
    """.stripMargin

  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while (y > 0) {
      if (x > y) {
        x -= y
      } else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }

  def sizableTest(tester: TreadleTester) {
    val values =
      for {
        x <- 10 to 100
        y <- 10 to 100
      } yield (x, y, computeGcd(x, y)._1)

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    for ((x, y, z) <- values) {
      tester.step()
      tester.poke("io_a", x)
      tester.poke("io_b", y)
      tester.poke("io_e", 1)
      tester.step()

      tester.poke("io_e", 0)
      tester.step()

      var count = 0
      while (tester.peek("io_v") != Big1) {
        count += 1
        tester.step()
      }

      tester.expect("io_z", BigInt(z))
    }
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0
    tester.report()
  }

  def runDataPath(tester: TreadleTester) {
    val iterCount = 20 // inst is of size 50
    val rng = Random
    // Ripped from reading the inputs of RiscVMini's TestUtils.insts
    // For some stupid reason, vals in object aren't initialized?
    val insts =
      """
        |478570167
        |38563735
        |2915248239
        |3564768999
        |1907427299
        |3110179171
        |3085616867
        |1020878307
        |947184995
        |2672195555
        |365104259
        |3435043587
        |1688743427
        |2329559427
        |3008912259
        |4291988003
        |3237846947
        |3597413667
        |3819537939
        |1459496467
        |2312550675
        |1908297363
        |1796040851
        |2151578131
        |27401619
        |28431123
        |1076846355
        |22677043
        |1089013939
        |10066483
        |13443507
        |13023539
        |31900211
        |26171059
        |1080186675
        |23978803
        |28865203
        |267386895
        |4111
        |810161523
        |874391155
        |3223239539
        |808017907
        |809625331
        |2551478515
        |115
        |1048691
        |268435571
        |19
        |908752575
        |""".stripMargin.trim.split("\n").map { _.toLong }

    tester.poke("reset", 1)
    for (_ <- 0 until iterCount) {
      for (inst <- insts) {
        tester.poke("io_icache_resp_bits_data", inst)
        tester.poke("io_icache_resp_valid", 1)
        tester.poke("io_dcache_resp_bits_data", rng.nextInt())
        tester.poke("io_dcache_resp_valid", 1)
        tester.step()
      }
    }
    tester.report()
  }

  val WIRES = 0
  val ANNOS = 1
  val COMP = 2

  // scalastyle:off cyclomatic.complexity
  override def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Usage: UsageTestHarness gcd|dpath wires|annos|comp")
      System.exit(1)
    }
    val argChoice = args(1) match {
      case "wires" => 0
      case "annos" => 1
      case "comp" => 2
      case _ => throw new Exception("Bad test option")
    }
//    println("CHOICE " + argChoice.toString + args(1))
    args(0) match {
      case "gcd" => runGcd(argChoice)
      case "dpath" => runDpath(argChoice)
      case _ => throw new Exception("Bad circuit option")
    }
  }

  def runner(choice: Int, firrtl: String, testFn: TreadleTester => Unit): Unit = {
    val cycles = 1000
    choice match {
      case 0 => WireUsageTest.testWithInterestingWires(firrtl, cycles, testFn)
      case 1 => WireUsageTest.testWithPermutedAnnotations(firrtl, testFn)
      case 2 => WireUsageTest.compareWithStatic(firrtl, cycles, testFn)
    }
  }

  def runGcd(choice: Int): Unit = {
    runner(choice, gcdFirrtl(16), sizableTest)
  }

  def runDpath(choice: Int): Unit = {
    val dpathFirrtl = FileUtils.getText(Paths.get("Datapath.fir").toFile)
    runner(choice, dpathFirrtl, runDataPath)
  }
}
