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

import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}

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
    val results = wireNames.map { wireName => tester.findDependentsOf(wireName, cycles) }
    val max = results.max
    val min = results.min
    val avg = results.sum / results.length
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

    val cycle = 11 // tester.engine.circuitState.stateCounter

    println(
      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
    tester.report()

  }


  override def main(args: Array[String]): Unit = {
    val tester = WireUsageTest.testWithPermutedAnnotations(
      gcdFirrtl(16),
      sizableTest
    )
  }
}
