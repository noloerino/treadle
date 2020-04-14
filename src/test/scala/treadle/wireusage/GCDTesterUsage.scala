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

package treadle.wireusage

import firrtl.AnnotationSeq
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import org.scalatest.{FlatSpec, Matchers}
import treadle.{Big1, ReportUsageAnnotation, TreadleTester, WriteVcdAnnotation}

// scalastyle:off magic.number

class GCDTesterUsage extends FlatSpec with Matchers {
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

  behavior.of("GCD")

  def getFirrtl(width: Int): String =
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

  //scalastyle:off
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

  //scalastyle:off
  def manyValuesTest(tester: TreadleTester) {
    val values =
      for {
        x <- 1 to 100
        y <- 1 to 100
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

      while (tester.peek("io_v") != Big1) {
        tester.step()
      }

      tester.expect("io_z", z)
    }
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    val cycle = tester.cycleCount

    println(
      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
    tester.report()
    tester.finish
    println(tester.usageReporter.reportUsedFraction)
  }

  val CYCLES = 1000

  it should "run with InterpretedTester at Int size 16" in {
    WireUsageTest.testWithPermutedAnnotations(getFirrtl(16), sizableTest)
    WireUsageTest.testWithInterestingWires(getFirrtl(16), CYCLES, sizableTest)
  }

  it should "run with InterpretedTester at Int size 44" in {
    WireUsageTest.testWithPermutedAnnotations(getFirrtl(44), sizableTest)
    WireUsageTest.testWithInterestingWires(getFirrtl(44), CYCLES, sizableTest)
  }

  it should "run with InterpretedTester at size 68" in {
    WireUsageTest.testWithPermutedAnnotations(getFirrtl(68), sizableTest)
    WireUsageTest.testWithInterestingWires(getFirrtl(68), CYCLES, sizableTest)
  }

  it should "run a lot of values" in {
    WireUsageTest.testWithPermutedAnnotations(getFirrtl(24), manyValuesTest)
    WireUsageTest.testWithInterestingWires(getFirrtl(24), CYCLES, sizableTest)
  }
}
