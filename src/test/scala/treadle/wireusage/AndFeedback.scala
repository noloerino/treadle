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

import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import logger.LazyLogging
import org.scalatest.{FreeSpec, Matchers}
import treadle.{ReportUsageAnnotation, TreadleTester, WriteVcdAnnotation}

class AndFeedback extends FreeSpec with Matchers with LazyLogging {

  private val unclockedInput = """
      |circuit AndNoFeedback:
      |  module AndNoFeedback:
      |    input clk : Clock
      |    input sel : UInt<1>
      |    input a : UInt<1>
      |    input shouldPrint : UInt<1>
      |    output q : UInt<1>
      |
      |    wire m : UInt<1>
      |    m <= UInt(1)
      |    q <= mux(sel, m, a)
      |    printf(clk, shouldPrint, "a=%d, sel=%d, m=%d, q=%d\n", a, sel, m, q)
      |
      |""".stripMargin

  private val clockedInput = """
      |circuit AndFeedback:
      |  module AndFeedback:
      |    input clk : Clock
      |    input sel : UInt<1>
      |    input a : UInt<1>
      |    output q : UInt<1>
      |
      |    reg m : UInt<1>, clk
      |    q <= mux(sel, m, a)
      |    m <= and(a, q)
      |
      |""".stripMargin

  private val input = clockedInput

  "internal prints should show up" in {
    val inputs = List(
      Map("a" -> 1, "sel" -> 1),
      Map("a" -> 1, "sel" -> 0),
      Map("a" -> 1, "sel" -> 1),
      Map("a" -> 1, "sel" -> 1),
      Map("a" -> 1, "sel" -> 1),
      Map("a" -> 0, "sel" -> 1),
    )
    WireUsageTest.testWithInterestingWires(input, 5, {
      tester =>
        for (inputMap <- inputs) {
          inputMap foreach {case (wire, value) => tester.poke(wire, value)}
          tester.step()
        }
        tester.findDependentsOf("q", 5, verbose = true)
        println(tester.usageReporter.reportUsedFraction)
    })
    // Expected result:
    /*
    """
      |*** At finish, examined symbol q @ 5; found dependencies on:
      |	sel @ 5
      |	m @ 5
      |	sel @ 4
      |	q @ 4
      |	m @ 4
      |	a @ 4
      |	sel @ 3
      |	q @ 3
      |	m @ 3
      |	a @ 3
      |	sel @ 2
      |	q @ 2
      |	a @ 2
      |""".stripMargin
     */
  }
}
