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

import java.io.{ByteArrayOutputStream, PrintStream}

import firrtl.stage.FirrtlSourceAnnotation
import logger.{LazyLogging, LogLevel, Logger}
import org.scalatest.{FreeSpec, Matchers}

class InternalPrints extends FreeSpec with Matchers with LazyLogging
{

  private val input =
    """
      |circuit AndFeedback:
      |  module AndFeedback :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input sel : UInt<1>
      |    input a : UInt<1>
      |    input shouldPrint : UInt<1>
      |    output q : UInt<1>
      |
      |    reg m : UInt<1>, clk with : (reset => (reset, UInt(0)))
      |    reg c : UInt<8>, clk with : (reset => (reset, UInt<8>(0)))
      |    q <= mux(sel, m, a)
      |    m <= mux(reset, UInt(0), and(a, q))
      |    c <= add(c, UInt(1))
      |    printf(clk, shouldPrint, "c=%d, a=%d, sel=%d, m=%d, q=%d\n", c, a, sel, m, q)
      |
      |""".stripMargin

  "internal prints should show up" in {
    val output = new ByteArrayOutputStream()
    val inputs = List(
      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
      Map("a" -> 1, "sel" -> 0, "shouldPrint" -> 1),
      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
      Map("a" -> 0, "sel" -> 1, "shouldPrint" -> 1),
    )
    Console.withOut(new PrintStream(output)) {
      val tester = TreadleTester(Seq(FirrtlSourceAnnotation(input), CallResetAtStartupAnnotation, WriteVcdAnnotation))
      for (inputMap <- inputs) {
        inputMap foreach {case (wire, value) => tester.poke(wire, value)}
        tester.step()
      }
      tester.finish
    }
    Logger.setLevel("treadle.InternalPrints", LogLevel.Debug)
    logger.debug(output.toString)
  }
}
