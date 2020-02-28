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
      |    input reset : UInt<1>
      |    input sel : UInt<1>
      |    input a : UInt<1>
      |    input shouldPrint : UInt<1>
      |    output q : UInt<1>
      |
      |    reg m : UInt<1>, clk
      |    reg c : UInt<8>, clk
      |    q <= mux(sel, m, a)
      |    m <= mux(reset, UInt(0), and(a, q))
      |    c <= mux(reset, UInt(0), add(c, UInt(1)))
      |    printf(clk, shouldPrint, "c=%d, a=%d, sel=%d, m=%d, q=%d\n", c, a, sel, m, q)
      |
      |""".stripMargin

  private val input = unclockedInput

  "internal prints should show up" in {
    val inputs = List(
      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
      Map("a" -> 1, "sel" -> 0, "shouldPrint" -> 1),
//      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
//      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
//      Map("a" -> 1, "sel" -> 1, "shouldPrint" -> 1),
//      Map("a" -> 0, "sel" -> 1, "shouldPrint" -> 1),
    )
    Console.withOut(Console.out) {
      val tester = TreadleTester(
        Seq(FirrtlSourceAnnotation(input),
          CallResetAtStartupAnnotation,
          WriteVcdAnnotation,
          VerboseAnnotation
        ))
      println(s"${Console.RESET}${Console.GREEN}STARTING TEST")
      for (inputMap <- inputs) {
        inputMap foreach {case (wire, value) => tester.poke(wire, value)}
        tester.step()
      }
      tester.finish
    }
  }
}
