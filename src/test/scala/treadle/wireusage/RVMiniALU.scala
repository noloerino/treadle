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
import org.scalatest.{FreeSpec, Matchers}
import treadle.executable.ClockInfo
import treadle.{ClockInfoAnnotation, TreadleTester, WriteVcdAnnotation}

abstract case class ALUCode(id: Int)

object ALUCode extends Enumeration {
  val Add, Sub, And, Or, Xor, Slt, Sll, Sltu, Srl, Sra, CpA, CpB = Value

  type ALUCode = Value

  // scalastyle:off cyclomatic.complexity
  def compute(aluCode: ALUCode, a: Int, b: Int): Int = {
    aluCode match {
      case Add => a + b
      case Sub => a - b
      case And => a & b
      case Or => a | b
      case Xor => a ^ b
      case Slt => if (a < b) 1 else 0
      case Sll => a << b
      case Sltu => if (a < b) 1 else 0 // TODO figure out how to properly do unsigned
      case Srl => a >>> b
      case Sra => a >> b
      case CpA => a
      case CpB => b
    }
  }
}

class RVMiniALU extends FreeSpec with Matchers {
  "alu should produce expected results" in {
    val stream = getClass.getResourceAsStream("/ALUSimple.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val tester = TreadleTester(
      Seq(
        FirrtlSourceAnnotation(input),
        ClockInfoAnnotation(
          Seq(ClockInfo("clock", period = 10, initialOffset = 1))
        ),
        WriteVcdAnnotation,
        TargetDirAnnotation("usage_vcds"),
        OutputFileAnnotation("rv_mini_alu.vcd")
      )
    )
    for (i <- 0.until(ALUCode.CpB.id + 1)) {
      val a = 3
      val b = 0
      tester.poke("io_A", a)
      tester.poke("io_B", b)
      tester.poke("io_alu_op", i)
      val aluCode = ALUCode(i)
      val expected = ALUCode.compute(aluCode, a, b)
      tester.expect("io_out",  expected, s"for ALU computation $aluCode($a, $b)")
      tester.step()
    }
    tester.report()
    tester.finishAndFindDependentsOf("io_out", ALUCode.CpA.id)
  }
}
