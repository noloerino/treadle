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

class RVMiniTile extends FreeSpec with Matchers {
  "tile should run" in {
    val stream = getClass.getResourceAsStream("/Tile.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val tester = TreadleTester(
      Seq(
        FirrtlSourceAnnotation(input),
        ClockInfoAnnotation(
          Seq(ClockInfo("clock", period = 10, initialOffset = 1))
        ),
        WriteVcdAnnotation,
        TargetDirAnnotation("usage_vcds"),
        OutputFileAnnotation("rv_mini_tile.vcd")
      )
    )

    // should run in around 16 seconds
    val cycleCount = 4000
    (0 until cycleCount) foreach { _ => tester.step() }
    tester.report()
//    tester.finishAndFindDependentsOf("core.dpath.csr.io_out", cycleCount / 2)
  }
}
