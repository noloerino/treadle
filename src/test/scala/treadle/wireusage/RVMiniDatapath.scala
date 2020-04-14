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

import firrtl.FileUtils
import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import org.scalatest.{FreeSpec, Matchers}
import treadle.{ClockInfoAnnotation, ReportUsageAnnotation, TreadleTester, WriteVcdAnnotation}
import treadle.executable.ClockInfo

import scala.util.Random

class RVMiniDatapath extends FreeSpec with Matchers {

  private val file = FileUtils.getTextResource("/Datapath.fir")
  private def getTester(out_path: String) = TreadleTester(
    Seq(
      FirrtlSourceAnnotation(file),
      ReportUsageAnnotation,
      ClockInfoAnnotation(
        Seq(ClockInfo("clock", period = 10, initialOffset = 1))
      ),
      WriteVcdAnnotation,
      TargetDirAnnotation("usage_vcds"),
      OutputFileAnnotation(s"$out_path.vcd")
    )
  )

  "datapath should run empty" in {
    val tester = getTester("rvmini_dpath_empty")
    val cycleCount = 15000
    (0 until cycleCount) foreach { _ => tester.step() }
    tester.report()
  }

  "datapath should run some instructions" in {
    val tester = getTester("rvmini_dpath_insts")
    // Ripped from reading the inputs of RiscVMini's TestUtils.insts
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
    val iterCount = 300 // amounts to 15k cycles
    val rng = Random
    tester.poke("reset", 1)
    (0 until iterCount) foreach { _ =>
      insts foreach { inst =>
        tester.poke("io_icache_resp_bits_data", inst)
        tester.poke("io_icache_resp_valid", 1)
        tester.poke("io_dcache_resp_bits_data", rng.nextInt())
        tester.poke("io_dcache_resp_valid", 1)
        tester.step()
      }
    }
    tester.report()
  }
}
