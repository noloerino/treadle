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
import firrtl.options.TargetDirAnnotation
import firrtl.stage.{FirrtlSourceAnnotation, OutputFileAnnotation}
import treadle.{ReportUsageAnnotation, TreadleTester, WriteVcdAnnotation}

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
