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

package treadle.executable

import firrtl.PrimOps.{And, Or}
import treadle.executable.SymbolTable.OperationGraph

import scala.collection.mutable

abstract class DataStorePlugin {
  def executionEngine: ExecutionEngine
  def dataStore:       DataStore
  var isEnabled: Boolean = false

  def setEnabled(enabled: Boolean): Unit = {
    isEnabled = enabled
    (isEnabled, dataStore.activePlugins.contains(this)) match {
      case (true, false) =>
        dataStore.activePlugins += this
      case (false, true) =>
        dataStore.activePlugins.remove(dataStore.activePlugins.indexOf(this))
      case _ =>
      /* do nothing */
    }
    if (dataStore.activePlugins.nonEmpty && dataStore.leanMode) {
      dataStore.leanMode = false
      executionEngine.scheduler.getAllAssigners.foreach { assigner =>
        assigner.setLeanMode(false)
      }
    } else if (dataStore.activePlugins.isEmpty && !dataStore.leanMode) {
      dataStore.leanMode = true
      executionEngine.scheduler.getAllAssigners.foreach { assigner =>
        assigner.setLeanMode(true)
      }
    }
  }

  def run(symbol: Symbol, offset: Int = -1, previousValue: Big): Unit
}

class ReportUsage(val executionEngine: ExecutionEngine) extends DataStorePlugin {

  val dataStore: DataStore = executionEngine.dataStore
  val symbolTable: SymbolTable = executionEngine.symbolTable
  val opGraph: OperationGraph = symbolTable.operationGraph

  private type Cycle = Int
  private var currentCycle: Cycle = 0

  // Maps sink + cycle to src wire - we know dep is always in same cycle
  private type SinkMap = mutable.Map[(Symbol.ID, Cycle), mutable.Set[Symbol.ID]]
  val sinkMap: SinkMap = mutable.Map()

  // Usually, it is reasonable to assume that more wires are used than not; this checks that
  var usedCount = 0
  var totalWireCount = 0
  var ignoredSymbolCount = 0
  var totalVisitedCount = 0

  def unusedCount: Int = totalWireCount - usedCount
  def usedFraction: Double =
    if (totalWireCount > 0) usedCount.toDouble / totalWireCount.toDouble else 0.0
  def ignoredFraction: Double =
    if (totalVisitedCount > 0) ignoredSymbolCount.toDouble / totalVisitedCount.toDouble else 0.0
  def reportUsedFraction: String =
    s"used $usedCount out of total $totalWireCount ($usedFraction)" +
      (if (ignoredSymbolCount > 0) s"; ignored $ignoredSymbolCount of $totalVisitedCount ($ignoredFraction)" else "")

  private def getSymbolVal(symbol: Symbol): Int = {
    dataStore(symbol).intValue()
  }

  def updateCycleMap(): Unit = {
    // Called by tester when cycle is stepped
    currentCycle += 1
  }

  private def addDependency(sink: Symbol, src: Symbol): Unit = {
    usedCount += 1
    val cycleSet = sinkMap.getOrElseUpdate((sink.uniqueId, currentCycle), mutable.Set())
    cycleSet.add(src.uniqueId)
  }

  // scalastyle:off cyclomatic.complexity method.length
  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    // TODO problem: clock is never here
    totalWireCount += 1
    if (offset != -1) {
      ignoredSymbolCount += 1
      return
    }
    val symbolVal = getSymbolVal(symbol)
    opGraph.get(symbol.uniqueId).foreach(
      opInfo => {
        totalWireCount += opInfo.totalSources
        opInfo match {
          case MuxOperation(condition, trueSym, falseSym) =>
            addDependency(symbol, condition)
            val conditionVal = getSymbolVal(condition)
            addDependency(symbol, if (conditionVal == 1) trueSym else falseSym)
            val usedArg = if (conditionVal == 1) trueSym else falseSym
            assert(getSymbolVal(usedArg) == symbolVal, "Selected mux argument and output must have same value")
          case PrimOperation(opcode, args) =>
            opcode match {
              case And =>
                // AND can be used in more than just the 1b case, e.g. for bit masks
                if (args.size < 2) return
                assert(args.size == 2, s"And expected args list of size 2, got $args")
                val inputA = args.head
                val inputB = args.last
                (getSymbolVal(inputA), getSymbolVal(inputB)) match {
                  case (0, 0) =>
                    addDependency(symbol, inputA)
                    addDependency(symbol, inputB)
                  case (0, _) => addDependency(symbol, inputA)
                  case (_, 0) => addDependency(symbol, inputB)
                  case _ =>
                    addDependency(symbol, inputA)
                    addDependency(symbol, inputB)
                }
              case Or =>
                // OR only matters in the 1b case
                if (args.size < 2) return
                assert(args.size == 2, s"Or expected args list of size 2, got $args")
                val inputA = args.head
                val inputB = args.last
                if (inputA.bitWidth != inputB.bitWidth) return
                val All1s = ~(-1 << inputA.bitWidth)
                (getSymbolVal(inputA), getSymbolVal(inputB)) match {
                  case (All1s, All1s) =>
                    addDependency(symbol, inputA)
                    addDependency(symbol, inputB)
                  case (All1s, _) => addDependency(symbol, inputA)
                  case (_, All1s) => addDependency(symbol, inputB)
                  case _ =>
                    addDependency(symbol, inputA)
                    addDependency(symbol, inputB)
                }
              case _ =>
                for (arg <- args) {
                  addDependency(symbol, arg)
                }
            }
          case _ =>
            for (arg <- opInfo.allSrcs) {
              addDependency(symbol, arg)
            }
        }
      }
    )

  }
}

class ReportAssignments(val executionEngine: ExecutionEngine) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (offset == -1) {
      val valueMessage = if (symbol.forcedValue.isDefined) {
        s" FORCED(${symbol.forcedValue.get}"
      } else {
        val showValue = symbol.normalize(dataStore(symbol))
        s"$showValue h${showValue.toString(16)}"
      }
      println(s"${symbol.name} <= $valueMessage")
    } else {
      val valueMessage = if (symbol.forcedValue.isDefined) {
        s" FORCED(${symbol.forcedValue.get}"
      } else {
        val showValue = symbol.normalize(dataStore(symbol, offset))
        s"$showValue h${showValue.toString(16)}"
      }
      println(s"${symbol.name}($offset) <= $valueMessage")
    }
  }
}

class RenderComputations(
  val executionEngine: ExecutionEngine,
  symbolNamesToWatch:  Seq[String]
) extends DataStorePlugin {

  val dataStore:      DataStore = executionEngine.dataStore
  val symbolsToWatch: mutable.HashSet[Symbol] = new mutable.HashSet
  symbolsToWatch ++= symbolNamesToWatch.flatMap { name =>
    executionEngine.symbolTable.get(name)
  }

  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (symbolsToWatch.contains(symbol)) {
      if (symbol.forcedValue.isDefined) {
        print(s"FORCED(${symbol.forcedValue.get} would have been: ")
      }
      println(executionEngine.renderComputation(symbol.name))
    }
  }
}

class VcdHook(val executionEngine: ExecutionEngine) extends DataStorePlugin {
  val dataStore: DataStore = executionEngine.dataStore

  override def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (offset == -1) {
      val value = dataStore(symbol)
      executionEngine.vcdOption.foreach { vcd =>
        vcd.wireChanged(symbol.name, value, width = symbol.bitWidth)
      }
    }
  }
}
