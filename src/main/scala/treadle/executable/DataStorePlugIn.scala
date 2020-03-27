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
  val opGraph: mutable.Map[Symbol, OperationInfo] = symbolTable.operationGraph

  private type Cycle = Int
  private var currentCycle: Cycle = 0

  private type CycleMap = mutable.Map[Symbol.ID, SrcMap] // keyed on SymbolID of sink wire
  // contains src symbol IDs; since registers aren't processed here the dependency is always on the same cycle
  private type SrcMap = mutable.BitSet
  val mapsPerCycle = mutable.ArrayBuffer[CycleMap](mutable.Map())
  var currentMap: CycleMap = mapsPerCycle(0)

  // Usually, it is reasonable to assume that more wires are used than not; this checks that
  var unusedCount = 0
  var totalWireCount = 0

  def usedCount: Int = totalWireCount - unusedCount
  def usedFraction: Double =
    if (totalWireCount > 0) usedCount.toDouble / totalWireCount.toDouble else 0.0
  def reportUsedFraction: String =
    s"used $usedCount out of total $totalWireCount ($usedFraction)"

  private def getSymbolVal(symbol: Symbol): Int = {
    dataStore(symbol).intValue()
  }

  def updateCycleMap(): Unit = {
    // Called by tester when cycle is stepped
    mapsPerCycle += currentMap
    currentCycle += 1
    currentMap = mutable.Map()
  }

  private def addAntiDependency(sink: Symbol, src: Symbol): Unit = {
    unusedCount += 1
    val srcMap: SrcMap = currentMap.getOrElseUpdate(sink.uniqueId, mutable.BitSet())
    srcMap.add(src.uniqueId)
  }

  // scalastyle:off cyclomatic.complexity method.length
  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    // TODO problem: clock is never here
    if (offset != -1) {
      return
    }
    val symbolVal = getSymbolVal(symbol)
    val opInfo = opGraph.getOrElse(symbol, return)
    totalWireCount += opInfo.totalSources
    opInfo match {
      case MuxOperation(condition, args) =>
        val conditionVal = getSymbolVal(condition)
        val argc = args.size
        var i = 0
        // Skip used arg (mux has 0 value as last argument)
        while (i < argc - conditionVal - 1) {
          addAntiDependency(symbol, args(i))
          i += 1
        }
        i += 1
        while (i < argc) {
          addAntiDependency(symbol, args(i))
          i += 1
        }
        val usedArg = args.reverse(conditionVal)
        assert(getSymbolVal(usedArg) == symbolVal, "Selected mux argument and output must have same value")
      case PrimOperation(opcode, args) =>
        opcode match {
          case And =>
            assert(args.size == 2)
            val inputA = args.head
            val inputB = args.last
            (getSymbolVal(inputA), getSymbolVal(inputB)) match {
              case (0, 1) => addAntiDependency(symbol, inputB)
              case (1, 0) => addAntiDependency(symbol, inputA)
              case (0, 0) | (1, 1) | _ =>
            }
          case Or =>
            assert(args.size == 2)
            val inputA = args.head
            val inputB = args.last
            (getSymbolVal(inputA), getSymbolVal(inputB)) match {
              case (1, 0) => addAntiDependency(symbol, inputB)
              case (0, 1) => addAntiDependency(symbol, inputA)
              case (0, 0) | (1, 1) | _ =>
            }
          case _ =>
        }
      case _ =>
    }
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
