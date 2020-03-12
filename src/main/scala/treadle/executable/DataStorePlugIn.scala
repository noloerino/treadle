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
import firrtl.ir.PrimOp

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
  // Maps target symbol to a map of source symbol -> cycle
  private type CycleUsageGraph = mutable.Map[Symbol, mutable.Map[Symbol, Cycle]]
  // Maps cycle to mapping of symbol to parents
  val concreteUsageGraph: mutable.ArrayBuffer[CycleUsageGraph] = mutable.ArrayBuffer()

  private var currentCycle: Cycle = 0
  private var currentMap: CycleUsageGraph = mutable.HashMap()

  private def getSymbolVal(symbol: Symbol): Int = {
    dataStore(symbol).intValue()
  }

  def updateCycleMap(): Unit = {
    // Called by tester when cycle is stepped
    concreteUsageGraph += currentMap
    currentCycle += 1
    currentMap = mutable.HashMap()
  }

  // scalastyle:off cyclomatic.complexity method.length
  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    // TODO problem: clock is never here
    if (offset != -1) {
      return
    }
    // Check for registers, which depend exclusively on the previous cycle
    if (currentCycle > 0 && symbolTable.contains(s"${symbol.name}/in")) {
//      println(s"register ${symbol.name} @ $currentCycle depended on input from previous cycle")
      // TODO more idiomatic way to get parent of register?
      currentMap.put(symbol, mutable.HashMap((symbolTable(s"${symbol.name}/in"), currentCycle - 1)))
      return
    }
    // Check for other stmts
    val symbolParents = mutable.HashMap[Symbol, Cycle]()
    def reportAllAsUsed(opcode: PrimOp, args: List[Symbol]): Unit = args foreach { symbolParents.put(_, currentCycle) }
    val symbolVal = getSymbolVal(symbol)
    opGraph.get(symbol) match {
      case Some(opInfo) =>
        opInfo match {
          case MuxOperation(condition, args) =>
            val conditionVal = getSymbolVal(condition)
//            val firstArgVal = getSymbolVal(args.head)
            // If all values are equal, condition is unused
//            if (!args.foldRight(true)((arg, acc) => (getSymbolVal(arg) == firstArgVal) && acc)) {
            symbolParents.put(condition, currentCycle)
//            }
            // Mux has 0 value as last argument; downcast because let's face it it's not going to be that big
            val usedArg = args.reverse(conditionVal)
            symbolParents.put(usedArg, currentCycle)
            assert(getSymbolVal(usedArg) == symbolVal, "Selected mux argument and output must have same value")
          case PrimOperation(opcode, args) =>
            opcode match {
              case And =>
                assert(args.size == 2)
                val inputA = args.head
                val inputB = args.last
                (getSymbolVal(inputA), getSymbolVal(inputB)) match {
                  case (0, 1) => symbolParents.put(inputA, currentCycle)
                  case (1, 0) => symbolParents.put(inputB, currentCycle)
                  case (0, 0) | (1, 1) | _ => reportAllAsUsed(opcode, args) // non-1b case
                }
              case Or =>
                assert(args.size == 2)
                val inputA = args.head
                val inputB = args.last
                (getSymbolVal(inputA), getSymbolVal(inputB)) match {
                  case (1, 0) => symbolParents.put(inputA, currentCycle)
                  case (0, 1) => symbolParents.put(inputB, currentCycle)
                  case (0, 0) | (1, 1) | _ => reportAllAsUsed(opcode, args) // non-1b case
                }
              case _ => reportAllAsUsed(opcode, args)
            }
          case LiteralAssignOperation() =>
          case ReferenceOperation(src) => symbolParents.put(src, currentCycle)
        }
      case _ =>
    }

//    val parentString = (symbolParents map { _.name }).mkString(",")
//    println(s"symbol ${symbol.name} @ $currentCycle = $symbolVal; depended on {$parentString}")
    currentMap.put(symbol, symbolParents)
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
