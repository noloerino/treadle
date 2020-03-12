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

class ReportUsage(val executionEngine: ExecutionEngine, val clockStepper: ClockStepper) extends DataStorePlugin {

  val dataStore: DataStore = executionEngine.dataStore
  val symbolTable: SymbolTable = executionEngine.symbolTable
  val opGraph: mutable.HashMap[Symbol, OperationInfo] = symbolTable.operationGraph

  // Needed to initialize cycle 0; if None then we haven't encountered a symbol yet
  private type Cycle = Int
  private type CycleUsageGraph = mutable.HashMap[Symbol, mutable.Set[(Symbol, Cycle)]]
  // Maps cycle to mapping of symbol to parents
  val concreteUsageGraph: mutable.HashMap[Cycle, CycleUsageGraph] = mutable.HashMap()
  private var currentCycle: Cycle = 0
  private def currentMap: Option[CycleUsageGraph] = concreteUsageGraph.get(currentCycle)

  private def getSymbolVal(symbol: Symbol): BigInt = {
    symbol.normalize(dataStore(symbol))
  }

  def updateCycleMap(): Unit = {
    // Called by tester when cycle is stepped
    currentMap match {
      case None => assert(currentCycle == 0, s"currentMap was None on cycle $currentCycle")
      case _ => currentCycle += 1
    }
    assert(currentCycle == clockStepper.cycleCount, "currentCycle did not match stepper cycle")
    concreteUsageGraph.put(currentCycle, mutable.HashMap())
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
      currentMap map { _.put(symbol, mutable.Set((symbolTable.get(s"${symbol.name}/in").get, currentCycle - 1)))}
      return
    }
    // Check for other stmts
    val symbolParents = mutable.Set[Symbol]()
    def reportAllAsUsed(opcode: PrimOp, args: List[Symbol]): Unit = args map { symbolParents add }
    val symbolVal = getSymbolVal(symbol)
    opGraph.get(symbol) match {
      case Some(opInfo) =>
        opInfo match {
          case MuxOperation(condition, args) =>
            val conditionVal = getSymbolVal(condition)
//            val firstArgVal = getSymbolVal(args.head)
            // If all values are equal, condition is unused
//            if (!args.foldRight(true)((arg, acc) => (getSymbolVal(arg) == firstArgVal) && acc)) {
//              symbolParents add condition
//            }
            // Mux has 0 value as last argument; downcast because let's face it it's not going to be that big
            val usedArg = args.reverse(conditionVal.intValue())
            symbolParents add usedArg
            assert(getSymbolVal(usedArg) == symbolVal, "Selected mux argument and output must have same value")
          case PrimOperation(opcode, args) =>
            opcode match {
              case And =>
                assert(args.size == 2)
                val inputA = args.head
                val inputB = args.last
                (getSymbolVal(inputA).toInt, getSymbolVal(inputB).toInt) match {
                  case (0, 1) => symbolParents add inputA
                  case (1, 0) => symbolParents add inputB
                  case (0, 0) | (1, 1) | _ => reportAllAsUsed(opcode, args) // non-1b case
                }
              case Or =>
                assert(args.size == 2)
                val inputA = args.head
                val inputB = args.last
                (getSymbolVal(inputA).toInt, getSymbolVal(inputB).toInt) match {
                  case (1, 0) => symbolParents add inputA
                  case (0, 1) => symbolParents add inputB
                  case (0, 0) | (1, 1) | _ => reportAllAsUsed(opcode, args) // non-1b case
                }
              case _ => reportAllAsUsed(opcode, args)
            }
          case LiteralAssignOperation() =>
          case ReferenceOperation(src) => symbolParents add src
        }
      case _ =>
    }

    val parentString = (symbolParents map { _.name }).mkString(",")
//    println(s"symbol ${symbol.name} @ $currentCycle = $symbolVal; depended on {$parentString}")
    currentMap map { _.put(symbol, symbolParents map { (_, currentCycle) }) }
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
