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

import firrtl.annotations.NoTargetAnnotation
import firrtl.options.Unserializable
import firrtl.PrimOps.{And, Or}
import firrtl.ir.PrimOp
import treadle.vcd.VCD

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
  val opGraph = executionEngine.symbolTable.operationGraph

  // Needed to initialize cycle 0; if None then we haven't encountered a symbol yet
  private var firstSymbol: Option[Symbol] = None
  private type Cycle = Int
  private type CycleUsageGraph = mutable.HashMap[Symbol, mutable.Set[(Symbol, Cycle)]]
  // Maps cycle to mapping of symbol to parents
  val concreteUsageGraph: mutable.HashMap[Cycle, CycleUsageGraph] = mutable.HashMap()
  private var currentCycle: Cycle = 0
  private def currentMap: Option[CycleUsageGraph] = concreteUsageGraph.get(currentCycle)

  private def getSymbolVal(symbol: Symbol): BigInt = {
    symbol.normalize(dataStore(symbol))
  }

  private def checkUpdateCycleMap(symbol: Symbol): Unit = {
    firstSymbol match {
      // If the first symbol came back around, then init a new map for it
      case Some(otherSym) if otherSym.equals(symbol) =>
        currentCycle += 1
        concreteUsageGraph.put(currentCycle, mutable.HashMap())
      case Some(_) => // pass
      // If we haven't yet seen a first symbol, initialize it
      case None =>
        assert(currentCycle == 0, s"firstSymbol was None on cycle ${currentCycle}")
        firstSymbol = Some(symbol)
        concreteUsageGraph.put(currentCycle, mutable.HashMap())
    }
  }

  // scalastyle:off cyclomatic.complexity
  def run(symbol: Symbol, offset: Int = -1, previousValue: BigInt): Unit = {
    if (offset != -1) {
      return
    }
    checkUpdateCycleMap(symbol)
    val symbolParents = mutable.Set[Symbol]()
    def reportAllAsUsed(opcode: PrimOp, args: List[Symbol]): Unit = args map { symbolParents add _ }
    val symbolVal = getSymbolVal(symbol)
    opGraph.get(symbol) match {
      case Some(opInfo) =>
        opInfo match {
          case MuxOperation(condition, args) =>
            val conditionVal = getSymbolVal(condition)
            symbolParents add condition
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
      case _ => println(s"\tno dependency info for symbol ${symbol.name}")
    }

    val parentString = (symbolParents map { _.name }).mkString(",")
    // TODO express relationship between m and m/in of previous cycle
    println(s"symbol ${symbol.name} @ $currentCycle = ${symbolVal}; depended on {$parentString}")
    currentMap map { _.put(symbol, symbolParents map { (_, -1 + currentCycle) }) }
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
