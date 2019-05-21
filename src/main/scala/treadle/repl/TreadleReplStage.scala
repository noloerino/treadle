// See LICENSE for license details.

package treadle.repl

import firrtl.AnnotationSeq
import firrtl.options.{Shell, Stage, StageMain}
import firrtl.stage.FirrtlCli
import logger.Logger
import treadle.TreadleRepl
import treadle.stage.TreadleCli

class TreadleReplStage extends Stage {
  override val shell: Shell = new Shell("treadle-repl")
          with TreadleReplCli with TreadleCli with FirrtlCli

  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    Logger.makeScope(annotations) {
      val repl = TreadleRepl(annotations)
      repl.run()
    }
    annotations
  }
}

/**
  * This is the primary entry point for running the Treadle Repl
  */
object TreadleReplMain extends StageMain(new TreadleReplStage)
