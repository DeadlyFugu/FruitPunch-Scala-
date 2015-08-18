package net.sekien.fruitpunch

import java.util.concurrent.{Executors, ExecutorService}

object EvaluatorPool {
  val pool: ExecutorService = Executors.newCachedThreadPool()

  def runTask(stack: Stack, closure: Closure, root: RootContext) = {
    pool.execute(new ExecutionThread(stack, closure, root))
  }

  class ExecutionThread(stack: Stack, closure: Closure, root: RootContext) extends Runnable {
    def run(): Unit = {
      new Evaluator(false, new Parser, root).executeCallable(closure, stack)
    }
  }
}
