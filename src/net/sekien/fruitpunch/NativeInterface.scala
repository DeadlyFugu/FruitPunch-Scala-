package net.sekien.fruitpunch

trait NativeInterface {
  // bind native functions to root
  def apply(root: Context)

  // helper method
  def native(context: Context, name: String, fn: (Stack, Type, Evaluator) => Unit): Unit = {
    context.bindv(name, new NativeClosure(name) {
      override def apply(stack: Stack, context: Type, eval: Evaluator): Unit = fn(stack, context, eval)
    })
  }
}
