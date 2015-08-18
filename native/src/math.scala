import net.sekien.fruitpunch._

import scala.reflect.io.File

class math extends NativeInterface {
  override def apply(root: Context): Unit = {
    val module = root.touchv("math")
    module.bindv("pi",DoubleValue(math.Pi))
    module.bindv("e",DoubleValue(math.E))
    native(module, "sin", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.sin(x)))
    })
    native(module, "cos", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.cos(x)))
    })
    native(module, "tan", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.tan(x)))
    })
    native(module, "asin", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.asin(x)))
    })
    native(module, "acos", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.acos(x)))
    })
    native(module, "atan", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.atan(x)))
    })
    native(module, "sqrt", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.sqrt(x)))
    })
    native(module, "cbrt", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.cbrt(x)))
    })
    native(module, "ln", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.log(x)))
    })
    native(module, "log", (stack: Stack, self: Type, eval: Evaluator) => {
      val x = stack.popAsNum
      stack.push(DoubleValue(math.log10(x)))
    })
    native(module, "random", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(math.random))
    })
  }
}
