import net.sekien.fruitpunch._

import scala.reflect.io.File

class files extends NativeInterface {
  override def apply(root: Context): Unit = {
    val n = root.touchv("native")
    native(n, "file_in", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(JavaWrapper(File(fname).inputStream()))
    })
    native(n, "file_out", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(JavaWrapper(File(fname).outputStream()))
    })
    native(n, "dir_ls", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(ListValue(File(fname).jfile.listFiles.toList map {f => StringValue(f.getName)}))
    })
    native(n, "file_exists", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(root.getv(if (File(fname).jfile.exists) "true" else "false"))
    })
    native(n, "file_hidden", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(root.getv(if (File(fname).jfile.isHidden) "true" else "false"))
    })
    native(n, "isfile", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(root.getv(if (File(fname).jfile.isFile) "true" else "false"))
    })
    native(n, "isdir", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(root.getv(if (File(fname).jfile.isDirectory) "true" else "false"))
    })
    native(n, "file_size", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(DoubleValue(File(fname).length))
    })
    native(n, "file_path", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      stack.push(StringValue(File(fname).jfile.getCanonicalPath))
    })
    native(n, "file_touch", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      val file: File = File(fname)
      if (!file.exists) {
        file.jfile.createNewFile()
      }
    })
    native(n, "mkdir", (stack: Stack, self: Type, eval: Evaluator) => {
      val fname = stack.popAsStr
      val file: File = File(fname)
      if (!file.exists) {
        file.jfile.mkdirs()
      }
    })
  }
}
