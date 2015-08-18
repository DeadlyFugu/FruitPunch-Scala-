import java.io.InputStream

import net.sekien.fruitpunch._
import org.lwjgl.input.{Controller, Keyboard, Mouse}
import org.lwjgl.opengl._
import org.newdawn.slick.opengl.{Texture, TextureLoader}

class gl extends NativeInterface {
  override def apply(root: Context): Unit = {
    val module = root.touchv("gl")

    val fruityHome: String = root.touchv("native").getv("fruity_home").asInstanceOf[StringValue].value
    System.setProperty("org.lwjgl.librarypath", fruityHome + "/native/lib/native")
    native(module, "window_create", (stack: Stack, self: Type, eval: Evaluator) => {
      val height = stack.popAsNum.toInt
      val width = stack.popAsNum.toInt
      Display.setDisplayMode(new DisplayMode(width,height))
      Display.setTitle("Fruity")
      Display.setResizable(true)
      Display.create()
      Display.setVSyncEnabled(true)

      GL11.glEnable(GL11.GL_TEXTURE_2D)
      GL11.glClearColor(236/255f, 240/255f, 241/255f, 1.0f)

      GL11.glEnable(GL11.GL_BLEND)
      GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

      GL11.glViewport(0,0,width,height)
      GL11.glMatrixMode(GL11.GL_MODELVIEW)

      GL11.glMatrixMode(GL11.GL_PROJECTION)
      GL11.glLoadIdentity()
      GL11.glOrtho(0, width, height, 0, 1, -1)
      GL11.glMatrixMode(GL11.GL_MODELVIEW)

      GL11.glClear(GL11.GL_COLOR_BUFFER_BIT)
    })
    native(module, "window_viewfix", (stack: Stack, self: Type, eval: Evaluator) => {
      val width = Display.getWidth
      val height = Display.getHeight
      GL11.glEnable(GL11.GL_TEXTURE_2D)
      GL11.glClearColor(236/255f, 240/255f, 241/255f, 1.0f)

      GL11.glEnable(GL11.GL_BLEND)
      GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

      GL11.glViewport(0,0,width,height)
      GL11.glMatrixMode(GL11.GL_MODELVIEW)

      GL11.glMatrixMode(GL11.GL_PROJECTION)
      GL11.glLoadIdentity()
      GL11.glOrtho(0, width, height, 0, 1, -1)
      GL11.glMatrixMode(GL11.GL_MODELVIEW)

      GL11.glClear(GL11.GL_COLOR_BUFFER_BIT)
    })
    native(module, "window_update", (stack: Stack, self: Type, eval: Evaluator) => {
      Display.update()
      GL11.glClearColor(236/255f, 240/255f, 241/255f, 1.0f)
      GL11.glClear(GL11.GL_COLOR_BUFFER_BIT)
    })
    native(module, "window_destroy", (stack: Stack, self: Type, eval: Evaluator) => {
      Display.destroy()
    })
    native(module, "window_width", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(Display.getWidth))
    })
    native(module, "window_height", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(Display.getHeight))
    })
    native(module, "mouse_x", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(Mouse.getX))
    })
    native(module, "mouse_y", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(DoubleValue(Mouse.getY))
    })
    def bool(value: Boolean): Type = {if (value) root.getv("true") else root.getv("false")}
    native(module, "is_mouse_down", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(bool(Mouse.isButtonDown(stack.popAsNum.toInt)))
    })
    native(module, "is_close_req", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(bool(Display.isCloseRequested))
    })
    native(module, "is_key_down", (stack: Stack, self: Type, eval: Evaluator) => {
      stack.push(bool(Keyboard.isKeyDown(stack.popAsNum.toInt)))
    })
    native(module, "util_load_tex", (stack: Stack, self: Type, eval: Evaluator) => {
      val is: InputStream = stack.popAs[JavaWrapper].value.asInstanceOf[InputStream]
      val texture = TextureLoader.getTexture("PNG", is)
      stack.push(new JavaWrapper(texture))
    })
    native(module, "util_load_tex", (stack: Stack, self: Type, eval: Evaluator) => {
      val is: InputStream = stack.popAs[JavaWrapper].value.asInstanceOf[InputStream]
      val texture = TextureLoader.getTexture("PNG", is)
      stack.push(new JavaWrapper(texture))
    })
    native(module, "util_bind_tex", (stack: Stack, self: Type, eval: Evaluator) => {
      val texture = stack.popAs[JavaWrapper].value.asInstanceOf[Texture]
      texture.bind()
    })
    native(module, "util_tex_width", (stack: Stack, self: Type, eval: Evaluator) => {
      val texture = stack.popAs[JavaWrapper].value.asInstanceOf[Texture]
      stack.push(DoubleValue(texture.getTextureWidth))
    })
    native(module, "util_tex_height", (stack: Stack, self: Type, eval: Evaluator) => {
      val texture = stack.popAs[JavaWrapper].value.asInstanceOf[Texture]
      stack.push(DoubleValue(texture.getTextureHeight))
    })
    native(module, "gl_enable", (stack: Stack, self: Type, eval: Evaluator) => {
      GL11.glEnable(stack.popAsNum.toInt)
    })
    native(module, "gl_disable", (stack: Stack, self: Type, eval: Evaluator) => {
      GL11.glDisable(stack.popAsNum.toInt)
    })
    native(module, "gl_begin", (stack: Stack, self: Type, eval: Evaluator) => {
      GL11.glBegin(stack.popAsNum.toInt)
    })
    native(module, "end", (stack: Stack, self: Type, eval: Evaluator) => {
      GL11.glEnd()
    })
    native(module, "color4d", (stack: Stack, self: Type, eval: Evaluator) => {
      val a = stack.popAsNum
      val b = stack.popAsNum
      val g = stack.popAsNum
      val r = stack.popAsNum
      GL11.glColor4d(r,g,b,a)
    })
    native(module, "vertex3d", (stack: Stack, self: Type, eval: Evaluator) => {
      val z = stack.popAsNum
      val y = stack.popAsNum
      val x = stack.popAsNum
      GL11.glVertex3d(x,y,z)
    })
    native(module, "texco2d", (stack: Stack, self: Type, eval: Evaluator) => {
      val y = stack.popAsNum
      val x = stack.popAsNum
      GL11.glTexCoord2d(x,y)
    })
  }
}
