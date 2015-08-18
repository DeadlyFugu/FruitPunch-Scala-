import net.sekien.fruitpunch.FruitPunch

def k(s: String) = FruitPunch.parser.parse(s)

def profile(in: String) = {
  val t0 = System.currentTimeMillis()
  val j = k(in)
  val t1 = System.currentTimeMillis()
  (t1-t0, j)
}

def m(s: String): Long =profile(s)._1

val s = BigTestString.s2 * 10
m(s)
m(s)
m(s)
m(s)
var sum: Long = 0
var c: Int = 0
for (i: Int <- 1 to 10) {sum += m(s); c += 1}
sum/c+"ms"