package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _
  // Default tests
test(run("""
  def f(x) = if (x == 0) 0 else (if (x % 3 == 2) h(x - 1) else x + g(x - 1));
  def g(x) = if (x == 0) 0 else (if (x % 3 == 1) f(x - 1) else x + h(x - 1));
  def h(x) = if (x == 0) 0 else (if (x % 3 == 0) g(x - 1) else x + f(x - 1));
  f(9)
"""), "45")
}
