package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _
  test(run("vcc done; done(1) - done(0)"), "1")
test(run("vcc done; done(2) == done(0)"), "2")
test(run("vcc done; done(3) :: done(0)"), "3")
test(run("vcc done; {done(4)}(done(0))"), "4")
test(run("vcc done; def f(x) = 2 * x; f(1, done(5))"), "5")
test(run("try (throw 6) % (throw 0) catch (n => n)"), "6")
test(run("try (throw 7) <= (throw 0) catch (n => n)"), "7")
test(run("try ((throw 8),(throw 0)) catch (n => n)"), "8")
test(run("try {(x,y)=>x+y}(throw 9, throw 0) catch (n => n)"), "9")
test(run("try vcc k; k(1, throw 10) catch (n => n)"), "10")
test(run("try (true + throw 1) catch (x => x)"), "1")
test(run("try 1(throw 2) catch (x => x)"), "2")
test(run("try (x => x)(1, throw 2) catch (x => x)"), "2")
test(run("vcc x; throw x(1)"), "1")
testExc(run("vcc k; k(0, 1)"), "")
testExc(run("vcc k; k()"), "")
testExc(run("try throw 1 catch 2"), "")
testExc(run("try throw 1 catch ((x, y) => x)"), "")
testExc(run("try throw 1 catch (() => false)"), "")
testExc(run("try if(0) true else false catch (x => x)"), "")
testExc(run("try throw(true(0)) catch (x => x)"),"")
testExc(run("{vcc done; val x = 0; done(0)} + x"), "")
testExc(run("try def f(x) = x; throw(0) catch f"),"")
testExc(run("val f = try (x => throw x) catch (x => x); f(0)"), "")
test(run("""
def fib(x) = {
    if (x == 0)
        return 0
    else if (x == 1)
        return 1
    else
        return fib(x -1) + fib(x - 2)
};
fib(10)
"""), "55")
test(run("""
def match(L) = {
    if(L.isEmpty)
        throw -1
    else
        (L.head, L.tail)
};
try
    val t = match(1::Nil)._2;
    match(t)
catch (x => x)
"""), "-1")
test(run("""
val MAX = 10;
def f(a, b) = {
    if (a == 0)
        return true
    else if (b == 0)
        return f(a-1, MAX)
    else if (b*(a/b) +  a%b != a)
        throw false
    else
        return f(a, b-1)
};
f(MAX, MAX)
"""), "true")

test(run("""
def interp(e) = {
    if(e.isInstanceOf[Int])
        return e
    else if(!e.isInstanceOf[List] || e.isEmpty)
        throw e
    else if(e.head)
        return interp_plus(e.tail)
    else
        return interp_minus(e.tail)
};
def interp_plus(e) = {
    if(!e.isInstanceOf[List] || e.isEmpty || e.tail.isEmpty || e.tail.tail.nonEmpty)
        throw e
    else
        val v1 = interp(e.head);
        val v2 = interp(e.tail.head);
        v1 + v2
};
def interp_minus(e) = {
    if(!e.isInstanceOf[List] || e.isEmpty || e.tail.isEmpty || e.tail.tail.nonEmpty)
        throw e
    else
        val v1 = interp(e.head);
        val v2 = interp(e.tail.head);
        v1 - v2
};
def f(x) = try interp(x) catch (e=>false);
(f(42), f(true::(false::2::1::Nil)::3::Nil), f(false::1::Nil), f(true::1::true::Nil), f(false::1::1::1::Nil))
"""), "(42, 4, false, false, false)")
test(run("""
  def break(x) = throw (Nil, x);
  def loop(f, init) = {
    def aux(n) = aux(f(n));
    try {
      aux(init)
    } catch {
      x =>
        if (x.isInstanceOf[Tuple] && x._1.isInstanceOf[List])
          x._2
        else
          throw x
    }
  };
  def f(p) =
    if (p._1 <= 0) break(p)
    else (p._1 - 1, p._1 + p._2);
  val sum = n => loop(f, (n, 0))._2;
  sum(10)
"""), "55")
test(run("""
  def Some(x) = (true, x);
  val None = (false, 0);
  def get(opt) =
    if (opt.isInstanceOf[Tuple] && opt._1.isInstanceOf[Boolean] && opt._1)
      opt._2
    else
      throw opt;
  def flatMap(opt, f) =
    if (opt.isInstanceOf[Tuple] && opt._1.isInstanceOf[Boolean])
      if (opt._1)
        f(opt._2)
      else
        opt
    else
      throw opt;
  def lift(f) =
    x =>
      try {
        Some(f(x))
      } catch {
        x => None
      };
  def div100(x) =
    if (x.isInstanceOf[Int] && x != 0)
      100 / x
    else
      throw x;
  val safeDiv = lift(div100);
  get(flatMap(safeDiv(10), safeDiv)) + try {
    get(flatMap(safeDiv(0), safeDiv))
  } catch {
    x => 0
  }
"""), "10")
test(run("""
  def merge(l, r) =
    (if (l.isEmpty) return r else 0) ::
    (if (r.isEmpty) return l else 0) ::
    (
      val x = l.head;
      val y = r.head;
      (if (x <= y) return x :: merge(l.tail, r) else 0) ::
      (return y :: merge(l, r.tail))
    );

  def split(o) =
    (if (o.isEmpty) return (Nil, Nil) else 0) ::
    (if (o.tail.isEmpty) return (o, Nil) else 0) ::
    (
      val x = o.head;
      val y = o.tail.head;
      val zs = o.tail.tail;
      val (xs, ys) = split(zs);
      return (x :: xs, y :: ys)
    );

  def mergeSort(o) =
    (if (o.isEmpty) return Nil else 0) ::
    (if (o.tail.isEmpty) return o else 0) ::
    (
      val (as, bs) = split(o);
      return merge(mergeSort(as), mergeSort(bs))
    );

  mergeSort(2 :: 5 :: 0 :: 3 :: 4 :: 1 :: Nil)
"""), "(0 :: (1 :: (2 :: (3 :: (4 :: (5 :: Nil))))))")

test(run("""
  def len(l) = if (l.isEmpty) 0 else 1 + len(l.tail);

  val yin = {
    val k = (vcc x; x);
    if (k.isInstanceOf[Function])
      (k, Nil)
    else if (len(k._2) < 10)
      (k._1, 2 :: k._2)
    else
      ((x) => x, k._2)
  };
  val yang = {
    val k = (vcc x; x);
    if (k.isInstanceOf[Function])
      (k, yin._2)
    else
      (k._1, 1 :: k._2)
  };
  val r = yin._1(yang);
  yin._2
"""), "(1 :: (1 :: (1 :: (1 :: (2 :: (1 :: (1 :: (1 :: (2 :: (1 :: (1 :: (2 :: (1 :: (2 :: Nil))))))))))))))")
  test(run("42"), "42")
test(run("1 + 2"), "3")
test(run("7 - 2"), "5")
test(run("2 * 4"), "8")
test(run("5 / 2"), "2")
test(run("13 % 5"), "3")
test(run("1 - -1"), "2")
test(run("true"), "true")
test(run("1 == 3 - 2"), "true")
test(run("1 < 3 - 2"), "false")
test(run("(1, 2 + 3, true)"), "(1, 5, true)")
test(run("((42, 3 * 2), false)"), "((42, 6), false)")
test(run("(1, 2 + 3, true)._1"), "1")
test(run("((42, 3 * 2), false)._1._2"), "6")
test(run("Nil"), "Nil")
test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
test(run("Nil.isEmpty"), "true")
test(run("(1 :: Nil).isEmpty"), "false")
test(run("(1 :: Nil).head"), "1")
test(run("(1 :: Nil).tail"), "Nil")
test(run("(1 :: 2 :: Nil).tail.head"), "2")
test(run("""
  val x = 1 + 2;
  val y = x * 4 + 1;
  y / (x - 1)
"""), "6")
test(run("""
  val (x, y) = (1 + 2, 3 + 4);
  val z = x * y;
  val (a, b, c) = (z, z + z, z + z + z);
  c - b
"""), "21")
test(run("x => x + x"), "<function>")
test(run("(x => x + x)(1)"), "2")
test(run("(x => y => x + y)(1)(2)"), "3")
test(run("((x, y) => x + y)(1, 2)"), "3")
test(run("1.isInstanceOf[Int]"), "true")
test(run("1.isInstanceOf[Boolean]"), "false")
test(run("(1 :: Nil).isInstanceOf[List]"), "true")
test(run("(x => x + x).isInstanceOf[Function]"), "true")
test(run("if (true) 1 else 2"), "1")
test(run("!true"), "false")
test(run("true && false"), "false")
test(run("true || false"), "true")
test(run("1 != 2"), "true")
test(run("1 <= 1"), "true")
test(run("1 > 1"), "false")
test(run("1 >= 1"), "true")
test(run("Nil.nonEmpty"), "false")
test(run("""
  def f(x) = x - 1;
  f(2)
"""), "1")
test(run("""
  def f(x) = if (x < 1) 0 else x + f(x - 1);
  f(10)
"""), "55")
test(run("""
  vcc x;
  1 + x(1) + 1
"""), "1")
test(run("""
  (x => x * x)(
    1 + vcc x; 1 + x(2) + 3
  )
"""), "9")
test(run("(x => (return 1) + x)(2)"), "1")
test(run("""
  def div(x) = (x => 10 / x)(
    if (x == 0) return 0 else x
  );
  div(0) + div(10)
"""), "1")
testExc(run("throw 1"), "")
testExc(run("throw throw 1"), "")
test(run("""
  try {
    throw 1
  } catch (
    x => x + x
  )
"""), "2")
test(run("""
  1 + vcc x;
    try {
      throw 1
    } catch x
"""), "2")
}
