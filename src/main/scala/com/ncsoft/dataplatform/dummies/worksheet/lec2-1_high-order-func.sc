// 두 숫자 사이에 있는 모든 정수의 합
def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a + 1, b)
assert(0 == sumInts(5, 3))
assert(12 == sumInts(3, 5))

// 큐브 함수와 두 숫자의 사이의 정수의 큐브 값의 합
def cube(x: Int): Int = x * x * x

// TODO 아래의 함수식에서 cube(4) + cube(5) + cube(6) + 0 이 보여야 한다
def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

// 팩토리얼 함수를 위와 같이 구해보자
// !4 = !4 + !3 + !2 + !1
def factorial(x: Int): Int =
  if (x == 0) 1 else x * factorial(x - 1)
assert(6 == factorial(3))

def sumFactorial(a: Int, b: Int): Int =
  if (a > b) 0 else factorial(a) + sumFactorial(a + 1, b)
var expected = 3 * 2 + 4 * 3 * 2 + 5 * 4 * 3 * 2 * 1
var actual = sumFactorial(3, 5)
assert(expected == actual)

// TODO rec_func but !tail_rec_func
def acc_fact(x: Int, acc: Int = 1): Int =
  if (x == 0) acc else acc_fact(x - 1, acc * x)

assert(6 == acc_fact(3))

// evaluation 을 나중에 하기 위해 call-by-name 결과값을 R
def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}

// the power of high-order function
val r1 = time { factorial(100) }
val r2 = time { acc_fact(100) }

val r3 = 1 to 1000 sum
val r4 = time { 1 to 1000 sum }

def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

def id(x: Int): Int = x

// TODO: 함수를 기존의 함수라고 보지 않고 literal 혹은 expression 으로 보여야 한다
def sum_ids(a: Int, b: Int) = sum(id, a, b)
def sum_cubes(a: Int, b: Int) = sum(cube, a, b)
def sum_factorials(a: Int, b: Int) = sum(factorial, a, b)


// TODO: anonymous function is Syntactic Sugar in scala
(x: Int, y: Int) => x + y

def x_plus_y(x: Int, y: Int) = x + y

def foo(f: (Int, Int) => Int, x: Int, y: Int): Int =
  f(x, y)

foo(x_plus_y, 1, 2)
foo((x: Int, y: Int) => x + y, 1, 2)

// tail rec function of sum

def loop(f: Int => Int, a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

expected = 1 + 2 * 2 + 3 * 3
actual = loop((x: Int) => x * x, 1, 3)
assert(expected == actual)
