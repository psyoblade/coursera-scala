
// TODO, Currying 을 배워봅시다, 기존에 만들어져 있는 함수의 파라메터를 수정하지 않고 기능을 추가해 봅시다

// 앞에서는 함수를 호출하고 결과를 반환하였으나, 이제는 함수 sumF 를 반환하는 함수 sum 으로 구현할 수도 있습니다
def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)

  sumF
}

def fact(x: Int): Int =
  if (x == 0) 1
  else x * fact(x - 1)

// 결국 함수의 결과가 함수이므로 파라메터만 추가하면 됩니다
def sumInts = sum(x => x)
def sumCubes = sum(x => x * x)
def sumFactorials = sum(fact)

// 이와 같이 함수를 통해 함수를 만들고 다시 조합할 수 있습니다
sumInts(1, 10) + sumFactorials(10, 20)
sumCubes(1, 10)

// TODO, 아래와 같이 함수를 만들지 않고 바로 호출할 수도 있습니다
sum(x => x * x)(1, 10)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
product(x => x * x)(1, 3)

// TODO a, b 파라메터를 통해서 loop 문과 같은 구조를 rec 함수에 넣는 것입니다
def factorial(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * factorial(f)(a + 1, b) // 모든 입력 값 a 에 f 함수를 적용하는 것이 map 연산이며
// 여기서 f(a) 와 factorial(...) 구절을 곱하거나 더하는 연산이 결국 combine 연산이 됩니다
factorial(x => x)(1, 10)

// TODO 이게 어떻게 맵리듀스가 되지?
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

// TODO 이제 맵리듀스 연산을 통해서 factorial 계산을 해보자
//assert(144 == mapReduce(x => x, (x, y) => x * y, 1)(3, 4))
mapReduce(x => x, (x, y) => x * y, 1)(1, 10)
