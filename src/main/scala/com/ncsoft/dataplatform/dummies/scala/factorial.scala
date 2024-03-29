package com.ncsoft.dataplatform.dummies.scala

// 아래의 accumulator 를 이용한 tail recursion 방식은 push, pop 이 번갈아 이루어져서 stack 이 늘어나지 않는다
object factorial extends App {

  // recursion 함수의 파라메터에 계산 값을 포함시키면 함수 수행 전에 evaluation 되므로 계산이 가능하지만
  @scala.annotation.tailrec
  def factorial_tail(n: Int, acc: Int = 1): Int = if (n == 0) acc else factorial_tail(n - 1, acc * n)

  // 아래와 같이 함수 밖에 표현식이 들어가는 경우에는 n * func 는 evaluation 되지 않으므로 stack 에 쌓이게 된다
  def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

  def run_factorial(): Unit = {
    val f = factorial(4)
    val ft = factorial_tail(4)
    println("factorial:%d, factorialt:%d".format(f, ft))
    assert(f == 24)
    assert(ft == 24)
  }

  run_factorial

}
