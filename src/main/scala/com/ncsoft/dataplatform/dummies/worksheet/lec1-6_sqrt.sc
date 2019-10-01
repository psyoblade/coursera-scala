val x = 3
val y = 4

// nested 함수로 변경하고, visibility 문제로 변수 refactor

def sqrt(x: Double): Double = {

  def abs(x: Double): Double = if (x < 0) -x else x

  def isGoodEnough(guess: Double): Boolean = abs(guess * guess - x) < 0.001

  def improve(guess: Double): Double = (guess + x / guess) / 2

  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  sqrtIter(1.0)
}

sqrt(4)
sqrt(30)
sqrt(1e-20)
//sqrt(1.0e20)
//sqrt(1.0e50)

