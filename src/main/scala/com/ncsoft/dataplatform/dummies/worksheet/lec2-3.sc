
// TODO 함수에서 우리는 좋은 추상화를 보아야 하고 이를 재활용하는 기회로 볼 줄 아는 능력이 필요합니다

def abs(x: Double): Double =
  if (x < 0) -x else x

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}
fixedPoint(x => 1 + x/2)(1)
def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1)
sqrt(2)
