class Rational(x: Int, y: Int) { // primary constructor
  // require 함수를 통해 caller 의 precondition 을 체크하자
  require(y > 0, "denominator must be positive")
  // assert 함수를 통해 함수 내에서는 코드의 validation 체크하자

  // this: function position
  def this(x: Int) = this(x, 1)

  // 큰 수를 앞으로 내어주고, 작은수로 계속 나누어 나머지가 0일 때까지 나눈다
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def numer: Int = x
  def denom: Int = y

  def add(that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg: Rational = new Rational(-numer, denom)
  // don't repeat code
  def sub(that: Rational): Rational = add(that.neg)

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom
  // 이렇게 2개 파라메터에 대한 함수를 정의하면 되는구나
  def max(that: Rational) = if (this.less(that)) that else this

  // Rational simplified form
  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }

}

object functions {
  new Rational(1, 2).numer
}
