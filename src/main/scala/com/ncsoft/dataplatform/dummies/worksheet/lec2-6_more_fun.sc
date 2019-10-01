/**
 * @see <a href="https://bit.ly/2ocVDYl">coursera lecture 2-6</a>
 * @param x
 * @param y
 */

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

  def addRational(r: Rational, s: Rational): Rational =
    new Rational(r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom)

  def makeString(r: Rational) =
    r.numer + "/" + r.denom

  def run = {
    val a = new Rational(1, 2)
    makeString(addRational(new Rational(1, 2), new Rational(2, 3)))
    val b = new Rational(2, 3)
    a.add(b)

    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)
    x.numer
    x.denom
    x.add(y)
//    x.sub(y).sub(z)
    y.add(y)
    x.less(y)
    x.max(y)

//    val strange = new Rational(1, 0)
//    strange.add(strange)

    new Rational(2)
  }

}

functions.run
