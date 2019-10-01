def factorial(n: Int): Int = if (n == 0) throw new Exception else n * factorial(n - 1)
factorial(10)
