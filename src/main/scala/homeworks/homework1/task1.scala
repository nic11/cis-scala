package homeworks.homework1

import scala.annotation.tailrec

object task1 extends App {

  /**
   * Напишите хвосторекурсивную функцию, возвращающую n-e число из последовательности коров Нараяны,
   * задаваемой отношением
   *
   * a_0 = a_1 = a_2 = 1
   * a_n = a_{n-1} + a_{n-3}
   *
   * https://oeis.org/A000930
   *
   * @param n номер числа последовательности
   * @return n-ое число последовательности коров Нараяны (согласно формуле выше)
   */
  def narayanaCows(n: Int): Int = {
    @tailrec
    def helper(n: Int, v2: Int, v1: Int, v0: Int): Int =
      n match {
        case 0 => v0
        case _ => helper(n - 1, v1, v0, v2 + v0)
      }

    if (n <= 2) 1 else helper(n - 2, 1, 1, 1)
  }

  (for (i <- 0 until 10) yield s"$i) ${narayanaCows(i)}").foreach(println)
  //  0) 1
  //  1) 1
  //  2) 1
  //  3) 2
  //  4) 3
  //  5) 4
  //  6) 6
  //  7) 9
  //  8) 13
  //  9) 19
}
