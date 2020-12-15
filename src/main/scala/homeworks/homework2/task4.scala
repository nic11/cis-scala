package homeworks.homework2

object task4 extends App {

  sealed trait BinaryTree[+A] {
    // Реализуйте методы insert и contains для бинарного дерева со значениями произвольного типа,
    // используя тайпкласс scala.Ordering.
    // Сигнатуры методов insert/contains или классов можно (и нужно) модифицировать
    // для работы с неявными параметрами
    def insert[B >: A](newValue: B)(implicit ord : Ordering[B]): BinaryTree[B] =
      this match {
        case Leaf =>
          Branch(newValue, Leaf, Leaf)
        case Branch(value, left, right) =>
          ord.compare(newValue, value) match {
            case result if result < 0 =>
              Branch(value, left.insert(newValue), right)
            case 0 =>
              this
            case result if result > 0 =>
              Branch(value, left, right.insert(newValue))
          }
      }

    def contains[B >: A](searchedValue: B)(implicit ord : Ordering[B]): Boolean =
      this match {
        case Leaf =>
          false
        case Branch(value, _, _) if ord.compare(value, searchedValue) == 0 =>
          true
        case Branch(value, left, _) if ord.compare(value, searchedValue) > 0 =>
          left.contains(searchedValue)
        case Branch(value, _, right) if ord.compare(value, searchedValue) < 0 =>
          right.contains(searchedValue)
      }

    def dump(depth: Int = 0): Unit =
      this match {
        case Branch(value, left, right) =>
          right.dump(depth + 1)
          println(" " * depth + value)
          left.dump(depth + 1)
        case _ =>
      }
  }

  final case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  case object Leaf extends BinaryTree[Nothing]

  Leaf
    .insert("DEF")
    .insert("ABC")
    .insert("IJK")
    .insert("FGH")
    .insert("LMN")
    .dump()

  //   LMN
  //  IJK
  //   FGH
  // DEF
  //  ABC
}
