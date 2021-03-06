package seminars.seminar4

import scala.util.Random

// List[+A] - неизменяемый односвязный список. Имеет две реализации
// * ::(head: A, tail: List[A]) - узел списка. Хранит в себе значение и ссылку на следующий элемент
// * Nil - пустой список. На этот объект ссылается узел, который хранит последний элемент списка
object listConstruction extends App {

  // Общий способ создания для многих коллекций
  val list0 =  List(1, 2, 3)
  // Функциональный синтаксис для создания списка.
  // Фактически обычный вызов метода: если метод оканчивается на двоеточие,
  // то его можно без точки писать слева от объекта (list1, list2 - вызов этого метода через точку)
  val list1 = 1 :: 2 :: 3 :: Nil
  val list2 = Nil.::(3).::(2).::(1)
  val list3 = Nil.prepended(3).prepended(2).prepended(1)
  println(list0)
  println(list1)
  println(list2)
  println(list3)
  println(list0 == list1 && list1 == list2 && list2 == list3)

  println()

  val list4 = List(4, 5, 6)
  // Конкатенация двух списков
  println(list3 ::: list4) // Специфичный для списка функциональный метод
  println(list4.:::(list3)) // Он же, запись через точку
  println(list3 ++ list4) // Общий для всех коллекций метод для конкатенации
  println(list3.++(list4))

  println()

  // Заполнение списка с помощью функции. Второй аргумент передается по имени (вычисляется при каждом использовании)
  val list5 = List.fill(10) {
    println("Evaluating")
    val randomDouble = Random.nextDouble()
    val randomInt = Random.nextInt(20)
    randomInt + randomDouble
  }

  println(list5)

  list5.take(2) match {
    case a :: b :: c :: tail => println(s"1: $a, 2: $b, 3: $c, tail: $tail")
    case List(a, b) => println(s"$a, $b")
    case a :: b :: Nil => println(s"$a, $b")
  }
}

object listTransform extends App {
  val list = List(0, 1, 2, 3, 4, 5, 6)

  println("map")
  val doubledList = list.map(_ * 2)
  println(doubledList)

  val alphabet = "abcdefg"
  val charList: List[Char] = list.map(el => alphabet(el))
  println(charList)

  val wordList = list.map {
    case 1 => "one"
    case 2 => "two"
    case _ => "oops"
  }
  println(wordList)
  println()

  println("filter")
  val filteredList = list.filter(_ % 2 == 0)
  println(filteredList)
  println()

  println("collect")
  val collectedList = list.collect {
    case 1 => "one"
    case 2 => "two"
  }
  println(collectedList)
  println()

  println("flatten / flatMap")
  val expandedList1: List[List[Int]] = list.map(el => List.fill(el)(el))
  val flattenedList1: List[Int] = expandedList1.flatten
  println(flattenedList1)

  val flattenedList2 = list.flatMap(el => List.fill(el)(el))
  println(flattenedList2)
  println()

  println("head / init / last / tail")
  println(list.head)
  println(list.init)
  println(list.last)
  println(list.tail)
  println()

  // Несколько методов, которые возвращают итератор
  println("iterator utilities")
  // Разбивает список на списки длины n
  println(list.grouped(2).toList)
  // Возвращает списки, полученные с помощью скользящего окна длины n
  println(list.sliding(3).toList)
  println()

  println("groupBy")
  // Возвращает ассоциативный массив, где ключами выступают значения, возвращаемые функцией
  println(list.groupBy(_ % 2))
}