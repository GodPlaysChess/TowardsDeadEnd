import java.text.SimpleDateFormat
import java.util.Date

import functionalex.part3._

object Hi {
  def main(args: Array[String]) {
    val seq1 = IndexedSeq(1, 2, 3, 4)
    val seq2 = IndexedSeq(1, 2, 4, 3)
    val seq3 = IndexedSeq(4, 3, 2, 1)
    println(Monoids.isOrdered(seq1))
    println(Monoids.isOrdered(seq2))
    println(Monoids.isOrdered(seq3))

    val ex1 = "word"
    val ex2 = "t w"
    val ex3 = "three words here"
    val ex4 = "string contains four words"
    val ex5 = "string contains exactly five words"
    val ex6 = "this string contains exactly six words"
    println(Monoids._countWords(ex1))
    println(Monoids._countWords(ex2))
    println(Monoids._countWords(ex3))
    println(Monoids._countWords(ex4))
    println(Monoids._countWords(ex5))
    println(Monoids._countWords(ex6))

    val map1 = Map("A" -> 1)
    val map2 = Map("A" -> 2, "B" -> 1)
    println(Monoids.mapMergeMonoid(Monoids.intAddition).op(map1, map2))
    println(Monoids.bag(Vector("a", "rose", "is", "a", "rose")))

    /* List replicate */
    val x = List(0, 1)
    println(s"Replicating $x")
    val opMonoid = Monad.listMonad.replicateM(2, x)
    println(opMonoid)

    /* Option replicate */
    val y = Option(Option(0, 1))
    println(s"Replicating $y \n")
    println(Monad.optionMonad.replicateM(2, y))

    /* Monadic filter */
    val ints = List(1, 2, 3, 4)
    println(Monad.optionMonad.filterM(ints)(i => Some(i % 2 == 0)))
    println(Monad.listMonad.filterM(ints)(i => List(i % 2 == 0)))

    /* Reader */
    val readerMonad = Reader.readerMonad[String]
    val r2: Reader[String, List[String]] = readerMonad.replicateM(3, readerMonad.unit("Hello"))
    val r1: Reader[String, Int] = readerMonad.map(r2)(_.length)
    println(r2.run("world"))
    println(r1.run("world"))
    println(readerMonad.map(readerMonad.unit("hello"))(_.length).run("w"))
    val lenReader = Reader[String, Int](_.length)
    println(lenReader.run("word"))
    println(readerMonad.map(lenReader)(_ * 2).run("word"))
    println(readerMonad.flatMap(lenReader)(x => Reader(y => y * x)).run("word"))

    /* Validation of WEB FORM */
    val wf1: Validation[String, WebForm] = validWebForm("Gleb", "1987-03-05", "1234567890")
    val wf2: Validation[String, WebForm] = validWebForm("Gleb Corrupted", "1987/03/05", "(+49 123 456 78)")
    println(wf1)
    println(wf2)
  }


  def validName(name: String): Validation[String, String] = {
    if (!name.isEmpty) Success(name)
    else Failure("Name can not be empty")
  }

  def validDate(birthDate: String): Validation[String, Date] =
    try {
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthDate))
    } catch {
      case _: Exception => Failure("BirthDate must be given in a form yyyy-MM-dd")
    }

  def validPhone(phone: String): Validation[String, String] = {
    if (phone.matches("[0-9]{10}")) Success(phone)
    else Failure("Phone must be 10 digits")
  }

  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] = {
    val F = Applicative.validationApplicative[String]
    F.apply(
      F.apply(
        F.apply(
          F.unit((WebForm(_, _, _)).curried))
        (validName(name))
      )(validDate(birthdate))
    )(validPhone(phone))
  }

  def validWebForm1(name: String, birthdate: String, phone: String): Validation[String, WebForm] = {
    val F = Applicative.validationApplicative[String]
    F.map3(
      validName(name),
      validDate(birthdate),
      validPhone(phone)
    )(WebForm)
  }
}

