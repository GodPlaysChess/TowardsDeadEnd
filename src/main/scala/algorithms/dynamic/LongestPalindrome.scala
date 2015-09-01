package algorithms.dynamic

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * 01-Sep-2015.
 */
object LongestPalindrome {

  // plain approach
  def longestPalyndrome(in: String): String = {
    if (in.isEmpty) ""
    else if (in.length == 1) in
    else {
      val (h, mid, l) = split(in)
      if (h == l) h + longestPalyndrome(mid) + l
      else max(longestPalyndrome(h + mid), longestPalyndrome(mid + l))
    }
  }
  
  def longestPalyndromeDynamic(in: String): String = {
    val all = lpSub(in, 0, in.length, Map[(Int, Int), String]())
    all(0 → in.length)
  }


  private def lpSub(in: String, s: Int, e: Int, all: Map[(Int, Int), String]): Map[(Int, Int), String] = {
    if (in.isEmpty) all updated (s → e, in) // check if contains key
    else if (in.length == 1) {
      updateOrReturn(in, s, e, all)
    }
    else {
      val (h, mid, l) = split(in)
      if (h == l) updateOrReturn(h + lpSub(mid, s, e, all) + l, s, e, all)
      else {
        all
        //        max(longestPalyndrome(h + mid), longestPalyndrome(mid + l))
        //      }
      }}
  }

  private def updateOrReturn(in: String, s: Int, e: Int, all: Map[(Int, Int), String]): Map[(Int, Int), String] = {
    if (all.contains(s → e)) all else all updated(s → e, in)
  }

  def split(in: String): (String, String, String) = {
    (in.head.toString, in.substring(1, in.length - 1) , in.last.toString)
  }

  def max(s1: String, s2: String) = {
    if (s1.length >= s2.length) s1
    else s2
  }

  def main(args: Array[String]) {
    println(longestPalyndrome("character"))
    println(new LongestPalindrome().lpDynamic("character"))
//    println(longestPalyndrome(
//      """
//        |fds;fksdfphello
//        | */
//        |public class DateModel extends DefaultReinitializableModel<Calendar> {
//        |	private final DateFormat dateFormat;
//        |
//        |	public DateModel(Calendar initialValue, @Nonnull DateFormat dateFormat) {
//        |		super(initialValue);
//        |		this.dateFormat = checkNotNull(dateFormat, "dateFormat");
//
//        |
//      """.stripMargin))
  }

}

class LongestPalindrome {
  val all: scala.collection.mutable.HashMap[(Int, Int), String] = mutable.HashMap.empty

  def lpDynamic(in: String): String = {
    longestPalindrome(0, in.length - 1, in)                                                               // ?
    all(0 → (in.length -1) )
  }

  private def longestPalindrome(s: Int, e: Int, in: String): Unit = {
    if (s == e || s + 1 == e && !all.contains(s → e)) all put (s → e, in.substring(s, e + 1))
    else {
      val (h, mid, l) = LongestPalindrome.split(in)
      if (h == l) {
        if (!all.contains((s + 1) → (e - 1))) {
          longestPalindrome(s + 1, e - 1, in)
        }
        all put (s → e, h + all((s + 1) → (e - 1)) + l)
      }
      else {
        if (!all.contains((s + 1) → e)) {
          longestPalindrome(s + 1, e, in)
        }
        if (!all.contains(s → (e - 1))) {
          longestPalindrome(s, e - 1, in)
        }
        all put (s → e, LongestPalindrome.max(all((s + 1) → e), all(s, e - 1)))
      }
    }
  }


}
