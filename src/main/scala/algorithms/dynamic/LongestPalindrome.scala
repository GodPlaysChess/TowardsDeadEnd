package algorithms.dynamic

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

  // and finally dynamic approach in a functional way
  def longestPalyndromeDynamic(in: String): String = {
    val all = lpSub(in, 0, in.length - 1, Map[(Int, Int), String]())
    all(0 → (in.length - 1))
  }


  private def lpSub(in: String, s: Int, e: Int, all: Map[(Int, Int), String]): Map[(Int, Int), String] = {
    if (s == e && !all.contains(s → e)) all updated (s → e, in.substring(s, e + 1))
    else {
      val h: String = in.charAt(s).toString
      val l: String = in.charAt(e).toString
      if (h == l) {
        if (!all.contains((s + 1) → (e - 1))) {
          val all1 = lpSub(in, s + 1, e - 1, all)
          all1 updated (s → e, h + all1((s + 1) → (e - 1)) + l)
        } else {
          all updated (s → e, h + all((s + 1) → (e - 1)) + l)
        }
      }
      else {
        var a = all
        if (!all.contains((s + 1) → e)) {
          val all1 = lpSub(in, s + 1, e, all)
          a = all1
        }
        if (!all.contains(s → (e - 1))) {
          val all2 = lpSub(in, s, e - 1, a)
          a = all2
        }
        a updated (s → e, LongestPalindrome.max(a((s + 1) → e), a(s, e - 1)))
      }
    }
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
    println(longestPalyndrome("chaqracter"))
    println(longestPalyndromeDynamic("chaqracter"))
    val L = new LongestPalindrome()
    println(L.lpDynamic("chaqracter"))
  }

  val sinput =  """
                  |fds;fksdfphello
                  | */
                  |public class DateModel extends DefaultReinitializableModel<Calendar> {
                  |	private final DateFormat dateFormat;
                  |
                  |	public DateModel(Calendar initialValue, @Nonnull DateFormat dateFormat) {
                  |		super(initialValue);
                  |		this.dateFormat = checkNotNull(dateFormat, "dateFormat");
                  |	private final DateFormat dateFormat;
                  |
                  |	public DateModel(Calendar initialValue, @Nonnull DateFormat dateFormat) {
                  |		super(initialValue);
                  |		this.dateFormat = checkNotNull(dateFormat, "dateFormat");
                  |	private final DateFormat dateFormat;
                  |
                  |	public DateModel(Calendar initialValue, @Nonnull DateFormat dateFormat) {
                  |		super(initialValue);
                  |		this.dateFormat = checkNotNull(dateFormat, "dateFormat");
                  |	private final DateFormat dateFormat;
                  |
                  |	public DateModel(Calendar initialValue, @Nonnull DateFormat dateFormat) {
                  |		super(initialValue);
                  |		this.dateFormat = checkNotNull(dateFormat, "dateFormat");
                  |
                  |
                """.stripMargin

}

class LongestPalindrome {
  val all: scala.collection.mutable.HashMap[(Int, Int), String] = mutable.HashMap.empty

  def lpDynamic(in: String): String = {
    longestPalindrome(0, in.length - 1, in)
    all(0 → (in.length -1) )
  }

  //Running time:  I would say N^2, as soon as to populate 1 entry, I need O(1), and I need to populate each entry only once.
  //Space complexity: n*n
  private def longestPalindrome(s: Int, e: Int, in: String): Unit = {
    if (s == e && !all.contains(s → e)) all put (s → e, in.substring(s, e + 1))   // O(1)
    else {
      val h: String = in.charAt(s).toString                                       //O(1)
      val l: String = in.charAt(e).toString
      if (h == l) {
        if (!all.contains((s + 1) → (e - 1))) {
          longestPalindrome(s + 1, e - 1, in)                                     // O(x)
        }
        all put (s → e, h + all((s + 1) → (e - 1)) + l)                              //O(1)
      }
      else {
        if (!all.contains((s + 1) → e)) {
          longestPalindrome(s + 1, e, in)                                            //O(x)
        }
        if (!all.contains(s → (e - 1))) {
          longestPalindrome(s, e - 1, in)                                             //O(x)
        }
        all put (s → e, LongestPalindrome.max(all((s + 1) → e), all(s, e - 1)))
      }
    }
  }


}
