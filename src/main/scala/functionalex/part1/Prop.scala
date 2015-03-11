package functionalex.part1

/**
 * Created by GlebP on 11-Mar-2015.
 */
trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop {
      def check = check && p.check
    }

}
