package functionalex.part2

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop {
      def check = check && p.check
    }

}
