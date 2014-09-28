package projectEuler.old.edu

/**
 * Created by fastgleb on 9/7/14.
 */
abstract class Animal {
  type SuitableFood <: Food
  def eat(food: SuitableFood)

}
