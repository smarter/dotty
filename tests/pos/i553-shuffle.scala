import scala.util.Random
object Test {
  def test = {
    val rand: Random = new Random
    rand.shuffle(List(1,2))// infers implicit argument list (List.canBuildFrom[Int])
  }
}
