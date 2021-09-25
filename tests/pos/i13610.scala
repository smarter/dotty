trait Lattice[A]:
  def intersect(a: A, b: A): A
  extension (a: A) def subsumes(b: A): Boolean = intersect(a, b) == b

trait Imprecise[A, P]:
  type Precision = P
  val ord: Ordering[Precision]
  def precision(a: A): Precision


case class Bounds[N: Numeric](min: N, max: N):
  if summon[Ordering[N]].compare(min, max) > 1 then
    throw new IllegalArgumentException

object Bounds:
  import scala.math.Numeric.Implicits.infixNumericOps
  import scala.math.Ordering.Implicits.infixOrderingOps
  given [N: Numeric]: Imprecise[Bounds[N], N] with
    type Precision = N
    val ord = summon[Ordering[N]]
    def precision(rng: Bounds[N]) = rng.max - rng.min

  given [N: Numeric]: Lattice[Bounds[N]] with
    override def intersect(a: Bounds[N], b: Bounds[N]) =
      Bounds(a.min max b.min, a.max min b.max)

case class RequiredFeature[A, P](val feature: Feature[A, P], val min_certainty: P)

trait Feature[A: Lattice, P](using imp: Imprecise[A, P]):
  type Observation = A
  type Precision = P

  def within(bound: P):RequiredFeature[A, P] = RequiredFeature(this, bound)

class ObservedFeatureSet(map: Map[Feature[_, _], Any]):
  def updated(f: Feature[_, _], o: f.Observation) = new ObservedFeatureSet(map.updated(f, o))
  def apply(f: Feature[_, _]): f.Observation = map(f).asInstanceOf[f.Observation]
  def features = map.keys

trait CalibrationEngine:
  def discover(features: List[RequiredFeature[_, _]]): ObservedFeatureSet

class AutomaticCalibrationEngine(methods: Set[Method[_]]) extends CalibrationEngine:
  val methods_by_feature = (for { method <- methods } yield method.feature -> method).toMap

  override def discover(required_features: List[RequiredFeature[_, _]]): ObservedFeatureSet = ???


trait Method[A](val feature: Feature[A, _]):
  val dependencies: List[RequiredFeature[_, _]]
  def perform(input: ObservedFeatureSet): A


case class Temperature(chamber_id: Int) extends Feature[Bounds[Double], Double]

case class Pressure(chamber_id: Int) extends Feature[Bounds[Float], Float]

class DiscoverTemp(target: Temperature) extends Method(target):
  override val dependencies = List(Pressure(target.chamber_id).within(7.0))

  override def perform(input: ObservedFeatureSet): feature.Observation = ???

