trait SuspendSupport:
  type Suspension[-T, +R]
  extension [T, R](s: Suspension[T, R])
    def resume(arg: T): R

  def suspend[T, R](body: Suspension[T, R] => R): T

trait Async(using val support: SuspendSupport)

private class YZ(using ac: Async):
  private def test =
    Some(()).getOrElse:
      ac.support.suspend[scala.util.Try[Unit], Unit](k =>
        k.resume(scala.util.Failure(Exception()))
        1
      )
