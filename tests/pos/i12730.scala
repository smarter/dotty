class InitState

class ComponentSimple

class Props {
  def apply(props: Any): Any = ???
}

class Foo[C](initState: InitState) {
  def build: ComponentSimple = ???
}

class Bar[E] {
  def render(r: E => Any): Unit = {}
}

trait Conv[A, B] {
  def apply(a: A): B
}

object Test {
  def toComponentCtor[F](c: ComponentSimple): Props = ???

  def defaultToNoBackend[G, H](ev: G => Foo[H]): Conv[Foo[H], Bar[H]] = ???

  def stateless[I]: InitState = ???

  def conforms[A]: A => A = ???

  def problem = Main // crashes

  val NameChanger =
    new Foo[Any](stateless)
      .build

  val Main =
    defaultToNoBackend(conforms).apply(new Foo[Any](stateless))
      .render(_ => toComponentCtor(NameChanger)(13))
}
