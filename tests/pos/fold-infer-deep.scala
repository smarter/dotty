class A {
  def interp(x: Map[String, Object]): Object = null

  def foo(cond: Boolean, xs: List[Int],  env: Map[String, Object]) = {
    xs.foldLeft(env)((accEnv, stat) =>
      accEnv.updated("", interp(accEnv))
    )
  }
}
