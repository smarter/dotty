object Test {
  // Type argument Int does not conform to upper bound Object
  val builder: Builder_1[Int] = ???
  builder.build(): Option[Int]
}
