trait B[T] {
    // This fails because Unit {} is temporarily transformed into
    //   trait <refinement> extends Unit { ... }
    // by refineTypeToClass for validity checking. This is wrong because
    // Unit is final (because it it a value class). The easiest way to fix
    // this would be to disallow refining final classes.
    //def f1(a: T): Unit { }

    def f2(a: T): Unit
    def f3(a: T): Unit = { }
}
