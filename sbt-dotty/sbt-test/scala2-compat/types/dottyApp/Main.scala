object Main {
  def main(args: Array[String]): Unit = {
    val z = new scala2Lib.Z

    z.a_01(null.asInstanceOf)
    z.a_02(null.asInstanceOf)
    z.a_03(null.asInstanceOf)
    z.b_04(null.asInstanceOf)
    z.b_05(null.asInstanceOf)
    z.a_06(null.asInstanceOf)
    z.a_07(null.asInstanceOf)
    z.b_08(null.asInstanceOf)
    z.a_09(null.asInstanceOf)
    z.b_10(null.asInstanceOf)
    z.b_11(null.asInstanceOf)
    z.b_12(null.asInstanceOf)
    z.d_13(null.asInstanceOf)
    z.d_14(null.asInstanceOf)
    z.d_15(null.asInstanceOf)
    z.d_16(null.asInstanceOf)
    z.d_17(null.asInstanceOf)
    z.d_18(null.asInstanceOf)
    z.a_19(null.asInstanceOf)
    z.z_20(null.asInstanceOf)
    z.a_21(null.asInstanceOf)
    z.a_22(null.asInstanceOf)
    z.z_23(null.asInstanceOf)
    z.z_24(null.asInstanceOf)
    z.b_25(null.asInstanceOf)
    z.a_26(null.asInstanceOf)
    z.a_27(null.asInstanceOf)
    z.a_28(null.asInstanceOf)
    z.f_29(null.asInstanceOf)
    z.f_30(null.asInstanceOf)
    z.f_31(null.asInstanceOf)
    z.f_32(null.asInstanceOf)
    z.f_33(null.asInstanceOf)
    z.f_34(null.asInstanceOf)

    val methods = classOf[scala2Lib.Z].getDeclaredMethods.toList ++ classOf[dottyApp.Z].getDeclaredMethods.toList
    methods.foreach { m =>
      m.getName match {
        case s"${prefix}_${ordinal}" =>
          val paramClass = m.getParameterTypes()(0).getSimpleName
          assert(prefix == paramClass.toLowerCase, s"Method `$m` erased to `$paramClass` which does not match its prefix `$prefix`")
        case _ =>
      }
    }
  }
}
