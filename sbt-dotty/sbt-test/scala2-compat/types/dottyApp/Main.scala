object Main {
  def main(args: Array[String]): Unit = {
    val z = new scala2Lib.Z

    def dummy[T]: T = null.asInstanceOf[T]

    z.a_01(dummy)
    z.a_02(dummy)
    z.b_02X(dummy)
    z.a_03(dummy)
    z.b_04(dummy)
    z.b_04X(dummy)
    z.b_05(dummy)
    z.a_06(dummy)
    z.a_07(dummy)
    z.b_08(dummy)
    z.a_09(dummy)
    z.b_10(dummy)
    z.b_11(dummy)
    z.b_12(dummy)
    z.d_13(dummy)
    z.d_14(dummy)
    z.d_15(dummy)
    z.d_15b(dummy)
    z.d_16(dummy)
    z.d_16b(dummy)
    z.d_17(dummy)
    z.d_18(dummy)
    z.a_19(dummy)
    z.d_19x(dummy)
    z.z_20(dummy)
    z.a_21(dummy)
    z.a_22(dummy)
    z.z_23(dummy)
    z.z_24(dummy)
    z.b_25(dummy)
    z.a_26(dummy)
    z.a_27(dummy)
    z.a_28(dummy)
    z.f_29(dummy)
    z.f_30(dummy)
    z.f_31(dummy)
    z.f_32(dummy)
    z.f_33(dummy)
    z.f_34(dummy)

    // crash in erasure asInstanceOf due to notype
    // z.a_35(dummy)
    // z.d_36(dummy)
    // z.z_37(dummy)
    // z.z_38(dummy)

    // z.c_39(dummy)
    // z.c_40(dummy)

    // z.c_41(dummy)
    // z.c_42(dummy)
    // z.b_43(dummy)
    // z.c_44(dummy)

    // z.c_45(dummy)
    // z.b_46(dummy)

    z.c_47(dummy)
    z.a_48(dummy)

    z.c_49(dummy)
    z.c_50(dummy)
    z.a_51(dummy)
    z.c_52(dummy)

    z.a_53(dummy)
    z.c_54(dummy)

    // same
    // z.b_55(dummy)

    // z.b_56(dummy)
    // z.a_57(dummy)

    val methods = classOf[scala2Lib.Z].getDeclaredMethods.toList ++ classOf[dottyApp.Z].getDeclaredMethods.toList
    methods.foreach { m =>
      m.getName match {
        case s"${prefix}_${suffix}" =>
          val paramClass = m.getParameterTypes()(0).getSimpleName
          assert(prefix == paramClass.toLowerCase, s"Method `$m` erased to `$paramClass` which does not match its prefix `$prefix`")
        case _ =>
      }
    }
  }
}
