
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction2$mcZJD$sp extends JFunction2 {
    abstract boolean apply$mcZJD$sp(long v1, double v2);

    default Object apply(Object v1, Object v2) { return (Boolean) apply$mcZJD$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)); }
}
