package util

package object realtime {
  implicit def Double2UnitFunc(d: => Double): (Unit => Double) = (Unit => d)
  implicit def Double2Func(d: => Double): (() => Double) = (() => d)
}