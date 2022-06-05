package com.github.imomushi8.systra.core.util

trait Initial[A]:
    def empty(): A

object Initial:
    def apply[A]()(using init: Initial[A]): A = init.empty()
