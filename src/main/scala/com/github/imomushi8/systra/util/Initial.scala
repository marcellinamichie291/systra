package com.github.imomushi8.systra.util

trait Initial[A]:
    def empty(): A

object Initial:
    def apply[A]()(using init: Initial[A]): A = init.empty()
