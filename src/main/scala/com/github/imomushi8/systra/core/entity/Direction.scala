package com.github.imomushi8.systra.core.entity

enum Direction(val dir: Int):
  case Long  extends Direction(1)
  case Short extends Direction(-1)