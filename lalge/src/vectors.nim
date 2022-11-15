import std/math, std/sequtils, std/sugar
import types

const dim_mismatch = "Dimensional mismatch - check your vectors"

## Vector addition
func `+`*(a, b: Vector): Vector =
  assert a.len() == b.len(), dim_mismatch
  result.setLen(a.len)
  for i in 0 ..< a.len():
    result[i] = (a[i] + b[i])

## Vector subtraction
func `-`*(a, b: Vector): Vector =
  assert a.len() == b.len(), dim_mismatch
  result.setLen(a.len)
  for i in 0 ..< a.len():
    result[i] = (a[i] - b[i])

## Vector dot product
func `*`*(a, b: Vector): float =
  assert a.len() == b.len(), dim_mismatch
  for i in 0 ..< a.len():
    result += a[i] * b[i]

func dot*(a, b: Vector): float =
  return a * b

## Scalar-Vector multiplication
func `*`*(a: float, b: Vector): Vector =
  return map(b, (x) => (a*x))

## Produce the length (in space) of a vector
func length*(a: Vector): float =
  return sqrt(a * a)

## Produce the number of elements in a vector
func size*(a: Vector): int =
  return len(a)

## Returns whether two vectors are orthagonal
func ortho*(a, b: Vector): bool =
  return a * b == 0

## Produce the angle between two vectors, in radians
func angle*(a, b: Vector): Radian =
  return arccos((a * b) / (a.length * b.length))

## Produce the cross product between two 3D vectors
func cross*(a, b: Vector): Vector =
  assert a.len() == 3, "The first vector is not three-dimensional"
  assert b.len() == 3, "The second vector is not three-dimensional"
  return @[
    (a[1]*b[2]) - (b[1]*a[2]),
    (a[2]*b[0]) - (b[2]*a[0]),
    (a[0]*b[1]) - (b[0]*a[1])
  ]
