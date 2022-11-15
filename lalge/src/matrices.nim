import std/math, std/sugar
import types

# A few notes about Nim:

# "result" lets us operate on the return type
# without (necessarily) constructing it.
# this is very useful, and as such i use it a lot.

# nim also supplies many functional tools which
# i also made liberal use of, occasionally to the
# detriment of readability. sorry.

# the "*" operator on procs is the public marker.
# oh, and functions with "side effects" are called "procs".

# TODO: implement a better error handling system
const not_square = "The matrix is not square"
const mismatch = "Mismatched rows and columns"

## Overrides the echo() proc to provide more human-readable output
proc echo*(A: Matrix) =
  echo "@["
  for row in A:
    echo "  ", row
  echo "]"

## Generate a new, filled XxX Matrix
func gen*(rows, columns: Natural, element: float = 0.0): Matrix =
  result.setLen(rows)
  for i in 0 ..< rows:
    result[i].setLen(columns)
    for j in 0 ..< columns:
      result[i][j] = element

## Returns the number of rows in a matrix
func rows*(A: Matrix): int =
  return A.len()

## Returns the number of columns in a matrix
func cols*(A: Matrix): int =
  if A.rows() == 0:
    return 0
  else:
    return A[0].len()

## Simple rows iterator for uniformity
iterator rows*(A: Matrix): RowVector =
  for row in A:
    yield row

## Simple column iterator for matrices
iterator cols*(A: Matrix): Vector =
  for i in 0 ..< A.cols():
    var row: Vector
    for j in 0 ..< A.rows():
      row.add(A[i][j])
    yield row

## Apply an arbitrary function to every element of a matrix
func map*(A: Matrix, oper: proc(i, j: int): float): Matrix =
  result = gen(A.rows(), A.cols())
  for i in 0 ..< A.rows():
    for j in 0 ..< A.cols():
      # Note that the position is _actively_ provided
      result[i][j] = oper(i, j)

# TODO: check if these meet assert requirements

## Matrix addition
func `+`*(A, B: Matrix): Matrix =
  return map(A, (i, j) => A[i][j] + B[i][j])

## Matrix subtraction
func `-`*(A, B: Matrix): Matrix =
  return map(A, (i, j) => A[i][j] - B[i][j])

## Scalar-Matrix multiplication
func `*`*(a: float, B: Matrix): Matrix =
  return map(B, (i, j) => a * B[i][j])

## Scalar-Matrix multiplication
# func `*`*(A: Matrix, b: int): Matrix =
#   return b * A

## Matrix multiplication
func `*`*(A, B: Matrix): Matrix =
  assert A.rows() == B.cols(), mismatch
  result = gen(A.rows(), B.cols())
  for i in 0 ..< A.rows():
    for j in 0 ..< B.cols():
      for k in 0 ..< B.rows():
        result[i][j] += A[i][k] * B[k][j]

## Absolute value of a matrix
func abs*(A: Matrix): Matrix =
  return map(A, (i, j) => abs(A[i][j]))

## Simple row function for uniformity
func row*(A: Matrix, r: int): RowVector =
  return A[r]

## Returns a "column vector" of a matrix as a Xx1 Matrix
# func col*(A: Matrix, j: int): Vector =
#   result = gen(A.rows(), 1)
#   for i in 0 ..< A.rows():
#     result[i] = @[A[i][j]]

## Alternate implementation of col, returns a Vector
func col*(A: Matrix, c: int): Vector =
  for row in A:
    result.add(@[row[c]])

## Produce a vector of the diagonal entries of a matrix
func diag*(A: Matrix): Vector =
  assert A.rows() == A.cols(), not_square
  for i in 0 ..< A.rows():
    result.add(A[i][i])

## Transpose the rows and columns of a matrix
func transpose*(A: Matrix): Matrix =
  result = gen(A.cols(), A.rows())
  return map(result, (i, j) => (A[j][i]))

# func transpose*(a: Vector): Matrix =
#   result = gen(a.len(), 1)
#   return map(result, (i, j) => (a[j]))

## Generate an arbitary sized identity matrix
func identity*(size: Natural): Matrix =
  return map(gen(size, size), (i, j) => (if i==j: 1.0 else: 0.0))

## Oft-used identity matrices
let
  I1*: Matrix = identity(1)
  I2*: Matrix = identity(2)
  I3*: Matrix = identity(3)
  I4*: Matrix = identity(4)
  I5*: Matrix = identity(5)

## Calculates the determinant of a matrix through Laplace expansion
func det*(A: Matrix): float =
  assert A.rows() == A.cols(), not_square
  # Shortcut by formula
  if A.rows() == 2:
    return (A[0][0]*A[1][1]) - (A[0][1]*A[1][0])
  # Actual factual formula
  for i in 0 ..< A.rows():
    var sub: Matrix
    for a in 1 ..< A.rows():
      var row: RowVector
      for b in 0 ..< A.cols():
        if b == i: continue
        row.add(A[a][b])
      sub.add(row)
    result += (-1.0)^i * A[0][i] * det(sub)

# func spans(A, B: Matrix): bool =
#   return false
# func subspace(a, b: Matrix): bool =
#   return false

# Matrix Properties

## Calculate the dimension of a matrix by Gaussian Elimination
# func dim(a: Matrix): int =
#   return 1
# func rank(a: Matrix): int =
#   return 1
# func nullity(a: Matrix): int =
#   return 1
