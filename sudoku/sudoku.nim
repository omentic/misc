# Simple, inefficient sudoku solver.
# But it's cute. Written on a plane.
import std/[sequtils, sugar]
type Board = seq[seq[int]]

const input: Board = @[
  @[0,5,8, 6,0,0, 0,4,0],
  @[1,0,0, 7,0,0, 0,9,0],
  @[0,6,0, 0,0,4, 8,0,0],
  @[0,8,0, 9,0,0, 0,0,0],
  @[4,0,0, 0,1,0, 0,0,2],
  @[0,0,0, 0,0,3, 0,6,0],
  @[0,0,2, 3,0,0, 0,7,0],
  @[0,9,0, 0,0,1, 0,0,4],
  @[0,1,0, 0,0,7, 5,3,0]
]

func valid_input(board: Board): bool =
  for j in 0 ..< 9:
    for i in 0 ..< 9:
      if input[i][j] != 0 and input[i][j] != board[i][j]:
        return false
  return true

assert input.valid_input()

func valid_rows(board: Board): bool =
  for i in 0 ..< 9:
    var used: seq[int]
    for j in 0 ..< 9:
      var num = board[i][j]
      if num in used:
        return false
      if num != 0:
        used.add(num)
  return true

assert input.valid_rows()

func valid_columns(board: Board): bool =
  for j in 0 ..< 9:
    var used: seq[int]
    for i in 0 ..< 9:
      var num = board[i][j]
      if num in used:
        return false
      if num != 0:
        used.add(num)
  return true

assert input.valid_rows()

func valid_boxes(board: Board): bool =
  for l in 0 ..< 3:
    for k in 0 ..< 3:
      var used: seq[int]
      for j in 0 ..< 3:
        for i in 0 ..< 3:
          var num = board[3*k + i][3*l + j]
          if num in used:
            return false
          if num != 0:
            used.add(num)
  return true

assert input.valid_boxes()

func valid(board: Board): bool =
  return board.valid_input() and board.valid_boxes() and
         board.valid_rows() and board.valid_columns()

assert input.valid()

proc print(board: Board) =
  for j in 0 ..< 9:
    for i in 0 ..< 9:
      stdout.write(board[j][i])
      stdout.write(" ")
      if i == 2 or i == 5:
        stdout.write("| ")
    stdout.write("\n")
    if j == 2 or j == 5:
      stdout.write("---------------------")
      stdout.write("\n")
  stdout.write("\n\n")

proc generate_boards(board: Board) =
  var board = board
  for j in 0 ..< 9:
    for i in 0 ..< 9:
      if board[i][j] == 0:
        for k in 1 .. 9:
          board[i][j] = k
          if board.valid():
            generate_boards(board)
  if board.valid():
    board.print()
    quit()

generate_boards(input)
