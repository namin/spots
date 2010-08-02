import math

blank = '-'
x = 'x'
o = 'o'

be = blank * 9
bu = '--x-xooox'
bo = '--x-xoo-x'
bx = 'xoxox-o-x'
bd = 'ooxxxooxx'
bs = 'o---x----'

def board_length(board):
  a = len(board)
  n = int(math.sqrt(a))
  assert a == n*n
  return n

## board_length(be)
#. 3

## board_length(blank * 9 * 9)
#. 9

def board_index(row, col, n):
  return row * n + col

def board_get(board, row, col, n=None):
  n = n or board_length(board)
  return board[board_index(row, col, n)]

## board_get(be, 0, 2)
#. '-'

## board_get(bu, 0, 2)
#. 'x'
  
## board_get(bu, 1, 2)
#. 'o'

def box_pretty(box):
  if box == blank:
    return ' '
  else:
    return box

def board_pretty(board, n=None):
  n = n or board_length(board)
  width = n * 2 - 1
  line = '\n' + '-' * width + '\n'
  return ''.join(
    (col == 0 and row != 0 and line or '') +
    (col != 0 and '|' or '') +
    box_pretty(board_get(board, row, col, n))
    for row in xrange(n) for col in xrange(n))

## print board_pretty(be)
#.  | | 
#. -----
#.  | | 
#. -----
#.  | | 
#. 

## print board_pretty(bu)
#.  | |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. 

def rows(n):
  return [
    [(row, col) for col in xrange(n)]
    for row in xrange(n)]

def cols(n):
  return [
    [(row, col) for row in xrange(n)]
    for col in xrange(n)]

def diagonals(n):
  last = n - 1
  return [
    [(i, i) for i in xrange(n)],
    [(last - i, i) for i in xrange(n)]]

def lines(n):
  return rows(n) + cols(n) + diagonals(n)

def board_lines(board, n=None):
  n = n or board_length(board)
  return (
    ''.join([
	board_get(board, row, col, n)
	for (row, col) in line])
    for line in lines(n))

## len(list(board_lines(be)))
#. 8

## (list(board_lines(bu)))
#. ['--x', '-xo', 'oox', '--o', '-xo', 'xox', '-xx', 'oxx']

def line_winner(line):
  first = line[0]
  return first != blank and all(
    first == other for other in line) and first or None

## print line_winner('xxx')
#. x
#. 

## print line_winner('xox')
#. None
#. 

draw = '!'
unknown = '?'

def some(seq):
  for e in seq:
    if e:
      return e
  return False

def board_winner(board, n=None):
  n = n or board_length(board)
  return some(
    line_winner(line) for line in board_lines(board, n))

def board_draw(board, n=None):
  return all([box != '-' for box in board]) and draw

def board_status(board, n=None):
  n = n or board_length(board)
  return (
    board_winner(board, n) or
    board_draw(board, n) or
    unknown)

## board_status(be)
#. '?'

## board_status(bu)
#. '?'

## board_status(bo)
#. '?'

## board_status(bx)
#. 'x'

## board_status(bd)
#. '!'

def board_turn(board):
  cx = board.count(x)
  co = board.count(o)

  assert cx == co or cx == co + 1

  return cx == co and x or o

## board_turn(be)
#. 'x'

## board_turn(bu)
#. 'x'

## board_turn(bo)
#. 'o'

def board_moves(board, n=None):
  n = n or board_length(board)

  return (
    (row, col)
    for row in xrange(n)
    for col in xrange(n)
    if board_get(board, row, col, n) == blank)

## list(board_moves(be))
#. [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

## list(board_moves(bu))
#. [(0, 0), (0, 1), (1, 0)]

## list(board_moves(bs))
#. [(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1), (2, 2)]

def swap(t):
  a, b = t
  return (b, a)

def fst(t):
  a, _ = t
  return a

def snd(t):
  _, b = t
  return b

def symmetries(n):
  last = n - 1

  sym_x = [
    ((row1, col), (row2, col))
    for (row1, row2) in [(i, last - i) for i in xrange((n + 1)/ 2)]
    for col in xrange(n)]

  sym_y = [(swap(m1), swap(m2)) for (m1, m2) in sym_x]

  sym_d = [
    ((row, col), (last - col, last - row))
     for row in xrange(n)
     for col in xrange(n - row)]

  lower_left_triangle = [
    (row, col)
    for row in xrange(n)
    for col in xrange(row + 1)]

  sym_dr = [(box, swap(box)) for box in lower_left_triangle]

  return [
    ([(m1, m2) for (m1, m2) in s if m1 != m2],
     set(m for (m, _) in s))
    for s in [sym_x, sym_y, sym_d, sym_dr]]

## symmetries(3)
#. [([((0, 0), (2, 0)), ((0, 1), (2, 1)), ((0, 2), (2, 2))], set([(0, 1), (1, 2), (0, 0), (1, 1), (1, 0), (0, 2)])), ([((0, 0), (0, 2)), ((1, 0), (1, 2)), ((2, 0), (2, 2))], set([(0, 1), (0, 0), (2, 1), (2, 0), (1, 0), (1, 1)])), ([((0, 0), (2, 2)), ((0, 1), (1, 2)), ((1, 0), (2, 1))], set([(0, 1), (0, 0), (1, 1), (2, 0), (1, 0), (0, 2)])), ([((1, 0), (0, 1)), ((2, 0), (0, 2)), ((2, 1), (1, 2))], set([(0, 0), (2, 1), (2, 0), (2, 2), (1, 0), (1, 1)]))]

def symmetry_try(sym, board, moves, n=None):
  n = n or board_length(board)

  pairs, boxes = sym

  for ((r1, c1), (r2, c2)) in pairs:
    v1 = board_get(board, r1, c1, n)
    v2 = board_get(board, r2, c2, n)
    if v1 != v2:
      return moves

  return moves.intersection(boxes)

def board_distinct_moves(board, n=None, syms=None):
  n = n or board_length(board)
  syms = symmetries(n)

  moves = set(board_moves(board, n))
  for sym in syms:
    moves = symmetry_try(sym, board, moves, n)

  return moves

## board_distinct_moves(be)
#. set([(1, 0), (0, 0), (1, 1)])

## board_distinct_moves(bu)
#. set([(0, 1), (1, 0), (0, 0)])

## board_distinct_moves(bs)
#. set([(2, 0), (1, 0), (2, 1), (2, 2)])

def turn_switch(turn):
  assert turn == x or turn == o

  if turn == x:
    return o
  else:
    return x

def board_apply(
  board, move, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  row, col = move
  i = board_index(row, col, n)
  new_board = [box for box in board]
  new_board[i] = turn

  return ''.join(new_board)

def board_analyze(board, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  winning_moves = []
  draw_moves = []
  losing_moves = []

  for move in board_distinct_moves(board, n):
    new_board = board_apply(
        board, move, n, turn)
    result = board_result(new_board, n, turn)
    if result == turn:
      winning_moves.append(move)
    elif result == draw:
      draw_moves.append(move)
    else:
      losing_moves.append(move)

  return (
    winning_moves, draw_moves, losing_moves)
      
def board_result(board, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  result = board_status(board, n)
  if result != unknown:
    return result

  other_turn = turn_switch(turn)
  result, _ = board_suggest(
    board, n, other_turn)

  return result

## board_analyze(bd)
#. ([], [], [])

## board_analyze(bx)
#. ([], [], [(1, 2), (2, 1)])

## board_analyze(bu)
#. ([(0, 0)], [(0, 1), (1, 0)], [])

## board_analyze(bo)
#. ([], [(0, 0)], [(0, 1), (1, 0), (2, 1)])

def board_suggest(board, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  draw_result = (None, None)
  for move in board_distinct_moves(board, n):
    new_board = board_apply(
        board, move, n, turn)
    result = board_result(new_board, n, turn)
    if result == turn:
      return (result, move)
    elif result == draw:
      draw_result = (result, move)

  return draw_result

## board_suggest(bu)
#. ('x', (0, 0))

## board_suggest('---ox----')
#. ('!', (0, 0))

def board_play(board, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  move = True
  while move:
    print 'Turn of ', turn
    print board_pretty(board, n)

    _, move = board_suggest(board, n, turn)
    if move:
      board = board_apply(
	board, move, n, turn)
      turn = turn_switch(turn)
  
  print 'Game over'
  print status_pretty(board_status(board, n), turn)

def status_pretty(status, turn):
  if status == unknown:
    return turn + ' gave up'
  elif status == draw:
    return '.... draw ...'
  else:
    return status + ' won'

## board_play(bu)
#. Turn of  x
#.  | |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. Turn of  o
#. x| |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. Game over
#. x won
#. 

## board_play(bo)
#. Turn of  o
#.  | |x
#. -----
#.  |x|o
#. -----
#. o| |x
#. Turn of  x
#. o| |x
#. -----
#.  |x|o
#. -----
#. o| |x
#. Turn of  o
#. o| |x
#. -----
#. x|x|o
#. -----
#. o| |x
#. Turn of  x
#. o|o|x
#. -----
#. x|x|o
#. -----
#. o| |x
#. Turn of  o
#. o|o|x
#. -----
#. x|x|o
#. -----
#. o|x|x
#. Game over
#. .... draw ...
#. 

## board_play('---ox----')
#. Turn of  x
#.  | | 
#. -----
#. o|x| 
#. -----
#.  | | 
#. Turn of  o
#. x| | 
#. -----
#. o|x| 
#. -----
#.  | | 
#. Turn of  x
#. x| | 
#. -----
#. o|x| 
#. -----
#.  | |o
#. Turn of  o
#. x| | 
#. -----
#. o|x| 
#. -----
#. x| |o
#. Turn of  x
#. x| |o
#. -----
#. o|x| 
#. -----
#. x| |o
#. Turn of  o
#. x| |o
#. -----
#. o|x|x
#. -----
#. x| |o
#. Turn of  x
#. x|o|o
#. -----
#. o|x|x
#. -----
#. x| |o
#. Turn of  o
#. x|o|o
#. -----
#. o|x|x
#. -----
#. x|x|o
#. Game over
#. .... draw ...
#. 

def interactive_game(n=3, board=None, turn=None):
  board = board or (blank * n * n)
  turn = turn or board_turn(board)

  print board_pretty(board)

  row, col = None, None

  while row == None or col == None:
    move = input('Your turn: ')
    try:
      row, col = move
    except ValueError:
      print 'Invalid move: enter a tuple, like (0, 0)'
    if row >= n:
      row = None
      print 'Invalid row', row
    if col >= n:
      col = None
      print 'Invalid column', col
    v = board_get(board, row, col, n)
    if v != blank:
      row, col = None, None
      print 'Box already taken by', v

  board = board_apply(board, move, n, x)
  status = board_status(board, n)

  print 'You played', turn, 'on', move
  print board_pretty(board)

  if status != unknown:
    print 'Game over'
    print status_pretty(status, turn)
    return

  other_turn = turn_switch(turn)

  _, move = board_suggest(
    board, n, other_turn)

  if move == None:
    print 'Game over'
    print status_pretty(unknown, other_turn)
    return

  board = board_apply(
    board, move, n, other_turn)
  print other_turn, 'played on', move

  status = board_status(board, n)
  if status != unknown:
    print board_pretty(board)
    print 'Game over'
    print status_pretty(status, other_turn)
    return
  
  interactive_game(3, board, turn)

if __name__ == '__main__':
  interactive_game()

