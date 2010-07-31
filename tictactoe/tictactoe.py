import math

blank = '-'
x = 'x'
o = 'o'

be = blank * 9
bu = '--x-xooox'
bo = '--x-xoo-x'
bx = 'xoxox-o-x'
bd = 'ooxxxooxx'

def board_length(board):
  a = len(board)
  n = int(math.sqrt(a))
  assert a == n*n
  return n

## board_length(be)
#. 3

## board_length(blank * 9 * 9)
#. 9

def board_index(board, row, col, n=None):
  n = n or board_length(board)

  return row * n + col

def board_get(board, row, col, n=None):
  n = n or board_length(board)

  return board[board_index(
      board, row, col, n)]

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

  r = []
  for row in xrange(0, n):
    for col in xrange(0, n):
      if col == 0 and row != 0:
	r.append('\n')
	r.append('-' * (n * 2 - 1))
	r.append('\n')
      if col != 0:
	r.append('|')
      r.append(box_pretty(
	  board_get(board, row, col)))
  return ''.join(r)

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

def board_lines(board, n=None):
  n = n or board_length(board)

  for row in xrange(0, n):
    yield board[
	board_index(board, row, 0, n):
	1 + board_index(board, row, n-1, n)]
  for col in xrange(0, n):
    line = []
    for row in xrange(0, n):
      line.append(board_get(
	  board, row, col, n))
    yield ''.join(line)

  diagonal = []
  for i in xrange(0, n):
    diagonal.append(board_get(
	board, i, i, n))
  yield ''.join(diagonal)

  rev_diagonal = []
  for i in xrange(0, n):
    rev_diagonal.append(board_get(
	board, n-1-i, i, n))
  yield ''.join(rev_diagonal)

## len(list(board_lines(be)))
#. 8

## (list(board_lines(bu)))
#. ['--x', '-xo', 'oox', '--o', '-xo', 'xox', '-xx', 'oxx']

def line_winning(line, n=None):
  n = n or len(line)

  return line[0] != blank and line.count(line[0]) == n

## line_winning('xxx')
#. True

## line_winning('xox')
#. False

def all(xs):
  for x in xs:
    if not x:
      return False
  return True

## all([True])
#. True

## all([True, False])
#. False

draw = '!'
unknown = '?'

def board_status(board, n=None):
  n = n or board_length(board)

  winning_lines = [
    line
    for line in board_lines(board, n)
    if line_winning(line, n)]
  
  if winning_lines != []:
    winner = winning_lines[0][0]
    assert all(
      [winner == line[0]
       for line in winning_lines])
    return winner

  if all([box != '-' for box in board]):
    return draw
  else:
    return unknown

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

  if cx == co:
    return x
  else:
    return o

## board_turn(be)
#. 'x'

## board_turn(bu)
#. 'x'

## board_turn(bo)
#. 'o'

def board_moves(board, n=None):
  n = n or board_length(board)

  for row in xrange(0, n):
    for col in xrange(0, n):
      if board_get(
	board, row, col, n) == blank:
	yield (row, col)

## list(board_moves(bu))
#. [(0, 0), (0, 1), (1, 0)]

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
  i = board_index(board, row, col, n)
  new_board = [box for box in board]
  new_board[i] = turn

  return ''.join(new_board)

def board_analyze(board, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  winning_moves = []
  draw_moves = []
  losing_moves = []

  for move in board_moves(board, n):
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
  (w, d, l) = board_analyze(
    board, n, other_turn)

  if w != []:
    return other_turn
  elif d != []:
    return draw
  else:
    return turn

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

  draw_move = None
  for move in board_moves(board, n):
    new_board = board_apply(
        board, move, n, turn)
    result = board_result(new_board, n, turn)
    if result == turn:
      return move
    elif result == draw:
      draw_move = move

  return draw_move

## board_suggest(bu)
#. (0, 0)

## board_suggest('---ox----')
#. (0, 0)

def board_play(board, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  move = True
  while move:
    print 'Turn of ', turn
    print board_pretty(board, n)

    move = board_suggest(board, n, turn)
    if move:
      board = board_apply(
	board, move, n, turn)
      turn = turn_switch(turn)
  
  print 'Game over'
  status = board_status(board, n)
  if status == unknown:
    print turn, ' gave up'
  elif status == draw:
    print '.... draw ...'
  else:
    print status, 'won'

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
#. o| |x
#. -----
#. x|x|o
#. -----
#. o|o|x
#. Turn of  o
#. o|x|x
#. -----
#. x|x|o
#. -----
#. o|o|x
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
#. Game over
#. o  gave up
#. 

if __name__ == '__main__':
  board_play(be)
