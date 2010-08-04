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

def board_index((row, col), n):
  return row * n + col

def board_get(board, move, n=None):
  n = n or board_length(board)
  return board[board_index(move, n)]

## board_get(be, (0, 2))
#. '-'

## board_get(bu, (0, 2))
#. 'x'
  
## board_get(bu, (1, 2))
#. 'o'

def cell_pretty(cell):
  return cell if cell != blank else ' '

def board_pretty(board, n=None):
  n = n or board_length(board)
  width = n * 2 - 1
  line = '\n' + '-' * width + '\n'
  return ''.join(
    (line if col == 0 and row != 0 else '') +
    ('|' if col != 0 else '') +
    cell_pretty(board_get(board, (row, col), n))
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
	board_get(board, move, n)
	for  move in line])
    for line in lines(n))

## len(list(board_lines(be)))
#. 8

## (list(board_lines(bu)))
#. ['--x', '-xo', 'oox', '--o', '-xo', 'xox', '-xx', 'oxx']

def line_winner(line):
  first = line[0]
  return first if first != blank and all(
    first == other for other in line) else None

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
  return all([cell != '-' for cell in board]) and draw

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

  return x if cx == co else o

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
    if board_get(board, (row, col), n) == blank)

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

  sym_dr = [(cell, swap(cell)) for cell in lower_left_triangle]

  return [
    ([(m1, m2) for (m1, m2) in s if m1 != m2],
     set(m for (m, _) in s))
    for s in [sym_x, sym_y, sym_d, sym_dr]]

## symmetries(3)
#. [([((0, 0), (2, 0)), ((0, 1), (2, 1)), ((0, 2), (2, 2))], set([(0, 1), (1, 2), (0, 0), (1, 1), (1, 0), (0, 2)])), ([((0, 0), (0, 2)), ((1, 0), (1, 2)), ((2, 0), (2, 2))], set([(0, 1), (0, 0), (2, 1), (2, 0), (1, 0), (1, 1)])), ([((0, 0), (2, 2)), ((0, 1), (1, 2)), ((1, 0), (2, 1))], set([(0, 1), (0, 0), (1, 1), (2, 0), (1, 0), (0, 2)])), ([((1, 0), (0, 1)), ((2, 0), (0, 2)), ((2, 1), (1, 2))], set([(0, 0), (2, 1), (2, 0), (2, 2), (1, 0), (1, 1)]))]

def symmetry_try(sym, board, moves, n=None):
  n = n or board_length(board)

  pairs, cells = sym

  for (m1, m2) in pairs:
    v1 = board_get(board, m1, n)
    v2 = board_get(board, m2, n)
    if v1 != v2:
      return moves

  return moves.intersection(cells)

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

  i = board_index(move, n)
  new_board = [cell for cell in board]
  new_board[i] = turn

  return ''.join(new_board)

def board_stats(board, b, r, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  status = board_status(board, n)
  if status != unknown:
    yield b(turn, status)
  else:
    for move in board_distinct_moves(board, n):
      new_board = board_apply(board, move, n, turn)
      yield r(turn, move, board_stats(
	  new_board, b, r, n, turn_switch(turn)))

def status_winning(status, turn):
  return status == turn

def status_loosing(status, turn):
  return status == turn_switch(turn)

def predict_status(substati, turn):
  return (
    turn
    if all(status_winning(s, turn) for s in substati) else
    turn_switch(turn)
    if any(status_loosing(s, turn) for s in substati) else
    draw)

def strategy_win_or_draw(board, n=None, turn=None):
  turn = turn or board_turn(board)

  def b(turn, status):
    return (status, None)

  def r(turn, move, stats):
    return (predict_status([s for (s, _) in stats], turn), move)

  result = list(board_stats(board, b, r))
  return (
    some(move for (status, move) in result
	 if status_winning(status, turn)) or
    some(move for (status, move) in result
         if not status_loosing(status, turn)) or
    None)

## strategy_win_or_draw(bu)
#. (0, 0)

## strategy_win_or_draw('---ox----')
#. (0, 1)

## strategy_win_or_draw('xx-oxoo--')
#. (0, 2)

## strategy_win_or_draw('----x----')
#. (0, 0)

def avg(lst):
  return sum(lst) / 1.0 * len(lst) if lst != [] else 0

def strategy_score(board, n=None, turn=None, scores=(1, 2, 4)):
  turn = turn or board_turn(board)

  winning_score, draw_score, loosing_score = scores

  def score(turn, status):
    return (
      winning_score if status_winning(status, turn) else
      draw_score if not status_loosing(status, turn) else
      loosing_score)

  def b(turn, status):
    return (status, [], None)

  def r(turn, move, stats):
    substati = [s for (s, _, _) in stats]
    return (predict_status(substati, turn), substati, move)

  stats = list(board_stats(board, b, r))
  result = [
    (score(turn, status), avg([score(turn, s) for s in substati]), move)
    for status, substati, move in stats]
  result.sort()
  return result[0][-1] if result != [] else None

## strategy_score(bu)
#. (0, 0)

## strategy_score('---ox----')
#. (0, 0)

## strategy_score('xx-oxoo--')
#. (0, 2)

## strategy_score('----x----')
#. (0, 0)

def board_play(board, strategy_turn, strategy_other, n=None, turn=None):
  n = n or board_length(board)
  turn = turn or board_turn(board)

  print board_pretty(board, n)

  status = board_status(board, n)
  if status == unknown:
    move = strategy_turn(board, n, turn)
    other_turn = turn_switch(turn)
    if move:
      print turn, 'plays on', move
      board_play(
	board_apply(board, move, n, turn),
	strategy_other, strategy_turn,
	n, other_turn)
    else:
      print turn, 'gave up so', other_turn, 'wins'
  else:
    print 'Game over'
    print status_pretty(board_status(board, n), turn)

def status_pretty(status, turn):
  assert status != unknown
  if status == draw:
    return '.... draw ...'
  else:
    return status + ' won'

## board_play(bu, strategy_win_or_draw, strategy_win_or_draw)
#.  | |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. x plays on (0, 0)
#. x| |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. Game over
#. x won
#. 

## board_play(bo, strategy_win_or_draw, strategy_win_or_draw)
#.  | |x
#. -----
#.  |x|o
#. -----
#. o| |x
#. o plays on (0, 0)
#. o| |x
#. -----
#.  |x|o
#. -----
#. o| |x
#. x plays on (1, 0)
#. o| |x
#. -----
#. x|x|o
#. -----
#. o| |x
#. o plays on (0, 1)
#. o|o|x
#. -----
#. x|x|o
#. -----
#. o| |x
#. x plays on (2, 1)
#. o|o|x
#. -----
#. x|x|o
#. -----
#. o|x|x
#. Game over
#. .... draw ...
#. 

## board_play('---ox----', strategy_win_or_draw, strategy_win_or_draw)
#.  | | 
#. -----
#. o|x| 
#. -----
#.  | | 
#. x plays on (0, 1)
#.  |x| 
#. -----
#. o|x| 
#. -----
#.  | | 
#. o gave up so x wins
#. 

## board_play('----x----', strategy_score, strategy_win_or_draw)
#.  | | 
#. -----
#.  |x| 
#. -----
#.  | | 
#. o plays on (0, 0)
#. o| | 
#. -----
#.  |x| 
#. -----
#.  | | 
#. x plays on (2, 0)
#. o| | 
#. -----
#.  |x| 
#. -----
#. x| | 
#. o plays on (0, 2)
#. o| |o
#. -----
#.  |x| 
#. -----
#. x| | 
#. x plays on (0, 1)
#. o|x|o
#. -----
#.  |x| 
#. -----
#. x| | 
#. o plays on (2, 1)
#. o|x|o
#. -----
#.  |x| 
#. -----
#. x|o| 
#. x plays on (1, 2)
#. o|x|o
#. -----
#.  |x|x
#. -----
#. x|o| 
#. o plays on (1, 0)
#. o|x|o
#. -----
#. o|x|x
#. -----
#. x|o| 
#. x plays on (2, 2)
#. o|x|o
#. -----
#. o|x|x
#. -----
#. x|o|x
#. Game over
#. .... draw ...
#. 

def strategy_ask(board, n=None, turn=None):
  turn = turn or board_turn(board)

  row, col = None, None
  while row == None or col == None:
    move = input('Play for ' + turn + ': ')
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
    v = board_get(board, (row, col), n)
    if v != blank:
      row, col = None, None
      print 'Cell already taken by', v

  return (row, col)

def interactive(strategy_computer=strategy_score, user_first=None):
  if user_first is None:
    input_first = raw_input('Do you want to play first (y/n)? ')
    user_first = not (input_first != "" and input_first[0].lower() == 'n')
    if user_first:
      print 'Yes.'
    else:
      print 'No.'
  
  strategy_turn, strategy_other = (
    (strategy_ask, strategy_computer)
    if user_first else
    (strategy_computer, strategy_ask))

  board_play(be, strategy_turn, strategy_other)

if __name__ == '__main__':
  interactive()

