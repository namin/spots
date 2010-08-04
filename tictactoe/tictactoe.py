blank = '-'
x = 'x'
o = 'o'

def indices(v, lst):
  return [i for i in xrange(len(lst)) if lst[i] == v]

def parse(game):
  assert len(game) == 9
  return (set(indices(x, game)), set(indices(o, game)))

emptyBoard = parse('---------')

def move(row, col):
  return row * 3 + col

def prettyBoard((xs, os)):
  line = '\n' + '-' * 5 + '\n'
  return ''.join(
    (line if col == 0 and row != 0 else '') +
    ('|' if col != 0 else '') +
    (x if move(row, col) in xs else o if move(row, col) in os else ' ')
    for row in xrange(3) for col in xrange(3))

## print prettyBoard(emptyBoard)
#.  | | 
#. -----
#.  | | 
#. -----
#.  | | 
#. 

## print prettyBoard(parse('--x-xooox'))
#.  | |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. 

def someContained(subsets, set):
  return any(subset <= set for subset in subsets)

winningSets = set(
  frozenset(ws) for ws in
  [[0,1,2],[3,4,5],[6,7,8],
   [0,3,6],[1,4,7],[2,5,8],
   [0,4,8],[2,4,6]])

def wins(moves):
  return someContained(winningSets, moves)

draw = '!'
unknown = '?'

def findStatus((xs, os)):
  assert len(xs) + len(os) <= 9
  return (
    x if wins(xs)
    else o if wins(os)
    else draw if len(xs) + len(os) == 9
    else unknown)

def findTurn((xs, os)):
  assert len(xs) in [len(os), len(os) + 1]
  return x if len(xs) == len(os) else o

def switch(turn):
  return x if turn == o else o

allMoves = set(xrange(9))

def findOpenMoves((xs, os)):
  return allMoves.difference(xs).difference(os)

def singleton(x):
  return set([x])

def play(turn, move, (xs, os)):
  if turn == x:
    xs = xs.union(singleton(move))
  else:
    os = os.union(singleton(move))
  return (xs, os)

def search(board, base, rec, turn):
  (xs, os) = board

  status = findStatus(board)
  if status != unknown:
    yield base(turn, status)
  else:
    for move in findOpenMoves(board):
      yield rec(turn, move, search(
          play(turn, move, board), base, rec, switch(turn)))

def predictStatus(subs, turn):
  return (
    turn if all(sub == turn for sub in subs)
    else switch(turn) if any(sub == switch(turn) for sub in subs)
    else draw)

def score(status, turn):
  return (
    1 if status == turn
    else -1 if status == switch(turn)
    else 0)

def strategy0(board, turn):
  def base(turn, status):
    return (status, None)

  def rec(turn, move, subresult):
    return (predictStatus([substatus for (substatus, _) in subresult], turn), move)

  _, move = max(
     (score(status, turn), move)
     for (status, move) in search(board, base, rec, turn))

  return move

## strategy0(parse('---ox----'), x)
#. 8

## strategy0(parse('xx-oxoo--'), x)
#. 8

def avg(lst):
  return sum(lst) / 1.0 * len(lst) if lst != [] else 0

def strategy1(board, turn):
  def base(turn, status):
    return (status, [], None)

  def rec(turn, move, subresult):
    subs = [substatus for (substatus, _, _) in subresult]
    return (predictStatus(subs, turn), subs, move)

  _, _, move = max(
     (score(status, turn), avg([score(sub, turn) for sub in subs]), move)
     for (status, subs, move) in search(board, base, rec, turn))

  return move

## strategy1(parse('---ox----'), x)
#. 8

## strategy1(parse('xx-oxoo--'), x)
#. 8

def prettyStatus(status, turn):
  assert status != unknown
  if status == draw:
    return '.... draw ...'
  else:
    return status + ' won'

def simulator(board, turn, strategyTurn, strategyOther):
  print prettyBoard(board)

  status = findStatus(board)
  if status == unknown:
    move = strategyTurn(board, turn)
    other = switch(turn)
    if move is not None:
      print turn, 'plays on', move
      simulator(
        play(turn, move, board), other,
        strategyOther, strategyTurn)
    else:
      print turn, 'gave up so', other, 'wins'
  else:
    print 'Game over'
    print prettyStatus(findStatus(board), turn)

## simulator(parse('--x-xooox'), x, strategy1, strategy1)
#.  | |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. x plays on 0
#. x| |x
#. -----
#.  |x|o
#. -----
#. o|o|x
#. Game over
#. x won
#. 

def strategyUser(board, turn):
  move = None
  while move is None:
    move = input('Play for ' + turn + ': ')
    if move not in allMoves:
      print 'Invalid move, not in', allMoves
    if move not in findOpenMoves(board):
      print 'Cell is not free.'
  return move

def interactive(strategyComputer=strategy1, userFirst=None):
  if userFirst is None:
    inputFirst = raw_input('Do you want to play first (y/n)? ')
    userFirst = not (inputFirst != "" and inputFirst[0].lower() == 'n')
    if userFirst:
      print 'Yes.'
    else:
      print 'No.'

  strategyTurn, strategyOther = (
    (strategyUser, strategyComputer)
    if userFirst else
    (strategyComputer, strategyUser))

  simulator(emptyBoard, findTurn(emptyBoard), strategyTurn, strategyOther)

if __name__ == '__main__':
  interactive()
