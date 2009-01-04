center = (2,2)
ex_board = [(2,4),(0,4),(1,2),(1,0),(4,2)]

def moves(board):
    for (a,b) in board:
        for (c,d) in board:
            if   a==c and abs(b-d)>=1 and not any([e==a and min(b,d)<f<max(b,d) for (e,f) in board]):
                yield (a,b), (a,d+(1 if b>d else -1))
            elif b==d and abs(a-c)>=1 and not any([f==b and min(a,c)<e<max(a,c) for (e,f) in board]):
                yield (a,b), (c+(1 if a>c else -1),b)

## [m for m in moves(ex_board)]
#. [((2, 4), (1, 4)), ((0, 4), (1, 4)), ((1, 2), (1, 1)), ((1, 2), (3, 2)), ((1, 0), (1, 1)), ((4, 2), (2, 2))]

def copy(board):
    return [piece for piece in board]

def apply_move(move, board):
    new_board = copy(board)
    (before,after) = move
    i = new_board.index(before)
    new_board[i] = after
    return new_board

## apply_move(((2,4),(1,4)),ex_board)
#. [(1, 4), (0, 4), (1, 2), (1, 0), (4, 2)]
    
def is_solution(board):
    return board[0] == center

## is_solution(ex_board)
#. False

def bfs(cs,seen=None):
    seen = seen or []
    next_cs = []
    for (path,board) in cs:
        if is_solution(board):
            return path
        for move in moves(board):
            new_board = apply_move(move,board)
            if new_board not in seen:
                next_cs.append((path+[move],new_board))
                seen.append(new_board)
    if next_cs == []:
        return None
    return bfs(next_cs, seen)

def solve(board):
    return bfs([([],board)])

## solve(ex_board)
#. [((2, 4), (1, 4)), ((1, 0), (1, 1)), ((1, 2), (3, 2)), ((1, 4), (1, 2)), ((1, 2), (2, 2))]

## solve([(3,1), (0,4), (0,2), (1,0), (2,4), (4,3)])
#. [((0, 4), (0, 3)), ((4, 3), (1, 3)), ((1, 3), (1, 1)), ((1, 1), (2, 1)), ((2, 1), (2, 3)), ((0, 3), (1, 3)), ((1, 3), (1, 1)), ((3, 1), (2, 1)), ((2, 1), (2, 2))]

## solve([(2,4),(0,4),(0,0),(4,4),(4,0)])
#. [((0, 4), (1, 4)), ((4, 4), (4, 1)), ((4, 0), (1, 0)), ((1, 4), (1, 1)), ((4, 1), (2, 1)), ((2, 4), (2, 2))]

## solve([(1,0),(0,4),(2,4),(4,4),(4,1)])
#. [((2, 4), (1, 4)), ((4, 4), (2, 4)), ((1, 4), (1, 1)), ((0, 4), (1, 4)), ((1, 4), (1, 2)), ((1, 1), (3, 1)), ((1, 0), (1, 1)), ((3, 1), (2, 1)), ((4, 1), (3, 1)), ((2, 1), (2, 3)), ((1, 1), (2, 1)), ((2, 1), (2, 2))]
