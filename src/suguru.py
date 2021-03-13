from typing import List
from dataclasses import dataclass

@dataclass
class Point():
    def __init__(self, value, sector):
        self.value = value
        self.sector = sector

    def __repr__(self):
        return f'Point(value: {self.value}, sector: {self.sector})'

@dataclass
class Board():
    def __init__(self, points, size):
        self.points = points
        self.size = size


    def __repr__(self):
        for i in range(self.size):
            for j in range(self.size):
                print(f' {self.points[i * self.size + j].value} ', end='')
            print('')

        return ''

@dataclass
class Direction():
    def __init__(self, dx, dy):
        self.dx = dx
        self.dy = dy

    def __repr__(self):
        return f"Direction(dx: {self.dx}, dy: {self.dy})"

solutions = []

board = Board(
    [
        Point(0, 0), Point(0, 0), Point(0, 0), Point(3, 1), Point(0, 2), Point(0, 2), Point(2, 3), Point(0, 3),
        Point(4, 0), Point(0, 4), Point(0, 4), Point(0, 1), Point(0, 2), Point(0, 2), Point(0, 3), Point(0, 3), 
        Point(0, 4), Point(2, 4), Point(0, 1), Point(0, 1), Point(0, 2), Point(0, 5), Point(0, 5), Point(0, 3), 
        Point(0, 4), Point(1, 6), Point(5, 6), Point(0, 1), Point(0, 7), Point(1, 7), Point(5, 5), Point(0, 5), 
        Point(0, 8), Point(2, 8), Point(0, 6), Point(0, 6), Point(0, 7), Point(0, 9), Point(0, 9), Point(0, 5), 
        Point(0, 10), Point(0, 8), Point(0, 8), Point(0, 6), Point(4, 7), Point(0, 11), Point(0, 9), Point(4, 9), 
        Point(0, 10), Point(0, 10), Point(0, 8), Point(0, 12), Point(0, 7), Point(3, 11), Point(0, 11), Point(0, 9), 
        Point(0, 10), Point(5, 10), Point(0, 12), Point(0, 12), Point(0, 12), Point(5, 12), Point(0, 11), Point(0, 11), 
    ],
    8
)


def print_sector(board: Board, sector: int):
    for i in range(board.size):
        for j in range(board.size):
            if board.points[i * board.size + j].sector == sector:
                print(' X ', end='')
            else: 
                print(' 0 ', end='')
        print('')

directions = [
    Direction(-1, -1), Direction(0, -1), Direction(1, -1),
    Direction(-1, 0) , Direction(0, 0) , Direction(1, 0),
    Direction(-1, 1) , Direction(0, 1) , Direction(1, 1)    
]

def check(board: Board, x: int, y: int, n: int):
    point = board.points[x * board.size + y]
    
    sector_conflicting_points = list(filter(lambda p: p.sector == point.sector and p.value == n, board.points))
    
    if len(sector_conflicting_points) > 0:
        return False
    
    for direction in directions:
        if (x + direction.dx) >= board.size or (x + direction.dx) < 0 or (y + direction.dy) >= board.size or (y + direction.dy) < 0:
            continue
        else:
            target = board.points[(x + direction.dx) * board.size + (y + direction.dy)]

            if target.value == n:
                return False

    return True

def nextEmpty(board: Board):
    for x in range(board.size):
        for y in range(board.size):
            if board.points[x * board.size + y].value == 0:
                return x, y

    return None, None

def solve(inputBoard: Board):
    row, column = nextEmpty(board)

    if row is None:
        return True

    sector_points = list(filter(lambda p: p.sector == board.points[row * board.size + column].sector, board.points))

    for i in range(1, len(sector_points) + 1):
        if check(board, row, column, i):
            board.points[row * board.size + column].value = i
        
            if(solve(board)):
                return True

        board.points[row * board.size + column].value = 0

    return False

if solve(board):
    print(board)
else:
    print("Não tem solução")
