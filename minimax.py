from konane import * 

class MinimaxPlayer(Konane, Player):

        def __init__(self, size, depthLimit):
            Konane.__init__(self, size)
            self.limit = depthLimit
    
        def initialize(self, side):
            self.side = side
            self.name = "MinimaxPlayer"
    
        def getMove(self, board):
            moves = self.generateMoves(board, self.side)
            n = len(moves)
            i = 0
            index = 0
            if n == 0:
                return []
            else:     
                alpha = -float('inf')
                beta = float('inf')
                max = alpha
                
                for move in moves:
                    nextBoard = self.nextBoard(board, self.side, move)
                    x = self.minimize(nextBoard, self.limit, alpha, beta)
                    if(max < x):
                        max = x
                        index = i
                        alpha = x

                    i += 1

            return moves[index]        

        def maximize(self, board, limit, alpha, beta):
            moves = self.generateMoves(board, self.side)
            n = len(moves)

            if n == 0:
                return -float('inf')
            elif(limit > 1):
                x = -float('inf')
                for move in moves: 
                    nextBoard = self.nextBoard(board, self.side, move)
                    x = max(x, self.minimize(nextBoard, limit-1, alpha, beta))
                    alpha = max(x, alpha)
                    # if x (move) is better than current beta prune. 
                    if(x >= beta):
                        return x
                return x
            else:
                return self.eval(board)
            
            

        def minimize(self, board, limit, alpha, beta):
            moves = self.generateMoves(board, self.opponent(self.side))
            n = len(moves)  
              
            if n == 0:
                return float('inf')
            elif(limit > 1):                                    
                x = float('inf')
                for move in moves: 
                    nextBoard = self.nextBoard(board, self.opponent(self.side), move)    
                    x = min(x,self.maximize(nextBoard, limit-1, alpha, beta))
                    # if x is less than alpha we can prune. 
                    beta = min(beta, x)
                    if(x <= alpha):
                        return x 
                    # if x (move) is not less than alpha must keep looking at moves.
                return x
            else:
                return self.eval(board)
            
    
        def eval(self, board):
            h = self.countSymbol(board, self.side)
            h -= self.countSymbol(board, self.opponent(self.side))
            for i in range(self.size):
                h += sum(self.check(board, i, i, i, self.size-1, 2, self.opponent(self.side)))
                h -= sum(self.check(board, i, i, i, self.size-1, 2, self.side))
                h += sum(self.check(board, i, i, self.size-1, i, 2, self.opponent(self.side)))
                h -= sum(self.check(board, i, i, self.size-1, i, 2, self.side))
            
            return h

game = Konane(8)
game.playNGames(2, MinimaxPlayer(8,4), RandomPlayer(8), 0)

