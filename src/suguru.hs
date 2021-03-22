import Prelude

size :: Int
size = 7

-- O identificador de setor utilizado
type Sector = Int

-- Um valor de 1 a N, onde N é o tamanho do setor no qual o ponto está
type Value = Int

-- Um par de coordenadas X, Y
type Location = (Int, Int)

-- Um par que representa a direção a ser checada em determinada posição
type Direction = (Int, Int)

-- Um ponto no tabuleiro, no formato (Value, Sector)
type Point = (Value, Sector)

-- Um tabuleiro de Suguru
type Board = [[Point]]

example = [ [ (0, 00), (0, 00), (0, 00), (3, 01), (0, 02), (0, 02), (2, 03), (0, 03) ],
            [ (4, 00), (0, 04), (0, 04), (0, 01), (0, 02), (0, 02), (0, 03), (0, 03) ], 
            [ (0, 04), (2, 04), (0, 01), (0, 01), (0, 02), (0, 05), (0, 05), (0, 03) ], 
            [ (0, 04), (1, 06), (5, 06), (0, 01), (0, 07), (1, 07), (5, 05), (0, 05) ], 
            [ (0, 08), (2, 08), (0, 06), (0, 06), (0, 07), (0, 09), (0, 09), (0, 05) ], 
            [ (0, 10), (0, 08), (0, 08), (0, 06), (4, 07), (0, 11), (0, 09), (4, 09) ], 
            [ (0, 10), (0, 10), (0, 08), (0, 12), (0, 07), (3, 11), (0, 11), (0, 09) ], 
            [ (0, 10), (5, 10), (0, 12), (0, 12), (0, 12), (5, 12), (0, 11), (0, 11) ] ]

main = do
    print (check example (5,0) 2)
    -- show board
    -- let solutions = solve board

-- Retorna o setor de um ponto
getSector :: Point -> Sector
getSector (_, sector) = sector

-- Retorna o valor de um ponto
getValue :: Point -> Value
getValue (value, _) = value

-- Retorna o valor de um ponto a partir de sua localização
getValueFromLocation :: Board -> Location -> Value
getValueFromLocation board location = getValue (getPoint board location)

-- Retorna o valor de x de uma localização
getX :: Location -> Int
getX (x, _) = x

-- Retorna o valor de y de uma localização
getY :: Location -> Int
getY (_, y) = y

-- Retorna o ponto na localização passada dentro do tabuleiro 
getPoint :: Board -> Location -> Point
getPoint board (x, y) = ((board !! x) !! y)

-- Retorna todos os pontos de um setor indicado pelo indice n
getSectorPoints :: Board -> Sector -> [Point]
getSectorPoints board n = [(board !! row) !! col | row <- [0..size], col <- [0..size], getSector ((board !! row) !! col) == n]

-- Checa a existência de um ponto com o valor [value] no [sector]
sectorHasValue :: Board -> Sector -> Value -> Bool
sectorHasValue board sector value = not (null (
        [point | point <- (getSectorPoints board sector), (getValue point) == value]
    ))

-- Retorna todos os pontos vazios do tabuleiro
emptyPoints :: Board -> [Point]
emptyPoints board = [(board !! row) !! col | row <- [0..size], col <- [0..size], getValue ((board !! row) !! col) == 0]

-- Retorna a localização de todos os pontos vazios do tabuleiro
emptyPointsLocation :: Board -> [Location]
emptyPointsLocation board = [(row, col) | row <- [0..size], col <- [0..size], getValue ((board !! row) !! col) == 0]

-- Retorna a localização do primeiro ponto vazio do tabuleiro
nextEmptyPointLocation :: Board -> Location
nextEmptyPointLocation board | (emptyPointsLocation board == []) = (-1, -1)
                             | otherwise = head (emptyPointsLocation board)

-- Checa se o valor v pode ser inserido na posição location
check :: Board -> Location -> Int -> Bool
check board location v | (sectorHasValue board (getSector (getPoint board location)) v) = False
                       | (checkAdjacents board location v) = False
                       | otherwise = True

-- Checa os valores adjacentes de determinada localização
-- Retorna True caso ache um ponto igual ao valor fornecido, False caso o contrário
checkAdjacents :: Board -> Location -> Int -> Bool
checkAdjacents board location v = do
                                  let c1 = checkAdjacent board ((getX location), ((getY location) - 1)) v
                                  let c2 = checkAdjacent board (((getX location) + 1), ((getY location) - 1)) v
                                  let c3 = checkAdjacent board (((getX location) + 1), (getY location)) v
                                  let c4 = checkAdjacent board (((getX location) + 1), ((getY location) + 1)) v
                                  let c5 = checkAdjacent board ((getX location), ((getY location) + 1)) v
                                  let c6 = checkAdjacent board (((getX location) - 1), ((getY location) + 1)) v
                                  let c7 = checkAdjacent board (((getX location) - 1), (getY location)) v
                                  let c8 = checkAdjacent board (((getX location) - 1), ((getY location) - 1)) v
                                  c1 || c2 || c3 || c4 || c5 || c6 || c7 || c8

-- Checa um valor adjacente
checkAdjacent :: Board -> Location -> Value -> Bool
checkAdjacent board target v | (((getX target) < 0) || ((getY target) < 0)) = False
                             | (((getX target) >= size) || ((getY target) >= size)) = False
                             | (v == (getValueFromLocation board target)) = True
                             | otherwise = False

-- Resolve o quebra-cabeças
solve :: Board -> Board
solve board | (nextEmptyPointLocation board == (-1, -1)) = board

