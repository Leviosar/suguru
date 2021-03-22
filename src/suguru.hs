import Prelude

size :: Int
size = 7

-- O identificador de setor utilizado
type Sector = Int

-- Um valor de 1 a N, onde N é o tamanho do setor no qual o ponto está
type Value = Int

-- Um par de coordenadas X, Y
type Location = (Int, Int)

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
    print (sectorHasValue example 0 4)
    -- show board
    -- let solutions = solve board

-- Retorna o setor de um ponto
getSector :: Point -> Sector
getSector (_, sector) = sector

-- Retorna o valor de um ponto
getValue :: Point -> Value
getValue (value, _) = value

-- Retorna o ponto na localização passada dentro do tabuleiro 
getPoint :: Board -> Location -> Point
getPoint board (x, y) = ((board !! x) !! y)

-- Retorna todos os pontos de um setor indicado pelo indice n
getSectorPoints :: Board -> Sector -> [Point]
getSectorPoints board n = [(board !! row) !! col | row <- [0..size], col <- [0..size], getSector ((board !! row) !! col) == n]

-- Checa a existência de um ponto com o valor [value] no [sector]
sectorHasValue :: Board -> Sector -> Value -> Bool
sectorHasValue board sector value = not (null (
        [point | point <- (getSectorPoints board sector), (getValue point) /= value] 
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

-- check :: Board -> Location -> Value -> Bool
-- check board (x, y) n = sectorHasValue board (getSector(getPoint (board (x, y)))) 1
