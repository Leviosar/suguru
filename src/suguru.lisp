;; Board
(setq example4 (make-array '(4 4)
    :initial-contents '(
                         ((2 0) (0 0) (0 1) (3 1))
                         ((0 0) (0 0) (4 1) (0 1))
                         ((3 2) (0 2) (0 1) (0 3))
                         ((0 2) (0 3) (0 3) (2 3))
                       )
               )
)

(setq example7 (make-array '(7 7)
    :initial-contents '(
                         ((2 00) (0 01) (0 02) (0 02) (0 02) (0 02) (0 03))
                         ((0 00) (0 01) (4 01) (0 04) (0 03) (0 03) (0 03))
                         ((0 00) (0 01) (0 01) (0 04) (0 05) (3 05) (0 03))
                         ((0 00) (0 06) (0 07) (0 08) (0 08) (0 05) (0 05))
                         ((0 06) (5 06) (0 06) (0 09) (2 08) (0 08) (0 05))
                         ((2 10) (0 10) (3 06) (0 09) (0 09) (0 09) (0 09))
                         ((0 10) (0 10) (0 11) (4 11) (0 11) (0 11) (3 11))
                       )
               )
)

;; Board 3
(setq example8 (make-array '(8 8)
    :initial-contents '(
                         ((0 00) (0 00) (0 00) (3 01) (0 02) (0 02) (2 03) (0 03))
                         ((4 00) (0 04) (0 04) (0 01) (0 02) (0 02) (0 03) (0 03))
                         ((0 04) (2 04) (0 01) (0 01) (0 02) (0 05) (0 05) (0 03))
                         ((0 04) (1 06) (5 06) (0 01) (0 07) (1 07) (5 05) (0 05))
                         ((0 08) (2 08) (0 06) (0 06) (0 07) (0 09) (0 09) (0 05))
                         ((0 10) (0 08) (0 08) (0 06) (4 07) (0 11) (0 09) (4 09))
                         ((0 10) (0 10) (0 08) (0 12) (0 07) (3 11) (0 11) (0 09))
                         ((0 10) (5 10) (0 12) (0 12) (0 12) (5 12) (0 11) (0 11))
                       )
              )
)

;; Verifica se as coordenadas são válidas para determinado board
(defun invalidCoordinates (board row column)
    (setq size (array-dimension board 0))

    (or
      (< row 0)
      (< column 0)

      (>= row size)
      (>= column size)
    )
)

;; Pega o ponto em determinada posição do board
(defun getPoint (board row column)
    (aref board row column)
)

;; Pega o valor de um ponto
(defun getValue (point)
    (car point)
)

;; Pega o setor de um ponto
(defun getSector (point)
    (car (last point))
)

;; Pega a linha de uma posição
(defun getRow (location)
    (car location)
)

;; Pega a coluna de uma posição
(defun getColumn (location)
    (car (last location))
)

;; Pega o ponto a partir de uma localização
(defun getPointFromLocation (board location)
    (getPoint board (getRow location) (getColumn location))
)

;; Pega o valor de um ponto a partir de sua localização
(defun getValueFromLocation (board row column)
    (getValue (getPoint board row column))
)

;; Pega a localização anterior a determinada localização
(defun getPreviousLocation (board location)
    (if (>= (- (getColumn location) 1) 0)
      (list (getRow location) (- (getColumn location) 1))
    (list (- (getRow location) 1) (- (array-dimension board 0) 1))
    )
)

;; Altera o valor de determinado ponto no board
(defun changeValue (board row column v)
    (setq newPoint (list v (getSector (getPoint board row column))))

    (setf (aref board row column) newPoint)
)

;; Pega os pontos de um setor
(defun getSectorPoints (board sector)
    (setq size (array-dimension board 0))
    (setq points ())

      (dotimes (row size)
        (dotimes (column size)
          (setq point (aref board row column))
          (setq pointSector (getSector point))

          (if (=  sector pointSector)
            (push point points)
          )
        )
      )

    points
)

;; Verifica se o setor tem determinado valor
(defun sectorHasValue (board sector value)
    (setq sectorPoints (getSectorPoints board sector))
    
    (dotimes (i (length sectorPoints))
      (setq point (nth i sectorPoints))

      (if (= value (getValue point))
        (return-from sectorHasValue T)
      )
    )
    NIL
)

;; Pega a posição do próximo ponto vazio, caso não existam mais
;; pontos vazio, retorna (-1 -1)
(defun nextEmptyPoint (board)
    (setq size (array-dimension board 0))
    
    (dotimes (row size)
      (dotimes (column size)
        (setq pointValue (getValue (getPoint board row column)))
        
        (if (= pointValue 0)
          (return-from nextEmptyPoint (list row column))
        )
      )
    )
    '(-1 -1)
)

;; Verifica se o valor pode ser inserido em determinada posição
;; no board
(defun check (board row column v)
    (not
      (or
        (sectorHasValue board (getSector (getPoint board row column)) v)
        (invalidAdjacents board row column v)
      )
    )
)

;; Checa os possíveis valores adjacentes de determinada posição para
;; verificar se o valor v pode ser inserido. Retorna True caso um
;; valor igual seja encontrado e False caso o contrário.
(defun invalidAdjacents (board row column v)
    (let (
          (c1 (invalidAdjacent board row         (- column 1) v))
          (c2 (invalidAdjacent board (+ row 1)   (- column 1) v))
          (c3 (invalidAdjacent board (+ row 1)   column       v))
          (c4 (invalidAdjacent board (+ row 1)   (+ column 1) v))
          (c5 (invalidAdjacent board row         (+ column 1) v))
          (c6 (invalidAdjacent board (- row 1)   (+ column 1) v))
          (c7 (invalidAdjacent board (- row 1)   column       v))
          (c8 (invalidAdjacent board (- row 1)   (- column 1) v))
         )
      (or c1 c2 c3 c4 c5 c6 c7 c8)
    )
)

;; Verifica um valor adjacente
(defun invalidAdjacent (board row column v)
    (if (invalidCoordinates board row column)
      (return-from invalidAdjacent NIL)
    (= v (getValueFromLocation board row column))
    )
)

;; Pega locais válidos para serem alterados no board
(defun validPlaces (board)
    (setq size (array-dimension board 0))
    (setq places ())

    (dotimes (row size)
      (dotimes (column size)
        (setq point (aref board row column))
        (if (= (getValue point) 0)
          (push (list row column) places)
        )
      )
    )
    places
)

;; Verifica se um ponto está numa lista
(defun pointInList (point validPoints)

    (dotimes (i (length validPoints))
      (setq validPoint (nth i validPoints))

      (if (and (= (getRow validPoint) (getRow point))
               (= (getColumn validPoint) (getColumn point))
          )
        (return-from pointInList T)
      )
    )
    NIL
)

;; Pega o valor anterior ao ponto que pode ser alterado
(defun findPreviousValidPlace (board currentLocation validLocations)
    (setq size (expt (array-dimension board 0) 2))
    (setq previous (getPreviousLocation board currentLocation))

    (dotimes (i size)
      (if (pointInList previous validLocations)
        (return-from findPreviousValidPlace previous)
       (setq previous (getPreviousLocation board previous))
      )
    )
)

;; Resolve o puzzle
(defun solve (board v)
    (defvar validLocations (validPlaces board))

    (setq emptyLocation (nextEmptyPoint board))

    ;; O puzzle está resolvido
    (if (< (car emptyLocation) 0)
      (return-from solve board)
    )

    (setq point (getPointFromLocation board emptyLocation))
    (setq sector (getSector point))
    (setq sectorPoints (getSectorPoints board sector))

    (setq maxValue (length sectorPoints))

    (if (> v maxValue)
      (progn
        (setq validPlace (findPreviousValidPlace board emptyLocation validLocations))
        (setq newLocationValue (+ (getValueFromLocation board (getRow validPlace) (getColumn validPlace)) 1))

        (changeValue board (getRow validPlace) (getColumn validPlace) 0)

        (solve board newLocationValue)
      )
    ;; else
      (if (check board (getRow emptyLocation) (getColumn emptyLocation) v)
        (progn
          (changeValue board (getRow emptyLocation) (getColumn emptyLocation) v)
          (solve board 1)
        )

        (solve board (+ v 1))
      )
    )
)

(print (solve example7 1))
