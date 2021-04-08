;; Board
(setq example (make-array '(4 4)
    :initial-contents '(
                       ((1 0) (0 0) (0 1) (0 1))
                       ((2 0) (0 0) (0 2) (0 1))
                       ((1 2) (0 2) (0 2) (0 2))
                       ((0 2) (0 2) (0 2) (3 2))
                       )
               )
)

;; Verifica se as coordenadas são válidas para determinado board
(defun invalidCoordinates (board row column)
    (defvar size (array-dimension board 0))

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

;; Altera o valor de determinado ponto no board
(defun changeValue (board row column v)
    (defvar newPoint (list v (getSector (getPoint board row column))))

    (setf (aref board row column) newPoint)
)

;; Pega os pontos de um setor
(defun getSectorPoints (board sector)
    (defvar size (array-dimension board 0))
    (defvar points ())

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
    (defvar sectorPoints (getSectorPoints board sector))
    
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
    (defvar size (array-dimension board 0))
    
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

;; Resolve o puzzle
(defun solve (board v)
    (defvar emptyLocation (nextEmptyPoint board))
    (defvar point (getPointFromLocation board emptyLocation))
    (defvar sector (getSector point))
    (defvar sectorPoints (getSectorPoints board sector))

    (defvar maxValue (length sectorPoints))


    ;; O puzzle está resolvido
    (if (< (car emptyLocation) 0)
      (return-from solve board)
    )

)

(print (solve example 1))

