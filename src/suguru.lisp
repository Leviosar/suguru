;; Board
(defvar example (make-array '(4 4) 
    :initial-contents '(
                       ((1 0) (0 0) (0 1) (0 1))
                       ((2 0) (0 0) (0 2) (0 1))
                       ((1 2) (0 2) (0 2) (0 2))
                       ((0 2) (0 2) (0 2) (3 2))
                       )
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
        (checkAdjacents board row column v)
      )
    )
)

(defun checkAdjacents (board row column v)

)

(print (check example 0 1 3))
