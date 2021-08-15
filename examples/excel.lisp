;; Evaluate (function CELL) on every cell in the `cells' symbol
(defun map-cells (function)
  (let ((i 0)
        (j 0))
    (dolist (row cells)
      (dolist (col row)
        (set (cells i j) (function (nth (cons i j) cells)))
        (+= j 1))
      (+= i 1)
      (set j 0))))

;; Print the cells
(defun print-cells ()
  (when (list? cells)
    (dolist (row cells)
      (print (join row " ")))))

;; Evaluate the references to cell coordinates (if present) in a cell
(defun parse-cell (expr)
  (if (list? expr)
      (map parse-cell expr)
    (do
     (when (symbol? expr)
       (let ((expr-string (symbol->string expr))
             (sep-index (find expr-string ",")))
         (when (>= sep-index 0)
           (let ((row (- (string->number (slice expr-string 0 sep-index)) 1))
                 (col (- (string->number (slice expr-string (+ sep-index 1))) 1))
                 (valid-position false))
             (when (< -1 row (length cells))
               (set valid-position (< -1 col (length (nth row cells)))))
             (if valid-position
                 (set expr (parse-cell (nth (cons row col) cells)))
               (error (concat "reference to invalid cell (" row ", " col ")")))))))
     expr)))

;; Evaluate the contents of a cell
(defun eval-cell (expr)
  (if (list? expr)
      (eval expr)
    expr))

;; Parse a file into rows and columns
(defun parse-file (file-path)
  (filter-map length
              (lambda (row)
                (filter-map length parse (split row "|")))
              (split (open file-path) "\n")))

;; Evaluate a file
(defun eval-file (file-path)
  (let cells (parse-file (nth 0 args)))
  (map-cells parse-cell)
  (map-cells eval-cell))

;; Evaluate and print a file
(defun print-file (file-path)
  (eval-file file-path)
  (print-cells))

;; Go through the command line arguments and print the evaluated files
(map print-file args)
