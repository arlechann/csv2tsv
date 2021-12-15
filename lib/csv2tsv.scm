(define (comma? c)
  (eq? #\, c))

(define (double-quotation? c)
  (eq? #\" c))

(define (newline? c)
  (eq? #\newline c))

(define (row-end? c)
  (or (newline? c) (eof-object? c)))

(define (field-end? c)
  (or (comma? c) (row-end? c)))

(define (read-unquoted-field . args)
  (let ((in (get-optional args (standard-input-port))))
    (let rec ((s '()))
      (if (field-end? (peek-char in))
          (list->string (reverse! s))
          (rec (cons (read-char in) s))))))

(define (read-quoted-field . args)
  (let ((in (get-optional args (standard-input-port))))
    (read in)))

(define (read-field . args)
  (let ((in (get-optional args (standard-input-port))))
    ((if (double-quotation? (peek-char in)) read-quoted-field read-unquoted-field) in)))

(define (read-row . args)
  (let ((in (get-optional args (standard-input-port))))
    (let rec ((row (list (read-field in))))
      (let ((c (peek-char in)))
        (if (row-end? c)
            (reverse! row)
            (begin (read-char in)
                   (rec (cons (read-field in) row))))))))

(define (read-csv . args)
  (let ((in (get-optional args (standard-input-port))))
    (let rec ((content '()))
      (let ((c (peek-char in)))
        (cond ((eof-object? c) (reverse! content))
              ((newline? c) (read-char in) (rec content))
              (else (rec (cons (read-row in) content))))))))

(define (length1? ls)
  (and (not (null? ls))
       (null? (cdr ls))))

(define (print-row row . args)
  (let ((out (get-optional args (standard-output-port))))
    (if (not (null? row))
      (let rec ((ls row))
        (display (car ls) out)
        (if (length1? ls)
            (newline out)
            (begin
              (display #\tab out)
              (rec (cdr ls))))))))

(define (print-tsv content . args)
  (let ((out (get-optional args (standard-output-port))))
    (let rec ((content content))
      (if (not (null? content))
          (begin (print-row (car content) out)
                 (rec (cdr content)))))))

(define (csv2tsv . args)
  (let-optionals* args ((in (standard-input-port))
                        (out (standard-output-port)))
    (print-tsv (read-csv in) out)))

