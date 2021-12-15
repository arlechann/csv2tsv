(load "./lib/csv2tsv.scm")

(define (help)
  (print "Usage: csv2tsv [-h]\nPass csv text via stdin"))

(define (main args)
  (if (= 1 (length args))
      (csv2tsv)
      (begin
        (help)
        (exit 1))))

