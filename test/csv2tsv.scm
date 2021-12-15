(use gauche.test)
(test-start "csv2tsv")

(load "./lib/csv2tsv.scm")

(define (call/test-section describe body)
  (test-section describe)
  (body))

(define call/ostr call-with-output-string)

(call/test-section "comma?" (lambda ()
  (test* "expect true" #t (comma? #\,))
  (test* "expect false" #f (comma? #\a))))

(call/test-section "double-quotation?" (lambda ()
  (test* "expect true" #t (double-quotation? #\"))
  (test* "expect false" #f (double-quotation? #\a))))

(call/test-section "newline?" (lambda ()
  (test* "expect true" #t (newline? #\newline))
  (test* "expect false" #f (newline? #\a))))

(call/test-section "row-end?" (lambda ()
  (test* "eof" #t (row-end? (eof-object)))
  (test* "newline" #t (row-end? #\newline))
  (test* "comma" #f (row-end? #\,))))

(call/test-section "field-end?" (lambda ()
  (test* "eof" #t (field-end? (eof-object)))
  (test* "newline" #t (field-end? #\newline))
  (test* "comma" #t (field-end? #\,))
  (test* "alphabet a" #f (field-end? #\a))))

(call/test-section "read-unquoted-field" (lambda ()
  (test* "empty" "" (read-unquoted-field (open-input-string ",abc")))
  (test* "comma" "abc" (read-unquoted-field (open-input-string "abc,def")))
  (test* "eof" "abcdef" (read-unquoted-field (open-input-string "abcdef")))))

(call/test-section "read-quoted-field" (lambda ()
  (test* "empty" "" (read-quoted-field (open-input-string "\"\",\"abc\"")))
  (test* "comma" "abc" (read-quoted-field (open-input-string "\"abc\",\"def\"")))
  (test* "eof" "abcdef" (read-quoted-field (open-input-string "\"abcdef\"")))))

(call/test-section "read-field" (lambda ()
  (test* "unquoted empty field" "" (read-field (open-input-string ",abc")))
  (test* "unquoted single field" "abc" (read-field (open-input-string "abc")))
  (test* "unquoted double field" "abc" (read-field (open-input-string "abc,def")))
  (test* "quoted empty field" "" (read-field (open-input-string "\"\",\"abc\"")))
  (test* "quoted single filed" "abc" (read-field (open-input-string "\"abc\"")))
  (test* "quoted double field" "abc" (read-field (open-input-string "\"abc\",\"def\"")))))

(call/test-section "read-row" (lambda ()
  (test* "single unquoted field" '("abc") (read-row (open-input-string "abc")))
  (test* "double unquoted field" '("abc" "def") (read-row (open-input-string "abc,def")))
  (test* "triple unquoted field" '("abc" "def" "ghi") (read-row (open-input-string "abc,def,ghi")))
  (test* "triple unquoted empty field" '("" "" "") (read-row (open-input-string ",,")))
  (test* "single quoted field" '("abc") (read-row (open-input-string "\"abc\"")))
  (test* "double quoted field" '("abc" "def") (read-row (open-input-string "\"abc\",\"def\"")))
  (test* "triple quoted field" '("abc" "def" "ghi") (read-row (open-input-string "\"abc\",\"def\",\"ghi\"")))
  (test* "triple quoted empty field" '("" "" "") (read-row (open-input-string "\"\",\"\",\"\"")))))

(call/test-section "read-csv" (lambda ()
  (test* "empty" '() (read-csv (open-input-string "")))
  (test* "single row" '(("abc" "def" "ghi")) (read-csv (open-input-string "abc,\"def\",ghi")))
  (test* "multi rows" '(("abc" "def") ("ghi" "jkl")) (read-csv (open-input-string "abc,def\n\"ghi\",\"jkl\"")))
  (test* "skip empty row" '(("abc" "def") ("ghi" "jkl")) (read-csv (open-input-string "\nabc,def\n\n\"ghi\",\"jkl\"\n")))))

(call/test-section "length1?" (lambda ()
  (test* "nil" #f (length1? '()))
  (test* "length 1" #t (length1? '(0)))
  (test* "length 2" #f (length1? '(0 1)))))

(call/test-section "print-row" (lambda ()
  (test* "null" "" (call/ostr (lambda (out) (print-row '() out))))
  (test* "empty" "\n" (call/ostr (lambda (out) (print-row '("") out))))
  (test* "single field" "abc\n" (call/ostr (lambda (out) (print-row '("abc") out))))
  (test* "double field" "abc\tdef\n" (call/ostr (lambda (out) (print-row '("abc" "def") out))))
  (test* "triple field" "abc\tdef\tghi\n" (call/ostr (lambda (out) (print-row '("abc" "def" "ghi") out))))))

(call/test-section "print-tsv" (lambda ()
  (test* "null" "" (call/ostr (lambda (out) (print-tsv '() out))))
  (test* "empty" "\n" (call/ostr (lambda (out) (print-tsv '(("")) out))))
  (test* "single rows" "abc\tdef\tghi\n" (call/ostr (lambda (out) (print-tsv '(("abc" "def" "ghi")) out))))
  (test* "multi rows" "abc\tdef\nghi\tjkl\n" (call/ostr (lambda (out) (print-tsv '(("abc" "def") ("ghi" "jkl")) out))))))

(call/test-section "csv2tsv" (lambda ()
  (test* "empty" "" (call/ostr (lambda (out) (csv2tsv (open-input-string "") out))))
  (test* "single rows" "abc\tdef\tghi\n" (call/ostr (lambda (out) (csv2tsv (open-input-string "abc,def,ghi\n") out))))
  (test* "multi rows" "abc\tdef\nghi\tjkl\n" (call/ostr (lambda (out) (csv2tsv (open-input-string "abc,def\nghi,jkl") out))))
  (test* "skip empty row" "abc\tdef\nghi\tjkl\n" (call/ostr (lambda (out) (csv2tsv (open-input-string "\nabc,def\n\nghi,jkl\n") out))))))

(test-end)
