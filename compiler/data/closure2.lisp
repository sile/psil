(let ((a 10))
  (let ((fn (lambda (b c) ($+ a ($+ b c)))))
    (setval a 100)
    (fn 20 30)))
