(let ((a 10))
  (let ((fn (lambda (b c) ($+ a ($+ b c)))))
    (fn 20 30)))
