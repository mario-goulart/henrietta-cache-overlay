(module henrietta-cache-overlay ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken)
  (use data-structures files posix setup-api))
 (chicken-5
  (import (chicken base)
          (chicken condition)
          (chicken file)
          (chicken file posix)
          (chicken format)
          (chicken irregex)
          (chicken pathname)
          (chicken process-context)
          (chicken sort)
          (chicken string))

  ;; From setup-api (chicken-4.13.0)
  (define (version>=? v1 v2)
    (define (version->list v)
      (map (lambda (x) (or (string->number x) x))
           (irregex-split "[-\\._]" (->string v))))
    (let loop ((p1 (version->list v1))
               (p2 (version->list v2)))
      (cond ((null? p1) (null? p2))
            ((null? p2))
            ((number? (car p1))
             (and (number? (car p2))
                  (or (> (car p1) (car p2))
                      (and (= (car p1) (car p2))
                           (loop (cdr p1) (cdr p2))))))
            ((number? (car p2)))
            ((string>? (car p1) (car p2)))
            (else
             (and (string=? (car p1) (car p2))
                  (loop (cdr p1) (cdr p2)))))))
  )
 (else
  (error "Unsupported CHICKEN version.")))

(define (create/replace-symbolic-link oldname newname)
  (condition-case
   (create-symbolic-link oldname newname)
   ((exn file)
    (delete-file newname)
    (create/replace-symbolic-link oldname newname))))

(define (overlay cache-dir overlay-dir)
  (for-each
   (lambda (cache-dirname)
     (let* ((egg-cache-dir (make-pathname cache-dir cache-dirname))
            (egg-versions (directory egg-cache-dir)))
       (if (null? egg-versions)
           (fprintf (current-error-port)
                    "WARNING: no version directory in ~a"
                    egg-cache-dir)
           (let ((latest-version
                  (car (sort egg-versions version>=?)))
                 (egg-overlay-dir (make-pathname overlay-dir cache-dirname)))
             (create-directory egg-overlay-dir 'recursive)
             (create/replace-symbolic-link
              (make-pathname egg-cache-dir latest-version)
              egg-overlay-dir)))))
   (directory cache-dir)))

(define (usage exit-code)
  (let ((port (if (zero? exit-code)
                  (current-output-port)
                  (current-error-port))))
    (fprintf
     port
     "Usage: henrietta-cache-overlay <henrietta cache dir> <overlay dir>\n")
    (exit exit-code)))

(let ((args (command-line-arguments)))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (unless (eq? (length args) 2)
    (usage 1))

  (let ((cache-dir (normalize-pathname (car args)))
        (overlay-dir (normalize-pathname (cadr args))))
    (overlay cache-dir overlay-dir)))

) ;; end module
