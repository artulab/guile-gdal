;;; guile-gdal --- FFI bindings for GDAL
;;; Copyright (c)  2021 Ahmet Artu Yildirim <ahmet@artulab.com>

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26)
             (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             ((guix build utils) #:select (with-directory-excursion))
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages geo)
             (gnu packages texinfo))

(define %source-dir (dirname (current-filename)))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

(package
  (name "guile-gdal")
  (version "0.1.0")
  (source (local-file %source-dir  #:recursive? #t #:select? git-file?))
  (build-system gnu-build-system)
  (arguments
   '(#:configure-flags 
		 (list (string-append "--with-libgdal-path=" (assoc-ref %build-inputs "gdal") "/lib/libgdal.so"))
     #:make-flags '("GUILE_AUTO_COMPILE=0")
     #:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'bootstrap
         (lambda _ (zero? (system* "sh" "bootstrap")))))))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs
   `(("guile" ,guile-2.2)
     ("gdal" ,gdal)
))
  (synopsis "Guile bindings for GDAL")
  (description "Guile-GDAL provides pure Guile Scheme bindings to the
GDAL C shared library via the foreign function interface.")
  (home-page "https://gitlab.com/ayild/guile-gdal.git")
  (license gpl3+))

