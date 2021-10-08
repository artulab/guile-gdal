#!/usr/bin/env -S guile -s
!#

(add-to-load-path "../..")
(use-modules (gdal))
(use-modules (rnrs bytevectors))

(all-register)

;; read dataset using the binding functions

(let* ((dataset (open-dataset "raster-small.txt" GA_READONLY))
       (h-band (get-raster-band dataset 1))
       (x-size (get-raster-band-x-size h-band))
       (y-size (get-raster-band-y-size h-band))
       (size (* x-size y-size))
       (bv (make-s32vector size)))
    (begin 
        (raster-io h-band GF_READ 0 0 x-size y-size bv x-size y-size GDT_INT32 0 0)
        (for-each (lambda (i) (format #t "~a " (s32vector-ref bv i)))
          (iota size))))

(newline)

;; and using the helper functions in the extesion module

(use-modules (gdal extension))

(let* ((dataset (open-dataset "raster-small.txt" GA_READONLY))
       (h-band (get-raster-band dataset 1))
       (buf (make-buffer-all-from-band h-band GDT_INT32)))
    (for-each-pixel (lambda (p) (format #t "~a " p)) buf))

(newline)
    