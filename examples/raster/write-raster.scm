#!/usr/bin/env -S guile -s
!#

(add-to-load-path "../..")

(use-modules (gdal))
(use-modules (rnrs bytevectors))
(use-modules (gdal extension))

(all-register)

;; we get the dataset handle of the file "raster-small.txt" in read-only
;; mode, and then get the handle of the first band. make-buffer-all-from-band
;; function reads all the pixels in the input file into the memory in INT32
;; format.

;; map-pixel function, here, returns a new binary buffer of type INT16
;; with 1 for pixels greater than 0, and 0 otherwise.

;; for-each-pixel method prints all the values of
;; the new raster into the console, and write-buffer-to-file saves the buffer
;; into the disk in GeoTIFF format.

(let* ((dataset (open-dataset "raster-small.txt" GA_READONLY))
       (h-band (get-raster-band dataset 1))
       (buf (make-buffer-all-from-band h-band GDT_INT32)))
    (begin
        (define new-buf (map-pixel (lambda (p) (if (> p 0) 1 0))
                                   buf #:buf-type GDT_INT16))
        (for-each-pixel (lambda (p) (format #t "~a " p)) new-buf)
        (write-buffer-to-file new-buf GDN_GTIFF
                              "new-raster-small.tif" #:no-data -1)))

(newline)
