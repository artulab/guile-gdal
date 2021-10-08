# Guile GDAL Bindings

[GDAL](https://gdal.org/) Scheme bindings for [Guile](https://www.gnu.org/software/guile/) programming language.

This library allows you to perform the following tasks:

* Open a raster file for reading and writing
* Access metadata of raster files
* Compute statistics of raster files
* Access layers of raster dataset
* Helper functions, providing idiomatic Scheme interface to the GDAL APIs
* TODO: OGR support

## Example

Read/write raster files using GDAL binding functions or helper functions

```
(use-modules (gdal))
(use-modules (gdal extension))

(use-modules (rnrs bytevectors))

;; initialize GDAL by registering GDAL drivers

(all-register)

;; read raster dataset using binding functions

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

;; or read raster dataset using helper functions in the extension module
;; providing more convenient way

(let* ((dataset (open-dataset "raster-small.txt" GA_READONLY))
       (h-band (get-raster-band dataset 1))
       (buf (make-buffer-all-from-band h-band GDT_INT32)))
    (for-each-pixel (lambda (p) (format #t "~a " p)) buf))

;; transform pixels using map-pixel function, returning a new binary buffer
;; of type INT16 with 1 for pixels greater than 0, and 0 otherwise.
;; use write-buffer-to-file to save the buffer into the disk in GeoTIFF format.

(let* ((dataset (open-dataset "raster-small.txt" GA_READONLY))
       (h-band (get-raster-band dataset 1))
       (buf (make-buffer-all-from-band h-band GDT_INT32)))
    (begin
        (define new-buf (map-pixel (lambda (p) (if (> p 0) 1 0))
                                   buf #:buf-type GDT_INT16))
        (write-buffer-to-file new-buf GDN_GTIFF
                              "new-raster-small.tif" #:no-data -1)))
```

See examples folder for more code samples.
