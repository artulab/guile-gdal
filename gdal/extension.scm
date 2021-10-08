(define-module (gdal extension)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (ice-9 streams)
  #:use-module (gdal config)
  #:use-module (gdal internal)
  #:use-module (gdal))

;;------------------------------------------------------------------------------

;;; Helper Functions

;;------------------------------------------------------------------------------

(define *buffer-makers*
  `((,GDT_BYTE . ,make-u8vector)
    (,GDT_UINT16 . ,make-u16vector)
    (,GDT_INT16 . ,make-s16vector)
    (,GDT_UINT32 . ,make-u32vector)
    (,GDT_INT32 . ,make-s32vector)
    (,GDT_FLOAT32 . ,make-f32vector)
    (,GDT_FLOAT64 . ,make-f64vector)
    (,GDT_CFLOAT32 . ,make-c32vector)
    (,GDT_CFLOAT64 . ,make-c64vector)))

(define *buffer-refs*
  `((,GDT_BYTE . ,u8vector-ref)
    (,GDT_UINT16 . ,u16vector-ref)
    (,GDT_INT16 . ,s16vector-ref)
    (,GDT_UINT32 . ,u32vector-ref)
    (,GDT_INT32 . ,s32vector-ref)
    (,GDT_FLOAT32 . ,f32vector-ref)
    (,GDT_FLOAT64 . ,f64vector-ref)
    (,GDT_CFLOAT32 . ,c32vector-ref)
    (,GDT_CFLOAT64 . ,c64vector-ref)))

(define *buffer-setters*
  `((,GDT_BYTE . ,u8vector-set!)
    (,GDT_UINT16 . ,u16vector-set!)
    (,GDT_INT16 . ,s16vector-set!)
    (,GDT_UINT32 . ,u32vector-set!)
    (,GDT_INT32 . ,s32vector-set!)
    (,GDT_FLOAT32 . ,f32vector-set!)
    (,GDT_FLOAT64 . ,f64vector-set!)
    (,GDT_CFLOAT32 . ,c32vector-set!)
    (,GDT_CFLOAT64 . ,c64vector-set!)))

;;------------------------------------------------------------------------------

(define* (make-buffer x-size y-size buf-type
                             #:optional (h-band %null-pointer)
                             (x-off 0) (y-off 0))
    "Creates a raster buffer of SRFI-4 vector with internal properties for
the use of extension functions.

Parameters:
    x-size: the width of the region.
    y-size: the height of the region.
    buf-type: the type of the pixel values to be returned.

Optional Parameters:
    h-band: a target band of GDALRasterBandH.
    x-off: the pixel offset to the top left corner of the region of the
target band.
    y-off: the line offset to the top left corner of the region of the
target band.

Note:
    Types GDT_CINT16 and GDT_CINT32 are not supported due to limitations in
SRFI-4 vectors. Use raster-io for reading these values."
  (let* ((size (* x-size y-size))
         (bv ((assv-ref *buffer-makers* buf-type) size)))
    (set! (%gdal-h-band% bv) h-band)
    (set! (%gdal-type% bv) buf-type)
    (set! (%gdal-x-off% bv) x-off)
    (set! (%gdal-y-off% bv) y-off)
    (set! (%gdal-x-size% bv) x-size)
    (set! (%gdal-y-size% bv) y-size)
    bv))

(export make-buffer)

;;------------------------------------------------------------------------------

(define* (copy-buffer data #:optional (copy-data #t)
                      (buf-type (%gdal-type% data)))
    "Copies a raster buffer of SRFI-4 vector with internal properties for
the use of extension functions.

Parameters:
    data: data buffer to copy.

Optional Parameters:
    copy-data: copy pixel values. by default it's true.
    buf-type: data type for the destination buffer."
  (let* ((size (* (%gdal-x-size% data) (%gdal-y-size% data)))
         (buffer-ref (assv-ref *buffer-refs* (%gdal-type% data)))
         (buffer-set! (assv-ref *buffer-setters* buf-type))
         (bv ((assv-ref *buffer-makers* buf-type) size)))
    (begin
        (set! (%gdal-h-band% bv) (%gdal-h-band% data))
        (set! (%gdal-type% bv) buf-type)
        (set! (%gdal-x-off% bv) (%gdal-x-off% data))
        (set! (%gdal-y-off% bv) (%gdal-y-off% data))
        (set! (%gdal-x-size% bv) (%gdal-x-size% data))
        (set! (%gdal-y-size% bv) (%gdal-y-size% data))
        (if copy-data
          (for-each (lambda (offset) (buffer-set! bv offset
                                                    (buffer-ref data offset)))
                      (iota size)))
        bv)))

(export copy-buffer)

;;------------------------------------------------------------------------------

(define (make-buffer-from-band h-band x-off y-off x-size y-size buf-type)
    "Read a region of image data for this band.

Returns the raster buffer which is also SRFI-4 vector with internal properties
for the use of extension functions. If the access fails, it reports error.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    buf-type: the type of the pixel values to be returned.

Note:
    Types GDT_CINT16 and GDT_CINT32 are not supported due to limitations in
SRFI-4 vectors. Use raster-io for reading these values."
  (let* ((size (* x-size y-size))
         (bv ((assv-ref *buffer-makers* buf-type) size)))
    (raster-io h-band GF_READ x-off y-off x-size y-size bv
               x-size y-size buf-type 0 0)
    (set! (%gdal-h-band% bv) h-band)
    (set! (%gdal-type% bv) buf-type)
    (set! (%gdal-x-off% bv) x-off)
    (set! (%gdal-y-off% bv) y-off)
    (set! (%gdal-x-size% bv) x-size)
    (set! (%gdal-y-size% bv) y-size)
    bv))

(export make-buffer-from-band)

;;------------------------------------------------------------------------------

(define (make-buffer-all-from-band h-band buf-type)
    "Read entire region of image data for this band.

Returns a raster buffer of SRFI-4 vector with internal properties for the use
of extension functions. If the access fails, it reports error.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    buf-type: the type of the pixel values to be returned.

Note:
    Types GDT_CINT16 and GDT_CINT32 are not supported due to limitations in
SRFI-4 vectors. Use raster-io for reading these values."
  (let* ((x-size (get-raster-band-x-size h-band))
         (y-size (get-raster-band-y-size h-band))
         (size (* x-size y-size))
         (bv ((assv-ref *buffer-makers* buf-type) size)))
    (raster-io h-band GF_READ 0 0 x-size y-size bv
               x-size y-size buf-type 0 0)
    (set! (%gdal-h-band% bv) h-band)
    (set! (%gdal-type% bv) buf-type)
    (set! (%gdal-x-off% bv) 0)
    (set! (%gdal-y-off% bv) 0)
    (set! (%gdal-x-size% bv) x-size)
    (set! (%gdal-y-size% bv) y-size)
    bv))

(export make-buffer-all-from-band)

;;------------------------------------------------------------------------------

(define (overwrite-buffer-in-band data)
    "Overwrite raster buffer in the associated band of the data.

If the access fails, it reports error. Otherwise it returns void.

Parameters:
    data: the raster buffer to be written."
  (raster-io (%gdal-h-band% data) GF_WRITE (%gdal-x-off% data)
             (%gdal-y-off% data) (%gdal-x-size% data)
             (%gdal-y-size% data) data
             (%gdal-x-size% data) (%gdal-y-size% data)
             (%gdal-type% data) 0 0))

(export overwrite-buffer-in-band)

;;------------------------------------------------------------------------------

(define (add-offset-to-geo-transform geo-transform x-off y-off)
  (let ((t-0 (list-ref geo-transform 0))
        (t-1 (list-ref geo-transform 1))
        (t-2 (list-ref geo-transform 2))
        (t-3 (list-ref geo-transform 3))
        (t-4 (list-ref geo-transform 4))
        (t-5 (list-ref geo-transform 5)))
    (let ((ot-0 (+ t-0 (* x-off t-1) (* y-off t-2)))
          (ot-3 (+ t-3 (* x-off t-4) (* y-off t-5))))
      (list ot-0 t-1 t-2 ot-3 t-4 t-5))))

;;------------------------------------------------------------------------------

(define* (write-buffer-to-file data driver-short-name
                              file-name #:key (no-data #f))
    "Write raster buffer to a new file.

If the access fails, it reports error. Otherwise it returns void.

Parameters:
    data: the raster buffer to be written.
    driver-short-name: the short name of the driver, such as 'GTiff' as a
string or GDN_GTIFF as an enum (see GDN_*), being searched for.
    file-name: the name of the dataset to create.

Optional Parameters:
    no-data: no data value."
    (let* ((driver (get-driver-by-name driver-short-name))
           (dataset (create-dataset driver file-name (%gdal-x-size% data)
                                    (%gdal-y-size% data) 1 (%gdal-type% data)))
           (h-band (get-raster-band dataset 1))
           (geo-transform
            (get-geo-transform (get-band-dataset (%gdal-h-band% data))))
           (projection
            (get-projection-ref (get-band-dataset (%gdal-h-band% data)))))
        (begin
            (set-projection dataset projection)
            (if no-data (set-raster-no-data-value h-band no-data))
            (set-geo-transform dataset
                               (add-offset-to-geo-transform geo-transform
                                                            (%gdal-x-off% data)
                                                            (%gdal-y-off% data)
                                                            ))
            (raster-io h-band GF_WRITE 0
                0 (%gdal-x-size% data)
                (%gdal-y-size% data) data
                (%gdal-x-size% data) (%gdal-y-size% data)
                (%gdal-type% data) 0 0)
            (close-dataset dataset))))

(export write-buffer-to-file)

;;------------------------------------------------------------------------------

(define (read-buffer-pixel data x-off y-off)
    "Read a pixel value of the the raster buffer.

Parameters:
     data: the raster vector.
     x-off: the pixel offset to the top left corner of the data.
     y-off: the line offset to the top left corner of the data."
  (let ((buffer-ref (assv-ref *buffer-refs* (%gdal-type% data)))
        (offset (+ x-off (* y-off (%gdal-x-size% data)))))
    (buffer-ref data offset)))

(export read-buffer-pixel)

;;------------------------------------------------------------------------------

(define (for-each-pixel proc data)
    "Apply proc to each element in the buffer, discarding the returned value.

Parameters:
     proc: the producedure.
     data: the raster vector."
  (let ((buffer-ref (assv-ref *buffer-refs* (%gdal-type% data)))
        (size (* (%gdal-x-size% data) (%gdal-y-size% data))))
    (for-each (lambda (offset) (proc (buffer-ref data offset))) (iota size))))

(export for-each-pixel)

;;------------------------------------------------------------------------------

(define* (map-pixel proc data #:key (buf-type (%gdal-type% data)))
    "Apply proc to each element in the buffer and return a new buffer.

Parameters:
     proc: the producedure.
     data: the raster vector.
     buf-type: data type of pixel values of the destination buffer."
  (let ((buffer-ref (assv-ref *buffer-refs* (%gdal-type% data)))
        (buffer-set! (assv-ref *buffer-setters* buf-type))
        (size (* (%gdal-x-size% data) (%gdal-y-size% data)))
        (bv (copy-buffer data #f buf-type)))
    (begin
        (for-each (lambda (offset) (buffer-set! bv offset
                                    (proc (buffer-ref data offset))))
            (iota size))
        bv)))

(export map-pixel)

;;------------------------------------------------------------------------------

(define (write-buffer-pixel! data x-off y-off value)
    "Write a pixel value in the raster buffer.

Parameters:
     data: the raster vector.
     x-off: the pixel offset to the top left corner of the data.
     y-off: the line offset to the top left corner of the data.
     value: the pixel value."
  (let ((buffer-set! (assv-ref *buffer-setters* (%gdal-type% data)))
        (offset (+ x-off (* y-off (%gdal-x-size% data)))))
    (buffer-set! data offset value)))

(export write-buffer-pixel!)

;;------------------------------------------------------------------------------

;; TODO: copy the data into temp
(define (buffer->stream data)
    "Creates a raster stream with the content of raster buffer.

Parameters:
     data: the raster buffer."
  (let* ((buffer-ref (assv-ref *buffer-refs* (%gdal-type% data)))
        (size (* (%gdal-x-size% data) (%gdal-y-size% data)))
        (stream (make-stream (lambda (offset)
                                     (if (= offset size)
                                       '()
                                       (cons (buffer-ref data offset)
                                             (1+ offset))))
                             0)))
    (set! (%gdal-h-band% stream) (%gdal-h-band% data))
    (set! (%gdal-type% stream) (%gdal-type% data))
    (set! (%gdal-x-off% stream) (%gdal-x-off% data))
    (set! (%gdal-y-off% stream) (%gdal-y-off% data))
    (set! (%gdal-x-size% stream) (%gdal-x-size% data))
    (set! (%gdal-y-size% stream) (%gdal-y-size% data))
    stream))

(export buffer->stream)

;;------------------------------------------------------------------------------

(define (stream->buffer stream)
    "Creates a raster buffer with the content of raster stream.

Parameters:
    stream: the raster stream."
  (let* ((size (* (%gdal-x-size% stream) (%gdal-y-size% stream)))
         (stream-type (%gdal-type% stream))
         (data ((assv-ref *buffer-makers* stream-type) size))
         (buffer-set! (assv-ref *buffer-setters* stream-type)))
    (let loop ((rest stream)
               (index 0))
      (if (stream-null? rest)
        (begin
          (set! (%gdal-h-band% data) (%gdal-h-band% stream))
          (set! (%gdal-type% data) (%gdal-type% stream))
          (set! (%gdal-x-off% data) (%gdal-x-off% stream))
          (set! (%gdal-y-off% data) (%gdal-y-off% stream))
          (set! (%gdal-x-size% data) (%gdal-x-size% stream))
          (set! (%gdal-y-size% data) (%gdal-y-size% stream))
          data)
        (begin
         (buffer-set! data index (stream-car rest))
         (loop (stream-cdr rest) (1+ index)))))))

(export stream->buffer)

;;------------------------------------------------------------------------------
