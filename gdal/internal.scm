(define-module (gdal internal)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (gdal config)
  #:use-module (ice-9 q)
  #:export (define-gdal-foreign)
  #:export (data-type-valid?)
  #:export (boolean->c-bool)
  #:export (c-bool->boolean)
  #:export (list->pointerpointer)
  #:export (list->pointer)
  #:export (pointer->list)
  #:export (pointerpointer->list)
  #:export (pointerpointer->string-list)
  #:export (string-list->pointerpointer)
  #:export (struct-list->pointer)
  #:export (pointer->struct-list))

;;------------------------------------------------------------------------------

;;; Enums

;;------------------------------------------------------------------------------

;;; GDALDataType enums
(define-public GDT_UNKNOWN 0)
(define-public GDT_BYTE 1)
(define-public GDT_UINT16 2)
(define-public GDT_INT16 3)
(define-public GDT_UINT32 4)
(define-public GDT_INT32 5)
(define-public GDT_FLOAT32 6)
(define-public GDT_FLOAT64 7)
(define-public GDT_CINT16 8)
(define-public GDT_CINT32 9)
(define-public GDT_CFLOAT32 10)
(define-public GDT_CFLOAT64 11)
(define-public GDT_TYPECOUNT 12)

;;------------------------------------------------------------------------------

;;; Object properties

;;------------------------------------------------------------------------------

;;; Buffer properties
(define-public %gdal-h-band% (make-object-property))
(define-public %gdal-type% (make-object-property))
(define-public %gdal-x-off% (make-object-property))
(define-public %gdal-y-off% (make-object-property))
(define-public %gdal-x-size% (make-object-property))
(define-public %gdal-y-size% (make-object-property))
(define-public %gdal-pixel-off% (make-object-property))
(define-public %gdal-line-off% (make-object-property))
(define-public %is-stream% (make-object-property))

;;------------------------------------------------------------------------------

;;; Internal definitions

;;------------------------------------------------------------------------------

(define gdal-func
  (lambda* (return-type function-name arg-types gdal-version)
    (if (>= *gdal-version* gdal-version)
      (pointer->procedure return-type
			  (dynamic-func function-name *libgdal*)
			  arg-types)
      (lambda* (#:rest r) (throw 'unsupported)))))

(define-syntax-rule (define-gdal-foreign
		      name return-type func-name arg-types gdal-version)
  (define name
    (gdal-func return-type func-name arg-types gdal-version)))

(define (data-type-valid? data-type)
  (and (< GDT_UNKNOWN data-type) (> GDT_TYPECOUNT data-type)))

(define (boolean->c-bool b)
  "Convert the boolean to a c boolean."
  (if b 1 0))

(define (c-bool->boolean b)
  "Convert the c boolean to boolean."
  (if (zero? b) #f #t))

(define bytevector-pointer-ref
  (case (sizeof '*)
    ((8) (lambda (bv offset)
                 (make-pointer (bytevector-u64-native-ref bv offset))))
    ((4) (lambda (bv offset)
                 (make-pointer (bytevector-u32-native-ref bv offset))))
    (else (error "what machine is this?"))))

(define bytevector-pointer-set!
  (case (sizeof '*)
    ((8) (lambda (bv offset ptr)
                 (bytevector-u64-native-set! bv offset (pointer-address ptr))))
    ((4) (lambda (bv offset ptr)
                 (bytevector-u32-native-set! bv offset (pointer-address ptr))))
    (else (error "what machine is this?"))))

(define (list->pointerpointer lst item->pointer)
  (if (null? lst)
    %null-pointer
    (let* ((size (length lst))
           (ptr (make-bytevector (*  (1+ size) (sizeof '*)))))
      (do ((i 0 (1+ i)))
        ((>= i size))
        (bytevector-pointer-set! ptr
                                 (* i (sizeof '*))
                                 (item->pointer (list-ref lst i))))
      (bytevector-pointer-set! ptr (* size (sizeof '*)) %null-pointer)
      (bytevector->pointer ptr))))

(define* (pointerpointer->list pointer pointer->item
                               #:optional (count -1))
  (let ((q (make-q)))
    (unless (null-pointer? pointer)
      (let lp ((sp (dereference-pointer pointer))
              (index 1))
        (unless (or (= count (q-length q)) (null-pointer? sp))
          (enq! q (pointer->item sp))
          (lp (dereference-pointer
               (make-pointer
                (+ (pointer-address pointer) (* index (sizeof '*)))))
              (1+ index)))))
    (car q)))

(define (struct-list->pointer lst struct-size struct->pointer)
  (let* ((size (length lst))
         (bv (make-bytevector (* size struct-size))))
    (do ((i 0 (1+ i)))
      ((>= i size))
      (let ((index (* i struct-size))
            (item (list-ref lst i)))
        (bytevector-copy! (pointer->bytevector
                           (struct->pointer item) struct-size)
                          0 bv index struct-size)))
    (bytevector->pointer bv)))

(define (pointer->struct-list pointer count struct-size pointer->struct)
  (let loop ((q (make-q))
             (index 0)
             (pointer pointer))
    (if (= index count)
      (car q)
      (begin
       (enq! q (pointer->struct pointer))
       (loop q (1+ index) (make-pointer (+ (pointer-address pointer)
                                           struct-size)))))))

(define (pointerpointer->string-list string-list-p)
  (pointerpointer->list string-list-p pointer->string))

(define (string-list->pointerpointer lst)
  (list->pointerpointer lst string->pointer))

(define *writers*
  `((,float . ,bytevector-ieee-single-native-set!)
    (,double . ,bytevector-ieee-double-native-set!)
    (,int8 . ,bytevector-s8-set!)
    (,uint8 . ,bytevector-u8-set!)
    (,int16 . ,bytevector-s16-native-set!)
    (,uint16 . ,bytevector-u16-native-set!)
    (,int32 . ,bytevector-s32-native-set!)
    (,uint32 . ,bytevector-u32-native-set!)
    (,int64 . ,bytevector-s64-native-set!)
    (,uint64 . ,bytevector-u64-native-set!)
    (,'* . ,bytevector-pointer-set!)))

(define *readers*
  `((,float . ,bytevector-ieee-single-native-ref)
    (,double . ,bytevector-ieee-double-native-ref)
    (,int8 . ,bytevector-s8-ref)
    (,uint8 . ,bytevector-u8-ref)
    (,int16 . ,bytevector-s16-native-ref)
    (,uint16 . ,bytevector-u16-native-ref)
    (,int32 . ,bytevector-s32-native-ref)
    (,uint32 . ,bytevector-u32-native-ref)
    (,int64 . ,bytevector-s64-native-ref)
    (,uint64 . ,bytevector-u64-native-ref)
    (,'* . ,bytevector-pointer-ref)))

(define (list->pointer lst type)
  (cond
    ((null? lst) %null-pointer)
    ((not (pair? lst)) (error "input is not a pair"))
    (else (let* ((size (length lst))
                 (bv (make-bytevector (* size (sizeof type)))))

            (do ((i 0 (1+ i)))
              ((>= i size))
              ((assv-ref *writers* type) bv
                                         (* i (sizeof type))
                                         (list-ref lst i)))
            (bytevector->pointer bv)))))

(define (pointer->list pointer count type)
  (let loop ((q (make-q))
             (index 0)
             (pointer pointer))
    (if (= index count)
      (car q)
      (begin
       (enq! q ((assv-ref *readers* type)
                (pointer->bytevector pointer (sizeof type)) 0))
       (loop q (1+ index) (make-pointer (+ (pointer-address pointer)
                                           (sizeof type))))))))
