(define-module (gdal ogr)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (gdal config)
  #:use-module (gdal internal))

  ;;------------------------------------------------------------------------------

  ;;; Enums

  ;;------------------------------------------------------------------------------

  ;;; OGRFieldType enums
  (define-public OFT_INTEGER 0)
  (define-public OFT_INTEGER_LIST 1)
  (define-public OFT_REAL 2)
  (define-public OFT_REAL_LIST 3)
  (define-public OFT_STRING 4)
  (define-public OFT_STRING_LIST 5)
  (define-public OFT_WIDE_STRING 6)
  (define-public OFT_WIDE_STRING_LIST 7)
  (define-public OFT_BINARY 8)
  (define-public OFT_DATE 9)
  (define-public OFT_TIME 10)
  (define-public OFT_DATE_TIME 11)
  (define-public OFT_INTEGER64 12)
  (define-public OFT_INTEGER64_LIST 13)

  ;;------------------------------------------------------------------------------

  ;;; Structures

  ;;------------------------------------------------------------------------------

  ;;; gdal-datetime

  (define-record-type <gdal-datetime>
    (make-gdal-datetime year month day hour minute second tz)
    gdal-datetime?
    (year gdal-datetime-year set-gdal-datetime-year!)
    (month gdal-datetime-month set-gdal-datetime-month!)
    (day gdal-datetime-day set-gdal-datetime-day!)
    (hour gdal-datetime-hour set-gdal-datetime-hour!)
    (minute gdal-datetime-minute set-gdal-datetime-minute!)
    (second gdal-datetime-second set-gdal-datetime-second!)
    (tz gdal-datetime-tz set-gdal-datetime-tz!))

  (export make-gdal-datetime
          gdal-datetime?
          gdal-datetime-year
          set-gdal-datetime-year!
          gdal-datetime-month
          set-gdal-datetime-month!
          gdal-datetime-day
          set-gdal-datetime-day!
          gdal-datetime-hour
          set-gdal-datetime-hour!
          gdal-datetime-minute
          set-gdal-datetime-minute!
          gdal-datetime-second
          set-gdal-datetime-second!
          gdal-datetime-tz
          set-gdal-datetime-tz!)

;;------------------------------------------------------------------------------

;;; OGR Function Bindings

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-l-reset-reading
  void "OGR_L_ResetReading" (list '*) 20)

(define (reset-layer-reading h-layer)
    "Reset feature reading to start on the first feature.

This affects get-next-feature.

Parameters:
    h-layer: handle to the layer on which features are read."
  (%ogr-l-reset-reading h-layer))

(export reset-layer-reading)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-l-get-next-feature
  '* "OGR_L_GetNextFeature" (list '*) 20)

(define (get-next-feature h-layer)
    "Fetch the next available feature from this layer.

The returned feature becomes the responsibility of the caller to delete with
\"destroy-feature\". It is critical that all features associated with an
OGRLayer (more specifically an OGRFeatureDefn) be deleted before that
layer/datasource is deleted.

This function implements sequential access to the features of a layer. The
\"reset-layer-reading\" function can be used to start at the beginning again.

Returns a handle to a feature, or NULL if no more features are available.

Parameters:
    h-layer: handle to the layer on which features are read."
  (%ogr-l-get-next-feature h-layer))

(export get-next-feature)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-l-get-layer-defn
  '* "OGR_L_GetLayerDefn" (list '*) 20)

(define (get-feature-definition-of-layer h-layer)
    "Fetch the schema information for this layer.

The returned handle to the OGRFeatureDefn is owned by the OGRLayer, and should
not be modified or freed by the application. It encapsulates the attribute
schema of the features of the layer.

Parameters:
    h-layer: handle to the layer on which features are read."
  (%ogr-l-get-layer-defn h-layer))

(export get-feature-definition-of-layer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-fd-get-field-count
  int "OGR_FD_GetFieldCount" (list '*) 20)

(define (get-field-count-of-feature-definition h-defn)
    "Fetch number of fields on the passed feature definition.

Parameters:
    h-defn: handle to the feature definition to get the fields count from."
  (%ogr-fd-get-field-count h-defn))

(export get-field-count-of-feature-definition)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-fd-get-field-defn
  '* "OGR_FD_GetFieldDefn" (list '* int) 20)

(define (get-field-definition-of-feature-definition h-defn i-field)
    "Fetch field definition of the passed feature definition.

Parameters:
    h-defn: handle to the feature definition to get the field definition from.
    i-field: the field to fetch, between 0 and
(- (get-field-count-of-feature-definition) 1)."
  (%ogr-fd-get-field-defn h-defn i-field))

(export get-field-definition-of-feature-definition)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-fld-get-type
  int "OGR_Fld_GetType" (list '*) 20)

(define (get-type-of-field h-defn)
    "Fetch type of this field. See OFT_* enums for possible return values.

Parameters:
    h-defn: handle to the field definition to get type from."
  (%ogr-fld-get-type h-defn))

(export get-type-of-field)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-integer
  int "OGR_F_GetFieldAsInteger" (list '* int) 20)

(define (get-field-as-integer h-feat i-field)
    "Fetch field value as integer.

OFTString features will be translated using atoi(). OFTReal fields will be cast
to integer. Other field types, or errors will result in a return value of zero.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (%ogr-f-get-field-as-integer h-feat i-field))

(export get-field-as-integer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-integer64
  int64 "OGR_F_GetFieldAsInteger64" (list '* int) 20)

(define (get-field-as-integer64 h-feat i-field)
    "Fetch field value as integer 64 bit.

OFTInteger are promoted to 64 bit. OFTReal fields will be cast to integer.
Other field types, or errors will result in a return value of zero.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (%ogr-f-get-field-as-integer64 h-feat i-field))

(export get-field-as-integer64)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-double
  double "OGR_F_GetFieldAsDouble" (list '* int) 20)

(define (get-field-as-double h-feat i-field)
    "Fetch field value as a double.

OFTString features will be translated using CPLAtof(). OFTInteger fields will
be cast to double. Other field types, or errors will result in a return value
of zero.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (%ogr-f-get-field-as-double h-feat i-field))

(export get-field-as-double)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-string
  '* "OGR_F_GetFieldAsString" (list '* int) 20)

(define (get-field-as-string h-feat i-field)
    "Fetch field value as a string.

OFTReal and OFTInteger fields will be translated to string using sprintf(),
but not necessarily using the established formatting rules. Other field types,
or errors will result in a return value of zero.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (pointer->string (%ogr-f-get-field-as-string h-feat i-field)))

(export get-field-as-string)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-integer-list
  '* "OGR_F_GetFieldAsIntegerList" (list '* int '*) 20)

(define (get-field-as-integer-list h-feat i-field)
    "Fetch field value as a list of integers.

Currently this function only works for OFTIntegerList fields.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (let* ((bv-count (make-bytevector (sizeof int)))
         (bv-result (%ogr-f-get-field-as-integer-list h-feat
                                                      i-field
                                                      (bytevector->pointer
                                                        bv-count))))
    (pointer->list bv-result
                   (bytevector-s32-native-ref bv-count 0)
                   int32)))

(export get-field-as-integer-list)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-integer64-list
  '* "OGR_F_GetFieldAsInteger64List" (list '* int '*) 20)

(define (get-field-as-integer64-list h-feat i-field)
    "Fetch field value as a list of 64 bit integers.

Currently this function only works for OFTInteger64List fields.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (let* ((bv-count (make-bytevector (sizeof int)))
         (bv-result (%ogr-f-get-field-as-integer64-list h-feat
                                                        i-field
                                                        (bytevector->pointer
                                                          bv-count))))
    (pointer->list bv-result
                   (bytevector-s32-native-ref bv-count 0)
                   int64)))

(export get-field-as-integer64-list)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-double-list
  '* "OGR_F_GetFieldAsDoubleList" (list '* int '*) 20)

(define (get-field-as-double-list h-feat i-field)
    "Fetch field value as a list of doubles.

Currently this function only works for OFTRealList fields.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (let* ((bv-count (make-bytevector (sizeof int)))
         (bv-result (%ogr-f-get-field-as-double-list h-feat
                                                     i-field
                                                     (bytevector->pointer
                                                       bv-count))))
    (pointer->list bv-result
                   (bytevector-s32-native-ref bv-count 0)
                   double)))

(export get-field-as-double-list)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-binary
  '* "OGR_F_GetFieldAsBinary" (list '* int '*) 20)

(define (get-field-as-binary h-feat i-field)
    "Fetch field value as binary.

This method only works for OFTBinary and OFTString fields.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (let* ((bv-count (make-bytevector (sizeof int)))
         (bv-result (%ogr-f-get-field-as-binary h-feat
                                                i-field
                                                (bytevector->pointer
                                                  bv-count))))
    (pointer->bytevector bv-result
                   (bytevector-s32-native-ref bv-count 0))))

(export get-field-as-binary)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-string-list
  '* "OGR_F_GetFieldAsStringList" (list '* int) 20)

(define (get-field-as-string-list h-feat i-field)
    "Fetch field value as a list of strings.

Currently this method only works for OFTStringList fields.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (pointerpointer->string-list (%ogr-f-get-field-as-string-list h-feat
                                                                i-field)))

(export get-field-as-string-list)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-datetime
  int "OGR_F_GetFieldAsDateTime" (list '* int '* '* '* '* '* '* '*) 20)

(define (get-field-as-datetime h-feat i-field)
    "Fetch field value as date and time.

Currently this method only works for OFTDate, OFTTime and OFTDateTime fields.
Use get-field-as-datetime-ex for second with millisecond accuracy.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (let* ((bv-year (make-bytevector (sizeof int)))
         (bv-month (make-bytevector (sizeof int)))
         (bv-day (make-bytevector (sizeof int)))
         (bv-hour (make-bytevector (sizeof int)))
         (bv-minute (make-bytevector (sizeof int)))
         (bv-second (make-bytevector (sizeof int)))
         (bv-tz (make-bytevector (sizeof int)))
         (result (%ogr-f-get-field-as-datetime h-feat
                                               i-field
                                               (bytevector->pointer bv-year)
                                               (bytevector->pointer bv-month)
                                               (bytevector->pointer bv-day)
                                               (bytevector->pointer bv-hour)
                                               (bytevector->pointer bv-minute)
                                               (bytevector->pointer bv-second)
                                               (bytevector->pointer bv-tz))))
    (if (c-bool->boolean result)
      (make-gdal-datetime (bytevector-s32-native-ref bv-year 0)
                          (bytevector-s32-native-ref bv-month 0)
                          (bytevector-s32-native-ref bv-day 0)
                          (bytevector-s32-native-ref bv-hour 0)
                          (bytevector-s32-native-ref bv-minute 0)
                          (bytevector-s32-native-ref bv-second 0)
                          (bytevector-s32-native-ref bv-tz 0))
      (error "failed to get datetime"))))

(export get-field-as-datetime)

;;------------------------------------------------------------------------------

(define-gdal-foreign %ogr-f-get-field-as-datetime-ex
  int "OGR_F_GetFieldAsDateTimeEx" (list '* int '* '* '* '* '* '* '*) 20)

(define (get-field-as-datetime-ex h-feat i-field)
    "Fetch field value as date and time.

Currently this method only works for OFTDate, OFTTime and OFTDateTime fields.

Parameters:
    h-feat: handle to the feature that owned the field.
    i-field: the field to fetch, from 0 to GetFieldCount()-1"
  (let* ((bv-year (make-bytevector (sizeof int)))
         (bv-month (make-bytevector (sizeof int)))
         (bv-day (make-bytevector (sizeof int)))
         (bv-hour (make-bytevector (sizeof int)))
         (bv-minute (make-bytevector (sizeof int)))
         (bv-second (make-bytevector (sizeof int)))
         (bv-tz (make-bytevector (sizeof int)))
         (result (%ogr-f-get-field-as-datetime-ex
                    h-feat
                    i-field
                    (bytevector->pointer bv-year)
                    (bytevector->pointer bv-month)
                    (bytevector->pointer bv-day)
                    (bytevector->pointer bv-hour)
                    (bytevector->pointer bv-minute)
                    (bytevector->pointer bv-second)
                    (bytevector->pointer bv-tz))))
    (if (c-bool->boolean result)
      (make-gdal-datetime (bytevector-s32-native-ref bv-year 0)
                          (bytevector-s32-native-ref bv-month 0)
                          (bytevector-s32-native-ref bv-day 0)
                          (bytevector-s32-native-ref bv-hour 0)
                          (bytevector-s32-native-ref bv-minute 0)
                          (bytevector-s32-native-ref bv-second 0)
                          (bytevector-s32-native-ref bv-tz 0))
      (error "failed to get datetime"))))

(export get-field-as-datetime-ex)

;;------------------------------------------------------------------------------
