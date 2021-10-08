(define-module (gdal)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-4 gnu)
  #:use-module (gdal config)
  #:use-module (gdal internal)
  #:re-export (GDT_UNKNOWN
               GDT_BYTE
               GDT_UINT16
               GDT_INT16
               GDT_UINT32
               GDT_INT32
               GDT_FLOAT32
               GDT_FLOAT64
               GDT_CINT16
               GDT_CINT32
               GDT_CFLOAT32
               GDT_CFLOAT64
               GDT_TYPECOUNT))

;;------------------------------------------------------------------------------

;;; Enums

;;------------------------------------------------------------------------------

;;; GDALAsyncStatusType enums
(define-public GARIO_PENDING 0)
(define-public GARIO_UPDATE 1)
(define-public GARIO_ERROR 2)
(define-public GARIO_COMPLETE 3)
(define-public GARIO_TYPECOUNT 4)

;;; GDALColorInterp enums
(define-public GCI_UNDEFINED 0)
(define-public GCI_GRAY_INDEX 1)
(define-public GCI_PALETTE_INDEX 2)
(define-public GCI_RED_BAND 3)
(define-public GCI_GREEN_BAND 4)
(define-public GCI_BLUE_BAND 5)
(define-public GCI_ALPHA_BAND 6)
(define-public GCI_HUE_BAND 7)
(define-public GCI_SATURATION_BAND 8)
(define-public GCI_LIGHTNESS_BAND 9)
(define-public GCI_CYAN_BAND 10)
(define-public GCI_MAGENTA_BAND 11)
(define-public GCI_YELLOW_BAND 12)
(define-public GCI_BLACK_BAND 13)
(define-public GCI_YCBCR_Y_BAND 14)
(define-public GCI_YCBCR_CB_Band 15)
(define-public GCI_YCBCR_CR_Band 16)
(define-public GCI_MAX 16)

;;; GDALPaletteInterp enums
(define-public GPI_GRAY 0)
(define-public GPI_RGB 1)
(define-public GPI_CMYK 2)
(define-public GPI_HLS 3)

;;; GDALAccess enums
(define-public GA_READONLY 0)
(define-public GA_UPDATE 1)

;;; CPLErr enums
(define-public CE_NONE 0)
(define-public CE_DEBUG 1)
(define-public CE_WARNING 2)
(define-public CE_FAILURE 3)
(define-public CE_FATAL 4)

;;; GDALRWFlag enums
(define-public GF_READ 0)
(define-public GF_WRITE 1)

;;; GDALRIOResampleAlg enums
(define-public GRIORA_NEAREST_NEIGHBOUR 0)
(define-public GRIORA_BILINEAR 1)
(define-public GRIORA_CUBIC 2)
(define-public GRIORA_CUBIC_SPLINE 3)
(define-public GRIORA_LANCZOS 4)
(define-public GRIORA_AVERAGE 5)
(define-public GRIORA_MODE 6)
(define-public GRIORA_GAUSS 7)

;;; GDALRIOResampleAlg v2 enums
(define-public GRIORAv2_NEAREST_NEIGHBOUR 0)
(define-public GRIORAv2_BILINEAR 1)
(define-public GRIORAv2_CUBIC 2)
(define-public GRIORAv2_CUBIC_SPLINE 3)
(define-public GRIORAv2_LANCZOS 4)
(define-public GRIORAv2_AVERAGE 5)
(define-public GRIORAv2_MODE 6)
(define-public GRIORAv2_GAUSS 7)
(define-public GRIORAv2_AVERAGE_MAGPHASE 8)
(define-public GRIORAv2_NONE 9)

(define *grioeav2-to-string*
  `((,GRIORAv2_NEAREST_NEIGHBOUR . ,"NEAREST")
    (,GRIORAv2_BILINEAR . ,"BILINEAR")
    (,GRIORAv2_CUBIC . ,"CUBIC")
    (,GRIORAv2_CUBIC_SPLINE . ,"CUBICSPLINE")
    (,GRIORAv2_LANCZOS . ,"LANCZOS")
    (,GRIORAv2_AVERAGE . ,"AVERAGE")
    (,GRIORAv2_MODE . ,"MODE")
    (,GRIORAv2_GAUSS . ,"GAUSS")
    (,GRIORAv2_AVERAGE_MAGPHASE . ,"AVERAGE_MAGPHASE")
    (,GRIORAv2_NONE . ,"NONE")))

;;; Enums of known driver short names
(define-public GDN_AAIGRID "AAIGrid")
(define-public GDN_ACE2 "ACE2")
(define-public GDN_ADRG "ADRG")
(define-public GDN_AERONAVFAA "AeronavFAA")
(define-public GDN_AIG "AIG")
(define-public GDN_AIRSAR "AirSAR")
(define-public GDN_AMIGOCLOUD "AmigoCloud")
(define-public GDN_ARCGEN "ARCGEN")
(define-public GDN_ARG "ARG")
(define-public GDN_AVCBIN "AVCBin")
(define-public GDN_AVCE00 "AVCE00")
(define-public GDN_BAG "BAG")
(define-public GDN_BIGGIF "BIGGIF")
(define-public GDN_BLX "BLX")
(define-public GDN_BMP "BMP")
(define-public GDN_BNA "BNA")
(define-public GDN_BSB "BSB")
(define-public GDN_BT "BT")
(define-public GDN_CAD "CAD")
(define-public GDN_CALS "CALS")
(define-public GDN_CARTO "Carto")
(define-public GDN_CEOS "CEOS")
(define-public GDN_CLOUDANT "Cloudant")
(define-public GDN_COASP "COASP")
(define-public GDN_COSAR "COSAR")
(define-public GDN_COUCHDB "CouchDB")
(define-public GDN_CPG "CPG")
(define-public GDN_CSV "CSV")
(define-public GDN_CSW "CSW")
(define-public GDN_CTABLE2 "CTable2")
(define-public GDN_CTG "CTG")
(define-public GDN_DERIVED "DERIVED")
(define-public GDN_DGN "DGN")
(define-public GDN_DIMAP "DIMAP")
(define-public GDN_DIPEX "DIPEx")
(define-public GDN_DODS "DODS")
(define-public GDN_DOQ1 "DOQ1")
(define-public GDN_DOQ2 "DOQ2")
(define-public GDN_DTED "DTED")
(define-public GDN_DXF "DXF")
(define-public GDN_E00GRID "E00GRID")
(define-public GDN_ECRGTOC "ECRGTOC")
(define-public GDN_EDIGEO "EDIGEO")
(define-public GDN_EHDR "EHdr")
(define-public GDN_EIR "EIR")
(define-public GDN_ELAS "ELAS")
(define-public GDN_ELASTICSEARCH "ElasticSearch")
(define-public GDN_ENVI "ENVI")
(define-public GDN_EPSILON "EPSILON")
(define-public GDN_ERS "ERS")
(define-public GDN_ESAT "ESAT")
(define-public GDN_ESRI_SHAPEFILE "ESRI Shapefile")
(define-public GDN_ESRIJSON "ESRIJSON")
(define-public GDN_FAST "FAST")
(define-public GDN_FIT "FIT")
(define-public GDN_FUJIBAS "FujiBAS")
(define-public GDN_GENBIN "GenBin")
(define-public GDN_GEOCONCEPT "Geoconcept")
(define-public GDN_GEOJSON "GeoJSON")
(define-public GDN_GEOMEDIA "Geomedia")
(define-public GDN_GEORSS "GeoRSS")
(define-public GDN_GFF "GFF")
(define-public GDN_GFT "GFT")
(define-public GDN_GIF "GIF")
(define-public GDN_GML "GML")
(define-public GDN_GMLAS "GMLAS")
(define-public GDN_GMT "GMT")
(define-public GDN_GNMDATABASE "GNMDatabase")
(define-public GDN_GNMFILE "GNMFile")
(define-public GDN_GPKG "GPKG")
(define-public GDN_GPSBABEL "GPSBabel")
(define-public GDN_GPSTRACKMAKER "GPSTrackMaker")
(define-public GDN_GPX "GPX")
(define-public GDN_GRASSASCIIGRID "GRASSASCIIGrid")
(define-public GDN_GRIB "GRIB")
(define-public GDN_GS7BG "GS7BG")
(define-public GDN_GSAG "GSAG")
(define-public GDN_GSBG "GSBG")
(define-public GDN_GSC "GSC")
(define-public GDN_GTIFF "GTiff")
(define-public GDN_GTX "GTX")
(define-public GDN_GXF "GXF")
(define-public GDN_HDF4 "HDF4")
(define-public GDN_HDF4IMAGE "HDF4Image")
(define-public GDN_HDF5 "HDF5")
(define-public GDN_HDF5IMAGE "HDF5Image")
(define-public GDN_HF2 "HF2")
(define-public GDN_HFA "HFA")
(define-public GDN_HTF "HTF")
(define-public GDN_HTTP "HTTP")
(define-public GDN_IDA "IDA")
(define-public GDN_IDRISI "Idrisi")
(define-public GDN_ILWIS "ILWIS")
(define-public GDN_INGR "INGR")
(define-public GDN_INTERLIS_1 "Interlis 1")
(define-public GDN_INTERLIS_2 "Interlis 2")
(define-public GDN_IRIS "IRIS")
(define-public GDN_ISCE "ISCE")
(define-public GDN_ISIS2 "ISIS2")
(define-public GDN_ISIS3 "ISIS3")
(define-public GDN_JAXAPALSAR "JAXAPALSAR")
(define-public GDN_JDEM "JDEM")
(define-public GDN_JML "JML")
(define-public GDN_JP2OPENJPEG "JP2OpenJPEG")
(define-public GDN_JPEG "JPEG")
(define-public GDN_JPEGLS "JPEGLS")
(define-public GDN_KML "KML")
(define-public GDN_KMLSUPEROVERLAY "KMLSUPEROVERLAY")
(define-public GDN_KRO "KRO")
(define-public GDN_L1B "L1B")
(define-public GDN_LAN "LAN")
(define-public GDN_LCP "LCP")
(define-public GDN_LEVELLER "Leveller")
(define-public GDN_LIBKML "LIBKML")
(define-public GDN_LOSLAS "LOSLAS")
(define-public GDN_MAP "MAP")
(define-public GDN_MAPINFO_FILE "MapInfo File")
(define-public GDN_MBTILES "MBTiles")
(define-public GDN_MEM "MEM")
(define-public GDN_MEMORY "Memory")
(define-public GDN_MFF "MFF")
(define-public GDN_MFF2 "MFF2")
(define-public GDN_MRF "MRF")
(define-public GDN_MSGN "MSGN")
(define-public GDN_MSSQLSPATIAL "MSSQLSpatial")
(define-public GDN_MVT "MVT")
(define-public GDN_MYSQL "MySQL")
(define-public GDN_NAS "NAS")
(define-public GDN_NDF "NDF")
(define-public GDN_NETCDF "netCDF")
(define-public GDN_NGSGEOID "NGSGEOID")
(define-public GDN_NITF "NITF")
(define-public GDN_NTV2 "NTv2")
(define-public GDN_NWT_GRC "NWT_GRC")
(define-public GDN_NWT_GRD "NWT_GRD")
(define-public GDN_ODBC "ODBC")
(define-public GDN_ODS "ODS")
(define-public GDN_OGR_DODS "OGR_DODS")
(define-public GDN_OGR_GMT "OGR_GMT")
(define-public GDN_OGR_OGDI "OGR_OGDI")
(define-public GDN_OGR_PDS "OGR_PDS")
(define-public GDN_OGR_SDTS "OGR_SDTS")
(define-public GDN_OGR_VRT "OGR_VRT")
(define-public GDN_OPENAIR "OpenAir")
(define-public GDN_OPENFILEGDB "OpenFileGDB")
(define-public GDN_OSM "OSM")
(define-public GDN_OZI "OZI")
(define-public GDN_PAUX "PAux")
(define-public GDN_PCIDSK "PCIDSK")
(define-public GDN_PCRASTER "PCRaster")
(define-public GDN_PDF "PDF")
(define-public GDN_PDS "PDS")
(define-public GDN_PDS4 "PDS4")
(define-public GDN_PGDUMP "PGDUMP")
(define-public GDN_PGEO "PGeo")
(define-public GDN_PLMOSAIC "PLMOSAIC")
(define-public GDN_PLSCENES "PLSCENES")
(define-public GDN_PNG "PNG")
(define-public GDN_PNM "PNM")
(define-public GDN_POSTGISRASTER "PostGISRaster")
(define-public GDN_POSTGRESQL "PostgreSQL")
(define-public GDN_PRF "PRF")
(define-public GDN_R "R")
(define-public GDN_RASTERLITE "Rasterlite")
(define-public GDN_RDA "RDA")
(define-public GDN_REC "REC")
(define-public GDN_RIK "RIK")
(define-public GDN_RMF "RMF")
(define-public GDN_ROI_PAC "ROI_PAC")
(define-public GDN_RPFTOC "RPFTOC")
(define-public GDN_RRASTER "RRASTER")
(define-public GDN_RS2 "RS2")
(define-public GDN_RST "RST")
(define-public GDN_S57 "S57")
(define-public GDN_SAFE "SAFE")
(define-public GDN_SAGA "SAGA")
(define-public GDN_SAR_CEOS "SAR_CEOS")
(define-public GDN_SDTS "SDTS")
(define-public GDN_SEGUKOOA "SEGUKOOA")
(define-public GDN_SEGY "SEGY")
(define-public GDN_SELAFIN "Selafin")
(define-public GDN_SENTINEL2 "SENTINEL2")
(define-public GDN_SGI "SGI")
(define-public GDN_SNODAS "SNODAS")
(define-public GDN_SOSI "SOSI")
(define-public GDN_SQLITE "SQLite")
(define-public GDN_SRP "SRP")
(define-public GDN_SRTMHGT "SRTMHGT")
(define-public GDN_SUA "SUA")
(define-public GDN_SVG "SVG")
(define-public GDN_SXF "SXF")
(define-public GDN_TERRAGEN "Terragen")
(define-public GDN_TIGER "TIGER")
(define-public GDN_TIL "TIL")
(define-public GDN_TOPOJSON "TopoJSON")
(define-public GDN_TSX "TSX")
(define-public GDN_UK_NTF "UK .NTF")
(define-public GDN_USGSDEM "USGSDEM")
(define-public GDN_VDV "VDV")
(define-public GDN_VFK "VFK")
(define-public GDN_VICAR "VICAR")
(define-public GDN_VRT "VRT")
(define-public GDN_WALK "Walk")
(define-public GDN_WASP "WAsP")
(define-public GDN_WCS "WCS")
(define-public GDN_WEBP "WEBP")
(define-public GDN_WFS "WFS")
(define-public GDN_WFS3 "WFS3")
(define-public GDN_WMS "WMS")
(define-public GDN_WMTS "WMTS")
(define-public GDN_XLS "XLS")
(define-public GDN_XLSX "XLSX")
(define-public GDN_XPLANE "XPlane")
(define-public GDN_XPM "XPM")
(define-public GDN_XYZ "XYZ")
(define-public GDN_ZMAP "ZMap")

;;; OGRwkbGeometryType enums
(define-public WKB_UNKNOWN 0)
(define-public WKB_POINT 1)
(define-public WKB_LINE_STRING 2)
(define-public WKB_POLYGON 3)
(define-public WKB_MULTI_POINT 4)
(define-public WKB_MULTI_LINE_STRING 5)
(define-public WKB_MULTI_POLYGON 6)
(define-public WKB_GEOMETRY_COLLECTION 7)
(define-public WKB_COMPOUND_CURVE 9)
(define-public WKB_CURVE_POLYGON 10)
(define-public WKB_MULTI_CURVE 11)
(define-public WKB_MULTI_SURFACE 12)
(define-public WKB_CURVE 13)
(define-public WKB_SURFACE 14)
(define-public WKB_POLYHEDRAL_SURFACE 15)
(define-public WKB_TIN 16)
(define-public WKB_TRIANGLE 17)
(define-public WKB_NONE 100)
(define-public WKB_LINEAR_RING 101)
(define-public WKB_CIRCULAR_STRING_Z 1008)
(define-public WKB_COMPOUND_CURVE_Z 1009)
(define-public WKB_CURVE_POLYGON_Z 1010)
(define-public WKB_MULTI_CURVE_Z 1011)
(define-public WKB_MULTI_SURFACE_Z 1012)
(define-public WKB_CURVE_Z 1013)
(define-public WKB_SURFACE_Z 1014)
(define-public WKB_POLYHEDRAL_SURFACE_Z 1015)
(define-public WKB_TIN_Z 1016)
(define-public WKB_TRIANGLE_Z 1017)
(define-public WKB_POINT_M 2001)
(define-public WKB_LINE_STRING_M 2002)
(define-public WKB_POLYGON_M 2003)
(define-public WKB_MULTI_POINT_M 2004)
(define-public WKB_MULTI_LINE_STRING_M 2005)
(define-public WKB_MULTI_POLYGON_M 2006)
(define-public WKB_GEOMETRY_COLLECTION_M 2007)
(define-public WKB_CIRCULAR_STRING_M 2008)
(define-public WKB_COMPOUND_CURVE_M 2009)
(define-public WKB_CURVE_POLYGON_M 2010)
(define-public WKB_MULTI_CURVE_M 2011)
(define-public WKB_MULTI_SURFACE_M 2012)
(define-public WKB_CURVE_M 2013)
(define-public WKB_SURFACE_M 2014)
(define-public WKB_POLYHEDRAL_SURFACE_M 2015)
(define-public WKB_TIN_M 2016)
(define-public WKB_TRIANGLE_M 2017)
(define-public WKB_POINT_ZM 3001)
(define-public WKB_LINE_STRING_ZM 3002)
(define-public WKB_POLYGON_ZM 3003)
(define-public WKB_MULTI_POINT_ZM 3004)
(define-public WKB_MULTI_LINE_STRING_ZM 3005)
(define-public WKB_MULTI_POLYGON_ZM 3006)
(define-public WKB_GEOMETRY_COLLECTION_ZM 3007)
(define-public WKB_CIRCULAR_STRING_ZM 3008)
(define-public WKB_COMPOUND_CURVE_ZM 3009)
(define-public WKB_CURVE_POLYGON_ZM 3010)
(define-public WKB_MULTI_CURVE_ZM 3011)
(define-public WKB_MULTI_SURFACE_ZM 3012)
(define-public WKB_CURVE_ZM 3013)
(define-public WKB_SURFACE_ZM 3014)
(define-public WKB_POLYHEDRAL_SURFACE_ZM 3015)
(define-public WKB_TIN_ZM 3016)
(define-public WKB_TRIANGLE_ZM 3017)
(define-public WKB_POINT_25D #x80000001)
(define-public WKB_LINE_STRING_25D #x80000002)
(define-public WKB_POLYGON_25D #x80000003)
(define-public WKB_MULTI_POINT_25D #x80000004)
(define-public WKB_MULTI_LINE_STRING_25D #x80000005)
(define-public WKB_MULTI_POLYGON_25D #x80000006)
(define-public WKB_GEOMETRY_COLLECTION_25D #x80000007)

;;; GMF Enums
(define-public GMF_ALL_VALID #x01)
(define-public GMF_PER_DATASET #x02)
(define-public GMF_ALPHA #x04)
(define-public GMF_NODATA #x08)

;;; GDAL_OF Enums
(define-public GDAL_OF_READONLY #x00)
(define-public GDAL_OF_UPDATE #x01)
(define-public GDAL_OF_ALL #x00)
(define-public GDAL_OF_RASTER #x02)
(define-public GDAL_OF_VECTOR #x04)
(define-public GDAL_OF_GNM #x08)
(define-public GDAL_OF_MULTIDIM_RASTER #x10)
(define-public GDAL_OF_KIND_MASK #x1e)
(define-public GDAL_OF_SHARED #x20)
(define-public GDAL_OF_VERBOSE_ERROR #x40)
(define-public GDAL_OF_INTERNAL #x80)
(define-public GDAL_OF_DEFAULT_BLOCK_ACCESS 0)
(define-public GDAL_OF_ARRAY_BLOCK_ACCESS #x100)
(define-public GDAL_OF_HASHSET_BLOCK_ACCESS #x200)
(define-public GDAL_OF_BLOCK_ACCESS_MASK #x300)

;;; ODs Enums
(define-public ODSC_CREATE_LAYER "CreateLayer")
(define-public ODSC_DELETE_LAYER "DeleteLayer")
(define-public ODSC_CREATE_GEOM_FIELD_AFTER_CREATE_LAYER
  "CreateGeomFieldAfterCreateLayer")
(define-public ODSC_CURVE_GEOMETRIES "CurveGeometries")
(define-public ODSC_TRANSACTIONS "Transactions")
(define-public ODSC_EMULATED_TRANSACTIONS "EmulatedTransactions")
(define-public ODSC_RANDOM_LAYER_READ "RandomLayerRead")
(define-public ODSC_RANDOM_LAYER_WRITE "RandomLayerWrite")

;;; OGRERR Enums
(define-public OGRERR_NONE 0)
(define-public OGRERR_NOT_ENOUGH_DATA 1)
(define-public OGRERR_NOT_ENOUGH_MEMORY 2)
(define-public OGRERR_UNSUPPORTED_GEOMETRY_TYPE 3)
(define-public OGRERR_UNSUPPORTED_OPERATION 4)
(define-public OGRERR_CORRUPT_DATA 5)
(define-public OGRERR_FAILURE 6)
(define-public OGRERR_UNSUPPORTED_SRS 7)
(define-public OGRERR_INVALID_HANDLE 8)
(define-public OGRERR_NON_EXISTING_FEATURE 9)

;;; RAT Enums
(define-public GFU_GENERIC 0)
(define-public GFU_PIXEL_COUNT 1)
(define-public GFU_NAME 2)
(define-public GFU_MIN 3)
(define-public GFU_MAX 4)
(define-public GFU_MIN_MAX 5)
(define-public GFU_RED 6)
(define-public GFU_GREEN 7)
(define-public GFU_BLUE 8)
(define-public GFU_ALPHA 9)
(define-public GFU_RED_MIN 10)
(define-public GFU_GREEN_MIN 11)
(define-public GFU_BLUE_MIN 12)
(define-public GFU_ALPHA_MIN 13)
(define-public GFU_RED_MAX 14)
(define-public GFU_GREEN_MAX 15)
(define-public GFU_BLUE_MAX 16)
(define-public GFU_ALPHA_MAX 17)

;;; GFT Enums
(define-public GFT_INTEGER 0)
(define-public GFT_REAL 1)
(define-public GFT_STRING 2)

;;; GRTT Enums
(define-public GRTT_THEMATIC 0)
(define-public GRTT_ATHEMATIC 1)

;;------------------------------------------------------------------------------

;;; Structures

;;------------------------------------------------------------------------------

;;; GCP

(define-record-type <gcp>
  (%make-gcp id info pixel line x y z)
  gcp?
  (id gcp-id set-gcp-id!)
  (info gcp-info set-gcp-info!)
  (pixel gcp-pixel set-gcp-pixel!)
  (line gcp-line set-gcp-line!)
  (x gcp-x set-gcp-x!)
  (y gcp-y set-gcp-y!)
  (z gcp-z set-gcp-z!))

(define* (make-gcp #:key
                   (id "")
                   (info "")
                   (pixel 0.0)
                   (line 0.0)
                   (x 0.0)
                   (y 0.0)
                   (z 0.0))
  (%make-gcp id info pixel line x y z))

(export make-gcp
        gcp?
        gcp-id
        set-gcp-id!
        gcp-info
        set-gcp-info!
        gcp-pixel
        set-gcp-pixel!
        gcp-line
        set-gcp-line!
        gcp-x
        set-gcp-x!
        gcp-y
        set-gcp-y!
        gcp-z
        set-gcp-z!)

(define gcp-types (list '* '* double double double double double))

(define (gcp->pointer record)
  (if (gcp? record)
    (make-c-struct
     gcp-types
     (list (string->pointer (gcp-id record))
           (string->pointer (gcp-info record))
           (gcp-pixel record)
           (gcp-line record)
           (gcp-x record)
           (gcp-y record)
           (gcp-z record)))
    %null-pointer))

(define (pointer->gcp pointer)
  (let ((lst (parse-c-struct pointer gcp-types)))
    (make-gcp #:id (pointer->string (list-ref lst 0))
              #:info (pointer->string (list-ref lst 1))
              #:pixel (list-ref lst 2)
              #:line (list-ref lst 3)
              #:x (list-ref lst 4)
              #:y (list-ref lst 5)
              #:z (list-ref lst 6))))

(define (gcp-list->pointer lst)
  (struct-list->pointer lst (sizeof gcp-types) gcp->pointer))

(define (pointer->gcp-list pointer count)
  (pointer->struct-list pointer count (sizeof gcp-types) pointer->gcp))

(export pointer->gcp-list)
;;; GDALRasterIOExtraArg

(define-record-type <grioea>
  (%make-grioea version resample-alg progress-callback progress-data
                is-fp-window-valid x-off y-off x-size y-size)
  grioea?
  (version grioea-version set-grioea-version!)
  (resample-alg grioea-resample-alg set-grioea-resample-alg!)
  (progress-callback grioea-progress-callback set-grioea-progress-callback!)
  (progress-data grioea-progress-data set-grioea-progress-data!)
  (is-fp-window-valid grioea-is-fp-window-valid set-grioea-is-fp-window-valid!)
  (x-off grioea-x-off set-grioea-x-off!)
  (y-off grioea-y-off set-grioea-y-off!)
  (x-size grioea-x-size set-grioea-x-size!)
  (y-size grioea-y-size set-grioea-y-size!))

(define (gdal-progress-func progress-callback)
  (if (null? progress-callback)
    %null-pointer
    (procedure->pointer int
                        (lambda (complete message progress-arg)
                                (progress-callback complete
                                                   (pointer->string message)
                                                   progress-arg))
                        (list double '* '*))))

(define* (make-grioea #:key
                      (version 1)
                      (resample-alg GRIORA_NEAREST_NEIGHBOUR)
                      (progress-callback '())
                      (progress-data %null-pointer)
                      (is-fp-window-valid #f)
                      (x-off 0.0)
                      (y-off 0.0)
                      (x-size 0.0)
                      (y-size 0.0))
  (%make-grioea version resample-alg
                (gdal-progress-func progress-callback)
                progress-data
                (boolean->c-bool is-fp-window-valid)
                x-off y-off x-size y-size))

(export make-grioea
        grioea?
        grioea-version
        set-grioea-version!
        grioea-resample-alg
        set-grioea-resample-alg!
        grioea-progress-callback
        set-grioea-progress-callback!
        grioea-progress-data
        set-grioea-progress-data!
        grioea-is-fp-window-valid
        set-grioea-is-fp-window-valid!
        grioea-x-off
        set-grioea-x-off!
        grioea-y-off
        set-grioea-y-off!
        grioea-x-size
        set-grioea-x-size!
        grioea-y-size
        set-grioea-y-size!)

(define (grioea->foreign-pointer record)
  (if (grioea? record)
    (make-c-struct
     (list int int '* '* int double double double double)
     (list (grioea-version record)
           (grioea-resample-alg record)
           (grioea-progress-callback record)
           (grioea-progress-data record)
           (grioea-is-fp-window-valid record)
           (grioea-x-off record)
           (grioea-y-off record)
           (grioea-x-size record)
           (grioea-y-size record)))
    %null-pointer))

(define (gdal-term-progress complete message arg)
  "Simple progress report to terminal.

This progress reporter prints simple progress report to the terminal window.
The progress report generally looks something like this:

17.0...33.0...50.0...67.0...83.0...100.0.

Use it when you create grioea record via \"make-grioea\" function:

  Sample:

(make-grioea #:progress-callback gdal-term-progress)"
  (let ((perc (round (* complete 100))))
    (display perc)
    (if (= perc 100)
      (newline)
      (display "..."))
    1))

(export gdal-term-progress)

;;------------------------------------------------------------------------------

;;; GDAL Function Bindings

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-all-register
  void "GDALAllRegister" '() 20)

(define (all-register)
  "Register all known configured GDAL drivers."
  (%gdal-all-register))

(export all-register)

;;------------------------------------------------------------------------------

(define-gdal-foreign %cpl-error-reset
  void "CPLErrorReset" '() 20)

(define (reset-error)
  "Erase any traces of previous errors.

This is normally used to ensure that an error which has been recovered from
does not appear to be still in play with high level functions."
  (%cpl-error-reset))

(export reset-error)

;;------------------------------------------------------------------------------

(define-gdal-foreign %cpl-get-last-error-msg
  '* "CPLGetLastErrorMsg" '() 20)

(define (get-last-error-message)
    "Get the last error message.

Fetches the last error message posted with CPLError(), that hasn't been cleared
by reset-error."
  (let ((ptr (%cpl-get-last-error-msg)))
    (if (null-pointer? ptr)
	     ""
     	(pointer->string ptr))))

(export get-last-error-message)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-size
  int "GDALGetDataTypeSize" (list int) 20)

(define (get-data-type-size data-type)
    "Get data type size in bits.

Deprecated.

Returns the size of a GDT_* type in bits, not bytes!
Use get-data-type-size-bytes for bytes. Use get-data-type-size-bits for bits.

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-size data-type)))
    (if (zero? result)
	(error "failed to recognize data type")
	result)))

(export get-data-type-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-size-bits
  int "GDALGetDataTypeSizeBits" (list int) 20)

(define (get-data-type-size-bits data-type)
    "Get data type size in bits.

Returns the size of a GDT_* type in bits, not bytes!
Use get-data-type-size-bytes for bytes.

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-size-bits data-type)))
    (if (zero? result)
	(error "failed to recognize data type")
	result)))

(export get-data-type-size-bits)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-size-bytes
  int "GDALGetDataTypeSizeBytes" (list int) 20)

(define (get-data-type-size-bytes data-type)
    "Get data type size in bytes.

Returns the size of a GDT_* type in bytes. In contrast, get-data-type-size and
get-data-type-size-bits return the size in bits.

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-size-bytes data-type)))
    (if (zero? result)
	(error "failed to recognize data type")
	result)))

(export get-data-type-size-bytes)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-is-complex
  int "GDALDataTypeIsComplex" (list int) 20)

(define (data-type-is-complex? data-type)
    "Is data type complex?

Returns #t if the passed type is complex (one of GDT_CINT16, GDT_CINT32,
GDT_CFLOAT32 or GDT_CFLOAT64), that is it consists of a real and imaginary
component.

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-is-complex data-type)))
    (if (= result 1)
	     #t
     	#f)))

(export data-type-is-complex?)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-is-integer
  int "GDALDataTypeIsInteger" (list int) 23)

(define (data-type-is-integer? data-type)
    "Is data type integer? (might be complex)

Returns #t if the passed type is integer (one of GDT_BYTE, GDT_INT16,
GDT_UINT16, GDT_INT32, GDT_UINT32, GDT_CINT16 or GDT_CINT32).

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-is-integer data-type)))
    (if (= result 1)
	     #t
     	#f)))

(export data-type-is-integer?)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-is-floating
  int "GDALDataTypeIsFloating" (list int) 23)

(define (data-type-is-floating? data-type)
    "Is data type floating? (might be complex)

Returns #t if the passed type is floating (one of GDT_FLOAT32, GDT_FLOAT64,
GDT_CFLOAT32, GDT_CFLOAT64).

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-is-floating data-type)))
    (if (= result 1)
	     #t
     	#f)))

(export data-type-is-floating?)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-is-signed
  int "GDALDataTypeIsSigned" (list int) 23)

(define (data-type-is-signed? data-type)
    "Is data type signed?

Returns #t if the passed type is signed.

Parameters:
    data-type: type, such as GDT_BYTE."
  (let ((result (%gdal-data-type-is-signed data-type)))
    (if (= result 1)
	     #t
     	#f)))

(export data-type-is-signed?)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-data-type-name
  '* "GDALGetDataTypeName" (list int) 20)

(define (get-data-type-name data-type)
    "Get name of data type.

Returns a symbolic name for the data type. This is essentially the enumerated
item name with the GDT_ prefix removed. So GDT_BYTE returns 'Byte'.

Parameters:
    data-type: type to get name of."
  (let ((ptr (%gdal-get-data-type-name data-type)))
    (if (null-pointer? ptr)
	     (error "failed to recognize data type")
     	(pointer->string ptr))))

(export get-data-type-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-data-type-by-name
  int "GDALGetDataTypeByName" (list '*) 20)

(define (get-data-type-by-name name)
    "Get data type by symbolic name.

Returns a data type corresponding to the given symbolic name. This function is
opposite to the get-data-type-name.

Parameters:
    name: string containing the symbolic name of the type."
  (let ((result (%gdal-get-data-type-by-name (string->pointer name))))
    (if (zero? result)
	     (error "failed to recognize data type")
     	result)))

(export get-data-type-by-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-union
  int "GDALDataTypeUnion" (list int int) 20)

(define (data-type-union data-type-1 data-type-2)
    "Return the smallest data type that can fully express both input data
types.

Parameters:
    data-type-1: first data type.
    data-type-2: second data type."
  (let ((result (%gdal-data-type-union data-type-1 data-type-2)))
    (if (zero? result)
	     (error "failed to recognize data type")
     	result)))

(export data-type-union)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-union-with-value
  int "GDALDataTypeUnionWithValue" (list int double int) 23)

(define (data-type-union-with-value data-type value is-complex)
    "Union a data type with the one found for a value.

Parameters:
    data-type: the first data type
    value: the value for which to find a data type and union with 'data-type'
    is-complex: boolean, #t if the value is complex."
  (let ((result (%gdal-data-type-union-with-value
                 data-type value (boolean->c-bool is-complex))))
    (if (zero? result)
	     (error "failed to union data type")
     	result)))

(export data-type-union-with-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-find-data-type
  int "GDALFindDataType" (list int int int int) 23)

(define (find-data-type n-bits is-signed is-floating is-complex)
    "Finds the smallest data type able to support the given requirements.

Parameters:
    n-bits:	number of bits necessary
    is-signed: if negative values are necessary
    is-floating: if non-integer values necessary
    is-complex: if complex values are necessary."
  (let ((result (%gdal-find-data-type n-bits (boolean->c-bool is-signed)
                 (boolean->c-bool is-floating) (boolean->c-bool is-complex))))
    (if (zero? result)
	     (error "failed to find data type")
     	result)))

(export find-data-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-find-data-type-for-value
  int "GDALFindDataTypeForValue" (list double int) 23)

(define (find-data-type-for-value value is-complex)
    "Finds the smallest data type able to support the provided value.

Parameters:
    value: double	value to support
    is-complex:	is the value complex."
  (let ((result (%gdal-find-data-type-for-value value
                                                (boolean->c-bool is-complex))))
    (if (zero? result)
	     (error "failed to find data type")
     	result)))

(export find-data-type-for-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-adjust-data-type-to-value
  double "GDALAdjustValueToDataType" (list int double '* '*) 20)

(define (adjust-data-type-to-value data-type value)
    "Adjust a value to the output data type.

Adjustment consist in clamping to minimum/maxmimum values of the data type and
rounding for integral types.

Returns multiple (3) values as adjusted double value,
boolean value to indicate if clamping has been made, and boolean value to
indicate if rounding has been made.

Parameters:
    data-type: target data type
    value: double value to adjust."
  (let* ((p-clamped (make-bytevector (sizeof int)))
        (p-rounded (make-bytevector (sizeof int)))
        (result (%gdal-adjust-data-type-to-value
                 data-type
                 value
                 (bytevector->pointer p-clamped)
                 (bytevector->pointer p-rounded))))
    (values
     result
     (c-bool->boolean (bytevector-sint-ref p-clamped
                                           0
                                           (native-endianness)
                                           (sizeof int)))
     (c-bool->boolean (bytevector-sint-ref p-rounded
                                           0
                                           (native-endianness)
                                           (sizeof int))))))

(export adjust-data-type-to-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-non-complex-data-type
  int "GDALGetNonComplexDataType" (list int) 23)

(define (get-non-complex-data-type data-type)
    "Return the base data type for the specified input.

If the input data type is complex this function returns the base type i.e.
the data type of the real and imaginary parts (non-complex). If the input data
type is already non-complex, then it is returned unchanged.

Parameters:
    data-type: type, such as GDT_CFLOAT32."
  (if (data-type-valid? data-type)
    (%gdal-get-non-complex-data-type data-type)
    (error "data-type is out of bounds")))

(export get-non-complex-data-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-data-type-is-conversion-lossy
  int "GDALDataTypeIsConversionLossy" (list int int) 23)

(define (data-type-is-conversion-lossy? data-type-from data-type-to)
    "Is conversion from data-type-from to data-type-to potentially lossy.

Parameters:
    data-type-from: input datatype
    data-type-to: output datatype."
  (if (and (data-type-valid? data-type-from) (data-type-valid? data-type-to))
    (c-bool->boolean (%gdal-data-type-is-conversion-lossy
                      data-type-from
                      data-type-to))
    (error "data-type is out of bounds")))

(export data-type-is-conversion-lossy?)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-async-status-type-name
  '* "GDALGetAsyncStatusTypeName" (list int) 20)

(define (get-async-status-type-name async-status-type)
    "Get name of AsyncStatus data type.

Returns a symbolic name for the AsyncStatus data type. This is essentially the
the enumerated item name with the GARIO_ prefix removed. So GARIO_COMPLETE
returns 'COMPLETE'.

Parameters:
    async-status-type: type to get name of."
    (let ((ptr (%gdal-get-async-status-type-name async-status-type)))
      (if (null-pointer? ptr)
  	     (error "failed to recognize async status type")
       	(pointer->string ptr))))

(export get-async-status-type-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-async-status-type-by-name
  int "GDALGetAsyncStatusTypeByName" (list '*) 20)

(define (get-async-status-type-by-name name)
    "Get AsyncStatusType by symbolic name.

Returns a data type corresponding to the given symbolic name. This function is
opposite to the get-async-status-type-name.

Parameters:
    name: string containing the symbolic name of the type."
    (let ((result (%gdal-get-async-status-type-by-name (string->pointer name))))
      (if (= GARIO_ERROR result)
  	     (error "failed to recognize async status type")
       	result)))

(export get-async-status-type-by-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-color-interpretation-name
  '* "GDALGetColorInterpretationName" (list int) 20)

(define (get-color-interpretation-name color-interpretation-type)
    "Get name of color interpretation.

Returns a symbolic name for the color interpretation. This is derived from the
enumerated item name with the GCI_ prefix removed, but there are some
variations. So GCI_GRAY_INDEX returns 'Gray' and GCI_RED_BAND returns 'Red'.

Parameters:
    color-interpretation-type: color interpretation to get name of."
    (let ((ptr (%gdal-get-color-interpretation-name
                color-interpretation-type)))
      (if (null-pointer? ptr)
  	     (error "failed to recognize color interpretation type")
       	(pointer->string ptr))))

(export get-color-interpretation-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-color-interpretation-by-name
  int "GDALGetColorInterpretationByName" (list '*) 20)

(define (get-color-interpretation-by-name name)
    "Get color interpretation by symbolic name.

Returns a color interpretation corresponding to the given symbolic name. This
function is opposite to the get-color-interpretation-name.

Parameters:
    name: string containing the symbolic name of the color interpretation."
    (let ((result (%gdal-get-color-interpretation-by-name
                   (string->pointer name))))
      (if (= GCI_UNDEFINED result)
  	     (error "failed to recognize color interpretation type")
       	result)))

(export get-color-interpretation-by-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-palette-interpretation-name
  '* "GDALGetPaletteInterpretationName" (list int) 20)

(define (get-palette-interpretation-name palette-interpretation-type)
    "Get name of palette interpretation.

Returns a symbolic name for the palette interpretation. This is the the
enumerated item name with the GPI_ prefix removed. So GPI_Gray returns 'Gray'.

Parameters:
    palette-interpretation-type: palette interpretation to get name of."
    (let ((ptr (%gdal-get-palette-interpretation-name
                palette-interpretation-type)))
      (if (null-pointer? ptr)
  	     (error "failed to recognize palette interpretation type")
       	(pointer->string ptr))))

(export get-palette-interpretation-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-create
  '* "GDALCreate" (list '* '* int int int int '*) 20)

(define (create-dataset driver file-name x-size y-size n-bands band-type)
    "Create a new dataset with this driver.

Parameters:
    driver: gdal driver
    file-name: the name of the dataset to create
    x-size: width of created raster in pixels
    y-size: height of created raster in pixels
    n-bands: number of bands
    band-type: type of raster."
  (let ((ptr (%gdal-create driver
                           (string->pointer file-name)
                           x-size
                           y-size
                           n-bands
                           band-type
                           %null-pointer)))
    (if (null-pointer? ptr)
	     (error "failed to create GDAL dataset")
     	ptr)))

(export create-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-create-copy
  '* "GDALCreateCopy" (list '* '* '* int '* '* '*) 20)

(define (copy-dataset driver file-name source-dataset is-strict)
    "Create a copy of a dataset.

Parameters:
    driver: GDAL driver
    file-name:	the name for the new dataset
    source-dataset: the dataset being duplicated.
    is-strict: #t if the copy must be strictly equivalent, or more normally
#f indicating that the copy may adapt as needed for the output format."
  (let ((ptr (%gdal-create-copy driver
                                (string->pointer file-name)
                                source-dataset
                                (boolean->c-bool is-strict)
                                %null-pointer
                                %null-pointer
                                %null-pointer)))
    (if (null-pointer? ptr)
	     (error "failed to create GDAL dataset")
     	ptr)))

(export copy-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-identify-driver
  '* "GDALIdentifyDriver" (list '* '*) 20)

(define (identify-driver file-name)
    "Identify the driver that can open a raster file.

Parameters:
    file-name: the name of the file to access. In the case of exotic drivers
this may not refer to a physical file, but instead contain information for the
driver on how to access a dataset."
    (let ((ptr (%gdal-identify-driver
                (string->pointer file-name)
                %null-pointer)))
      (if (null-pointer? ptr)
  	     (error "failed to identify driver")
       	ptr)))

(export identify-driver)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-open
  '* "GDALOpen" (list '* int) 20)

(define (open-dataset file-name access)
    "Open a raster file as a GDALDataset pointer.

Parameters:
    file-name: the name of the file to access. In the case of exotic drivers
this may not refer to a physical file, but instead contain information for the
driver on how to access a dataset.
    access: the desired access, either GA_UPDATE or GA_READONLY. Many drivers
support only read only access."
    (let ((ptr (%gdal-open
                (string->pointer file-name)
                access)))
      (if (null-pointer? ptr)
  	     (error "failed to open dataset")
       	ptr)))

(export open-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-open-ex
  '* "GDALOpenEx" (list '* int '* '* '*) 20)

(define* (open-dataset-ex file-name flags #:key
                          (allowed-drivers '())
                          (open-options '())
                          (sibling-files '()))
    "Open a raster or vector file as a GDALDataset.

Parameters:
    file-name: the name of the file to access. In the case of exotic drivers
this may not refer to a physical file, but instead contain information for the
driver on how to access a dataset.
    flags: a combination of GDAL_OF_ flags that may be combined through
logical or operator.
    allowed-drivers (optional): empty list '() to consider all candidate
drivers, or a list of strings with the driver short names that must be
considered.
    open-options (optional): empty list '(), or a list of strings with open
options passed to candidate drivers.
    sibling-files (optional): empty list '() or a list of strings that are
filenames that are auxiliary to the main filename."
    (let ((ptr (%gdal-open-ex
                (string->pointer file-name)
                flags
                (string-list->pointerpointer allowed-drivers)
                (string-list->pointerpointer open-options)
                (string-list->pointerpointer sibling-files))))
      (if (null-pointer? ptr)
  	     (error "failed to open dataset")
       	ptr)))

(export open-dataset-ex)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-open-shared
  '* "GDALOpenShared" (list '* int) 20)

(define (open-shared-dataset file-name access)
    "Open a raster file as a GDALDataset pointer.

Parameters:
    file-name: the name of the file to access. In the case of exotic drivers
this may not refer to a physical file, but instead contain information for the
driver on how to access a dataset.
    access: the desired access, either GA_UPDATE or GA_READONLY. Many drivers
support only read only access."
    (let ((ptr (%gdal-open-shared
                (string->pointer file-name)
                access)))
      (if (null-pointer? ptr)
  	     (error "failed to open dataset")
       	ptr)))

(export open-shared-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-driver-by-name
  '* "GDALGetDriverByName" (list '*) 20)

(define (get-driver-by-name driver-short-name)
    "Fetch a driver based on the short name.

Parameters:
    driver-short-name: the short name of the driver, such as 'GTiff' as a
string or GDN_GTIFF as an enum (see GDN_*), being searched for."
    (let ((ptr (%gdal-get-driver-by-name
                (string->pointer driver-short-name))))
      (if (null-pointer? ptr)
  	     (error "failed to find driver")
       	ptr)))

(export get-driver-by-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-driver-count
  int "GDALGetDriverCount" '() 20)

(define (get-driver-count)
    "Fetch the number of registered drivers."
    (%gdal-get-driver-count))

(export get-driver-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-driver
  '* "GDALGetDriver" (list int) 20)

(define (get-driver driver-index)
    "Fetch driver by index.

Parameters:
    driver-index: the driver index from 0 to (1- (get-driver-count))."
    (let ((ptr (%gdal-get-driver driver-index)))
      (if (null-pointer? ptr)
  	     (error "failed to find driver")
       	ptr)))

(export get-driver)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-destroy-driver
  void "GDALDestroyDriver" (list '*) 20)

(define (destroy-driver driver)
    "Destroy a GDALDriver.

Parameters:
    driver: the driver to destroy"
    (%gdal-destroy-driver driver))

(export destroy-driver)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-delete-dataset
  int "GDALDeleteDataset" (list '* '*) 20)

(define (delete-dataset driver file-name)
    "Delete named dataset.

Parameters:
    driver: the driver to use for deleting file-name
    file-name: name of dataset to delete."
    (let ((result (%gdal-delete-dataset driver (string->pointer file-name))))
      (unless (= result CE_NONE)
        (error "failed to delete dataset"))))

(export delete-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rename-dataset
  int "GDALRenameDataset" (list '* '* '*) 20)

(define (rename-dataset driver new-file-name old-file-name)
    "Rename a dataset.

Parameters:
    driver: the driver to use for deleting file-name
    new-file-name: new name for the dataset
    old-file-name: old name for the dataset."
    (let ((result (%gdal-rename-dataset
                   driver
                   (string->pointer new-file-name)
                   (string->pointer old-file-name))))
      (unless (= result CE_NONE)
        (error "failed to rename dataset"))))

(export rename-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-copy-dataset-files
  int "GDALCopyDatasetFiles" (list '* '* '*) 20)

(define (copy-dataset-files driver new-file-name old-file-name)
    "Copy all the files associated with a dataset.

Parameters:
    driver: the driver to use for deleting file-name
    new-file-name: new name for the dataset
    old-file-name: old name for the dataset."
    (let ((result (%gdal-copy-dataset-files
                   driver
                   (string->pointer new-file-name)
                   (string->pointer old-file-name))))
      (unless (= result CE_NONE)
        (error "failed to copy dataset files"))))

(export copy-dataset-files)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-gcps-to-geo-transform
  int "GDALGCPsToGeoTransform" (list int '* '* int) 20)

(define (gcps-to-geo-transform gcp-lst is-approx-ok)
  "Generate Geotransform from GCPs.

Parameters:
    gcp-list: list of GCPs
    is-approx-ok: If #f the function will fail if the geotransform is not
essentially an exact fit (within 0.25 pixel) for all GCPs."
  (cond ((not (pair? gcp-lst))
         (error "input is not a list or empty"))
    ((not (= (count gcp? gcp-lst) (length gcp-lst)))
     (error "input list has at least one non-gcp record"))
    (else (let* ((bv-gcps (make-bytevector (* 6 (sizeof double))))
                 (result (%gdal-gcps-to-geo-transform
                          (length gcp-lst)
                          (gcp-list->pointer gcp-lst)
                          (bytevector->pointer bv-gcps)
                          (boolean->c-bool is-approx-ok))))
            (if (c-bool->boolean result)
              (let ((ne (native-endianness))
                    (coef-max-index 5)
                    (coefs-q (make-q)))
                (do ((i 0 (1+ i)))
                  ((> i coef-max-index))
                  (enq! coefs-q
                        (bytevector-ieee-double-ref bv-gcps
                                                    (* i (sizeof double)) ne)))
                (car coefs-q))
              #f)))))

(export gcps-to-geo-transform)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-inv-geo-transform
  int "GDALInvGeoTransform" (list '* '*) 20)

(define (inv-geo-transform gt-lst)
  "Invert Geotransform.

Parameters:
    gt-lst: list of input geotransform (six doubles)."
  (cond ((not (pair? gt-lst))
         (error "input is not a list or empty"))
    ((not (= 6 (length gt-lst)))
     (error "insufficient number of coefficients in the list (6 doubles)"))
    (else (let* ((bv-gp-out (make-bytevector (* 6 (sizeof double))))
                 (coef-max-index 5)
                 (result (%gdal-inv-geo-transform
                          (list->pointer gt-lst double)
                          (bytevector->pointer bv-gp-out))))
            (if (c-bool->boolean result)
              (let ((coefs-q (make-q)))
                (do ((i 0 (1+ i)))
                  ((> i coef-max-index))
                  (enq! coefs-q (bytevector-ieee-double-native-ref
                                 bv-gp-out
                                 (* i (sizeof double)))))
                (car coefs-q))
              #f)))))

(export inv-geo-transform)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-apply-geo-transform
  int "GDALApplyGeoTransform" (list '* double double '* '*) 20)

(define (apply-geo-transform gt-lst pixel line)
  "Apply GeoTransform to x/y coordinate.

Parameters:
    gt-lst: list of input geotransform (six doubles)
    pixel: input pixel location
    line: input line location."

  (cond ((not (pair? gt-lst))
         (error "input is not a list or empty"))
    ((not (= 6 (length gt-lst)))
     (error "insufficient number of coefficients in the list (6 doubles)"))
    (else (let ((bv-geo-x (make-bytevector (sizeof double)))
                (bv-geo-y (make-bytevector (sizeof double))))
            (%gdal-apply-geo-transform
             (list->pointer gt-lst double)
             pixel
             line
             (bytevector->pointer bv-geo-x)
             (bytevector->pointer bv-geo-y))
            `(,(bytevector-ieee-double-native-ref bv-geo-x 0)
              .
              ,(bytevector-ieee-double-native-ref bv-geo-y 0))))))

(export apply-geo-transform)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-compose-geo-transforms
  void "GDALComposeGeoTransforms" (list '* '* '*) 20)

(define (compose-geo-transforms gt1-lst gt2-lst)
  "Compose two geotransforms.

Parameters:
    gt1-lst: list of first geotransform (six doubles)
    gt2-lst: list of first geotransform (six doubles)"
  (cond ((not (pair? gt1-lst))
         (error "input gt1-lst is not a list or empty"))
    ((not (pair? gt2-lst))
     (error "input gt2-lst is not a list or empty"))
    ((not (= 6 (length gt1-lst)))
     (error "insufficient number of coefficients in the list gt1-lst"))
    ((not (= 6 (length gt2-lst)))
      (error "insufficient number of coefficients in the list gt2-lst"))
    (else (let ((bv-gt-out (make-bytevector (* 6 (sizeof double)))))
            (%gdal-compose-geo-transforms
             (list->pointer gt1-lst double)
             (list->pointer gt2-lst double)
             (bytevector->pointer bv-gt-out))
             (let ((coefs-q (make-q)))
               (do ((i 0 (1+ i)))
                 ((> i 5))
                 (enq! coefs-q (bytevector-ieee-double-native-ref
                                bv-gt-out
                                (* i (sizeof double)))))
               (car coefs-q))))))

(export compose-geo-transforms)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-metadata-domain-list
  '* "GDALGetMetadataDomainList" (list '*) 20)

(define (get-metadata-domain-list h-object)
  "Fetch list of metadata domains.

Returns a string list of metadata domains.

Parameters:
    h-object: a handle representing various GDAL objects.
See https://www.gdal.org/classGDALMajorObject.html for more information."
  (pointerpointer->string-list
   (%gdal-get-metadata-domain-list h-object)))

(export get-metadata-domain-list)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-metadata
  '* "GDALGetMetadata" (list '* '*) 20)

(define (get-metadata h-object domain)
  "Return a string list of metadata which is owned by the object, and may
change at any time. It is formatted as a \"Name=value\" list.

Parameters:
    h-object: a handle representing various GDAL objects.
See https://www.gdal.org/classGDALMajorObject.html for more information
    domain: the domain of interest. Use empty string \"\" for the default
domain."
  (pointerpointer->string-list
   (%gdal-get-metadata h-object (string->pointer domain))))

(export get-metadata)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-metadata
  int "GDALSetMetadata" (list '* '* '*) 20)

(define (set-metadata h-object metadata domain)
    "Sets a string list of a metadata where each member is formatted as
a \"Name=value\".

Parameters:
    h-object: a handle representing various GDAL objects.
See https://www.gdal.org/classGDALMajorObject.html for more information
    metadata: the metadata in name=value string list format to apply.
    domain: the domain of interest. Use empty string \"\" for the default
domain."
    (let ((result (%gdal-set-metadata
		   h-object
		   (string-list->pointerpointer metadata)
		   (string->pointer domain))))
      (unless (= result CE_NONE)
        (error "failed to set metadata"))))

(export set-metadata)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-close
  void "GDALClose" (list '*) 20)

(define (close-dataset h-ds)
    "Close GDAL dataset.

Parameters:
    h-ds: the dataset to close."
    (%gdal-close h-ds))

(export close-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-metadata-item
  '* "GDALGetMetadataItem" (list '* '* '*) 20)

(define (get-metadata-item h-object name domain)
    "Fetch single metadata item.

Parameters:
    h-object: a handle representing various GDAL objects
    name: the key for the metadata item to fetch
    domain: the domain to fetch for, use \"\" for the default domain."
    (let ((ptr (%gdal-get-metadata-item
		 h-object
		 (string->pointer name)
		 (string->pointer domain))))
      (if (null-pointer? ptr)
	  ""
	  (pointer->string ptr))))

(export get-metadata-item)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-metadata-item
  int "GDALSetMetadataItem" (list '* '* '* '*) 20)

(define (set-metadata-item h-object name value domain)
    "Sets a single metadata item which is formatted as \"Name=value\".

Parameters:
    h-object: a handle representing various GDAL objects.
See https://www.gdal.org/classGDALMajorObject.html for more information
    name: the key for the metadata item to fetch.
    value: the value to assign to the key.
    domain: the domain to set within, use \"\" for the default domain."
    (let ((result (%gdal-set-metadata-item
		   h-object
		   (string->pointer name)
       (string->pointer value)
		   (string->pointer domain))))
      (unless (= result CE_NONE)
        (error "failed to set metadata item"))))

(export set-metadata-item)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-description
  '* "GDALGetDescription" (list '*) 20)

(define (get-description h-object)
    "Fetch object description.

Parameters:
    h-object: a handle representing various GDAL objects.
See https://www.gdal.org/classGDALMajorObject.html for more information."
    (pointer->string (%gdal-get-description h-object)))

(export get-description)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-description
  void "GDALSetDescription" (list '* '*) 20)

(define (set-description h-object description)
    "Set object description.

Parameters:
    h-object: a handle representing various GDAL objects.
See https://www.gdal.org/classGDALMajorObject.html for more information.
    description: new description."
    (%gdal-set-description h-object (string->pointer description)))

(export set-description)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-dataset-driver
  '* "GDALGetDatasetDriver" (list '*) 20)

(define (get-dataset-driver h-dataset)
    "Fetch the driver to which this dataset relates.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-dataset-driver h-dataset))

(export get-dataset-driver)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-file-list
  '* "GDALGetFileList" (list '*) 20)

(define (get-file-list h-dataset)
    "Fetch files forming dataset.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (pointerpointer->string-list (%gdal-get-file-list h-dataset)))

(export get-file-list)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-x-size
  int "GDALGetRasterXSize" (list '*) 20)

(define (get-raster-x-size h-dataset)
    "Fetch raster width in pixels.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-raster-x-size h-dataset))

(export get-raster-x-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-y-size
  int "GDALGetRasterYSize" (list '*) 20)

(define (get-raster-y-size h-dataset)
    "Fetch raster height in pixels.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-raster-y-size h-dataset))

(export get-raster-y-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-band-x-size
  int "GDALGetRasterBandXSize" (list '*) 20)

(define (get-raster-band-x-size h-band)
    "Fetch raster width in pixels for the band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
    (%gdal-get-raster-band-x-size h-band))

(export get-raster-band-x-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-band-y-size
  int "GDALGetRasterBandYSize" (list '*) 20)

(define (get-raster-band-y-size h-band)
    "Fetch raster height in pixels for the band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
    (%gdal-get-raster-band-y-size h-band))

(export get-raster-band-y-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-count
  int "GDALGetRasterCount" (list '*) 20)

(define (get-raster-count h-dataset)
    "Fetch the number of raster bands on this dataset.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-raster-count h-dataset))

(export get-raster-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-band
  '* "GDALGetRasterBand" (list '* int) 20)

(define (get-raster-band h-dataset band-id)
    "Fetch a band object for a dataset.

Parameters:
    h-dataset: a handle representing GDALDataset.
    band-id: the index number of the band to fetch, from 1 to get-raster-count."
    (%gdal-get-raster-band h-dataset band-id))

(export get-raster-band)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-add-band
  int "GDALAddBand" (list '* int '*) 20)

(define (add-band h-dataset type options)
    "Add a band to a dataset.

Parameters:
    h-dataset: a handle representing GDALDataset.
    type: the data type of the pixels in the new band.
    options: the list of NAME=VALUE option strings. The supported options are
format specific. Empty list '() may be passed by default."
  (let ((result (%gdal-add-band h-dataset
                                type
                                (string-list->pointerpointer
                                 options))))
    (unless (= result CE_NONE)
      (error "failed to add band"))))

(export add-band)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-raster-io
  int "GDALRasterIO" (list '* int int int int int '*
                             int int int int int) 20)

(define (raster-io h-band rw-flag x-off y-off x-size y-size data
                   buf-x-size buf-y-size buf-type pixel-space line-space)
    "Read/write a region of image data for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    rw-flag: either GF_READ to read, or GF_WRITE to write a region of data.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    data: the bytevector buffer into which the data should be read, or from
which it should be written. This buffer must contain at least
(* buf-x-size buf-y-size) words of type buf-type. It is organized in left to
right, top to bottom pixel order. Spacing is controlled by the pixel-space, and
line-space parameters.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    pixel-space: the byte offset from the start of one pixel value in data to
the start of the next pixel value within a scanline. If defaulted (0) the size
of the datatype buf-type is used.
    line-space: the byte offset from the start of one scanline in pData to the
start of the next. If defaulted (0) the size of the datatype
(* buf-type buf-x-size) is used."
  (let ((result (%gdal-raster-io h-band rw-flag x-off y-off x-size y-size
                                 (bytevector->pointer data)
                                 buf-x-size buf-y-size buf-type
                                 pixel-space line-space)))
    (unless (= result CE_NONE)
      (error "failed to read/write data for this band"))))

(export raster-io)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-raster-io-ex
  int "GDALDatasetRasterIOEx" (list '* int int int int int '*
                                    int int int int '* int64 int64
                                    int64 '*) 20)

(define (dataset-raster-io-ex h-dataset rw-flag x-off y-off x-size y-size data
                              buf-x-size buf-y-size buf-type band-count
                              band-map pixel-space line-space band-space
                              extra-arg)
    "Read/write a region of image data for multiple bands.

Parameters:
    h-dataset: a handle representing GDALDatasetH.
    rw-flag: either GF_READ to read, or GF_WRITE to write a region of data.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    data: the bytevector buffer into which the data should be read, or from
which it should be written. This buffer must contain at least
(* buf-x-size buf-y-size band-count) words of type buf-type. It is organized
in left to right, top to bottom pixel order. Spacing is controlled by the
pixel-space, and line-space parameters.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    band-count: the number of bands being read or written.
    band-map: the list of band-count numbers being read/written. Note band
numbers are 1 based. This may be empty list '() to select the first band-count
bands.
    pixel-space: the byte offset from the start of one pixel value in data to
the start of the next pixel value within a scanline. If defaulted (0) the size
of the datatype buf-type is used.
    line-space: the byte offset from the start of one scanline in pData to the
start of the next. If defaulted (0) the size of the datatype
(* buf-type buf-x-size) is used.
    band-space: the byte offset from the start of one bands data to the start
of the next. If defaulted (0) the value will be (* line-space buf-y-size)
implying band sequential organization of the data buffer.
    extra-arg: (new in GDAL 2.0) pointer to a GDALRasterIOExtraArg structure
constructed with \"make-grioea\" function which creates grioea record
with additional arguments to specify resampling and progress callback, or
#f for default behaviour. The GDAL_RASTERIO_RESAMPLING configuration option can
also be defined to override the default resampling to one of BILINEAR, CUBIC,
CUBICSPLINE, LANCZOS, AVERAGE or MODE."
  (let ((result (%gdal-dataset-raster-io-ex h-dataset rw-flag x-off y-off x-size
                                            y-size (bytevector->pointer data)
                                            buf-x-size buf-y-size buf-type
                                            band-count
                                            (list->pointer band-map int)
                                            pixel-space line-space band-space
                                            (grioea->foreign-pointer extra-arg))
                ))
    (unless (= result CE_NONE)
      (error "failed to read/write data for this band"))))

(export dataset-raster-io-ex)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-raster-io
  int "GDALDatasetRasterIO" (list '* int int int int int '*
                                  int int int int '* int int int) 20)

(define (dataset-raster-io h-dataset rw-flag x-off y-off x-size y-size data
                           buf-x-size buf-y-size buf-type band-count
                           band-map pixel-space line-space band-space)
    "Read/write a region of image data for multiple bands. Use
dataset-raster-io-ex if 64 bit spacings or extra arguments (resampling
resolution, progress callback, etc. are needed).

Parameters:
    h-dataset: a handle representing GDALDatasetH.
    rw-flag: either GF_READ to read, or GF_WRITE to write a region of data.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    data: the bytevector buffer into which the data should be read, or from
which it should be written. This buffer must contain at least
(* buf-x-size buf-y-size band-count) words of type buf-type. It is organized
in left to right, top to bottom pixel order. Spacing is controlled by the
pixel-space, and line-space parameters.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    band-count: the number of bands being read or written.
    band-map: the list of band-count numbers being read/written. Note band
numbers are 1 based. This may be empty list '() to select the first band-count
bands.
    pixel-space: the byte offset from the start of one pixel value in data to
the start of the next pixel value within a scanline. If defaulted (0) the size
of the datatype buf-type is used.
    line-space: the byte offset from the start of one scanline in pData to the
start of the next. If defaulted (0) the size of the datatype
(* buf-type buf-x-size) is used.
    band-space: the byte offset from the start of one bands data to the start
of the next. If defaulted (0) the value will be (* line-space buf-y-size)
implying band sequential organization of the data buffer."
  (let ((result (%gdal-dataset-raster-io h-dataset rw-flag x-off y-off x-size
                                         y-size (bytevector->pointer data)
                                         buf-x-size buf-y-size buf-type
                                         band-count
                                         (list->pointer band-map int)
                                         pixel-space line-space band-space)))
    (unless (= result CE_NONE)
      (error "failed to read/write data for this band"))))

(export dataset-raster-io)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-begin-async-reader
  '* "GDALBeginAsyncReader" (list '* int int int int '* int int
                                   int int '* int int int '*) 20)

(define (begin-async-reader h-dataset x-off y-off x-size y-size data
                            buf-x-size buf-y-size buf-type band-count
                            band-map pixel-space line-space band-space
                            options)
    "Set up an asynchronous data request and return handle representing the
request.

Parameters:
    h-dataset: a handle representing GDALDatasetH.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    data: the bytevector buffer into which the data should be read, or from
which it should be written. This buffer must contain at least
(* buf-x-size buf-y-size band-count) words of type buf-type. It is organized
in left to right, top to bottom pixel order. Spacing is controlled by the
pixel-space, and line-space parameters.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    band-count: the number of bands being read or written.
    band-map: the list of band-count numbers being read/written. Note band
numbers are 1 based. This may be empty list '() to select the first band-count
bands.
    pixel-space: the byte offset from the start of one pixel value in data to
the start of the next pixel value within a scanline. If defaulted (0) the size
of the datatype buf-type is used.
    line-space: the byte offset from the start of one scanline in pData to the
start of the next. If defaulted (0) the size of the datatype
(* buf-type buf-x-size) is used.
    band-space: the byte offset from the start of one bands data to the start
of the next. If defaulted (0) the value will be (* line-space buf-y-size)
implying band sequential organization of the data buffer.
    options: driver specific control options in a string list or empty
list '(). Consult driver documentation for options supported."
  (%gdal-begin-async-reader h-dataset x-off y-off x-size y-size
                            (bytevector->pointer data) buf-x-size buf-y-size
                            buf-type band-count (list->pointer band-map int)
                            pixel-space line-space band-space
                            (string-list->pointerpointer options)))

(export begin-async-reader)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-end-async-reader
  void "GDALEndAsyncReader" (list '* '*) 20)

(define (end-async-reader h-dataset h-async-reader)
    "End asynchronous request.

Parameters:
    h-dataset: handle representing GDALDataset.
    h-async-reader: handle representing async reader request."
    (%gdal-end-async-reader h-dataset h-async-reader))

(export end-async-reader)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-raster-io-ex
  int "GDALRasterIOEx" (list '* int int int int int '*
                             int int int int64 int64 '*) 20)

(define (raster-io-ex h-band rw-flag x-off y-off x-size y-size data
                      buf-x-size buf-y-size buf-type pixel-space
                      line-space extra-arg)
    "Read/write a region of image data for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    rw-flag: either GF_READ to read, or GF_WRITE to write a region of data.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    data: the bytevector buffer into which the data should be read, or from
which it should be written. This buffer must contain at least
(* buf-x-size buf-y-size) words of type buf-type. It is organized in left to
right, top to bottom pixel order. Spacing is controlled by the pixel-space, and
line-space parameters.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    pixel-space: the byte offset from the start of one pixel value in data to
the start of the next pixel value within a scanline. If defaulted (0) the size
of the datatype buf-type is used.
    line-space: the byte offset from the start of one scanline in pData to the
start of the next. If defaulted (0) the size of the datatype
(* buf-type buf-x-size) is used.
  extra-arg: (new in GDAL 2.0) pointer to a GDALRasterIOExtraArg structure
constructed with \"make-grioea\" function which creates grioea record
with additional arguments to specify resampling and progress callback, or
#f for default behaviour. The GDAL_RASTERIO_RESAMPLING configuration option can
also be defined to override the default resampling to one of BILINEAR, CUBIC,
CUBICSPLINE, LANCZOS, AVERAGE or MODE."
  (let ((result (%gdal-raster-io-ex h-band rw-flag x-off y-off x-size y-size
                                    (bytevector->pointer data)
                                    buf-x-size buf-y-size buf-type
                                    pixel-space line-space
                                    (grioea->foreign-pointer extra-arg))))
    (unless (= result CE_NONE)
      (error "failed to read/write data for this band"))))

(export raster-io-ex)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-advise-read
  int "GDALDatasetAdviseRead" (list '* int int int int
                                    int int int int '* '*) 20)

(define (dataset-advise-read h-dataset x-off y-off x-size y-size
                            buf-x-size buf-y-size buf-type band-count
                            band-map options)
    "Advise driver of upcoming read requests. Return CE_FAILURE if the request
is invalid and CE_NONE if it works or is ignored.

Parameters:
    h-dataset: a handle representing GDALDatasetH.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    band-count: the number of bands being read or written.
    band-map: the list of band-count numbers being read/written. Note band
numbers are 1 based. This may be empty list '() to select the first band-count
bands.
    options: driver specific control options in a string list or empty
list '(). Consult driver documentation for options supported."
  (%gdal-dataset-advise-read h-dataset x-off y-off x-size y-size
                             buf-x-size buf-y-size buf-type band-count
                             (list->pointer band-map int)
                             (string-list->pointerpointer options)))

(export dataset-advise-read)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-projection-ref
  '* "GDALGetProjectionRef" (list '*) 20)

(define (get-projection-ref h-dataset)
    "Fetch the projection definition string for this dataset.

The returned string defines the projection coordinate system of the image in
OpenGIS WKT format.

When a projection definition is not available an empty string is returned.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (pointer->string (%gdal-get-projection-ref h-dataset)))

(export get-projection-ref)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-spatial-ref
  '* "GDALGetSpatialRef" (list '*) 30)

(define (get-spatial-ref h-dataset)
    "Fetch the spatial reference for this dataset. Available since GDAL 3.0.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-spatial-ref h-dataset))

(export get-spatial-ref)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-projection
  int "GDALSetProjection" (list '* '*) 20)

(define (set-projection h-dataset projection)
    "Set the projection reference string in OGC WKT or PROJ.4 format for this
dataset.

Parameters:
    h-dataset: a handle representing GDALDataset.
    projection: projection reference string."
    (let ((result (%gdal-set-projection h-dataset
                                        (string->pointer projection))))
      (unless (= result CE_NONE)
        (error "failed to set projection"))))

(export set-projection)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-spatial-ref
  int "GDALSetSpatialRef" (list '* '*) 30)

(define (set-spatial-ref h-dataset srs)
    "Set the spatial reference system for this dataset. Available since
GDAL 3.0.

Parameters:
    h-dataset: a handle representing GDALDataset.
    srs: spatial reference system object. %null-pointer can potentially be
passed for drivers that support unsetting the SRS."
    (let ((result (%gdal-set-spatial-ref h-dataset srs)))
      (unless (= result CE_NONE)
        (error "failed to set spatial reference"))))

(export set-spatial-ref)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-geo-transform
  int "GDALGetGeoTransform" (list '* '*) 20)

(define (get-geo-transform h-dataset)
  "Return a list of transformation coefficients.

Parameters:
    h-dataset: a handle representing GDALDataset."

  (let* ((bv-gp-out (make-bytevector (* 6 (sizeof double))))
         (coef-max-index 5)
         (result (%gdal-get-geo-transform
                  h-dataset
                  (bytevector->pointer bv-gp-out))))
    (if (= result CE_FAILURE)
      (error "failed to fetch transform")
      (let ((coefs-q (make-q)))
        (do ((i 0 (1+ i)))
          ((> i coef-max-index))
          (enq! coefs-q (bytevector-ieee-double-native-ref
                         bv-gp-out
                         (* i (sizeof double)))))
        (car coefs-q)))))

(export get-geo-transform)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-geo-transform
  int "GDALSetGeoTransform" (list '* '*) 20)

(define (set-geo-transform h-dataset transform)
  "Set the affine transformation coefficients.

Parameters:
    h-dataset: a handle representing GDALDataset.
    transform: a list of transformation coefficients."
  (cond ((not (pair? transform))
         (error "transform is not a list or empty"))
    ((not (= 6 (length transform)))
     (error "insufficient number of coefficients in the list (6 doubles)"))
    (else (let ((result (%gdal-set-geo-transform
                         h-dataset
                         (list->pointer transform double))))
            (unless (= result CE_NONE)
              (error "failed to set transform"))))))

(export set-geo-transform)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-gcp-count
  int "GDALGetGCPCount" (list '*) 20)

(define (get-gcp-count h-dataset)
    "Return number of GCPs for this dataset.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-gcp-count h-dataset))

(export get-gcp-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-gcp-projection
  '* "GDALGetGCPProjection" (list '*) 20)

(define (get-gcp-projection h-dataset)
    "Return internal projection string for GCPs or empty string if there are
no GCPs.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (pointer->string (%gdal-get-gcp-projection h-dataset)))

(export get-gcp-projection)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-gcp-spatial-ref
  '* "GDALGetGCPSpatialRef" (list '*) 30)

(define (get-gcp-spatial-ref h-dataset)
    "Return a pointer to an internal object of output spatial reference system
for GCPs. Available since GDAL 3.0.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-gcp-spatial-ref h-dataset))

(export get-gcp-spatial-ref)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-gcps
  '* "GDALGetGCPs" (list '*) 20)

(define (get-gcps h-dataset)
  "Return a list of internal GCP structure.

Parameters:
    h-dataset: a handle representing GDALDataset."
  (pointer->gcp-list (%gdal-get-gcps h-dataset) (get-gcp-count h-dataset)))

(export get-gcps)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-gcps
  int "GDALSetGCPs" (list '* int '* '*) 20)

(define* (set-gcps h-dataset gcp-lst #:key (projection ""))
  "Assign GCPs.

Parameters:
    h-dataset: a handle representing GDALDataset.
    gc-lst: list of GCP structures where each structure is created by
\"make-gcp\" function.
    projection (optional): string of the new OGC WKT coordinate system to
assign for the GCP output coordinates."
  (let ((result (%gdal-set-gcps h-dataset
                                (length gcp-lst)
                                (gcp-list->pointer gcp-lst)
                                (string->pointer projection))))
    (unless (= result CE_NONE)
      (error "failed to set GCPs"))))

(export set-gcps)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-gcps2
  int "GDALSetGCPs2" (list '* int '* '*) 30)

(define* (set-gcps2 h-dataset gcp-lst #:key (spatial-ref %null-pointer))
  "Assign GCPs. Available since GDAL 3.0.

Parameters:
    h-dataset: a handle representing GDALDataset.
    gc-lst: list of GCP structures where each structure is created by
\"make-gcp\" function.
    spatial-ref (optional): the new coordinate reference system to assign for
the GCP output coordinates."
  (let ((result (%gdal-set-gcps2 h-dataset
                                (length gcp-lst)
                                (gcp-list->pointer gcp-lst)
                                spatial-ref)))
    (unless (= result CE_NONE)
      (error "failed to set GCPs"))))

(export set-gcps2)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-internal-handle
  '* "GDALGetInternalHandle" (list '* '*) 20)

(define (get-internal-handle h-dataset request)
    "Fetch a format specific internally meaningful handle.

Parameters:
    h-dataset: a handle representing GDALDataset.
    request: the handle name desired. The meaningful names will be specific to
the file format."
    (%gdal-get-internal-handle h-dataset (string->pointer request)))

(export get-internal-handle)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-reference-dataset
  int "GDALReferenceDataset" (list '*) 20)

(define (reference-dataset h-dataset)
    "Add one to dataset reference count.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-reference-dataset h-dataset))

(export reference-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dereference-dataset
  int "GDALDereferenceDataset" (list '*) 20)

(define (dereference-dataset h-dataset)
    "Subtract one from dataset reference count.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-dereference-dataset h-dataset))

(export dereference-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-release-dataset
  int "GDALReleaseDataset" (list '*) 20)

(define (release-dataset h-dataset)
    "Drop a reference to this object, and destroy if no longer referenced.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-release-dataset h-dataset))

(export release-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-build-overviews
  int "GDALBuildOverviews" (list '* '* int '* int '* '* '*) 20)

(define* (build-overviews h-dataset resampling overview-list band-list
                          #:key (progress-callback '())
                          (progress-data %null-pointer))
    "Build raster overview(s).

Parameters:
    h-dataset: a handle representing GDALDatasetH.
    resampling: one of GRIORAv2 enums controlling the downsampling method
applied.
    overview-list: the list of overview decimation factors to build, or '() to
clean overviews.
    band-list: list of band numbers to build overviews. Build for all bands if
this is '().
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let ((result (%gdal-build-overviews h-dataset
                                       (string->pointer (assv-ref
                                          *grioeav2-to-string* resampling))
                                       (length overview-list)
                                       (list->pointer overview-list int)
                                       (length band-list)
                                       (list->pointer band-list int)
                                       (gdal-progress-func progress-callback)
                                       progress-data)))
    (when (= result CE_FAILURE)
      (error "failed to build overview"))))

(export build-overviews)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-open-datasets
  void "GDALGetOpenDatasets" (list '* '*) 20)

(define (get-open-datasets)
  "Return a list of all open GDAL dataset handles."
  (let ((bv-ds (make-bytevector (sizeof '*)))
        (bv-count (make-bytevector (sizeof '*))))
    (%gdal-get-open-datasets (bytevector->pointer bv-ds)
                             (bytevector->pointer bv-count))
    (pointerpointer->list (bytevector->pointer bv-ds)
                          dereference-pointer
                          (bytevector-sint-ref
                           bv-count 0
                           (native-endianness)
                           (sizeof int)))))

(export get-open-datasets)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-access
  int "GDALGetAccess" (list '*) 20)

(define (get-access h-dataset)
    "Return access flag.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-get-access h-dataset))
(export get-access)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-flush-cache
  void "GDALFlushCache" (list '*) 20)

(define (flush-cache h-dataset)
    "Flush all write cached data to disk.

Parameters:
    h-dataset: a handle representing GDALDataset."
    (%gdal-flush-cache h-dataset))

(export flush-cache)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-create-dataset-mask-band
  int "GDALCreateDatasetMaskBand" (list '* int) 20)

(define (create-dataset-mask-band h-dataset flags)
  "Adds a mask band to the dataset.

Parameters:
    h-dataset: a handle representing GDALDataset.
    flags: 0 or combination of GMF_PER_DATASET / GMF_ALPHA. GMF_PER_DATASET
will be always set, even if not explicitly specified."
  (let ((result (%gdal-create-dataset-mask-band h-dataset flags)))
    (unless (= result CE_NONE)
      (error "failed to create mask band"))))

(export create-dataset-mask-band)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-copy-whole-raster
  int "GDALDatasetCopyWholeRaster" (list '* '* '* '* '*) 20)

(define* (dataset-copy-whole-raster src-dataset dst-dataset options
                                    #:key (progress-callback '())
                                    (progress-data %null-pointer))
  "Copy all dataset raster data.

Currently the only options value supported are :

    \"INTERLEAVE=PIXEL\" to force pixel interleaved operation
    \"COMPRESSED=YES\" to force alignment on target dataset block sizes to
achieve best compression.
    \"SKIP_HOLES=YES\" to skip chunks for which \"get-data-coverage-status\"
returns GDAL_DATA_COVERAGE_STATUS_EMPTY (GDAL >= 2.2)

Parameters:
    src-dataset: the source dataset
    dst-dataset: the destination dataset.
    options: a list of strings for transfer hints in Name=Value format.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let ((result (%gdal-dataset-copy-whole-raster src-dataset
                                                 dst-dataset
                                                 (string-list->pointerpointer
                                                  options)
                                                 (gdal-progress-func
                                                  progress-callback)
                                                 progress-data)))
    (unless (= result CE_NONE)
      (error "failed to copy raster dataset"))))

(export dataset-copy-whole-raster)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-raster-band-copy-whole-raster
  int "GDALRasterBandCopyWholeRaster" (list '* '* '* '* '*) 20)

(define* (raster-band-copy-whole-raster src-band dst-band options
                                        #:key (progress-callback '())
                                        (progress-data %null-pointer))
  "Copy a whole raster band.

Currently the only options value supported are :

    \"COMPRESSED=YES\" to force alignment on target dataset block sizes to
achieve best compression.
    \"SKIP_HOLES=YES\" to skip chunks for which \"get-data-coverage-status\"
returns GDAL_DATA_COVERAGE_STATUS_EMPTY (GDAL >= 2.2)

Parameters:
    src-band: the source band.
    dst-band: the destination band.
    options: a list of strings for transfer hints in Name=Value format.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let ((result (%gdal-raster-band-copy-whole-raster
                 src-band
                 dst-band
                (string-list->pointerpointer options)
                (gdal-progress-func progress-callback)
                progress-data)))
    (unless (= result CE_NONE)
      (error "failed to copy raster band"))))

(export raster-band-copy-whole-raster)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-regenerage-overviews
  int "GDALRegenerateOverviews" (list '* int '* '* '* '*) 20)

(define* (regenerage-overviews src-band band-list resampling
                               #:key (progress-callback '())
                               (progress-data %null-pointer))
  "Generate downsampled overviews.

Parameters:
    src-band: the source (base level) band.
    band-list: the list of downsampled bands to be generated.
    resampling: one of GRIORAv2 enums controlling the downsampling method
applied.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let ((result (%gdal-regenerage-overviews
                 src-band
                 (length band-list)
                 (list->pointer band-list '*)
                 (string->pointer (assv-ref
                                   *grioeav2-to-string* resampling))
                 (gdal-progress-func progress-callback)
                 progress-data)))
    (when (= result CE_FAILURE)
      (error "failed to regenerate overview"))))

(export regenerage-overviews)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-get-layer-count
  int "GDALDatasetGetLayerCount" (list '*) 20)

(define (dataset-get-layer-count h-dataset)
    "Get the number of layers in this dataset.

Parameters:
    h-dataset: a dataset handle."
    (%gdal-dataset-get-layer-count h-dataset))

(export dataset-get-layer-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-get-layer
  '* "GDALDatasetGetLayer" (list '* int) 20)

(define (dataset-get-layer h-dataset layer)
    "Fetch a layer by index. Return the layer handle or %null-pointer if layer
is out of range or an error occurs.

Parameters:
    h-dataset: a dataset handle.
    layer: a layer number between 0 and (1- dataset-get-layer-count)"
    (%gdal-dataset-get-layer h-dataset layer))

(export dataset-get-layer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-get-layer-by-name
  '* "GDALDatasetGetLayerByName" (list '* '*) 20)

(define (dataset-get-layer-by-name h-dataset name)
    "Fetch a layer by name. Return the layer handle or %null-pointer if layer
is not found or an error occurs.

Parameters:
    h-dataset: a dataset handle.
    name: the layer name of the layer to fetch."
    (%gdal-dataset-get-layer-by-name h-dataset (string->pointer name)))

(export dataset-get-layer-by-name)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-delete-layer
  int "GDALDatasetDeleteLayer" (list '* int) 20)

(define (dataset-delete-layer h-dataset layer)
  "Delete the indicated layer from the datasource.

Parameters:
    h-dataset: a dataset handle.
    layer: the index of the layer to delete."
  (let ((result (%gdal-dataset-delete-layer h-dataset layer)))
    (unless (= result CE_NONE)
      (error "failed to delete layer"))))

(export dataset-delete-layer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-create-layer
  '* "GDALDatasetCreateLayer" (list '* '* '* int '*) 20)

(define* (dataset-create-layer h-dataset name
                               #:key (spatial-ref %null-pointer)
                                          (type 0)
                                          (options '()))
    "This function attempts to create a new layer on the dataset with the
indicated name, coordinate system, geometry type. Return an handle to the layer
or %null-pointer is returned on failure.

Parameters:
    h-dataset: a dataset handle.
    name: the name for the new layer. This should ideally not match any
existing layer on the datasource.
    spatial-ref (optional): the coordinate system handle to use for the new
layer. Default is %null-pointer where no coordinate system is available.
    type (optional): the geometry type for the layer. Default is WKB_UNKNOWN
providing no constraints on the types geometry to be written. Use the module
(gdal ogr) to access WKB enums.
    options (optional): a list of strings in name=value format. Default is
empty list. Options are driver specific"
    (%gdal-dataset-create-layer h-dataset
                                (string->pointer name)
                                spatial-ref
                                type
                                (string-list->pointerpointer options)))

(export dataset-create-layer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-copy-layer
  '* "GDALDatasetCopyLayer" (list '* '* '* '*) 20)

(define* (dataset-copy-layer h-dataset layer name
                             #:key (options '()))
  "Duplicate an existing layer. Return an handle to the layer or %null-pointer
if an error occurs.

Parameters:
    h-dataset: a dataset handle.
    layer: source layer.
    name: the name for the new layer. This should ideally not match any
existing layer on the datasource.
    options (optional): a list of strings in name=value format. Default is
empty list. Options are driver specific"
  (%gdal-dataset-copy-layer h-dataset
                            layer
                            (string->pointer name)
                            (string-list->pointerpointer options)))

(export dataset-copy-layer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-reset-reading
  void "GDALDatasetResetReading" (list '*) 20)

(define (dataset-reset-reading h-dataset)
    "Reset feature reading to start on the first feature.

Parameters:
    h-dataset: a dataset handle."
    (%gdal-dataset-reset-reading h-dataset))

(export dataset-reset-reading)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-get-next-feature
  '* "GDALDatasetGetNextFeature" (list '* '* '* '* '*) 20)

(define* (dataset-get-next-feature h-dataset #:key (progress-callback '())
                                   (progress-data %null-pointer))
  "Fetch the next available feature from this dataset. Return multiple values
of feature in a list, belonging layer pointer which the object belongs to and
a double variable for the precentage progress in [0, 1] range, respectively.
Otherwise returns empty list if no more features are available.

Parameters:
    h-dataset: a dataset handle.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let* ((layer (make-bytevector (sizeof '*)))
         (progress (make-bytevector (sizeof double)))
         (result (%gdal-dataset-get-next-feature
                 h-dataset
                 (bytevector->pointer layer)
                 (bytevector->pointer progress)
                 (gdal-progress-func progress-callback)
                 progress-data)))
    (if (null-pointer? result)
      '()
      (list (dereference-pointer (bytevector->pointer layer))
            (bytevector-ieee-double-ref progress 0 (native-endianness))))))

(export dataset-get-next-feature)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-test-capability
  int "GDALDatasetTestCapability" (list '* '*) 20)

(define (dataset-test-capability h-dataset cap)
    "Test if capability is available. Return #t if capability available
otherwise #f.

- ODSC_CREATE_LAYER: True if this datasource can create new layers.
- ODSC_DELETE_LAYER: True if this datasource can delete existing layers.
- ODSC_CREATE_GEOM_FIELD_AFTER_CREATE_LAYER: True if the layers of this
datasource support create-geom-field just after layer creation.
- ODSC_CURVE_GEOMETRIES: True if this datasource supports curve geometries.
- ODSC_TRANSACTIONS: True if this datasource supports (efficient) transactions.
- ODSC_EMULATED_TRANSACTIONS: True if this datasource supports transactions
through emulation.
- ODSC_RANDOM_LAYER_READ: True if this datasource has a dedicated
get-next-feature implementation, potentially returning features from layers in
a non sequential way.
- ODSC_RANDOM_LAYER_WRITE: True if this datasource supports calling
create-feature on layers in a non sequential way.

Parameters:
    h-dataset: a dataset handle.
    cap: ODCS enum for the capability to test."
    (c-bool->boolean (%gdal-dataset-test-capability h-dataset
                                                    (string->pointer cap))))

(export dataset-test-capability)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-execute-sql
  '* "GDALDatasetExecuteSQL" (list '* '* '* '*) 20)

(define* (dataset-execute-sql h-dataset statement #:key
                              (spatial-filter %null-pointer)
                              (dialect %null-pointer))
    "Execute an SQL statement against the data store. Return a pointer
of an OGRLayer containing the results of the query. Deallocate with
\"release-result-set\".

Parameters:
    h-dataset: the dataset handle.
    statement: the SQL statement to execute.
    spatial-filter: geometry which represents a spatial filter. Default is
%null-pointer.
    dialect: a string that allows control of the statement dialect. By default
the OGR SQL engine will be used."
    (%gdal-dataset-execute-sql h-dataset
                               (string->pointer statement)
                               spatial-filter
                               (if (null-pointer? dialect)
                                dialect
                                (string->pointer dialect))))

(export dataset-execute-sql)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-release-result-set
  void "GDALDatasetReleaseResultSet" (list '* '*) 20)

(define (dataset-release-result-set h-dataset h-layer)
    "Release results of dataset-execute-sql.

Parameters:
    h-dataset: a dataset handle.
    h-layer: the result of a previous dataset-execute-sql call."
    (%gdal-dataset-release-result-set h-dataset h-layer))

(export dataset-release-result-set)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-get-style-table
  '* "GDALDatasetGetStyleTable" (list '*) 20)

(define (dataset-get-style-table h-dataset)
    "Return the OGRStyleTableH handle of dataset style table which should not
be modified or freed by the caller.

Parameters:
    h-dataset: a dataset handle."
    (%gdal-dataset-get-style-table h-dataset))

(export dataset-get-style-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-set-style-table-directly
  void "GDALDatasetSetStyleTableDirectly" (list '* '*) 20)

(define (dataset-set-style-table-directly h-dataset h-style-table)
    "Set dataset style table.

Parameters:
    h-dataset: a dataset handle.
    h-style-table: the style table handle to set."
    (%gdal-dataset-set-style-table-directly h-dataset h-style-table))

(export dataset-set-style-table-directly)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-set-style-table
  void "GDALDatasetSetStyleTable" (list '* '*) 20)

(define (dataset-set-style-table h-dataset h-style-table)
    "Set dataset style table. This function operate exactly as
dataset-set-style-table-directly except that it assumes ownership of the
passed table.

Parameters:
    h-dataset: a dataset handle.
    h-style-table: the style table handle to set."
    (%gdal-dataset-set-style-table h-dataset h-style-table))

(export dataset-set-style-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-start-transaction
  int "GDALDatasetStartTransaction" (list '* int) 20)

(define (dataset-start-transaction h-dataset force)
  "For datasources which support transactions, dataset-start-transaction
creates a transaction. Return OGRERR_NONE on success. If starting the
transaction fails, will return OGRERR_FAILURE. Datasources which do not
support transactions will always return OGRERR_UNSUPPORTED_OPERATION.

Parameters:
    h-dataset: a handle representing GDALDataset.
    force: a boolean value that can be set to #t if an emulation, possibly
slow, of a transaction mechanism is acceptable."
  (%gdal-dataset-start-transaction h-dataset
                                   (boolean->c-bool force)))

(export dataset-start-transaction)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-commit-transaction
  int "GDALDatasetCommitTransaction" (list '*) 20)

(define (dataset-commit-transaction h-dataset)
  "For datasources which support transactions, dataset-commit-transaction
commits a transaction. Return OGRERR_NONE on success. If no transaction is
active, or the commit fails, will return OGRERR_FAILURE. Datasources which do
not support transactions will always return OGRERR_UNSUPPORTED_OPERATION.

Parameters:
    h-dataset: a handle representing GDALDataset."
  (%gdal-dataset-commit-transaction h-dataset))

(export dataset-commit-transaction)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-dataset-rollback-transaction
  int "GDALDatasetRollbackTransaction" (list '*) 20)

(define (dataset-rollback-transaction h-dataset)
  "For datasources which support transactions, dataset-rollback-transaction
will roll back a datasource to its state before the start of the current
transaction. Return OGRERR_NONE on success. If no transaction is active, or
the rollback fails, will return OGRERR_FAILURE. Datasources which do not
support transactions will always return OGRERR_UNSUPPORTED_OPERATION.

Parameters:
    h-dataset: a handle representing GDALDataset."
  (%gdal-dataset-rollback-transaction h-dataset))

(export dataset-rollback-transaction)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-data-type
  int "GDALGetRasterDataType" (list '*) 20)

(define (get-raster-data-type h-band)
  "Fetch the pixel data type for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-raster-data-type h-band))

(export get-raster-data-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-block-size
  void "GDALGetBlockSize" (list '* '* '*) 20)

(define (get-block-size h-band)
  "Fetch the \"natural\" block size of this band as values of x size and
y size, respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (let* ((bv-x-size (make-bytevector (sizeof int)))
         (bv-y-size (make-bytevector (sizeof int))))
    (%gdal-get-block-size h-band
                          (bytevector->pointer bv-x-size)
                          (bytevector->pointer bv-y-size))
    (values
     (bytevector-sint-ref bv-x-size 0 (native-endianness) (sizeof int))
     (bytevector-sint-ref bv-y-size 0 (native-endianness) (sizeof int)))))

(export get-block-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-actual-block-size
  int "GDALGetActualBlockSize" (list '* int int '* '*) 20)

(define (get-actual-block-size h-band x-block-off y-block-off)
  "Retrieve the actual block size for a given block offset as values of the
number of valid pixels in the x direction and y direction, respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    x-block-off: 	the horizontal block offset for which to calculate the
number of valid pixels, with zero indicating the left most block, 1 the next
block and so forth.
    y-block-off: the vertical block offset, with zero indicating the left most
block, 1 the next block and so forth."
  (let* ((bv-x-size (make-bytevector (sizeof int)))
         (bv-y-size (make-bytevector (sizeof int))))
    (%gdal-get-actual-block-size h-band x-block-off y-block-off
                                 (bytevector->pointer bv-x-size)
                                 (bytevector->pointer bv-y-size))
    (values
     (bytevector-sint-ref bv-x-size 0 (native-endianness) (sizeof int))
     (bytevector-sint-ref bv-y-size 0 (native-endianness) (sizeof int)))))

(export get-actual-block-size)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-raster-advise-read
  int "GDALRasterAdviseRead" (list '* int int int int int int int '*) 20)

(define (raster-advise-read h-band x-off y-off x-size y-size
                            buf-x-size buf-y-size buf-type options)
    "Advise driver of upcoming read requests. Return CE_FAILURE if the request
is invalid and CE_NONE if it works or is ignored.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    x-off: the pixel offset to the top left corner of the region of the band.
    y-off: the line offset to the top left corner of the region of the band.
    x-size: the width of the region of the band.
    y-size: the height of the region of the band.
    buf-x-size: the width of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-y-size: the height of the buffer image into which the desired region is
to be read, or from which it is to be written.
    buf-type: the type of the pixel values to be returned. The pixel values
will automatically be translated to/from the GDALRasterBand data type as needed.
    options: driver specific control options in a string list or empty
list '(). Consult driver documentation for options supported."
  (%gdal-raster-advise-read h-band x-off y-off x-size y-size
                            buf-x-size buf-y-size buf-type
                            (string-list->pointerpointer options)))

(export raster-advise-read)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-read-block
  int "GDALReadBlock" (list '* int int '*) 20)

(define (read-block h-band x-block-off y-block-off data)
  "Read a block of image data efficiently.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    x-block-off: the horizontal block offset, with zero indicating the left
most block, 1 the next block and so forth
    y-block-off: the vertical block offset, with zero indicating the top most
block, 1 the next block and so forth.
    data: the bytevector buffer into which the data should be read. The buffer
must be large enough to hold (* block-x-size block-y-size) words of type
raster-data-type."
  (let ((result (%gdal-read-block h-band x-block-off y-block-off
                                  (bytevector->pointer data))))
    (unless (= result CE_NONE)
      (error "failed to read a block of data for this band"))))

(export read-block)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-write-block
  int "GDALWriteBlock" (list '* int int '*) 20)

(define (write-block h-band x-block-off y-block-off data)
  "Write a block of image data efficiently.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    x-block-off: the horizontal block offset, with zero indicating the left
most block, 1 the next block and so forth
    y-block-off: the vertical block offset, with zero indicating the top most
block, 1 the next block and so forth.
    data: the bytevector buffer from which the data will be written. The buffer
must be large enough to hold (* block-x-size block-y-size) words of type
raster-data-type."
  (let ((result (%gdal-write-block h-band x-block-off y-block-off
                                   (bytevector->pointer data))))
    (unless (= result CE_NONE)
      (error "failed to write a block of data for this band"))))

(export write-block)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-access
  int "GDALGetRasterAccess" (list '*) 20)

(define (get-raster-access h-band)
  "Find out if we have update permission for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-raster-access h-band))

(export get-raster-access)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-band-number
  int "GDALGetBandNumber" (list '*) 20)

(define (get-band-number h-band)
  "Fetch the band number for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-band-number h-band))

(export get-band-number)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-band-dataset
  '* "GDALGetBandDataset" (list '*) 20)

(define (get-band-dataset h-band)
  "Fetch the owning dataset handle for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-band-dataset h-band))

(export get-band-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-color-interpretation
  '* "GDALGetRasterColorInterpretation" (list '*) 20)

(define (get-raster-color-interpretation h-band)
  "Fetch the handle of GDALColorInterp to figure out how this band is
interpreted as color.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-raster-color-interpretation h-band))

(export get-raster-color-interpretation)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-color-interpretation
  int "GDALSetRasterColorInterpretation" (list '* '*) 20)

(define (set-raster-color-interpretation h-band color-interp)
  "Set color interpretation of a band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    color-interp: a handle of	the new color interpretation to apply to this
band."
  (let ((result (%gdal-set-raster-color-interpretation h-band
                                                       color-interp)))
    (unless (= result CE_NONE)
      (error "failed to set color interpretation"))))

(export set-raster-color-interpretation)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-color-table
  '* "GDALGetRasterColorTable" (list '*) 20)

(define (get-raster-color-table h-band)
  "Fetch the color table associated with band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-raster-color-table h-band))

(export get-raster-color-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-color-table
  int "GDALSetRasterColorTable" (list '* '*) 20)

(define (set-raster-color-table h-band color-table)
  "Set the raster color table of a band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    color-table: the color table to apply. This may be %null-pointer to clear
the color table (where supported)."
  (let ((result (%gdal-set-raster-color-table h-band
                                              color-table)))
    (unless (= result CE_NONE)
      (error "failed to set color table"))))

(export set-raster-color-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-has-arbitrary-overviews
  int "GDALHasArbitraryOverviews" (list '*) 20)

(define (has-arbitrary-overviews h-band)
  "Check for arbitrary overviews. Return #t if arbitrary overviews available
(efficiently), otherwise #f.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (c-bool->boolean (%gdal-has-arbitrary-overviews h-band)))

(export has-arbitrary-overviews)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-overview-count
  int "GDALGetOverviewCount" (list '*) 20)

(define (get-overview-count h-band)
  "Return the number of overview layers available.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (%gdal-get-overview-count h-band))

(export get-overview-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-overview
  '* "GDALGetOverview" (list '* int) 20)

(define (get-overview h-band id)
    "Fetch overview raster band object.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    id: overview index between 0 and (- (get-overview-count h-band) 1)"
    (%gdal-get-overview h-band id))

(export get-overview)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-no-data-value
  double "GDALGetRasterNoDataValue" (list '* '*) 20)

(define (get-raster-no-data-value h-band)
  "Fetch the no data value as double for this band, or report an error
to indicate if no value is actually associated with this layer.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (let* ((bv-success (make-bytevector (sizeof int)))
         (no-data (%gdal-get-raster-no-data-value
                   h-band
                   (bytevector->pointer bv-success))))
    (if (c-bool->boolean (bytevector-sint-ref bv-success
                                              0
                                              (native-endianness)
                                              (sizeof int)))
      no-data
      (error "failed to fetch no data value"))))

(export get-raster-no-data-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-no-data-value
  int "GDALSetRasterNoDataValue" (list '* double) 20)

(define (set-raster-no-data-value h-band no-data)
  "Set the no data value for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    no-data: the value to set."
  (let* ((result (%gdal-set-raster-no-data-value
                  h-band
                  no-data)))
    (unless (= result CE_NONE)
      (error "failed to set no data"))))

(export set-raster-no-data-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-delete-raster-no-data-value
  int "GDALDeleteRasterNoDataValue" (list '*) 20)

(define (delete-raster-no-data-value h-band)
    "Remove the no data value for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH."
    (let ((result (%gdal-delete-raster-no-data-value h-band)))
      (unless (= result CE_NONE)
        (error "failed to delete no-data value"))))

(export delete-raster-no-data-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-category-names
  '* "GDALGetRasterCategoryNames" (list '*) 20)

(define (get-raster-category-names h-band)
    "Fetch the list of category names for this raster as a list of strings.

Parameters:
    h-band: a handle representing GDALRasterBandH."
    (pointerpointer->string-list (%gdal-get-raster-category-names h-band)))

(export get-raster-category-names)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-category-names
  int "GDALSetRasterCategoryNames" (list '* '*) 20)

(define* (set-raster-category-names h-band #:key (category-names '()))
    "Set the category names for this band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    category-names (optional): a list of strings for category names. Default is
empty list '() that clears the existing list."
    (let ((result (%gdal-set-raster-category-names
                   h-band
                   (string-list->pointerpointer category-names))))
      (unless (= result CE_NONE)
        (error "failed to set category names"))))

(export set-raster-category-names)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-minimum
  double "GDALGetRasterMinimum" (list '* '*) 20)

(define (get-raster-minimum h-band)
  "Return values of minimum value for this band (excluding no data pixels) and
a boolean, to indicate if the returned value is a tight minimum or not,
respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (let* ((bv-success (make-bytevector (sizeof int)))
         (minimum (%gdal-get-raster-minimum
                   h-band
                   (bytevector->pointer bv-success))))
    (values minimum (c-bool->boolean
                     (bytevector-sint-ref bv-success
                                          0
                                          (native-endianness)
                                          (sizeof int))))))

(export get-raster-minimum)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-maximum
  double "GDALGetRasterMaximum" (list '* '*) 20)

(define (get-raster-maximum h-band)
  "Return values of maximum value for this band (excluding no data pixels) and
a boolean, to indicate if the returned value is a tight maximum or not,
respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (let* ((bv-success (make-bytevector (sizeof int)))
         (maximum (%gdal-get-raster-maximum
                   h-band
                   (bytevector->pointer bv-success))))
    (values maximum (c-bool->boolean
                     (bytevector-sint-ref bv-success
                                          0
                                          (native-endianness)
                                          (sizeof int))))))

(export get-raster-maximum)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-statistics
  int "GDALGetRasterStatistics" (list '* int int '* '* '* '*) 20)

(define (get-raster-statistics h-band approx-ok force)
  "Fetch image statistics as multiple values of minimum, maximum, mean and
standard deviation, respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    approx-ok: a boolean value. If #t statistics may be computed based on
overviews or a subset of all tiles.
    force: a boolean value. if #f statistics will only be returned if it can
be done without rescanning the image."
  (let* ((bv-min (make-bytevector (sizeof double)))
         (bv-max (make-bytevector (sizeof double)))
         (bv-mean (make-bytevector (sizeof double)))
         (bv-std-dev (make-bytevector (sizeof double)))
         (result (%gdal-get-raster-statistics
                  h-band
                  (boolean->c-bool approx-ok)
                  (boolean->c-bool force)
                  (bytevector->pointer bv-min)
                  (bytevector->pointer bv-max)
                  (bytevector->pointer bv-mean)
                  (bytevector->pointer bv-std-dev))))
    (if (= result CE_NONE)
      (values (bytevector-ieee-double-ref bv-min 0 (native-endianness))
              (bytevector-ieee-double-ref bv-max 0 (native-endianness))
              (bytevector-ieee-double-ref bv-mean 0 (native-endianness))
              (bytevector-ieee-double-ref bv-std-dev 0 (native-endianness)))
      (error "failed to compute raster statistics"))))

(export get-raster-statistics)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-compute-raster-statistics
  int "GDALComputeRasterStatistics" (list '* int '* '* '* '* '* '*) 20)

(define* (compute-raster-statistics h-band approx-ok
                                 #:key (progress-callback '())
                                 (progress-data %null-pointer))
  "Compute image statistics as multiple values of minimum, maximum, mean and
standard deviation, respectively. Once computed, the statistics will generally
be \"set\" back on the raster band using \"set-render-statistics\".

Parameters:
    h-band: a handle representing GDALRasterBandH.
    approx-ok: a boolean value. If #t statistics may be computed based on
overviews or a subset of all tiles.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let* ((bv-min (make-bytevector (sizeof double)))
         (bv-max (make-bytevector (sizeof double)))
         (bv-mean (make-bytevector (sizeof double)))
         (bv-std-dev (make-bytevector (sizeof double)))
         (result (%gdal-compute-raster-statistics
                  h-band
                  (boolean->c-bool approx-ok)
                  (bytevector->pointer bv-min)
                  (bytevector->pointer bv-max)
                  (bytevector->pointer bv-mean)
                  (bytevector->pointer bv-std-dev)
                  (gdal-progress-func progress-callback)
                  progress-data)))
    (if (= result CE_NONE)
      (values (bytevector-ieee-double-ref bv-min 0 (native-endianness))
              (bytevector-ieee-double-ref bv-max 0 (native-endianness))
              (bytevector-ieee-double-ref bv-mean 0 (native-endianness))
              (bytevector-ieee-double-ref bv-std-dev 0 (native-endianness)))
      (error "failed to compute raster statistics"))))

(export compute-raster-statistics)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-statistics
  int "GDALSetRasterStatistics" (list '* double double double double) 20)

(define (set-raster-statistics h-band minimum maximum mean std-dev)
  "Set image statistics on band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    minimum: minimum pixel value.
    maximum: maximum pixel value.
    mean: mean (average) of all pixel values.
    std-dev: standard deviation of all pixel values."
  (let ((result (%gdal-set-raster-statistics
                 h-band
                 minimum
                 maximum
                 mean
                 std-dev)))
    (unless (= result CE_NONE)
      (error "failed to set raster statistics"))))

(export set-raster-statistics)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-raster-band-as-md-array
  '* "GDALRasterBandAsMDArray" (list '*) 31)

(define (raster-band-as-md-array h-band)
    "Return a view of this raster band as a 2D multidimensional GDALMDArray.
The returned pointer must be released with \"md-array-release\".

Parameters:
    h-band: a handle representing GDALRasterBandH."
    (%gdal-raster-band-as-md-array h-band))

(export raster-band-as-md-array)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-unit-type
  '* "GDALGetRasterUnitType" (list '*) 20)

(define (get-raster-unit-type h-band)
    "Return raster unit type as a string.

Parameters:
    h-band: a handle representing GDALRasterBandH."
    (pointer->string (%gdal-get-raster-unit-type h-band)))

(export get-raster-unit-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-unit-type
  int "GDALSetRasterUnitType" (list '* '*) 18)

(define (set-raster-unit-type h-band new-value)
    "Set unit type.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    new-value: the new unit type value."
    (unless (= CE_NONE (%gdal-set-raster-unit-type
                        h-band
                        (string->pointer new-value)))
      (error "failed to set raster unit type")))

(export set-raster-unit-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-offset
  double "GDALGetRasterOffset" (list '* '*) 20)

(define (get-raster-offset h-band)
  "Fetch the raster value offset.

  Parameters:
      h-band: a handle representing GDALRasterBandH."
  (let* ((bv-success (make-bytevector (sizeof int)))
         (offset (%gdal-get-raster-offset
                  h-band
                  (bytevector->pointer bv-success))))
    (if (c-bool->boolean (bytevector-sint-ref bv-success
                                              0
                                              (native-endianness)
                                              (sizeof int)))
      offset
      (error "failed to get raster offset"))))

(export get-raster-offset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-offset
  int "GDALSetRasterOffset" (list '* double) 20)

(define (set-raster-offset h-band new-offset)
  "Set scaling offset.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    new-offset: the new offset."
  (unless (= CE_NONE (%gdal-set-raster-offset
                      h-band
                      new-offset))
    (error "failed to set raster offset")))

(export set-raster-offset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-scale
  double "GDALGetRasterScale" (list '* '*) 20)

(define (get-raster-scale h-band)
  "Fetch the raster value scale.

  Parameters:
      h-band: a handle representing GDALRasterBandH."
  (let* ((bv-success (make-bytevector (sizeof int)))
         (scale (%gdal-get-raster-scale
                 h-band
                 (bytevector->pointer bv-success))))
    (if (c-bool->boolean (bytevector-sint-ref bv-success
                                              0
                                              (native-endianness)
                                              (sizeof int)))
      scale
      (error "failed to get raster scale"))))

(export get-raster-scale)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-raster-scale
  int "GDALSetRasterScale" (list '* double) 20)

(define (set-raster-scale h-band new-scale)
  "Set scaling ratio.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    new-scale: the new scale."
  (unless (= CE_NONE (%gdal-set-raster-scale
                      h-band
                      new-scale))
    (error "failed to set raster scale")))

(export set-raster-scale)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-compute-raster-min-max
  void "GDALComputeRasterMinMax" (list '* int '*) 20)

(define (compute-raster-min-max h-band approx-ok)
  "Compute the min/max values for a band and return as values with minimum
and maximum, respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    approx-ok: #t if an approximate (faster) answer is OK, otherwise #f."
  (let ((bv-minmax (make-bytevector (* 2 (sizeof double)))))
    (%gdal-compute-raster-min-max h-band
                                  (boolean->c-bool approx-ok)
                                  (bytevector->pointer bv-minmax))
    (values
     (bytevector-ieee-double-ref bv-minmax
                                 0
                                 (native-endianness))
     (bytevector-ieee-double-ref bv-minmax
                                 (sizeof double)
                                 (native-endianness)))))

(export compute-raster-min-max)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-flush-raster-cache
  int "GDALFlushRasterCache" (list '*) 20)

(define (flush-raster-cache h-band)
  "Flush raster data cache.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (unless (= CE_NONE (%gdal-flush-raster-cache h-band))
    (error "failed to flush raster cache")))

(export flush-raster-cache)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-histogram-ex
  int "GDALGetRasterHistogramEx" (list '* double double int '*
                                       int int '* '*) 20)

(define* (get-raster-histogram h-band minimum maximum n-buckets
                               histogram include-out-of-range approx-ok
                               #:key (progress-callback '())
                               (progress-data %null-pointer))
  "Compute raster histogram.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    minimum: the lower bound of the histogram.
    maximum: the upper bound of the histogram.
    n-buckets: the number of buckets in histogram.
    histogram: int64 buffer of bytevector into which the histogram totals are
placed.
    include-out-of-range: if #t values below the histogram range will mapped
into histogram[0], and values above will be mapped into histogram[n-buckets-1]
otherwise out of range values are discarded.
    approx-ok: #t if an approximate, or incomplete histogram OK.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let ((result (%gdal-get-raster-histogram-ex h-band
                                               minimum
                                               maximum
                                               n-buckets
                                               (bytevector->pointer histogram)
                                               (boolean->c-bool
                                                include-out-of-range)
                                               (boolean->c-bool approx-ok)
                                               (gdal-progress-func
                                                progress-callback)
                                               progress-data)))
    (when (= result CE_FAILURE)
      (error "failed to get raster histogram"))))

(export get-raster-histogram)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-default-histogram-ex
  int "GDALGetDefaultHistogramEx" (list '* '* '* '* '* int '* '*) 20)

(define* (get-default-histogram h-band is-force
                                #:key (progress-callback '())
                                (progress-data %null-pointer))
  "Fetch default raster histogram. Return values of the lower bound of the
histogram, the upper bound of the histogram, number of buckets and a int
bytevector of histogram, respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    is-force: #t to force the computation. If #f and no default histogram is
available, the method will return CE_WARNING.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let* ((bv-min (make-bytevector (sizeof double)))
         (bv-max (make-bytevector (sizeof double)))
         (bv-n-buckets (make-bytevector (sizeof int)))
         (bv-histogram (make-bytevector (sizeof '*)))
         (result (%gdal-get-default-histogram-ex
                  h-band
                  (bytevector->pointer bv-min)
                  (bytevector->pointer bv-max)
                  (bytevector->pointer bv-n-buckets)
                  (bytevector->pointer bv-histogram)
                  (boolean->c-bool
                   is-force)
                  (gdal-progress-func
                   progress-callback)
                  progress-data)))
    (if (= result CE_FAILURE)
      (error "failed to get default histogram")
      (let ((minimum (bytevector-ieee-double-ref
                      bv-min 0
                      (native-endianness)))
            (maximum (bytevector-ieee-double-ref
                      bv-max 0
                      (native-endianness)))
            (n-buckets (bytevector-sint-ref bv-n-buckets
                                            0
                                            (native-endianness)
                                            (sizeof int))))
        (values minimum
                maximum
                n-buckets
                (pointer->list (dereference-pointer
                                (bytevector->pointer bv-histogram))
                               n-buckets
                               int64))))))

(export get-default-histogram)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-default-histogram-ex
  int "GDALSetDefaultHistogramEx" (list '* double double int '*) 20)

(define (set-default-histogram h-band minimum maximum n-buckets histogram)
  "Set default histogram.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    minimum: the lower bound of the histogram.
    maximum: the upper bound of the histogram.
    n-buckets: the number of buckets in histogram.
    histogram: the int64 bytevector of the histogram."
  (let ((result (%gdal-set-default-histogram-ex
                 h-band
                 minimum
                 maximum
                 n-buckets
                 (bytevector->pointer histogram))))
    (unless (= result CE_NONE)
      (error "failed to set default histogram"))))

(export set-default-histogram)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-random-raster-sample
  int "GDALGetRandomRasterSample" (list '* int '*) 20)

(define (get-random-raster-sample h-band count)
  "Return a list of random pixels as floating point numbers on the band.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    count: number of samples."
  (let* ((bv-buffer (make-bytevector (* count (sizeof float))))
         (real-count (%gdal-get-random-raster-sample
                      h-band
                      count
                      (bytevector->pointer bv-buffer))))
    (pointer->list (bytevector->pointer bv-buffer) real-count float)))

(export get-random-raster-sample)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-raster-sample-overview-ex
  '* "GDALGetRasterSampleOverviewEx" (list '* int64) 20)

(define (get-raster-sample-overview h-band desired-samples)
  "Fetch best sampling overview.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    desired-samples: the returned band will have at least this many pixels."
  (%gdal-get-raster-sample-overview-ex h-band desired-samples))

(export get-raster-sample-overview)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-fill-raster
  int "GDALFillRaster" (list '* double double) 20)

(define* (fill-raster h-band real #:key (imaginary 0))
  "Fill this band with a constant value.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    real: real component of fill value.
    imaginary (optional): imaginary component of fill value, defaults to zero."
  (let ((result (%gdal-fill-raster h-band real imaginary)))
    (unless (= result CE_NONE)
      (error "failed to fill raster"))))

(export fill-raster)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-compute-band-stats
  int "GDALComputeBandStats" (list '* int '* '* '* '*) 20)

(define* (compute-band-stats h-band n-sample-step
                             #:key (progress-callback '())
                             (progress-data %null-pointer))
  "Compute image statistics as multiple values of mean and standard deviation,
respectively.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    n-sample-step: a number of sample steps.
    progress-callback (optional): a function to call to report progress.
    progress-data (optional): application data to pass to the progress
function."
  (let* ((bv-mean (make-bytevector (sizeof double)))
         (bv-std-dev (make-bytevector (sizeof double)))
         (result (%gdal-compute-band-stats
                  h-band
                  n-sample-step
                  (bytevector->pointer bv-mean)
                  (bytevector->pointer bv-std-dev)
                  (gdal-progress-func progress-callback)
                  progress-data)))
    (if (= result CE_NONE)
      (values (bytevector-ieee-double-ref bv-mean 0 (native-endianness))
              (bytevector-ieee-double-ref bv-std-dev 0 (native-endianness)))
      (error "failed to compute band statistics"))))

(export compute-band-stats)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-default-rat
  '* "GDALGetDefaultRAT" (list '*) 20)

(define (get-default-rat h-band)
  "Fetch default Raster Attribute Table. Returns #f if not set.

Parameters:
    h-band: a handle representing GDALRasterBandH."
  (let ((ptr (%gdal-get-default-rat h-band)))
    (if (null-pointer? ptr)
	     #f
     	ptr)))

(export get-default-rat)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-default-rat
  int "GDALSetDefaultRAT" (list '* '*) 20)

(define (set-default-rat h-band h-rat)
  "Set default Raster Attribute Table.

Parameters:
    h-band: a handle representing GDALRasterBandH.
    h-rat: a handle for raster attribute table to set."
  (let ((result (%gdal-set-default-rat h-band h-rat)))
    (unless (= result CE_NONE)
      (error "failed to set raster attribute table"))))

(export set-default-rat)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-create-raster-attribute-table
  '* "GDALCreateRasterAttributeTable" '() 20)

(define (make-raster-attribute-table)
  "Construct empty table."
  (%gdal-create-raster-attribute-table))

(export make-raster-attribute-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-destroy-raster-attribute-table
  void "GDALDestroyRasterAttributeTable" (list '*) 20)

(define (destroy-raster-attribute-table h-rat)
  "Destroys a RAT."
  (%gdal-destroy-raster-attribute-table h-rat))

(export destroy-raster-attribute-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-column-count
  int "GDALRATGetColumnCount" (list '*) 20)

(define (rat-get-column-count h-rat)
  "Fetch table column count.

Parameters:
  h-rat: handle representing raster attribute table."
  (%gdal-rat-get-column-count h-rat))

(export rat-get-column-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-name-of-col
  '* "GDALRATGetNameOfCol" (list '* int) 20)

(define (get-rat-name-of-column h-rat i-col)
    "Fetch name of indicated column.

Parameters:
    h-rat: handle representing raster attribute table.
    i-col: column index."
  (let ((ptr (%gdal-rat-get-name-of-col h-rat i-col)))
    (if (null-pointer? ptr)
	     (error "failed to get name of column")
     	(pointer->string ptr))))

(export get-rat-name-of-column)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-usage-of-col
  int "GDALRATGetUsageOfCol" (list '* int) 20)

(define (rat-get-usage-of-column h-rat i-col)
  "Fetch column usage value. See GFU_* enums for possible values.

Parameters:
  h-rat: handle representing raster attribute table.
  i-col: column index."
  (%gdal-rat-get-usage-of-col h-rat i-col))

(export rat-get-usage-of-column)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-type-of-col
  int "GDALRATGetTypeOfCol" (list '* int) 20)

(define (rat-get-type-of-column h-rat i-col)
  "Fetch column type. See GFT_* enums for possible values.

Parameters:
  h-rat: handle representing raster attribute table.
  i-col: column index."
  (%gdal-rat-get-usage-of-col h-rat i-col))

(export rat-get-type-of-column)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-col-of-usage
  int "GDALRATGetColOfUsage" (list '* int) 20)

(define (rat-get-col-of-usage h-rat usage)
  "Fetch column index for given usage.

Parameters:
  h-rat: handle representing raster attribute table.
  usage: field usage. see GFU_* enums for possible values"
  (%gdal-rat-get-col-of-usage h-rat usage))

(export rat-get-col-of-usage)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-row-count
  int "GDALRATGetRowCount" (list '*) 20)

(define (rat-get-row-count h-rat)
  "Fetch table row count.

Parameters:
  h-rat: handle representing raster attribute table."
  (%gdal-rat-get-row-count h-rat))

(export rat-get-row-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-value-as-string
  '* "GDALRATGetValueAsString" (list '* int int) 20)

(define (rat-get-value-as-string h-rat i-row i-field)
    "Fetch field value as a string.

Parameters:
    h-rat: handle representing raster attribute table.
    i-row: row index.
    i-field: column index"
  (let ((ptr (%gdal-rat-get-value-as-string h-rat i-row i-field)))
    (if (null-pointer? ptr)
	     (error "failed to get value as string")
     	(pointer->string ptr))))

(export rat-get-value-as-string)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-value-as-int
  int "GDALRATGetValueAsInt" (list '* int int) 20)

(define (rat-get-value-as-int h-rat i-row i-field)
    "Fetch field value as an integer.

Parameters:
    h-rat: handle representing raster attribute table.
    i-row: row index.
    i-field: column index."
  (%gdal-rat-get-value-as-int h-rat i-row i-field))

(export rat-get-value-as-int)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-value-as-double
  double "GDALRATGetValueAsDouble" (list '* int int) 20)

(define (rat-get-value-as-double h-rat i-row i-field)
    "Fetch field value as a double.

Parameters:
    h-rat: handle representing raster attribute table.
    i-row: row index.
    i-field: column index."
  (%gdal-rat-get-value-as-double h-rat i-row i-field))

(export rat-get-value-as-double)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-set-value-as-string
  void "GDALRATSetValueAsString" (list '* int int '*) 20)

(define (rat-set-value-as-string h-rat i-row i-field value)
    "Set field value from string.

Parameters:
    h-rat: handle representing raster attribute table.
    i-row: row index.
    i-field: column index.
    value: string value to set."
  (%gdal-rat-set-value-as-string h-rat i-row i-field (string->pointer value)))

(export rat-set-value-as-string)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-set-value-as-int
  void "GDALRATSetValueAsInt" (list '* int int int) 20)

(define (rat-set-value-as-int h-rat i-row i-field value)
    "Set field value from integer.

Parameters:
    h-rat: handle representing raster attribute table.
    i-row: row index.
    i-field: column index.
    value: integer value to set."
  (%gdal-rat-set-value-as-int h-rat i-row i-field value))

(export rat-set-value-as-int)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-set-value-as-double
  void "GDALRATSetValueAsDouble" (list '* int int double) 20)

(define (rat-set-value-as-double h-rat i-row i-field value)
    "Set field value from double.

Parameters:
    h-rat: handle representing raster attribute table.
    i-row: row index.
    i-field: column index.
    value: double value to set."
  (%gdal-rat-set-value-as-double h-rat i-row i-field value))

(export rat-set-value-as-double)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-changes-are-written-to-file
  int "GDALRATChangesAreWrittenToFile" (list '*) 20)

(define (rat-changes-are-written-to-file h-rat)
    "Determine whether changes made to this RAT are reflected directly
in the dataset.

Parameters:
    h-rat: handle representing raster attribute table."
  (c-bool->boolean (%gdal-rat-changes-are-written-to-file h-rat)))

(export rat-changes-are-written-to-file)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-values-io-as-double
  int "GDALRATValuesIOAsDouble" (list '* int int int int '*) 20)

(define (rat-read-values-as-double h-rat i-field i-start-row i-length)
  "Read a block of doubles from the Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table.
    i-field: column index.
    i-start-row: row index to start reading (zero based).
    i-length: number of rows to read."
  (let* ((bv (make-bytevector (* i-length (sizeof double))))
         (result (%gdal-rat-values-io-as-double h-rat GF_READ
                                                i-field i-start-row
                                                i-length
                                                (bytevector->pointer bv))))
    (if (= result CE_FAILURE)
      (error "failed to read values as doubles")
      (pointer->list (bytevector->pointer bv) i-length double))))

(export rat-read-values-as-double)

(define (rat-write-values-as-double h-rat i-field i-start-row lst)
  "Write a block of doubles to the Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table.
    i-field: column index.
    i-start-row: row index to start writing (zero based).
    lst: list of doubles to write."
  (let ((result (%gdal-rat-values-io-as-double
                  h-rat GF_WRITE
                  i-field i-start-row
                  (length lst)
                  (list->pointer lst double))))
    (if (= result CE_FAILURE)
      (error "failed to write values as doubles"))))

(export rat-write-values-as-double)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-values-io-as-integer
  int "GDALRATValuesIOAsInteger" (list '* int int int int '*) 20)

(define (rat-read-values-as-integer h-rat i-field i-start-row i-length)
  "Read a block of integers from the Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table.
    i-field: column index.
    i-start-row: row index to start reading (zero based).
    i-length: number of rows to read."
  (let* ((bv (make-bytevector (* i-length (sizeof int))))
         (result (%gdal-rat-values-io-as-integer h-rat GF_READ
                                                 i-field i-start-row
                                                 i-length
                                                 (bytevector->pointer bv))))
    (if (= result CE_FAILURE)
      (error "failed to read values as integers")
      (bytevector->sint-list bv (native-endianness) i-length))))

(export rat-read-values-as-integer)

(define (rat-write-values-as-integer h-rat i-field i-start-row lst)
  "Write a block of integers to the Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table.
    i-field: column index.
    i-start-row: row index to start writing (zero based).
    lst: list of integers to write."
  (let ((result (%gdal-rat-values-io-as-integer
                  h-rat GF_WRITE
                  i-field i-start-row
                  (length lst)
                  (sint-list->bytevector lst (native-endianness)
                                         (length lst)))))
    (if (= result CE_FAILURE)
      (error "failed to write values as integers"))))

(export rat-write-values-as-integer)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-values-io-as-string
  int "GDALRATValuesIOAsString" (list '* int int int int '*) 20)

(define (rat-read-values-as-string h-rat i-field i-start-row i-length)
  "Read a block of strings from the Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table.
    i-field: column index.
    i-start-row: row index to start reading (zero based).
    i-length: number of rows to read."
  (let* ((bv (make-bytevector (* i-length (sizeof '*))))
         (result (%gdal-rat-values-io-as-string h-rat GF_READ
                                                i-field i-start-row
                                                i-length
                                                (bytevector->pointer bv))))
    (if (= result CE_FAILURE)
      (error "failed to read values as strings")
      (pointerpointer->string-list (bytevector->pointer bv)))))

(export rat-read-values-as-string)

(define (rat-write-values-as-string h-rat i-field i-start-row lst)
  "Write a block of strings to the Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table.
    i-field: column index.
    i-start-row: row index to start writing (zero based).
    lst: list of strings to write."
  (let ((result (%gdal-rat-values-io-as-string
                  h-rat GF_WRITE
                  i-field i-start-row
                  (length lst)
                  (string-list->pointerpointer lst))))
    (if (= result CE_FAILURE)
      (error "failed to write values as strings"))))

(export rat-write-values-as-string)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-set-row-count
  void "GDALRATSetRowCount" (list '* int) 20)

(define (rat-set-row-count h-rat new-count)
    "Set row count.

Parameters:
    h-rat: handle representing raster attribute table.
    new-count: the new number of rows."
  (%gdal-rat-set-row-count h-rat new-count))

(export rat-set-row-count)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-create-column
  int "GDALRATCreateColumn" (list '* '* int int) 20)

(define (rat-create-column h-rat field-name field-type field-usage)
  "Create new column. If the table already has rows, all row values for the
new column will be initialized to the default value (\"\", or zero). The new
column is always created as the last column, column index will be
\"(- (get-column-count) 1)\" after rat-create-column has completed successfully.

Parameters:
    h-rat: handle representing raster attribute table.
    field-name: the name of the field to create.
    field-type: the field type. see GFT_* enums for possible values.
    field-usage: the field usage. see GFU_* enums for possible values."
  (let ((result (%gdal-rat-create-column h-rat (string->pointer field-name)
                                         field-type
                                         field-usage)))
    (if (= result CE_FAILURE)
      (error "failed to create column"))))

(export rat-create-column)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-set-linear-binning
  int "GDALRATSetLinearBinning" (list '* double double) 20)

(define (rat-set-linear-binning h-rat row-min bin-size)
  "Set linear binning information.

For RATs with equal sized categories (in pixel value space) that are evenly
spaced, this method may be used to associate the linear binning information
with the table.

Parameters:
    h-rat: handle representing raster attribute table.
    row-min: the lower bound (pixel value) of the first category.
    bin-size:the width of each category (in pixel value units)."
  (let ((result (%gdal-rat-set-linear-binning h-rat row-min bin-size)))
    (if (= result CE_FAILURE)
      (error "failed to set linear binning information"))))

(export rat-set-linear-binning)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-linear-binning
  int "GDALRATGetLinearBinning" (list '* '* '*) 20)

(define (rat-get-linear-binning h-rat)
  "Get linear binning information. Returns values of (#t, row-min, bin-size)
if linear binning information exists or (values #f 0.0 0.0) if there is none.
row-min is the lower bound (pixel value) of the first category, and bin-size
is the width of each category (in pixel value units).

Parameters:
    h-rat: handle representing raster attribute table."
  (let* ((row-min ((make-bytevector (sizeof double))))
         (bin-size ((make-bytevector (sizeof double))))
         (result (%gdal-rat-get-linear-binning h-rat
                                               (bytevector->pointer row-min)
                                               (bytevector->pointer bin-size))))
    (if (c-bool->boolean result)
      (values #t
              (bytevector-ieee-double-ref row-min 0 (native-endianness))
              (bytevector-ieee-double-ref bin-size 0 (native-endianness)))
      (values #f 0.0 0.0))))

(export rat-get-linear-binning)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-set-table-type
  int "GDALRATSetTableType" (list '* int) 24)

(define (rat-set-table-type h-rat table-type)
  "Set whether the RAT is thematic or athematic (continuous).

Parameters:
    h-rat: handle for raster attribute table to set.
    table-type: table type to set."
  (let ((result (%gdal-rat-set-table-type h-rat table-type)))
    (unless (= result CE_NONE)
      (error "failed to set table type"))))

(export rat-set-table-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-table-type
  int "GDALRATGetTableType" (list '*) 24)

(define (rat-get-table-type h-rat)
    "Indicates whether the RAT is thematic or athematic (continuous).

Parameters:
    h-rat: handle representing raster attribute table."
  (%gdal-rat-get-table-type h-rat))

(export rat-get-table-type)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-initialize-from-color-table
  int "GDALRATInitializeFromColorTable" (list '* '*) 20)

(define (make-rat-from-color-table h-rat h-table)
  "Initialize from color table.

This method will setup a whole raster attribute table based on the contents of
the passed color table. The Value (GFU_MIN_MAX), Red (GFU_RED),
Green (GFU_GREEN), Blue (GFU_BLUE), and Alpha (GFU_ALPHA) fields are created,
and a row is set for each entry in the color table.

The raster attribute table must be empty before calling
make-rat-from-color-table

The Value fields are set based on the implicit assumption with color tables
that entry 0 applies to pixel value 0, 1 to 1, etc.

Parameters:
    h-rat: handle for raster attribute table to set.
    h-table: color table to copy from."
  (let ((result (%gdal-rat-initialize-from-color-table h-rat h-table)))
    (unless (= result CE_NONE)
      (error "failed to make rat from color table"))))

(export make-rat-from-color-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-translate-to-color-table
  '* "GDALRATTranslateToColorTable" (list '* int) 20)

(define (rat-translate-to-color-table h-rat entry-count)
  "Translate to a color table.

Parameters:
    h-rat: handle representing raster attribute table.
    entry-count: the number of entries to produce (0 to entry-count - 1)."
  (let ((result (%gdal-rat-translate-to-color-table h-rat entry-count)))
    (if (null-pointer? result)
      (error "failed to translate to color table")
      result)))

(export rat-translate-to-color-table)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-clone
  '* "GDALRATClone" (list '*) 20)

(define (clone-rat h-rat)
    "Copy Raster Attribute Table.

Parameters:
    h-rat: handle representing raster attribute table."
  (%gdal-rat-clone h-rat))

(export clone-rat)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-get-row-of-value
  int "GDALRATGetRowOfValue" (list '* double) 20)

(define (rat-get-row-of-value h-rat value)
    "Get row for pixel value.

Given a raw pixel value, the raster attribute table is scanned to determine
which row in the table applies to the pixel value. The row index is returned.

Parameters:
    h-rat: handle representing raster attribute table.
    value: the pixel value."
  (%gdal-rat-get-row-of-value h-rat value))

(export rat-get-row-of-value)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-rat-remove-statistics
  void "GDALRATRemoveStatistics" (list '*) 24)

(define (rat-remove-statistics h-rat)
    "Remove statistics from the RAT.

Parameters:
    h-rat: handle representing raster attribute table."
  (%gdal-rat-remove-statistics h-rat))

(export rat-remove-statistics)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-cache-max
  void "GDALSetCacheMax" (list int) 20)

(define (set-cache-max new-size-in-bytes)
    "Set maximum cache memory.

This function sets the maximum amount of memory that GDAL is permitted to use
for GDALRasterBlock caching. The unit of the value is bytes.

The maximum value is 2GB, due to the use of a signed 32 bit integer.

Parameters:
    new-size-in-bytes: the maximum number of bytes for caching."
  (%gdal-set-cache-max new-size-in-bytes))

(export set-cache-max)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-set-cache-max-64
  void "GDALSetCacheMax64" (list int64) 18)

(define (set-cache-max-64 new-size-in-bytes)
    "Set maximum cache memory.

This function sets the maximum amount of memory that GDAL is permitted to use
for GDALRasterBlock caching. The unit of the value is bytes.

Note: On 32 bit platforms, the maximum amount of memory that can be addressed
by a process might be 2 GB or 3 GB, depending on the operating system
capabilities. This function will not make any attempt to check the consistency
of the passed value with the effective capabilities of the OS.

Parameters:
    new-size-in-bytes: the maximum number of bytes for caching."
  (%gdal-set-cache-max-64 new-size-in-bytes))

(export set-cache-max-64)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-cache-max
  int "GDALGetCacheMax" '() 20)

(define (get-cache-max)
    "Get maximum cache memory.

Gets the maximum amount of memory available to the GDALRasterBlock caching
system for caching GDAL read/write imagery.

The first type this function is called, it will read the GDAL_CACHEMAX
configuration option to initialize the maximum cache memory. Starting with
GDAL 2.1, the value can be expressed as x% of the usable physical RAM
(which may potentially be used by other processes). Otherwise it is expected to
be a value in MB.

This function cannot return a value higher than 2 GB. Use get-cache-max-64 to
get a non-truncated value."
  (%gdal-get-cache-max))

(export get-cache-max)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-cache-max-64
  int64 "GDALGetCacheMax64" '() 18)

(define (get-cache-max-64)
    "Get maximum cache memory.

Gets the maximum amount of memory available to the GDALRasterBlock caching
system for caching GDAL read/write imagery.

The first type this function is called, it will read the GDAL_CACHEMAX
configuration option to initialize the maximum cache memory. Starting with
GDAL 2.1, the value can be expressed as x% of the usable physical RAM
(which may potentially be used by other processes). Otherwise it is expected
to be a value in MB."
  (%gdal-get-cache-max-64))

(export get-cache-max-64)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-cache-used
  int "GDALGetCacheUsed" '() 20)

(define (get-cache-used)
    "Get cache memory used."
  (%gdal-get-cache-used))

(export get-cache-used)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-get-cache-used-64
  int64 "GDALGetCacheUsed64" '() 18)

(define (get-cache-used-64)
    "Get cache memory used."
  (%gdal-get-cache-used-64))

(export get-cache-used-64)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-flush-cache-block
  int "GDALFlushCacheBlock" '() 20)

(define (flush-cache-block)
  "Try to flush one cached raster block.

This function will search the first unlocked raster block and will flush it to
release the associated memory.

Returns #t if one block was flushed, #f if there are no cached blocks or if
they are currently locked."
  (c-bool->boolean (%gdal-flush-cache-block)))

(export flush-cache-block)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-create-multi-multidimensional
  '* "GDALCreateMultiDimensional" (list '* '* '* '*) 31)

(define* (make-multidimensional-dataset h-driver file-name #:key
                                        (root-group-options '())
                                        (options '()))
  "Create a new multidimensional dataset with this driver.

Only drivers that advertise the GDAL_DCAP_MULTIDIM_RASTER capability and
implement the pfnCreateMultiDimensional method might return a non nullptr
GDALDataset.

Parameters:
    file-name: the name of the dataset to create.
    root-group-options (optional): driver specific options regarding the
creation of the root group.
    options (optional): driver specific options regarding the creation of the
dataset."
  (let ((ptr (%gdal-create-multi-multidimensional
              h-driver
              (string->pointer file-name)
              (string-list->pointerpointer root-group-options)
              (string-list->pointerpointer options))))
    (if (null-pointer? ptr)
	     (error "failed to create multidimensional dataset")
     	ptr)))

(export make-multidimensional-dataset)

;;------------------------------------------------------------------------------

(define-gdal-foreign %gdal-group-get-vector-layer-names
  '* "GDALGroupGetVectorLayerNames" (list '* '*) 34)

(define* (group-get-vector-layer-names h-group #:key
                                       (options '()))
  "Return the list of layer names contained in this group.

Parameters:
    h-group: handle of the group.
    options (optional): driver specific options determining how layers should
be retrieved."
  (let ((ptr (%gdal-group-get-vector-layer-names
              h-group
              (string-list->pointerpointer options))))
    (pointerpointer->string-list ptr)))

(export group-get-vector-layer-names)

;;------------------------------------------------------------------------------
