;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Kamil Toman"}
  uncomplicate.neanderthal.internal.host.serialization
  (:require [uncomplicate.commons.core :refer [info let-release release with-release]]
            [uncomplicate.commons.utils :refer [direct-buffer dragan-says-ex]]
            [uncomplicate.neanderthal.core :refer [transfer! copy!]]
            [uncomplicate.neanderthal.native :as native :refer [native-double native-float native-long native-int]]
            [uncomplicate.neanderthal.internal.api :as api]
            [uncomplicate.neanderthal.internal
             [api :as api]
             [common :refer [real-accessor]]
             [navigation :as navigation]]
            [uncomplicate.neanderthal.internal.host
             [buffer-block :refer [real-block-vector integer-block-vector
                                   real-ge-matrix real-uplo-matrix real-banded-matrix
                                   real-tb-matrix real-sb-matrix real-packed-matrix
                                   real-diagonal-matrix]]]
            [taoensso.nippy :as nippy])
  (:import (java.nio ByteBuffer)
           (uncomplicate.neanderthal.internal.api LayoutNavigator Region DataAccessor RealVector
                                                  IntegerVector RealNativeMatrix DenseStorage VectorSpace Block RealNativeVector IntegerNativeVector)
           (java.io OutputStream InputStream DataOutput DataInput)
           (java.nio.channels Channels)
           (uncomplicate.neanderthal.internal.host.buffer_block FloatBufferAccessor DoubleBufferAccessor
                                                                LongBufferAccessor IntBufferAccessor RealBlockVector IntegerBlockVector RealGEMatrix RealUploMatrix RealBandedMatrix RealPackedMatrix RealDiagonalMatrix)))

(def ^{:dynamic true
       :doc "Dynamically bound factory that is used in Neanderthal vector and matrix constructors."}
  *neanderthal-factory* nil)

(defn- write-buffer [^OutputStream out ^ByteBuffer native-buffer ^long offset]
  (let [buf (.slice native-buffer)]                         ; BIG ENDIAN buffer view with its own position
    (when (pos? offset)
      (.position buf (int offset)))
    (.write (Channels/newChannel out) buf)))

(defn- read-buffer [^InputStream in ^ByteBuffer native-buffer ^long offset]
  (let [buf (.slice native-buffer)]                         ; BIG ENDIAN buffer view with its own position
    (when (pos? offset)
      (.position buf (int offset)))
    (.read (Channels/newChannel in) buf)))

(defn- entry-type-class ^Class [x]
  (.entryType (api/data-accessor x)))

(defn- engine-by-matrix-type [factory matrix-type]
  (case matrix-type
    :sy (api/sy-engine factory)
    :tr (api/tr-engine factory)
    :gb (api/gb-engine factory)
    :sb (api/sb-engine factory)
    :tb (api/tb-engine factory)
    :sp (api/sp-engine factory)
    :tp (api/tp-engine factory)
    :gt (api/gt-engine factory)
    :gd (api/gd-engine factory)
    :dt (api/dt-engine factory)
    :st (api/st-engine factory)
    (dragan-says-ex (format "%s is not a valid matrix type. Please send me a bug report." matrix-type)
                    {:type matrix-type})))

(defn write-vctr-data! [^OutputStream out ^Block source-vctr]
  (when (pos? (.dim ^VectorSpace source-vctr))
    (let [offset (.offset source-vctr)
          entry-width (.entryWidth (api/data-accessor source-vctr))
          buf (.buffer source-vctr)]
      (write-buffer out buf (* entry-width offset)))))

(defn read-vctr-data! [^InputStream in ^Block destination-vctr]
  (when (pos? (.dim ^VectorSpace destination-vctr))
    (let [offset (.offset destination-vctr)
          entry-width (.entryWidth (api/data-accessor destination-vctr))
          buf (.buffer destination-vctr)]
      (read-buffer in buf (* entry-width offset)))))

(defn entry-type-kw [x]
  (let [class (entry-type-class x)]
    (keyword (.. class getSimpleName toLowerCase))))

(defn freeze-through-native! [data-output x]
  (let [native-x (api/native x)]
    (try
      (nippy/freeze-to-out! data-output native-x)
      (finally
        (when-not (identical? native-x x)
          (release native-x))))))

(defn create-through-native [native-x factory]
  (if (or (nil? factory)
          (not (satisfies? api/Container native-x))
          (identical? (api/factory native-x) factory))
    native-x
    (if (api/compatible? (api/factory native-x) (api/native-factory factory))
      (let [x (api/raw native-x factory)]
        (try
          (transfer! native-x x)
          (finally
            (release native-x))))
      (dragan-says-ex "You must provide a compatible factory for this data input."))))

(defn thaw-through-native! [data-input factory]
  (let [native-x (nippy/thaw-from-in! data-input)]
    (create-through-native native-x factory)))

(nippy/extend-freeze java.nio.DirectByteBuffer
                     :uncomplicate.neanderthal/direct-buffer
                     [buf data-output]
                     (.writeInt data-output (.capacity buf))
                     (write-buffer data-output buf 0))

(nippy/extend-thaw :uncomplicate.neanderthal/direct-buffer
                   [data-input]
                   (let [capacity (.readInt data-input)
                         buf (direct-buffer capacity)]
                     (read-buffer data-input buf 0)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BandRegion
                     :uncomplicate.neanderthal.internal/BandRegion
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:m header))
                       (.writeLong data-output (:n header))
                       (.writeLong data-output (:kl header))
                       (.writeLong data-output (:ku header))
                       (.writeInt data-output (.uplo ^Region x))
                       (.writeInt data-output (.diag ^Region x))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BandRegion
                   [data-input]
                   (let [m (.readLong data-input)
                         n (.readLong data-input)
                         kl (.readLong data-input)
                         ku (.readLong data-input)
                         uplo (.readInt data-input)
                         diag (.readInt data-input)]
                     (navigation/->BandRegion m n kl ku uplo diag)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.GERegion
                     :uncomplicate.neanderthal.internal/GERegion
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:m header))
                       (.writeLong data-output (:n header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/GERegion
                   [data-input]
                   (let [m (.readLong data-input)
                         n (.readLong data-input)]
                     (navigation/->GERegion m n)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.StripeFullStorage
                     :uncomplicate.neanderthal.internal/StripeFullStorage
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:sd header))
                       (.writeLong data-output (:fd header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/StripeFullStorage
                   [data-input]
                   (let [sd (.readLong data-input)
                         fd (.readLong data-input)
                         ld sd]
                     (navigation/->StripeFullStorage sd fd ld)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BandStorage
                     :uncomplicate.neanderthal.internal/BandStorage
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:height header))
                       (.writeLong data-output (:width header))
                       (.writeLong data-output (:ld header))
                       (.writeLong data-output (:kl header))
                       (.writeLong data-output (:ku header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BandStorage
                   [data-input]
                   (let [height (.readLong data-input)
                         width (.readLong data-input)
                         ld (.readLong data-input)
                         kl (.readLong data-input)
                         ku (.readLong data-input)]
                     (navigation/->BandStorage height width ld kl ku)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.TopPackedStorage
                     :uncomplicate.neanderthal.internal/TopPackedStorage
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:n header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/TopPackedStorage
                   [data-input]
                   (let [n (.readLong data-input)]
                     (navigation/->TopPackedStorage n)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BottomPackedStorage
                     :uncomplicate.neanderthal.internal/BottomPackedStorage
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:n header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BottomPackedStorage
                   [data-input]
                   (let [n (.readLong data-input)]
                     (navigation/->BottomPackedStorage n)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.TridiagonalStorage
                     :uncomplicate.neanderthal.internal/TridiagonalStorage
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:n header))
                       (.writeLong data-output (:capacity header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/TridiagonalStorage
                   [data-input]
                   (let [n (.readLong data-input)
                         cap (.readLong data-input)]
                     (navigation/->TridiagonalStorage n cap)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BidiagonalStorage
                     :uncomplicate.neanderthal.internal/BidiagonalStorage
                     [x data-output]
                     (let [header (info x)]
                       (.writeLong data-output (:n header))
                       (.writeLong data-output (:capacity header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BidiagonalStorage
                   [data-input]
                   (let [n (.readLong data-input)
                         cap (.readLong data-input)]
                     (navigation/->BidiagonalStorage n cap)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealBlockVector
                     :uncomplicate.neanderthal/RealBlockVector
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.dim ^RealBlockVector x))
                     (write-vctr-data! data-output x)
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/RealBlockVector
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         dim (.readLong data-input)
                         fac (native/factory-by-type entry-type)
                         accessor (api/data-accessor fac)
                         buf (direct-buffer (* (.entryWidth ^DataAccessor accessor) dim))
                         real-vctr (real-block-vector fac true buf dim 0 1)]
                     (read-vctr-data! data-input real-vctr)
                     (create-through-native real-vctr *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.IntegerBlockVector
                     :uncomplicate.neanderthal/IntegerBlockVector
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.dim ^IntegerBlockVector x))
                     (write-vctr-data! data-output x)
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/IntegerBlockVector
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         dim (.readLong data-input)
                         fac (native/factory-by-type entry-type)
                         accessor (api/data-accessor fac)
                         buf (direct-buffer (* (.entryWidth ^DataAccessor accessor) dim))
                         int-vctr (integer-block-vector fac true buf dim 0 1)]
                     (read-vctr-data! data-input int-vctr)
                     (create-through-native int-vctr *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealGEMatrix
                     :uncomplicate.neanderthal/RealGEMatrix
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.mrows ^RealGEMatrix x))
                     (.writeLong data-output (.ncols ^RealGEMatrix x))
                     (.writeBoolean data-output (.isColumnMajor ^LayoutNavigator (api/navigator x)))
                     (nippy/freeze-to-out! data-output (api/storage x))
                     (nippy/freeze-to-out! data-output (api/region x))
                     (when (pos? (.dim ^RealGEMatrix x))
                       (if (.isContiguous ^RealGEMatrix x)
                         (write-vctr-data! data-output (api/view-vctr x))
                         (dragan-says-ex "You cannot save a non-contiguous matrix. Do the (core/copy matrix) first."
                                         (info x))))
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/RealGEMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         m (.readLong data-input)
                         n (.readLong data-input)
                         column? (.readBoolean data-input)
                         storage ^DenseStorage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         accessor ^DataAccessor (api/data-accessor fac)
                         buf (direct-buffer (* (.entryWidth accessor) (.capacity storage)))
                         gem (real-ge-matrix fac true buf m n 0 (navigation/layout-navigator column?) storage region)]
                     (when (pos? (.dim ^RealGEMatrix gem))
                       (read-vctr-data! data-input (api/view-vctr gem)))
                     (create-through-native gem *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealUploMatrix
                     :uncomplicate.neanderthal/RealUploMatrix
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.ncols ^RealUploMatrix x))
                     (.writeBoolean data-output (.isColumnMajor ^LayoutNavigator (api/navigator x)))
                     (nippy/freeze-to-out! data-output (api/storage x))
                     (nippy/freeze-to-out! data-output (api/region x))
                     (nippy/freeze-to-out! data-output (.matrixType ^RealUploMatrix x))
                     (when (pos? (.dim ^RealUploMatrix x))
                       (let [view (api/view-vctr x)]
                         (if (.isContiguous ^RealNativeVector view)
                           (write-vctr-data! data-output view)
                           (dragan-says-ex "You cannot save a matrix with gaps. Do the (core/copy matrix) first."
                                           (info x)))))
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/RealUploMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         n (.readLong data-input)
                         column? (.readBoolean data-input)
                         storage ^DenseStorage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         accessor ^DataAccessor (api/data-accessor fac)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))
                         buf (direct-buffer (* (.entryWidth accessor) (.capacity storage)))
                         uplom (real-uplo-matrix fac true buf n 0 (navigation/layout-navigator column?)
                                                 storage region matrix-type default engine)]
                     (when (pos? (.dim ^RealUploMatrix uplom))
                       (read-vctr-data! data-input (api/view-vctr uplom)))
                     (create-through-native uplom *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealBandedMatrix
                     :uncomplicate.neanderthal/RealBandedMatrix
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.mrows ^RealBandedMatrix x))
                     (.writeLong data-output (.ncols ^RealBandedMatrix x))
                     (.writeBoolean data-output (.isColumnMajor ^LayoutNavigator (api/navigator x)))
                     (nippy/freeze-to-out! data-output (api/storage x))
                     (nippy/freeze-to-out! data-output (api/region x))
                     (nippy/freeze-to-out! data-output (.matrixType ^RealBandedMatrix x))
                     (when (pos? (.dim ^RealBandedMatrix x))
                       (write-vctr-data! data-output (api/view-vctr x)))
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/RealBandedMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         m (.readLong data-input)
                         n (.readLong data-input)
                         column? (.readBoolean data-input)
                         storage ^DenseStorage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         accessor ^DataAccessor (api/data-accessor fac)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))
                         buf (direct-buffer (* (.entryWidth accessor) (.capacity storage)))
                         bandedm (real-banded-matrix fac true buf m n 0 (navigation/layout-navigator column?)
                                                     storage region matrix-type default engine)]
                     (when (pos? (.dim ^RealBandedMatrix bandedm))
                       (read-vctr-data! data-input (api/view-vctr bandedm)))
                     (create-through-native bandedm *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealPackedMatrix
                     :uncomplicate.neanderthal/RealPackedMatrix
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.ncols ^RealPackedMatrix x))
                     (.writeBoolean data-output (.isColumnMajor ^LayoutNavigator (api/navigator x)))
                     (nippy/freeze-to-out! data-output (api/storage x))
                     (nippy/freeze-to-out! data-output (api/region x))
                     (nippy/freeze-to-out! data-output (.matrixType ^RealPackedMatrix x))
                     (when (pos? (.dim ^RealPackedMatrix x))
                       (write-vctr-data! data-output (api/view-vctr x)))
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/RealPackedMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         n (.readLong data-input)
                         column? (.readBoolean data-input)
                         storage ^DenseStorage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         accessor ^DataAccessor (api/data-accessor fac)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))
                         buf (direct-buffer (* (.entryWidth accessor) (.capacity storage)))
                         packedm (real-packed-matrix fac true buf n 0 (navigation/layout-navigator column?)
                                                     storage region matrix-type default engine)]
                     (when (pos? (.dim ^RealPackedMatrix packedm))
                       (read-vctr-data! data-input (api/view-vctr packedm)))
                     (create-through-native packedm *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealDiagonalMatrix
                     :uncomplicate.neanderthal/RealDiagonalMatrix
                     [x data-output]
                     (nippy/freeze-to-out! data-output (entry-type-kw x))
                     (.writeLong data-output (.ncols ^RealDiagonalMatrix x))
                     (.writeBoolean data-output (.isColumnMajor ^LayoutNavigator (api/navigator x)))
                     (nippy/freeze-to-out! data-output (api/storage x))
                     (nippy/freeze-to-out! data-output (api/region x))
                     (nippy/freeze-to-out! data-output (.matrixType ^RealDiagonalMatrix x))
                     (when (pos? (.dim ^RealDiagonalMatrix x))
                       (write-vctr-data! data-output (api/view-vctr x)))
                     nil)

(nippy/extend-thaw :uncomplicate.neanderthal/RealDiagonalMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         n (.readLong data-input)
                         column? (.readBoolean data-input)
                         storage ^DenseStorage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         accessor ^DataAccessor (api/data-accessor fac)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))
                         buf (direct-buffer (* (.entryWidth accessor) (.capacity storage)))
                         diagm (real-diagonal-matrix fac true buf n 0 (navigation/layout-navigator column?)
                                                     storage region matrix-type default engine)]
                     (when (pos? (.dim ^RealDiagonalMatrix diagm))
                       (read-vctr-data! data-input (api/view-vctr diagm)))
                     (create-through-native diagm *neanderthal-factory*)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.api.CUVector
                     :uncomplicate.neanderthal/CUVector
                     [x data-output]
                     (freeze-through-native! data-output x))

(nippy/extend-thaw :uncomplicate.neanderthal/CUVector
                   [data-input]
                   (thaw-through-native! data-input *neanderthal-factory*))

(nippy/extend-freeze uncomplicate.neanderthal.internal.api.CUMatrix
                     :uncomplicate.neanderthal/CUMatrix
                     [x data-output]
                     (freeze-through-native! data-output x))

(nippy/extend-thaw :uncomplicate.neanderthal/CUMatrix
                   [data-input]
                   (thaw-through-native! data-input *neanderthal-factory*))

(nippy/extend-freeze uncomplicate.neanderthal.internal.api.CLVector
                     :uncomplicate.neanderthal/CLVector
                     [x data-output]
                     (freeze-through-native! data-output x))

(nippy/extend-thaw :uncomplicate.neanderthal/CLVector
                   [data-input]
                   (thaw-through-native! data-input *neanderthal-factory*))

(nippy/extend-freeze uncomplicate.neanderthal.internal.api.CLMatrix
                     :uncomplicate.neanderthal/CLMatrix
                     [x data-output]
                     (freeze-through-native! data-output x))

(nippy/extend-thaw :uncomplicate.neanderthal/CLMatrix
                   [data-input]
                   (thaw-through-native! data-input *neanderthal-factory*))
