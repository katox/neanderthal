;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Kamil Toman"}
  uncomplicate.neanderthal.internal.host.serialization
  (:require [uncomplicate.commons.core :refer [info]]
            [uncomplicate.commons.utils :refer [direct-buffer]]
            [uncomplicate.neanderthal.native :as native]
            [uncomplicate.neanderthal.internal
             [api :refer [storage navigator region]]
             [navigation :as navigation]]
            [uncomplicate.neanderthal.internal.host
             [buffer-block :refer [engine-by-matrix-type real-block-vector integer-block-vector
                                   real-ge-matrix real-uplo-matrix real-banded-matrix
                                   real-tb-matrix real-sb-matrix real-packed-matrix
                                   real-diagonal-matrix]]]
            [taoensso.nippy :as nippy])
  (:import (java.nio ByteBuffer)
           (uncomplicate.neanderthal.internal.api LayoutNavigator Region)
           (java.io OutputStream InputStream)
           (java.nio.channels Channels)))

(defn- write-buffer [^OutputStream out ^ByteBuffer buf]
  (let [pos (.position buf)]
    (with-open [channel (Channels/newChannel out)]
      (.write channel buf))
    (.position buf pos)))

(defn- read-buffer [^InputStream in ^ByteBuffer buf]
  (let [pos (.position buf)]
    (with-open [channel (Channels/newChannel in)]
      (.read channel buf))
    (.position buf pos)))

(defn- entry-type-kw [header]
  (let [class ^Class (:entry-type header)]
    (keyword (.. class getSimpleName toLowerCase))))

(nippy/extend-freeze java.nio.DirectByteBuffer
                     :uncomplicate.neanderthal/direct-buffer
                     [buf data-output]
                     (.writeInt data-output (.capacity buf))
                     (write-buffer data-output buf))

(nippy/extend-thaw :uncomplicate.neanderthal/direct-buffer
                   [data-input]
                   (let [capacity (.readInt data-input)
                         buf (direct-buffer capacity)]
                     (read-buffer data-input buf)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BandRegion
                     :uncomplicate.neanderthal.internal/BandRegion
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:m header))
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:kl header))
                       (nippy/freeze-to-out! data-output (:ku header))
                       (nippy/freeze-to-out! data-output (.uplo ^Region x))
                       (nippy/freeze-to-out! data-output (.diag ^Region x))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BandRegion
                   [data-input]
                   (let [m (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)
                         kl (nippy/thaw-from-in! data-input)
                         ku (nippy/thaw-from-in! data-input)
                         uplo (nippy/thaw-from-in! data-input)
                         diag (nippy/thaw-from-in! data-input)]
                     (navigation/->BandRegion m n kl ku uplo diag)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.GERegion
                     :uncomplicate.neanderthal.internal/GERegion
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:m header))
                       (nippy/freeze-to-out! data-output (:n header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/GERegion
                   [data-input]
                   (let [m (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)]
                     (navigation/->GERegion m n)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.StripeFullStorage
                     :uncomplicate.neanderthal.internal/StripeFullStorage
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:sd header))
                       (nippy/freeze-to-out! data-output (:fd header))
                       (nippy/freeze-to-out! data-output (:ld header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/StripeFullStorage
                   [data-input]
                   (let [sd (nippy/thaw-from-in! data-input)
                         fd (nippy/thaw-from-in! data-input)
                         ld (nippy/thaw-from-in! data-input)]
                     (navigation/->StripeFullStorage sd fd ld)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BandStorage
                     :uncomplicate.neanderthal.internal/BandStorage
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:height header))
                       (nippy/freeze-to-out! data-output (:width header))
                       (nippy/freeze-to-out! data-output (:ld header))
                       (nippy/freeze-to-out! data-output (:kl header))
                       (nippy/freeze-to-out! data-output (:ku header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BandStorage
                   [data-input]
                   (let [h (nippy/thaw-from-in! data-input)
                         w (nippy/thaw-from-in! data-input)
                         ld (nippy/thaw-from-in! data-input)
                         kl (nippy/thaw-from-in! data-input)
                         ku (nippy/thaw-from-in! data-input)]
                     (navigation/->BandStorage h w ld kl ku)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.TopPackedStorage
                     :uncomplicate.neanderthal.internal/TopPackedStorage
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:n header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/TopPackedStorage
                   [data-input]
                   (let [n (nippy/thaw-from-in! data-input)]
                     (navigation/->TopPackedStorage n)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BottomPackedStorage
                     :uncomplicate.neanderthal.internal/BottomPackedStorage
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:n header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BottomPackedStorage
                   [data-input]
                   (let [n (nippy/thaw-from-in! data-input)]
                     (navigation/->BottomPackedStorage n)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.TridiagonalStorage
                     :uncomplicate.neanderthal.internal/TridiagonalStorage
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:capacity header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/TridiagonalStorage
                   [data-input]
                   (let [n (nippy/thaw-from-in! data-input)
                         cap (nippy/thaw-from-in! data-input)]
                     (navigation/->TridiagonalStorage n cap)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.navigation.BidiagonalStorage
                     :uncomplicate.neanderthal.internal/BidiagonalStorage
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:capacity header))))

(nippy/extend-thaw :uncomplicate.neanderthal.internal/BidiagonalStorage
                   [data-input]
                   (let [n (nippy/thaw-from-in! data-input)
                         cap (nippy/thaw-from-in! data-input)]
                     (navigation/->BidiagonalStorage n cap)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealBlockVector
                     :uncomplicate.neanderthal/RealBlockVector
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:dim header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (:stride header))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/RealBlockVector
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         dim (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         stride (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)]
                     (real-block-vector fac true buf dim offset stride)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.IntegerBlockVector
                     :uncomplicate.neanderthal/IntegerBlockVector
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:dim header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (:stride header))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/IntegerBlockVector
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         dim (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         stride (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)]
                     (integer-block-vector fac true buf dim offset stride)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealGEMatrix
                     :uncomplicate.neanderthal/RealGEMatrix
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:m header))
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (.isColumnMajor ^LayoutNavigator (navigator x)))
                       (nippy/freeze-to-out! data-output (storage x))
                       (nippy/freeze-to-out! data-output (region x))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/RealGEMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         m (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         column? (nippy/thaw-from-in! data-input)
                         storage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)]
                     (real-ge-matrix fac true buf m n offset (navigation/layout-navigator column?) storage region)))


(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealUploMatrix
                     :uncomplicate.neanderthal/RealUploMatrix
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (.isColumnMajor ^LayoutNavigator (navigator x)))
                       (nippy/freeze-to-out! data-output (storage x))
                       (nippy/freeze-to-out! data-output (region x))
                       (nippy/freeze-to-out! data-output (:matrix-type header))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/RealUploMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         column? (nippy/thaw-from-in! data-input)
                         storage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))]
                     (real-uplo-matrix fac true buf n offset (navigation/layout-navigator column?)
                                       storage region matrix-type default engine)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealBandedMatrix
                     :uncomplicate.neanderthal/RealBandedMatrix
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:m header))
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (.isColumnMajor ^LayoutNavigator (navigator x)))
                       (nippy/freeze-to-out! data-output (storage x))
                       (nippy/freeze-to-out! data-output (region x))
                       (nippy/freeze-to-out! data-output (:matrix-type header))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/RealBandedMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         m (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         column? (nippy/thaw-from-in! data-input)
                         storage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))]
                     (real-banded-matrix fac true buf m n offset (navigation/layout-navigator column?)
                                         storage region matrix-type default engine)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealPackedMatrix
                     :uncomplicate.neanderthal/RealPackedMatrix
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (.isColumnMajor ^LayoutNavigator (navigator x)))
                       (nippy/freeze-to-out! data-output (storage x))
                       (nippy/freeze-to-out! data-output (region x))
                       (nippy/freeze-to-out! data-output (:matrix-type header))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/RealPackedMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         column? (nippy/thaw-from-in! data-input)
                         storage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))]
                     (real-packed-matrix fac true buf n offset (navigation/layout-navigator column?)
                                         storage region matrix-type default engine)))

(nippy/extend-freeze uncomplicate.neanderthal.internal.host.buffer_block.RealDiagonalMatrix
                     :uncomplicate.neanderthal/RealDiagonalMatrix
                     [x data-output]
                     (let [header (info x)]
                       (nippy/freeze-to-out! data-output (entry-type-kw header))
                       (nippy/freeze-to-out! data-output (:n header))
                       (nippy/freeze-to-out! data-output (:offset header))
                       (nippy/freeze-to-out! data-output (.isColumnMajor ^LayoutNavigator (navigator x)))
                       (nippy/freeze-to-out! data-output (storage x))
                       (nippy/freeze-to-out! data-output (region x))
                       (nippy/freeze-to-out! data-output (:matrix-type header))
                       (nippy/freeze-to-out! data-output (.buffer x))))

(nippy/extend-thaw :uncomplicate.neanderthal/RealDiagonalMatrix
                   [data-input]
                   (let [entry-type (nippy/thaw-from-in! data-input)
                         n (nippy/thaw-from-in! data-input)
                         offset (nippy/thaw-from-in! data-input)
                         column? (nippy/thaw-from-in! data-input)
                         storage (nippy/thaw-from-in! data-input)
                         region ^Region (nippy/thaw-from-in! data-input)
                         matrix-type (nippy/thaw-from-in! data-input)
                         buf (nippy/thaw-from-in! data-input)
                         fac (native/factory-by-type entry-type)
                         engine (engine-by-matrix-type fac matrix-type)
                         default (navigation/real-default matrix-type (.isDiagUnit region))]
                     (real-diagonal-matrix fac true buf n offset (navigation/layout-navigator column?)
                                           storage region matrix-type default engine)))



