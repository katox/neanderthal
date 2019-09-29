;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:author "Kamil Toman"}
  uncomplicate.neanderthal.serialization
  (:require [uncomplicate.commons.core :refer [info release let-release with-release]]
            [uncomplicate.commons.utils :refer [dragan-says-ex]]
            [uncomplicate.neanderthal.core :refer [transfer! native]]
            [uncomplicate.neanderthal.internal.api :as api]
            [uncomplicate.neanderthal.internal.host.serialization
             :refer [*neanderthal-factory* write-vctr-data! read-vctr-data!]]
            [uncomplicate.neanderthal.native :as native])
  (:import (java.io DataOutput DataInput)
           (uncomplicate.neanderthal.internal.api DenseStorage LayoutNavigator Region
                                                  RealNativeMatrix CUMatrix CUVector CLVector CLMatrix)
           (uncomplicate.neanderthal.internal.host.buffer_block IntegerBlockVector RealBlockVector)))

(defmacro with-real-factory
  "Create a bind to use the `factory` during Neanderthal real vector and matrix construction.
      (clojurecuda/with-default
       (with-open [in (DataInputStream. (io/input-stream (io/file \"/tmp/myv.bin\")))]
             (with-neanderthal-factory (cuda-double (current-context) default-stream)
                 (s/thaw-from-in! in)))
  "
  [factory & body]
  `(binding [*neanderthal-factory* ~factory]
     ~@body))

(defmethod transfer! [IntegerBlockVector DataOutput]
  [^IntegerBlockVector source ^DataOutput destination]
  (if (.isContiguous source)
    (do
      (write-vctr-data! destination source)
      destination)
    (dragan-says-ex "You cannot directly transfer from a non-contiguous vector. Make the (core/copy vector) first."
                    (info source))))

(defmethod transfer! [DataInput IntegerBlockVector]
  [^DataInput source ^IntegerBlockVector destination]
  (if (.isContiguous destination)
    (do
      (read-vctr-data! source destination)
      destination)
    (dragan-says-ex "You cannot directly transfer to a non-contiguous vector. Make the (core/copy vector) first."
                    (info source))))

(defmethod transfer! [RealBlockVector DataOutput]
  [^RealBlockVector source ^DataOutput destination]
  (if (.isContiguous source)
    (do
      (write-vctr-data! destination source)
      destination)
    (dragan-says-ex "You cannot directly transfer from a non-contiguous vector. Make the (core/copy vector) first."
                    (info source))))

(defmethod transfer! [DataInput RealBlockVector]
  [^DataInput source ^RealBlockVector destination]
  (if (.isContiguous destination)
    (do
      (read-vctr-data! source destination)
      destination)
    (dragan-says-ex "You cannot directly transfer to a non-contiguous vector. Make the (core/copy vector) first."
                    (info source))))

(defmethod transfer! [RealNativeMatrix DataOutput]
  [^RealNativeMatrix source ^DataOutput destination]
  (if (.isContiguous source)
    (do
      (write-vctr-data! destination (api/view-vctr source))
      destination)
    (dragan-says-ex "You cannot directly transfer from a non-contiguous matrix. Make the (core/copy matrix) first."
                    (info source))))

(defmethod transfer! [DataInput RealNativeMatrix]
  [^DataInput source ^RealNativeMatrix destination]
  (if (.isContiguous destination)
    (do
      (read-vctr-data! source (api/view-vctr destination))
      destination)
    (dragan-says-ex "You cannot directly transfer to a non-contiguous matrix. Do the (core/copy matrix) first."
                    (info source))))

(defmethod transfer! [CUVector DataOutput]
  [source destination]
  (with-release [h (api/host source)]
    (transfer! h destination)))

(defmethod transfer! [DataInput CUVector]
  [source destination]
  (with-release [h (api/host destination)]
    (transfer! source h)
    (transfer! h destination)))

(defmethod transfer! [CUMatrix DataOutput]
  [source destination]
  (with-release [h (api/host source)]
    (transfer! h destination)))

(defmethod transfer! [DataInput CUMatrix]
  [source destination]
  (with-release [h (api/host destination)]
    (transfer! source h)
    (transfer! h destination)))

(defmethod transfer! [CLVector DataOutput]
  [source destination]
  (with-release [h (api/host source)]
    (transfer! h destination)))

(defmethod transfer! [DataInput CLVector]
  [source destination]
  (with-release [h (api/host destination)]
    (transfer! source h)
    (transfer! h destination)))

(defmethod transfer! [CLMatrix DataOutput]
  [source destination]
  (with-release [h (api/host source)]
    (transfer! h destination)))

(defmethod transfer! [DataInput CLMatrix]
  [source destination]
  (with-release [h (api/host destination)]
    (transfer! source h)
    (transfer! h destination)))
