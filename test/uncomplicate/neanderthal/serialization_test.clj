;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.neanderthal.serialization-test
  (:require [midje.sweet :refer [fact facts throws => roughly truthy]]
            [midje.experimental :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [uncomplicate.commons.core :refer [release with-release info]]
            [uncomplicate.neanderthal
             [core :refer [transfer! copy! subvector vctr submatrix ge tr sy gb sb tb tp sp gd gt dt st]]
             [block :refer [entry-type]]
             [native :refer [native-int native-long native-double]]
             [random :as random]]
            [uncomplicate.neanderthal.internal.api :refer [data-accessor]]
            [uncomplicate.neanderthal.internal.host.serialization]
            [taoensso.nippy :as nippy])
  (:import (clojure.lang ExceptionInfo)
           (uncomplicate.neanderthal.internal.api RealNativeMatrix)))

(def ^:const MAX-SIZE 1000)

(def gen-size (gen/frequency [[1 (gen/return 0)]
                              [90 (gen/large-integer* {:min 1 :max 100})]
                              [9 (gen/large-integer* {:min 101 :max MAX-SIZE})]]))
(def gen-band (gen/bind (gen/tuple gen-size gen-size)
                        (fn [[^long m ^long n]]
                          (gen/tuple (gen/return m)
                                     (gen/return n)
                                     (gen/large-integer* {:min 0 :max (max 0 (dec m))})
                                     (gen/large-integer* {:min 0 :max (max 0 (dec n))})))))
(def gen-sym-band (gen/bind gen-size
                            (fn [^long n]
                              (gen/tuple (gen/return n)
                                         (gen/large-integer* {:min 0 :max (max 0 (dec n))})))))
(def gen-subvec-indexes (gen/bind gen-sym-band
                                  (fn [[^long n ^long l]]
                                    (gen/tuple (gen/return n)
                                               (gen/large-integer* {:min 0 :max (max 0 (dec (- n l)))})
                                               (gen/return l)))))
(def gen-submatrix-indexes (gen/bind gen-band
                                     (fn [[^long m ^long n ^long k ^long l]]
                                       (gen/tuple (gen/return m)
                                                  (gen/return n)
                                                  (gen/large-integer* {:min 0 :max (max 0 (dec (- m k)))})
                                                  (gen/large-integer* {:min 0 :max (max 0 (dec (- n l)))})
                                                  (gen/return k)
                                                  (gen/return l)))))
(def gen-banded-submatrix-indexes (gen/bind gen-band
                                            (fn [[^long m ^long n ^long kl ^long ku]]
                                              (gen/tuple (gen/return m)
                                                         (gen/return n)
                                                         (gen/return kl)
                                                         (gen/return ku)
                                                         (gen/large-integer* {:min 0 :max (max 0 (dec m))})
                                                         (gen/large-integer* {:min 0 :max (max 0 (dec n))})))))

(def gen-layout (gen/elements [:column :row]))
(def gen-uplo (gen/elements [:upper :lower]))
(def gen-diag (gen/elements [:unit :non-unit]))

(defn- create-uniform [factory size]
  (with-release [real (random/rand-uniform! 0 MAX-SIZE (vctr native-double size))]
    (transfer! real (vctr factory size))))

(defn create-random-vector [factory size]
  (condp = (entry-type (data-accessor factory))
    Float/TYPE (random/rand-uniform! 0 MAX-SIZE (vctr factory size))
    Double/TYPE (random/rand-uniform! 0 MAX-SIZE (vctr factory size))
    Integer/TYPE (create-uniform native-int size)
    Long/TYPE (create-uniform native-long size)
    float (random/rand-uniform! 0 MAX-SIZE (vctr factory size))
    double (random/rand-uniform! 0 MAX-SIZE (vctr factory size))
    int (create-uniform native-int size)
    long (create-uniform native-long size)))

(defn- packed-items ^long [^long n]
  (inc (long (/ (* (dec n) (+ 2 n)) 2))))

(defn- banded-items ^long [^long n ^long kl ^long ku]
  (* (+ kl ku 1) n))

(defn- tridiagonal-items ^long [^long n]
  (max 0 (+ n (* (dec n) 2))))

(defn- bidiagonal-items ^long [^long n]
  (+ n (max 0 (dec n))))

(defn vector-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory MAX-SIZE)]
    (for-all
      [^long n gen-size]
      {:num-tests 1000}
      (fact "Vector freeze/thaw should round-trip"
            (with-release [a (vctr factory (subvector vctr-source 0 n))
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn subvector-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory MAX-SIZE)]
    (for-all
      [[^long n ^long k ^long l] gen-subvec-indexes]
      {:num-tests 1000}
      (fact "Vector freeze/thaw should round-trip"
            (with-release [x (vctr factory (subvector vctr-source 0 n))
                           a (subvector x k l)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn ge-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (* MAX-SIZE MAX-SIZE))]
    (for-all
      [^long m gen-size
       ^long n gen-size
       layout gen-layout]
      {:num-tests 1000}
      (fact "General matrix freeze/thaw should round-trip"
            (with-release [a (ge factory m n vctr-source {:layout layout})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn submatrix-ge-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (* MAX-SIZE MAX-SIZE))]
    (for-all
      [[^long m ^long n ^long i ^long j ^long k ^long l] gen-submatrix-indexes
       layout gen-layout]
      {:num-tests 1000}
      (fact "General submatrix freeze/thaw should round-trip"
            (with-release [x (ge factory m n vctr-source {:layout layout})
                           a (submatrix x i j k l)]
              (if (or (zero? (.dim ^RealNativeMatrix a)) (.isContiguous ^RealNativeMatrix a))
                (with-release [b (nippy/thaw (nippy/freeze a))]
                  (= a b) => true)
                (nippy/freeze a) => (throws ExceptionInfo)))))))

(defn tr-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (packed-items MAX-SIZE))]
    (for-all
      [^long n gen-size
       layout gen-layout
       uplo gen-uplo
       diag gen-diag]
      {:num-tests 1000}
      (fact "Triangular matrix freeze/thaw should round-trip"
            (with-release [a (tr factory n vctr-source
                                 {:layout layout :uplo uplo :diag diag})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn sy-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (packed-items MAX-SIZE))]
    (for-all
      [^long n gen-size
       layout gen-layout
       uplo gen-uplo]
      {:num-tests 1000}
      (fact "Dense symmetric matrix freeze/thaw should round-trip"
            (with-release [a (sy factory n vctr-source
                                 {:layout layout :uplo uplo})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn gb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (banded-items MAX-SIZE (dec MAX-SIZE) (dec MAX-SIZE)))]
    (for-all
      [[^long m ^long n ^long kl ^long ku] gen-band
       layout gen-layout]
      {:num-tests 1000}
      (fact "General banded matrix freeze/thaw should round-trip"
            (with-release [a (gb factory m n kl ku vctr-source
                                 {:layout layout})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

; TODO - enable in mkl-test
(defn submatrix-gb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (* MAX-SIZE MAX-SIZE))]
    (for-all
      [[^long m ^long n ^long kl ^long ku ^long i ^long k ^long l]
       (gen/bind gen-banded-submatrix-indexes
                 (fn [[^long m ^long n ^long kl ^long ku ^long k ^long l]]
                   (let [dim (max 0 (dec (min m n)))
                         len (max k l)]
                     (gen/tuple (gen/return m)
                                (gen/return n)
                                (gen/return kl)
                                (gen/return ku)
                                (gen/large-integer* {:min 0 :max (max 0 (- dim len))})
                                (gen/return k)
                                (gen/return l)))))
       layout gen-layout]
      {:num-tests 1000}
      (fact "General banded submatrix freeze/thaw should round-trip"
            (println [m n kl ku i i k l])
            (with-release [x (gb factory m n kl ku vctr-source {:layout layout})
                           a (submatrix x i i k l)]
              (if (or (zero? (.dim ^RealNativeMatrix a)) (.isContiguous ^RealNativeMatrix a))
                (with-release [b (nippy/thaw (nippy/freeze a))]
                  (prn a)
                  (prn b)
                  (= a b) => true)
                (nippy/freeze a) => (throws ExceptionInfo)))))))

(defn sb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (banded-items MAX-SIZE (dec MAX-SIZE) 0))]
    (for-all
      [[^long n ^long k] gen-sym-band
       layout gen-layout]
      {:num-tests 1000}
      (fact "Symmetric banded matrix freeze/thaw should round-trip"
            (with-release [a (sb factory n k vctr-source
                                 {:layout layout
                                  :uplo   (if (= layout :column) :lower :upper)})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn tb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (banded-items MAX-SIZE (dec MAX-SIZE) 0))]
    (for-all
      [[^long n ^long k] gen-sym-band
       layout gen-layout
       diag gen-diag]
      {:num-tests 1000}
      (fact "Triangular banded matrix freeze/thaw should round-trip"
            (with-release [a (tb factory n k vctr-source
                                 {:layout layout
                                  :uplo   (if (= layout :column) :lower :upper)
                                  :diag   diag})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn tp-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (packed-items MAX-SIZE))]
    (for-all
      [^long n gen-size
       layout gen-layout
       uplo gen-uplo
       diag gen-diag]
      {:num-tests 1000}
      (fact "Triangular packed matrix freeze/thaw should round-trip"
            (with-release [a (tp factory n vctr-source
                                 {:layout layout
                                  :uplo   uplo
                                  :diag   diag})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn sp-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (packed-items MAX-SIZE))]
    (for-all
      [^long n gen-size
       layout gen-layout
       uplo gen-uplo]
      {:num-tests 1000}
      (fact "Symmetric packed matrix freeze/thaw should round-trip"
            (with-release [a (sp factory n vctr-source
                                 {:layout layout
                                  :uplo   uplo})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn gd-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory MAX-SIZE)]
    (for-all
      [^long n gen-size]
      {:num-tests 1000}
      (fact "Diagonal matrix freeze/thaw should round-trip"
            (with-release [a (gd factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn gt-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (tridiagonal-items MAX-SIZE))]
    (for-all
      [^long n gen-size]
      {:num-tests 1000}
      (fact "Tridiagonal matrix freeze/thaw should round-trip"
            (with-release [a (gt factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn dt-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (tridiagonal-items MAX-SIZE))]
    (for-all
      [^long n gen-size]
      {:num-tests 1000}
      (fact "Diagonally dominant tridiagonal matrix freeze/thaw should round-trip"
            (with-release [a (dt factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn st-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (bidiagonal-items MAX-SIZE))]
    (for-all
      [^long n gen-size]
      {:num-tests 1000}
      (fact "Symmetric tridiagonal matrix freeze/thaw should round-trip"
            (with-release [a (st factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))
