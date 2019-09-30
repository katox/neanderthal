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
            [uncomplicate.commons.core :refer [release let-release with-release info]]
            [uncomplicate.neanderthal
             [core :refer [transfer! copy! subvector vctr ge tr sy gb sb tb tp sp gd gt dt st]]
             [block :refer [entry-type]]
             [native :refer [native-int native-long native-double]]
             [random :as random]]
            [uncomplicate.neanderthal.internal.api :refer [data-accessor]]
            [uncomplicate.neanderthal.internal.host.serialization]
            [taoensso.nippy :as nippy]))

(def ^:const MAX-SIZE 1000)

(def gen-size (gen/frequency [[1 (gen/return 0)]
                              [90 (gen/large-integer* {:min 1 :max 100})]
                              [9 (gen/large-integer* {:min 101 :max MAX-SIZE})]]))
(def gen-band (gen/such-that
                (fn [[^long m ^long n ^long kl ^long ku]]
                  (or (= m n kl ku 0)
                      (and (>= m 2) (>= n 2) (> kl 0) (> ku 0))))
                (gen/bind (gen/tuple gen-size gen-size)
                          (fn [[^long m ^long n]]
                            (gen/tuple (gen/return m)
                                       (gen/return n)
                                       (gen/large-integer* {:min 0 :max (max 0 (dec m))})
                                       (gen/large-integer* {:min 0 :max (max 0 (dec n))}))))
                {:max-tries 100}))
(def gen-sym-band (gen/such-that
                    (fn [[^long n ^long k]]
                      (or (= n k 0)
                          (and (>= n 2) (> k 0))))
                    (gen/bind gen-size
                              (fn [^long n]
                                (gen/tuple (gen/return n)
                                           (gen/large-integer* {:min 0 :max (max 0 (dec n))}))))
                    {:max-tries 100}))

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
      [n gen-size]
      {:num-tests 50}
      (fact "Vector freeze/thaw should round-trip"
            (with-release [a (vctr factory (subvector vctr-source 0 n))
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn ge-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (* MAX-SIZE MAX-SIZE))]
    (for-all
      [m gen-size
       n gen-size
       layout gen-layout]
      {:num-tests 50}
      (fact "General matrix freeze/thaw should round-trip"
            (with-release [a (ge factory m n vctr-source {:layout layout})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn tr-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (packed-items MAX-SIZE))]
    (for-all
      [n gen-size
       layout gen-layout
       uplo gen-uplo
       diag gen-diag]
      {:num-tests 50}
      (fact "Triangular matrix freeze/thaw should round-trip"
            (with-release [a (tr factory n vctr-source
                                 {:layout layout :uplo uplo :diag diag})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn sy-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (packed-items MAX-SIZE))]
    (for-all
      [n gen-size
       layout gen-layout
       uplo gen-uplo]
      {:num-tests 50}
      (fact "Dense symmetric matrix freeze/thaw should round-trip"
            (with-release [a (sy factory n vctr-source
                                 {:layout layout :uplo uplo})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn gb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (banded-items MAX-SIZE (dec MAX-SIZE) (dec MAX-SIZE)))]
    (for-all
      [[m n kl ku] gen-band
       layout gen-layout]
      {:num-tests 50}
      (fact "General banded matrix freeze/thaw should round-trip"
            (with-release [a (gb factory m n kl ku vctr-source
                                 {:layout layout})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn sb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (banded-items MAX-SIZE (dec MAX-SIZE) 0))]
    (for-all
      [[n k] gen-sym-band
       layout gen-layout]
      {:num-tests 50}
      (fact "Symmetric banded matrix freeze/thaw should round-trip"
            (with-release [a (sb factory n k vctr-source
                                 {:layout layout
                                  :uplo   (if (= layout :column) :lower :upper)})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn tb-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (banded-items MAX-SIZE (dec MAX-SIZE) 0))]
    (for-all
      [[n k] gen-sym-band
       layout gen-layout
       diag gen-diag]
      {:num-tests 50}
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
      [n gen-size
       layout gen-layout
       uplo gen-uplo
       diag gen-diag]
      {:num-tests 50}
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
      [n gen-size
       layout gen-layout
       uplo gen-uplo]
      {:num-tests 50}
      (fact "Symmetric packed matrix freeze/thaw should round-trip"
            (with-release [a (sp factory n vctr-source
                                 {:layout layout
                                  :uplo   uplo})
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn gd-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory MAX-SIZE)]
    (for-all
      [n gen-size]
      {:num-tests 50}
      (fact "Diagonal matrix freeze/thaw should round-trip"
            (with-release [a (gd factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn gt-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (tridiagonal-items MAX-SIZE))]
    (for-all
      [n gen-size]
      {:num-tests 50}
      (fact "Tridiagonal matrix freeze/thaw should round-trip"
            (with-release [a (gt factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn dt-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (tridiagonal-items MAX-SIZE))]
    (for-all
      [n gen-size]
      {:num-tests 50}
      (fact "Diagonally dominant tridiagonal matrix freeze/thaw should round-trip"
            (with-release [a (dt factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))

(defn st-round-trip-test [factory]
  (with-release [vctr-source (create-random-vector factory (bidiagonal-items MAX-SIZE))]
    (for-all
      [n gen-size]
      {:num-tests 50}
      (fact "Symmetric tridiagonal matrix freeze/thaw should round-trip"
            (with-release [a (st factory n vctr-source nil)
                           b (nippy/thaw (nippy/freeze a))]
              (= a b) => true)))))
