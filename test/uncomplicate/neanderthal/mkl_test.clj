;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns uncomplicate.neanderthal.mkl-test
  (:require [midje.sweet :refer [facts throws =>]]
            [uncomplicate.neanderthal
             [native :refer [factory-by-type]]
             [block-test :as block-test]
             [real-test :as real-test]
             [math-test :as math-test]
             [random-test :as random-test]]
            [uncomplicate.neanderthal.internal.host.mkl :refer [mkl-float mkl-double mkl-int mkl-long]]))

(facts "factory-by-type test"
       (factory-by-type :float) => mkl-float
       (factory-by-type :double) => mkl-double
       (factory-by-type :int) => mkl-int
       (factory-by-type :long) => mkl-long)

(block-test/test-all mkl-double)
(block-test/test-all mkl-float)
(block-test/test-host mkl-double)
(block-test/test-host mkl-float)
(block-test/test-both-factories mkl-float mkl-double)
(block-test/test-both-factories mkl-double mkl-float)

(block-test/test-extend-buffer mkl-float)

(real-test/test-blas mkl-double)
(real-test/test-blas mkl-float)
(real-test/test-blas-host mkl-double)
(real-test/test-blas-host mkl-float)
(real-test/test-lapack mkl-double)
(real-test/test-lapack mkl-float)

(real-test/test-blas-sy mkl-double)
(real-test/test-blas-sy mkl-float)
(real-test/test-blas-sy-host mkl-double)
(real-test/test-blas-sy-host mkl-float)

(real-test/test-blas-gb mkl-double)
(real-test/test-blas-gb mkl-float)
(real-test/test-blas-gb-host mkl-double)
(real-test/test-blas-gb-host mkl-float)

(real-test/test-blas-sb mkl-double)
(real-test/test-blas-sb mkl-float)
(real-test/test-blas-sb-host mkl-double)
(real-test/test-blas-sb-host mkl-float)

(real-test/test-blas-tb mkl-double)
(real-test/test-blas-tb mkl-float)
(real-test/test-blas-tb-host mkl-double)
(real-test/test-blas-tb-host mkl-float)

(real-test/test-blas-tp mkl-double)
(real-test/test-blas-tp mkl-float)
(real-test/test-blas-tp-host mkl-double)
(real-test/test-blas-tp-host mkl-float)

(real-test/test-blas-sp mkl-double)
(real-test/test-blas-sp mkl-float)
(real-test/test-blas-sp-host mkl-double)
(real-test/test-blas-sp-host mkl-float)

(real-test/test-blas-gd mkl-double)
(real-test/test-blas-gd mkl-float)
(real-test/test-blas-gd-host mkl-double)
(real-test/test-blas-gd-host mkl-float)

(real-test/test-blas-gt mkl-double)
(real-test/test-blas-gt mkl-float)
(real-test/test-blas-gt-host mkl-double)
(real-test/test-blas-gt-host mkl-float)

(real-test/test-blas-dt mkl-double)
(real-test/test-blas-dt mkl-float)
(real-test/test-blas-dt-host mkl-double)
(real-test/test-blas-dt-host mkl-float)

(real-test/test-blas-st mkl-double)
(real-test/test-blas-st mkl-float)
(real-test/test-blas-st-host mkl-double)
(real-test/test-blas-st-host mkl-float)

(math-test/test-all-host mkl-double)
(math-test/test-all-host mkl-float)

(random-test/test-all mkl-double)
(random-test/test-all mkl-float)
(random-test/test-all-host mkl-double)
(random-test/test-all-host mkl-float)

(defn gb-corruption-test [factory]
  (uncomplicate.commons.core/with-release
    [source-vctr (uncomplicate.neanderthal.core/vctr factory (range 1 10000))]
    (dotimes [_ 100000]
      (uncomplicate.commons.core/with-release
        [_ (uncomplicate.neanderthal.core/gb factory 2 2 0 0 source-vctr)]))))

(gb-corruption-test mkl-double)                             ; should SEGFAULT