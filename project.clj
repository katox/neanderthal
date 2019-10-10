;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(defproject uncomplicate/neanderthal "0.27.0-SNAPSHOT"
  :description "Neanderthal is a Clojure library for fast matrix and linear algebra computations."
  :url "https://github.com/uncomplicate/neanderthal"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/neanderthal"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [uncomplicate/commons "0.8.0"]
                 [uncomplicate/fluokitten "0.9.1"]
                 [uncomplicate/neanderthal-native "0.25.0"]
                 [uncomplicate/clojurecl "0.13.0"]
                 [org.jocl/jocl-blast "1.5.0"]
                 [uncomplicate/clojurecuda "0.8.0"]
                 [org.jcuda/jcublas "10.1.0"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [com.taoensso/nippy "2.15.0-RC1"]]

  :codox {:metadata {:doc/format :markdown}
          :src-dir-uri "http://github.com/uncomplicate/neanderthal/blob/master/"
          :src-linenum-anchor-prefix "L"
          :namespaces [uncomplicate.neanderthal.core
                       uncomplicate.neanderthal.linalg
                       uncomplicate.neanderthal.native
                       uncomplicate.neanderthal.opencl
                       uncomplicate.neanderthal.cuda
                       uncomplicate.neanderthal.math
                       uncomplicate.neanderthal.vect-math
                       uncomplicate.neanderthal.real
                       uncomplicate.neanderthal.auxil
                       uncomplicate.neanderthal.random]
          :output-path "docs/codox"}

  ;;also replaces lein's default JVM argument TieredStopAtLevel=1
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"
                       "-XX:MaxDirectMemorySize=16g" "-XX:+UseLargePages"
                       "--add-opens=java.base/jdk.internal.ref=ALL-UNNAMED"]

  :profiles {:dev {:plugins [[lein-midje "3.2.1"]
                             [lein-codox "0.10.6"]]
                   :global-vars {*warn-on-reflection* true
                                 *assert* false
                                 *unchecked-math* :warn-on-boxed
                                 *print-length* 128}
                   :dependencies [[midje "1.9.9"]
                                  [org.clojure/test.check "0.10.0"]]}
             :java8 {:jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"
                                          "-XX:MaxDirectMemorySize=16g"
                                          "-XX:+UseLargePages"]}}

  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"]
  :source-paths ["src/clojure" "src/device"]
  :java-source-paths ["src/java"]
  :test-paths ["test"])
