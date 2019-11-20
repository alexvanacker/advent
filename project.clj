(defproject advent "0.1.0-SNAPSHOT"
  :description "My project for solving coding problems from [advent of code](https://adventofcode.com/2)"
  :url "https://www.github.com/alexvanacker/advent"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot advent.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
