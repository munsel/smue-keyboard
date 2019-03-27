(defproject smue-keyboard "0.1.0-SNAPSHOT"
  :description "SUPER MEGA ULTRA ERGO KEYBOARD"
  :license {:name "Affero GNU Public License"
            :url "https://www.gnu.org/licenses/agpl-3.0"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                                        ;[scad-clj "0.5.3"]
                 [com.github.munsel/scad-clj "multmatrix-fix"]
                 [uncomplicate/neanderthal "0.22.1"] ;; because i need SVD
                 ]
  :repositories [["jitpack" "https://jitpack.io"]])
