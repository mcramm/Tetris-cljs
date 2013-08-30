(defproject tetris "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.0-SNAPSHOT"]]
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild {
              :builds [{
                        :source-paths ["src"]
                        :compiler {:output-to "javascripts/app.js"
                                   :optimizations :whitespace
                                   :pretty-print true}
                        }]})
