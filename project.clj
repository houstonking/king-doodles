(defproject king "0.1.0-SNAPSHOT"
  :description "This is Houston King's doodles (maybe) in the direction of a PhD"
  :plugins [[lein-modules "0.3.11"]
            [lein-hiera "0.9.0"]
            [lein-cascade "0.1.2"]
            [lein-shell "0.4.0"]]
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:fast {:modules {:subprocesses nil}}
            :warn-reflection {:global-vars {*warn-on-reflection* true}}
            :dev {:aliases
                  {"build" ["modules" "do" "clean," "test," "install"]}}
             }
  :modules {:inherited
            {:dependencies [[org.clojure/clojure "1.7.0-alpha4"]
                            [prismatic/schema _]
                            [prismatic/plumbing _]
                            [com.taoensso/timbre _]]
             :jvm-opts ^:replace ["-Xmx1g"]
             }


            :versions {king :version
                       prismatic/schema "0.4.2"
                       prismatic/plumbing "0.4.3"
                       com.taoensso/timbre "3.4.0"}}
  )
