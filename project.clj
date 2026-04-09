(defproject organism "0.0.1"

  :description "a server for playing ORGANISM synchronously or asynchronously"
  :url "https://github.com/prismofeverything/organism"

  :dependencies [[cljs-ajax "0.8.1"]
                 [clojure.java-time "0.3.2"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [com.cognitect/transit-clj "1.0.324"]
                 [com.cognitect/transit-cljs "0.8.264"]
                 [cprop "0.1.17"]
                 [expound "0.8.9"]
                 [funcool/struct "1.4.0"]
                 [http-kit "2.8.0"]
                 [luminus-transit "0.1.2"]
                 [markdown-clj "1.10.5"]
                 [metosin/muuntaja "0.6.8"]
                 [metosin/reitit "0.5.12"]
                 [metosin/ring-http-response "0.9.2"]
                 [mount "0.1.16"]
                 [com.cgore/mersenne-twister "1.0.0"]
                 [com.novemberain/monger "3.5.0"
                  :exclusions [com.google.guava/guava]]
                 ;; [org.mongodb/mongodb-driver-sync "4.7.1"]
                 ;; [mongo-driver-3 "0.7.0"]
                 [nrepl "0.8.3"]
                 [org.clojure/clojure "1.12.0"]
                 [thheller/shadow-cljs "2.28.21" :scope "provided"]
                 [org.clojure/tools.cli "1.0.194"]
                 [org.clojure/tools.logging "1.1.0"]
                 [org.webjars.npm/bulma "0.9.1"]
                 [org.webjars.npm/material-icons "0.3.1"]
                 [org.webjars/webjars-locator "0.40"]
                 [org.webjars/webjars-locator-jboss-vfs "0.1.0"]
                 [reagent "1.0.0"]
                 [ring-webjars "0.2.0"]
                 [ring/ring-core "1.9.1"]
                 [ring/ring-defaults "0.3.2"]
                 [selmer "1.12.33"]
                 [thi.ng/color "1.4.0"]
                 [buddy/buddy-auth "2.2.0"]
                 [buddy/buddy-hashers "1.8.1"]]

  :min-lein-version "2.0.0"

  ;; ClojureScript is built by shadow-cljs, not Leiningen.
  ;; Dev:   npx shadow-cljs watch organism journey oroboros future
  ;; Prod:  npx shadow-cljs release organism journey oroboros future
  ;; See shadow-cljs.edn for build configuration.

  :source-paths ["src/clj" "src/cljs" "src/cljc" "src/java"]
  :test-paths ["test/clj"]
  :resource-paths ["resources"]
  :target-path "target/%s/"
  :main ^:skip-aot organism.core

  :plugins []

  :profiles
  {;; ClojureScript is built by shadow-cljs out of band (see deploy.sh or
   ;; `npx shadow-cljs release organism journey oroboros future`). Shadow
   ;; writes to resources/public/js/, which is already on the resource
   ;; path, so the uberjar picks it up automatically.
   :uberjar {:omit-source true
             :prep-tasks ["compile"]
             :aot :all
             :uberjar-name "organism.jar"
             :source-paths ["env/prod/clj"]
             :resource-paths ["env/prod/resources"]}

   ;; Back-compat alias — deploy.sh historically used this name.
   :uberjar-nocljs [:uberjar]

   :dev           [:project/dev :profiles/dev]
   :test          [:project/dev :project/test :profiles/test]

   :project/dev  {:jvm-opts ["-Dconf=dev-config.edn"]
                  :dependencies [[binaryage/devtools "1.0.2"]
                                 [pjstadig/humane-test-output "0.10.0"]
                                 [prone "2020-01-17"]
                                 [ring/ring-devel "1.9.1"]
                                 [ring/ring-mock "0.4.0"]]
                  :plugins      [[com.jakemccrary/lein-test-refresh "0.24.1"]
                                 [jonase/eastwood "0.3.5"]]
                  :source-paths ["env/dev/clj"]
                  :resource-paths ["env/dev/resources"]
                  :repl-options {:init-ns user
                                 :timeout 120000}
                  :injections [(require 'pjstadig.humane-test-output)
                               (pjstadig.humane-test-output/activate!)]}
   :project/test {:jvm-opts ["-Dconf=test-config.edn"]
                  :resource-paths ["env/test/resources"]}
   :profiles/dev {}
   :profiles/test {}})