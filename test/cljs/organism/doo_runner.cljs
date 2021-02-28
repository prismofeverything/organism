(ns organism.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [organism.core-test]))

(doo-tests 'organism.core-test)

