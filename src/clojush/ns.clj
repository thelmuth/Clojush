(ns clojush.ns) ;; provides a macro that uses all clojush namespaces except for examples/* and experimental/*
  
(defmacro use-clojush
  []
  '(do
     (use '(clojush evaluate globals individual interpreter pushstate random simplification util))
     (use '(clojush.instructions boolean code common numbers random-instructions return string tag zip))
     (use '(clojush.pushgp breed genetic-operators parent-selection pushgp report))))
