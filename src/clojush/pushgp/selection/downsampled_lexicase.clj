(ns clojush.pushgp.selection.downsampled-lexicase
  (:use [clojush random]))

(defn down-sample
  "Performs downsampling on training cases by returning only a random subsample
   of the training cases
   If downsample-factor is a number, it is used directly.
   If downsample-factor is a vector [lower upper], then lower and upper are used
   as lower and upper bounds, and a new factor is uniformly randomly sampled
   from that range every generation."
  [{:keys [training-cases downsample-factor]}]
  (if (number? downsample-factor)
    (take (* downsample-factor (count training-cases))
          (shuffle training-cases))
    (let [lower (first downsample-factor)
          upper (second downsample-factor)
          range (- upper lower)
          ds-factor (+ lower (rand range))]
      (println "Downsampled lexicase factor this generation:" ds-factor)
      (take (* ds-factor (count training-cases))
            (shuffle training-cases)))
    ))
