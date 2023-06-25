(ns alphabet-cipher.coder)

(def min-char (int \a))
(def max-char (int \z))
(def max-offset (inc (- max-char min-char)))

(defn- to-offsets [s]
  (->> s
     (map int)
     (map #(- % min-char))))

(assert (= '(0) (to-offsets "a")) "Offsets should be based on 'a'")
(assert (= '(0 -65 19 4 18 19) (to-offsets "a test")) "Offsets should be applied to the list")

(defn- conform [key msg]
  (apply str (take (count msg) (cycle key))))

(assert (= "keykeyk" (conform "key" "message")))

(defn- fit-into-alphas [s]
  (->> s
     (map #(if (> % max-char) (- % max-offset) %))
     (map #(if (< % min-char) (+ % max-offset) %))))

(defn encode [keyword message]
  (->> message
     (map int)
     (map + (to-offsets (conform keyword message)))
     (fit-into-alphas)
     (map char)
     (apply str)))

(assert (= "booz" (encode "a" "booz")))
(assert (= "cppa" (encode "b" "booz")))

(defn decode [keyword message]
  (->> message
     (map int)
     (map #(- %2 %1) (to-offsets (conform keyword message)))
     (fit-into-alphas)
     (map char)
     (apply str)))

(assert (= "booz" (decode "a" "booz")))
(assert (= "anny" (decode "b" "booz")))
(assert (= "cppa" (decode "z" "booz")))

(defn decipher [cipher message]
  (let [offsets      (map - (map int cipher) (map int message))
        longest-key  (apply str (map char (fit-into-alphas (map #(+ min-char %) offsets))))
        pile-of-keys (map #(apply str %) (map #(take % longest-key) (map inc (range (count longest-key)))))]
    (first (filter #(= cipher (encode % message)) pile-of-keys))))

(assert (= "a" (decipher "booz" "booz")))
(assert (= "b" (decipher "cppa" "booz")))
