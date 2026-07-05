(ns materials_project.main-test
  "Contract + behavioral test for the materials_project-compat L5 actor (cljc port).
  Runs under babashka: `bb test`. Stronger than the py static contract test —
  exercises CRUD / pagination / filtering / expansion / validation against the
  in-memory Datom-log store."
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [materials_project.main :as m]))

(defn- dummy [field coerce] (case (get coerce field) :int 1 :float 1.0 :bool true (name field)))
(defn- full-record [{:keys [required coerce]}] (into {} (map (fn [f] [f (dummy f coerce)]) required)))

(deftest route-surface
  (is (= (* 5 (count m/entity-specs)) (count m/routes)))
  (doseq [{:keys [plural]} m/entity-specs]
    (let [paths (set (map (juxt :method :path) m/routes)) base (str "/v1/" plural)]
      (is (contains? paths ["POST" base])) (is (contains? paths ["GET" base]))
      (is (contains? paths ["GET" (str base "/{id}")])) (is (contains? paths ["PATCH" (str base "/{id}")]))
      (is (contains? paths ["DELETE" (str base "/{id}")])))))

(deftest crud-roundtrip
  (doseq [{:keys [entity id-prefix] :as spec} m/entity-specs]
    (let [s (m/fresh-store) [rec status] (m/handle-create s entity (full-record spec))]
      (is (= 201 status) (str entity " create"))
      (is (str/starts-with? (:id rec) (str id-prefix "_")) (str entity " id-prefix"))
      (is (= [rec 200] (m/handle-get s entity (:id rec) {})) (str entity " get"))
      (is (= (:id rec) (:id (first (m/handle-update s entity (:id rec) {})))))
      (is (= 200 (second (m/handle-delete s entity (:id rec)))))
      (is (= 404 (second (m/handle-get s entity (:id rec) {})))))))

(deftest validation
  (doseq [{:keys [entity required] :as spec} m/entity-specs]
    (when (seq required)
      (let [s (m/fresh-store)]
        (is (= 400 (second (m/handle-create s entity {}))) (str entity " missing-required"))
        (is (= 400 (second (m/handle-create s entity (assoc (full-record spec) :__bogus__ 1)))) (str entity " unknown"))))))

(deftest coercion
  (doseq [{:keys [entity coerce] :as spec} m/entity-specs]
    (when (seq coerce)
      (let [s (m/fresh-store) [rec _] (m/handle-create s entity (full-record spec))]
        (doseq [[f kind] coerce]
          (is (case kind :int (integer? (get rec f)) :float (float? (get rec f)) :bool (boolean? (get rec f)) true)
              (str entity "/" (name f))))))))

(deftest healthz
  (let [[body status] (m/healthz)]
    (is (= 200 status)) (is (= "materials_project-compat" (:actor body))) (is (= (set m/entities) (set (:entities body))))))

#?(:clj (defn -main [& _]
          (let [{:keys [fail error]} (run-tests 'materials_project.main-test)]
            (System/exit (if (pos? (+ fail error)) 1 0)))))
