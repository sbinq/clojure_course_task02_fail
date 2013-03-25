(ns clojure-course-task02.core
  (:import [java.io File]
           [java.util.concurrent Executors])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn dirs-and-files [^File parent]
  "List of two vectors - directories and files in given directory
   (any of them can be nil if there are no files/directories inside)"
  (let [children (.listFiles parent)
        {dirs true, files false} (group-by (fn [^File file] (.isDirectory file)) children)]
    (list dirs files)))

(defn make-regex-file-filter [regex-str]
  "Returns function which takes file as argument and returns boolean
   indicating whether it matches or not"
  (let [pattern (re-pattern regex-str)]
    (fn [^File file]
      (not (nil? (re-matches pattern (.getName file)))))))

;;; This creates extremaly many threads on large inputs,
;;; last seen through jvisualvm value was about 30 000.
;;; Leads to total system hangup/out of memory/swapping.
;;; Was run as `./run.sh '.*jar' ~/` on user home directory
;;; with the following content:
;;; ➜  ~  find ~/ -type d | wc -l
;;; 61204
;;; ➜  ~  find ~/ -type f | wc -l
;;; 209649
;;; ➜  ~  
;;; TODO: It is probably clear why this solution is broken,
;;; TODO: but not clear how would be best to handle such kind of
;;; TODO: hierarchical parallelized structure in clojure where
;;; TODO: parent threads do not actually need to wait for child threads?
(defn find-files-pmap [file-filter ^File root]
  (let [[dirs files] (dirs-and-files root)]
    (concat (filter file-filter files)
            (reduce concat
                    ;; seems like this thread should wait for all (some part?) child threads
                    ;; to terminate here which would lead to thread per dir (or close to this)
                    (pmap #(find-files-pmap file-filter %) dirs)))))

(defn find-files [file-name ^String path]
  "Implements searching for a file using his name as a regexp."
  (let [file-filter (make-regex-file-filter file-name)
        files (find-files-pmap file-filter (File. path))]
    (map (fn [^File file] (.getName file)) files)))

(defn usage []
  (println "Usage: $ run.sh file_name path"))

(defn -main [file-name path]
  (if (or (nil? file-name)
          (nil? path))
    (usage)
    (do
      (println "Searching for " file-name " in " path "...")
      (dorun (map println (find-files file-name path)))
      (println "Finished with" (count (Thread/getAllStackTraces)) "threads")
      (shutdown-agents))))
