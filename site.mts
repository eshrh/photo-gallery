(defun get-photos []
  (->> (file<- "./photos.json")
       (json-decode)
       ## convert string keys to keywords.
       (map (fn [j] (zipcoll (map keyword (keys j))
                            (values j))))))

(defun save-photos [p]
  (file-> "./photos.json"
          (s/>* "},{" "},\n{"
                (json-encode p))))

(defun construct-photo [id filename]
  (let [file (s+ "./photos/" (basename filename) ".jpg")
        filename_t (s+ "./photos_thumb/" (basename filename)
                       "_t.jpg")
        [size_w size_h] (->> ($ identify $filename) (fst)
                             (peg>!* '(* (<- (some :d)) "x"
                                         (<- (some :d))))
                             (fst))
        [title loc meta caption] (map input ["Photo title:"
                                             "Location:"
                                             "Metadata:"
                                             "Caption:"])]
    ($ cp $filename $file)
    ($ convert -scale "20%" $filename $filename_t)
    (table :pid id :img file :img_t filename_t
           :size_w size_w :size_h size_h :title title
           :loc loc :meta meta :caption caption)))


(defun html-entry [photo]
  (def crop-size (->> [:size_w :size_h]
                      (map |(s->n (photo $)))
                      (min-of)
                      (* 0.2)
                      (floor)))
  (html
   ~(a {:href ,(photo :img)
        :data-pswp-width ,(photo :size_w)
        :data-pswp-height ,(photo :size_h)
        :data-cropped "true"
        :target "_blank"}
       (img {:class "thumb"
             :src ,(photo :img_t)
             :width ,crop-size
             :height ,crop-size} nil)
       (span {:class "pswp-caption-content"}
             (p {:class "cap_title"} ,(photo :title))
             (p {:class "cap_location"} ,(photo :loc))
             ## (p {:class "cap_meta"} ,(photo :meta)) (br)
             (p {:class "cap_cap"} ,(photo :caption))))))

(defun make-site [photos]
  (file-> "index.html" (s/> "!!REPLACE_ME!!"
                            (s-join (map html-entry
                                         (reverse photos)) "\n")
                            (file<- "index_template.html"))))

(defmacro zip2 [c1 c2] ~(pairs (zipcoll ,c1 ,c2)))

(defun delete-photo [id photos]
  (def item (find (fn [el] (= (el :pid) id)) photos))
  (let [filename (item :img)
        thumb (item :img_t)]
    ($ rm $filename)
    ($ rm $thumb))
  (var new-photos (filter (fn [el] (not (= (el :pid) id))) photos))
  (for i 0 (length new-photos)
       (set ((new-photos i) :pid) i))
  (save-photos new-photos)
  (make-site new-photos))

(cli
 (var photos (get-photos))
 (match (map id args)
        @["-a" file] (do
                       (arr<- photos
                              (construct-photo
                               (if (empty? photos) 0
                                   (inc ((last photos) :pid)))
                               (fst args)))
                       (save-photos photos)
                       (print (length photos))
                       (make-site photos))
        @["-d" id] (delete-photo (s->n id) photos)
        @["-r"] (make-site photos)
        _ (pp "invalid")))
