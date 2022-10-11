(defun get-photos []
  (->> (file<- "./photos.json")
       (json-decode)
       # convert string keys to keywords.
       (map (fn [j] (zipcoll (map keyword (keys j))
                            (values j))))))

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
  (def crop-size (floor (* 0.2 (min-of (map |(s->n (photo $))
                                            [:size_w :size_h])))))
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
             (p {:class "cap_title"} ,(photo :title)) (br)
             (p {:class "cap_location"} ,(photo :loc))
             (p {:class "cap_meta"} ,(photo :meta)) (br)
             (p {:class "cap_cap"} ,(photo :caption))))))

(defun make-site [template-file photos]
  (file-> "index.html" (s/> "!!REPLACE_ME!!"
                            (s-join (map html-entry photos) "\n")
               (file<- template-file))))

(cli
 (var photos (get-photos))
 (unless (empty? args)
   (def new-photo (construct-photo
                   (if (empty? photos) 0           # start with id 0
                       (inc ((last photos) :pid))) # last id + 1
                   (fst args)))
   (arr<- photos new-photo)
   (file-> "./photos.json" (s/>* "},{" "},\n{" (json-encode photos))))
 (print (length photos))
 (make-site "index_template.html" (reverse photos)))
