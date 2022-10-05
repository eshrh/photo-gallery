(defun get-photos [& filename]
  (->> (if (empty? filename)
         "./photos.json"
         filename)
       (file<-)
       (json-decode)
       (map (fn [j] (zipcoll (map keyword (keys j))
                             (values j))))))

(defun input [prompt]
  (pp prompt)
  (->> (getline) (s//)))

(defun construct-photo [id filename]
  (let [file (s+ "./photos/" (basename filename) ".jpg")
        filename_t (s+ "./photos_thumb/" (basename filename)
                       "_t.jpg")
        size (->> ($ identify $filename) (fst)
                  (peg>!* '(* (<- (some :d))
                              "x"
                              (<- (some :d))))
                  (fst))
        title (input "Photo title:")
        loc (input "Location:")
        meta (input "Metadata:")
        caption (input "Caption:")]
    (pp filename)
    (pp file)
    ($ cp $filename $file)
    ($ convert -scale "20%" $filename $filename_t)
    (table :pid id
           :img file
           :img_t filename_t
           :size_w (fst size)
           :size_h (snd size)
           :title title
           :loc loc
           :meta meta
           :caption caption)))

(defun html-entry [photo]
  (def crop-size (floor (* 0.2 (min (s->n (photo :size_w)) (s->n (photo :size_h))))))
  (s+ "<a href=" qt (photo :img) qt s
      "data-pswp-width=" qt (photo :size_w) qt s
      "data-pswp-height=" qt (photo :size_h) qt s
      "data-cropped=\"true\" "
      "target=\"_blank\">" nl
      "<img class=\"thumb\" src=" qt (photo :img_t) qt
      "width=" qt crop-size qt s
      "height=" qt crop-size qt
      "/>" nl
      "<span class=\"pswp-caption-content\">"
      "<p class=\"cap_title\">" (photo :title) "</p><br>" nl
      "<p class=\"cap_location\">" (photo :loc) "</p>"
      "<p class=\"cap_meta\">" (photo :meta) "</p><br>"
      "<p class=\"cap_cap\">" (photo :caption) "</p></span></a>"
      ))

(defun make-site [template-file photos]
  (def template (file<- template-file))
  (file-> "index.html" (s/> "!!REPLACE_ME!!"
                            (s-join (map html-entry photos) "\n")
               template)))

(cli
 (var photos (get-photos))
 (unless (empty? args)
   (def new-photo (construct-photo
                   (if (empty? photos) 0
                       (inc ((last photos) :pid)))
                   (fst args)))
   (arr<- photos new-photo)
   (file-> "./photos.json" (json-encode photos)))
 (make-site "index_template.html" (reverse photos))
 )

## (pp (construct-photo 1 "test.jpg"))
