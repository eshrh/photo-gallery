(defun path [album file]
  (s+ "albums/" album "/" file))

(defun get-photos [album]
  (->> (file<- (path album "photos.json"))
       (json-decode)
       ## convert string keys to keywords.
       (map (fn [j] (zipcoll (map keyword (keys j))
                            (values j))))))

(defun save-photos [album p]
  (file-> (path album "photos.json")
          (s/>* "},{" "},\n{"
                (json-encode p))))

(defun construct-photo [album id filename]
  (let [file (s+ "photos/" (basename filename) ".jpg")
        file_t (s+ "photos_thumb/" (basename filename)
                       "_t.jpg")
        abs_file (path album file)
        abs_file_t (path album file_t)
        [size_w size_h] (->> ($ identify $filename) (fst)
                             (peg>!* '(* (<- (some :d)) "x"
                                         (<- (some :d))))
                             (fst))
        [title loc meta caption] (map input ["Photo title:"
                                             "Location:"
                                             "Metadata:"
                                             "Caption:"])]
    ($ cp $filename $abs_file)
    ($ convert -scale "20%" $filename $abs_file_t)
    (table :pid id :img file :img_t file_t
           :size_w size_w :size_h size_h :title title
           :loc loc :meta meta :caption caption)))


(defun html-entry [photo]
  (def crop-size 200)
  (html
   ~(a {:href ,(photo :img)
        :data-pswp-width ,(photo :size_w)
        :data-pswp-height ,(photo :size_h)
        :data-cropped "true"
        :target "_blank"}
       (img {:class "thumb"
             :src ,(photo :img_t)
             :width ,crop-size
             :height ,crop-size} :noclose)
       (span {:class "pswp-caption-content"}
             (p {:class "cap_title"} ,(photo :title))
             (p {:class "cap_location"} ,(photo :loc))
             ## (p {:class "cap_meta"} ,(photo :meta)) (br)
             (p {:class "cap_cap"} ,(photo :caption))))))

(defun make-site [album photos]
  (let [out_path (path album "index.html")
        template (file<- "./album_template.html")]
    (file-> out_path
            (->> template
                 (s/> "!!CONTENT!!"
                      (s-join (map html-entry
                                   (reverse photos)) "\n"))
                 (s/> "!!ALBUM!!" album)))))

(defmacro zip2 [c1 c2] ~(pairs (zipcoll ,c1 ,c2)))

(defun delete-photo [album id photos]
  (def item (find (fn [el] (= (el :pid) id)) photos))
  (let [filename (path album (item :img))
        thumb (path album (item :img_t))]
    ($ rm $filename)
    ($ rm $thumb))
  (var new-photos (filter (fn [el] (not (= (el :pid) id))) photos))
  (for i 0 (length new-photos)
       (set ((new-photos i) :pid) i))
  (save-photos album new-photos)
  (make-site album new-photos))

(cli
 (var album (fst args))
 (var photos (get-photos album))
 (match (map id (tail args))
        @["-a" file] (do
                       (arr<- photos
                              (construct-photo album
                               (if (empty? photos) 0
                                   (inc ((last photos) :pid)))
                               file))
                       (save-photos album photos)
                       (print (length photos))
                       (make-site album photos))
        @["-d" id] (delete-photo album (s->n id) photos)
        @["-r"] (make-site album photos)
        _ (pp "invalid")))
