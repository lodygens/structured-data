(ns structured-data)

(defn do-a-thing [x]
  (let  [
         xx (+ x x)
         ]
    (Math/pow xx xx)
    )
)

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

; This destructures the vector v
(defn spiff-destructuring [v]
  (let [[x y z]  v]
    (+ x z)
    )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let  [
         [bl tr] rectangle
         [x1 y1] bl
         [x2 y2] tr         
         ]
    (- x2 x1)
    )
)

(defn height [rectangle]
  (let  [[bl tr] rectangle
         [x1 y1] bl
         [x2 y2] tr         
         ]
    (- y2 y1)
    )
)

(defn square? [rectangle]
  (== (height rectangle) (width rectangle))
  )

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
   )

(defn contains-point? [rectangle point]
  (let  [
         [bl tr] rectangle
         [x1 y1] bl
         [x2 y2] tr
         [px py] point
         ]
    (and
     (<= x1 px x2)
     (<= y1 py y2)
     )
    )
  )

(defn contains-rectangle? [outer inner]
  (let  [
         [bl tr] inner
         ]
    (and
     (contains-point? outer bl)
     (contains-point? outer tr)
     )
    )
  )

;
; let define some books
; - author is a map
; - a book is a map containing
;    - a string
;    - a vector of authors
;

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})


(def books [cities, wild-seed, embassytown, little-schemer])



(defn title-length [book]
  (count
   (:title book)
   )
  )

(defn author-count [book]
  (count
   (:authors book)
   )
  )

(defn multiple-authors? [book]
  (> (count (:authors book)) 1 )
  )

; on recupere les auteurs du livre et on y ajoute le nouvel auteur
(defn add-author [book new-author]
  (conj (:authors book) new-author
         )
  )

(defn alive? [author]
  (not (contains? author :death-year)
       )
  )


; on applique la fonction count() à tous les éléments de la collection
; la fonction count() est appelée autant de fois qu'il y a d'éléments
; contrairement à "apply" (Cf "stars")
; param: collection is a vector of vectors
(defn element-lengths [collection]
  (map count (seq collection))
  )


; this returns all seconds elements of all vectors
; param : collection is a vector of vectors
(defn second-elements [collection]
  
  ; second(col) returns the 2nd element of the given vector
  (let [second (fn [col] (first (rest col))) ]
    
    ; we call second() for each vector in the collection
    (seq (map second collection)
         )
    )
  )


; on appele :title() pour chaque livre
; param: books is a vector of book
(defn titles [books]
  (map :title books)
  )


(defn monotonic? [a-seq]
  (let [
        elem  (fn [a] ( (a) ))
                 ]
    (<=
     (map elem a-seq
          )
     )
    )
  )

; on appelle une seule fois la fonction avec tous les paramètres
; la fonction str() est appelée une seule fois
; contrairement à "map" qui faut autant d'appels qu'il y a de paramètres
;    (Cf "elements-length")
(defn stars [n]
  (apply str (repeat n "*"))
  )


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

; un set ne contient aucun duplicat
; donc créer un set à partir d'une séquence enlève les duplicats
(defn contains-duplicates? [a-seq]
  (not
   (==
    (count a-seq)
    (count (set a-seq))
    )
   )
  )

(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(defn old-book->new-book [book]
  (assoc book :authors (set (map :name (:authors book))))
  )


;
; books new representation
; - author is a map
; - a book is a map containing
;    - a string
;    - a set of authors
;
(def china     {:name "China Miéville",
                :birth-year 1972})
(def octavia   {:name "Octavia E. Butler"
                :birth-year 1947
                :death-year 2006})
(def friedman  {:name "Daniel Friedman"
                :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})


(def cities         {:title "The City and the City"
                     :authors #{china}
                     }
  )
(def wild-seed      {:title "Wild Seed",
                     :authors #{octavia}
                     }
  )
(def embassytown    {:title "Embassytown",
                     :authors #{china}
                     }
  )
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}
                     }
  )

(def books [cities, wild-seed, embassytown, little-schemer])

;
; This checks if a book has been written by an author
; @param book   is a  book
; @param author is an author
; @return true or false
;
; map() retourne une sequence
; some() verifie qu'il existe au moins un predicat vrai sur une sequence
; Cf every() qui verifie que tous les predicats sont vrai sur une sequence
; mais some() peut retourner nil 
; donc on utilise true?(some())
(defn has-author? [book author]
  (let [
        nom (fn [a] (= (:name author) (:name a)))
        ]
    (true?
     (some true?
           (map nom (:authors book)
                )
           )
     )
    )
  )

(defn authors [books]
  (set
   (apply concat
          (map :authors books
               )
          )
   )
  )



(defn all-author-names [books]
  (map :name (authors books)
       )
  )

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
