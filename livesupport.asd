;;;; livesupport.asd

(asdf:defsystem #:livesupport
  :description "Some helpers that make livecoding with slime/sly a little easier"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :components ((:file "package")
               (:file "livesupport")))
