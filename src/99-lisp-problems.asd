(defpackage #:99-lisp-problems
  (:use :cl :asdf))

(in-package :99-lisp-problems)

(defsystem 99-lisp-problems
  :name "99 Lisp problems"
  :description "Solutions to the 99 Prolog problems in Common Lisp"
  :author "Ata Deniz Aydin"
  :licence "The MIT License"
  :components
    ((:module lists
      :pathname "01-28 lists"
      :components ((:file "p01-p07")
                   (:file "p08-p13"
                    :depends-on ("p01-p07"))
                   (:file "p14-p21")
                   (:file "p22-p28"
                    :depends-on ("p01-p07"
                                 "p08-p13"
                                 "p14-p21"))))
     (:module arithmetic
      :pathname "31-41 arithmetic"
      :components ((:file "p31-p41"
                    :depends-on ("p08-p13"
                                 "p22-p28"))))
     (:module logic
      :pathname "46-50 logic"
      :components ((:file "p46-p50")))
     (:module bintrees
      :pathname "54-69 bintrees"
      :components ((:file "p54-p60"
                    :depends-on ("p22-p28"))
                   (:file "p61-p63"
                    :depends-on ("p22-p28"
                                 "p54-p60"))
                   (:file "p64-p66"
                    :depends-on ("p22-p28"
                                 "p54-p60"
                                 "p67-p69"))
                   (:file "p67-p69")))
     (:module multitrees
      :pathname "70-73 multitrees"
      :components ((:file "p70-p73")))
     (:module graphs
      :pathname "80-89 graphs"
      :components ((:file "p80")
                   (:file "p81-p84"
                    :depends-on ("p80"))
                   (:file "p85-p89"
                    :depends-on ("p80"))))
     (:module misc
      :pathname "90-99 misc"
      :components ((:file "p90-p91"
                    :depends-on ("p22-p28"))
                   (:file "p92"
                    :depends-on ("p22-p28"))
                   (:file "p93"
                    :depends-on ("p14-p21"
                                 "p22-p28"))
                   (:file "p94"
                    :depends-on ("p22-p28"
                                 "p80"
                                 "p85-p89"))
                   (:file "p95-p96"
                    :depends-on ("p22-p28"))
                   (:file "p97-p98"
                    :depends-on ("p22-p28"))
                   (:file "p99"
                    :depends-on ("p80"))))))
