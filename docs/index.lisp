(defpackage :formulatum/docs/index
  (:nicknames :frml/docs)
  (:use :cl
        #:40ants-doc)
  (:import-from #:40ants-doc/autodoc
                #:defautodoc)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package)
  (:import-from #:40ants-doc-full/builder
                #:render-to-files)
  (:import-from #:40ants-doc-full/page
                #:make-page)
  (:export #:generate-manual)
  (:documentation "Documentation package to general Formulatum Manual"))
(in-package :formulatum/docs/index)

(defsection @full-manual (:title "Manual")
  (@intro section)
  (@api section))

(defsection @intro (:title "Introduction")
  "A formulation chemistry tool for building formulas with chemical and regulatory 
intelligence. You can add/create chemical raw materials, defined with the 
required regulatory data. Formulas may be created/added defined by the chemicals 
used, with manufacturing instructions and finish product specifications. 
Formulas can be associated with specific conditions (i.e. no-no lists) and will 
advise the formulation chemist accordingly as they build their formulas. The 
regulatory database will link to both the chemical and formula databases 
providing real-time intelligence (perhaps this will be chance to incorporate AI).

This program will comprise of the following modules:

 - Chemicals Module
 - Formulation Module
 - Regulatory Module
 - Quality Module
 - Production Module
")

(defautodoc @api (:system :formulatum))


(defun generate-manual ()
  (render-to-files
   (list @full-manual
         (make-page (list @intro
                          @api)
                    :format :markdown
                    :base-dir #P"./docs/_manual/"
                    :base-filename "Manual"))))

