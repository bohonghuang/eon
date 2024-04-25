(defsystem eon.editor
  :version "0.1.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "Some simple editors provided with EON."
  :homepage "https://github.com/bohonghuang/eon"
  :bug-tracker "https://github.com/bohonghuang/eon/issues"
  :source-control (:git "https://github.com/bohonghuang/eon.git")
  :pathname "src/editor/"
  :depends-on (#:eon #:eon.debug
               #:alexandria #:closer-mop #:cffi-ops #:promise-async-await)
  :components ((:file "package")
               (:file "preview" :depends-on ("package"))
               (:file "editor" :depends-on ("package" "preview"))
               (:module "edit"
                :components ((:file "common")
                             (:file "asset" :depends-on ("common"))
                             (:file "construct" :depends-on ("common" "asset"))
                             (:file "timeline" :depends-on ("common")))
                :pathname ""
                :depends-on ("package" "preview" "editor")))
  :in-order-to ((test-op (test-op #:eon.editor/test))))

(defsystem eon.editor/test
  :depends-on (#:eon.editor/test.base)
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:eon.editor.test))))

(defsystem eon.editor/test.base
  :pathname "src/editor/test/"
  :depends-on (#:parachute #:eon.editor #:eon/test.base)
  :components ((:file "package")))
