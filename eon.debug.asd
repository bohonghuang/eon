(defsystem eon.debug
  :version "0.1.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "Some convenient debugging facilities provided with EON."
  :homepage "https://github.com/bohonghuang/eon"
  :bug-tracker "https://github.com/bohonghuang/eon/issues"
  :source-control (:git "https://github.com/bohonghuang/eon.git")
  :pathname "src/debug/"
  :depends-on (#:eon #:alexandria #:closer-mop #:cffi-ops #:promise-async-await)
  :components ((:file "package")
               (:file "ui" :depends-on ("package"))
               (:file "promise" :depends-on ("package" "ui"))
               (:file "info" :depends-on ("package")))
  :in-order-to ((test-op (test-op #:eon.debug/test))))

(defsystem eon.debug/test
  :depends-on (#:eon.debug/test.base)
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:eon.debug.test))))

(defsystem eon.debug/test.base
  :pathname "src/debug/test/"
  :depends-on (#:parachute #:eon.debug #:eon/test.base)
  :components ((:file "package")))
