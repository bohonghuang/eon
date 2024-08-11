(defsystem eon
  :version "0.1.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "An easy-to-use but flexible game framework based on Raylib for Common Lisp."
  :homepage "https://github.com/bohonghuang/eon"
  :bug-tracker "https://github.com/bohonghuang/eon/issues"
  :source-control (:git "https://github.com/bohonghuang/eon.git")
  :pathname "src/"
  :depends-on (#:alexandria #:bordeaux-threads #:trivial-garbage #:cffi #:cffi-ops #:cl-tiled
               #:claw-raylib #:universal-tween-engine #:promise-async-await #:lparallel #:log4cl)
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "tween" :depends-on ("package" "utils" "loop"))
               (:file "texture" :depends-on ("package"))
               (:file "misc" :depends-on ("package" "texture"))
               (:file "asset" :depends-on ("package" "loop"))
               (:file "viewport" :depends-on ("package" "asset"))
               (:file "loop" :depends-on ("package"))
               (:file "input" :depends-on ("package" "loop"))
               (:module "scene2d"
                :components ((:file "basic")
                             (:file "construct" :depends-on ("basic" "scroll"))
                             (:file "focus" :depends-on ("basic"))
                             (:file "scroll" :depends-on ("basic" "focus"))
                             (:file "select" :depends-on ("basic" "focus"))
                             (:module "ui"
                              :components ((:file "dialog")
                                           (:file "select")
                                           (:file "input")
                                           (:file "arrow")
                                           (:file "bar")
                                           (:file "keyboard" :depends-on ("select")))
                              :depends-on ("basic" "construct" "focus" "scroll" "select")))
                :depends-on ("package" "texture" "misc" "input" "tween" "shader"))
               (:file "particle" :depends-on ("package" "loop"))
               (:module "scene3d"
                :components ((:file "basic")
                             (:file "particle" :depends-on ("basic")))
                :depends-on ("package" "texture" "tween" "particle" "loop"))
               (:file "shader" :depends-on ("package" "asset"))
               (:file "tiled" :depends-on ("package" "scene2d" "loop" "texture" "asset"))
               (:file "audio" :depends-on ("package" "loop" "tween"))
               (:file "screen" :depends-on ("package" "loop" "asset" "tween" "shader"))
               (:file "shadow" :depends-on ("package" "viewport" "scene2d")))
  :in-order-to ((test-op (test-op #:eon/test))))

(defsystem eon/test
  :depends-on (#:eon/test.base
               #:eon.editor/test
               #:eon.debug/test)
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:eon.test))))

(defsystem eon/test.base
  :pathname "src/test/"
  :depends-on (#:parachute #:eon)
  :components ((:file "package")))
