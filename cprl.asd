(defsystem cprl
  :author "Lewis Weinberger"
  :license "MIT"
  :version "0.0.0"
  :homepage "https://github.com/lewis-weinberger/cprl"
  :bug-tracker "https://github.com/lewis-weinberger/cprl/issues"
  :source-control (:git "git@github.com:lewis-weinberger/cprl.git")
  :description "A Cyberpunk-inspired Roguelike game."
  :depends-on (:cffi :float-features)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "cprl"
  :entry-point "cprl:main"
  :components ((:module "src"
                :serial t
                :components
                ((:file "packages")
		 (:file "macros")
		 (:file "config")
                 (:file "blt")
		 (:file "entity")
		 (:file "location")
		 (:file "jobs")
		 (:file "player")
		 (:file "bazaar")
		 (:file "ui")
                 (:file "main")))))
