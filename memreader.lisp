;;;; memreader.lisp
(in-package #:memreader)

(define-foreign-library kernel32
    (:win32 "kernel32.dll"))

(define-foreign-library user32
    (:win32 "User32.dll"))

(use-foreign-library kernel32)
(use-foreign-library user32)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmethod translate-name-from-foreign ((spec string)
                                          (package (eql *package*))
                                          &optional varp)
    ;; Prevent it from splitting 32 into two words
    (let ((name (translate-camelcase-name spec :special-words '("32"))))
      (if varp (intern (format nil "*~a*" name)) name))))

(defctype handle :uint32)
(defctype pid :uint32)
(defctype dword :uint32)
(defctype bool (:boolean :int))

;; Should be a specific char*. Might break stuff
(defctype lpctstr :string)

(defparameter +PROCESS-TERMINATE+ #x0010)
(defparameter +PROCESS-QUERY-INFORMATION+ #x0400)

(defcfun "OpenProcess" handle (dwDesiredAccess dword) (bInheritHandle bool) (dwProcessId dword))

(defcfun "CloseHandle" handle (hobject handle))

(defcfun "GetCurrentProcessId" pid)

(defcfun "GetLastError" dword)

(defcfun "FindWindowA" handle (lpClassName lpctstr) (lpWindowName lpctstr))


(defparameter +TH32CS-INHERIT+ #x80000000)
(defparameter +TH32CS-SNAPHEAPLIST+ #x00000001)
(defparameter +TH32CS-SNAPMODULE+ #x00000008)
(defparameter +TH32CS-SNAPMODULE32+ #x00000010)
(defparameter +TH32CS-SNAPPROCESS+ #x00000002)
(defparameter +TH32CS-SNAPTHREAD+ #x00000004)

(defparameter +TH32CS-SNAPALL+ (logior
                                +TH32CS-SNAPHEAPLIST+
                                +TH32CS-SNAPMODULE+
                                +TH32CS-SNAPPROCESS+
                                +TH32CS-SNAPTHREAD+))

(defparameter +MAX-PATH+ 260)

(defcfun "CreateToolhelp32Snapshot" handle (dwFlags dword) (th32ProcessId dword))

(defcfun "Process32First" :bool (hSnapshot handle) (lppe :pointer))

(defcfun "Process32Next" :bool (hSnapshot handle) (lppe :pointer))

(defcstruct tag-process-entry-32
  (dwSize dword)
  (cntUsage dword)
  (th32Processid dword)
  ;; ULONG pointer
  (th32DefaultHeapID :pointer)
  (cntThreads dword)
  (th32ParentProcessID dword)
  (pcPriClassBase :long)
  (dwFlags dword)
  (szExeFile :char :count 260))

(defun main ()
  (get-process-list))


(defun get-process-list ()
  (with-foreign-object (pe32 '(:struct tag-process-entry-32))
    (let ((hprocess-snap
            (create-toolhelp-32-snapshot +TH32CS-SNAPPROCESS+ 0)))

      (unwind-protect (progn
                        (when (= hprocess-snap -1)
                          (error "CreateToolHelp32Snapshot failed"))


                        (setf (foreign-slot-value pe32 '(:struct tag-process-entry-32) 'dwSize)
                              (foreign-type-size '(:struct tag-process-entry-32)))

                        (unless (process-32-first hprocess-snap pe32)
                          (error "First process unsuccessful"))

                        (unless (process-32-first hprocess-snap pe32)
                          (error "First process unsuccessful"))

                        (loop while (process-32-next hprocess-snap pe32)
                              do (format t "Process Name: ~a ~%"
                                         (foreign-slot-value pe32 '(:struct tag-process-entry-32) 'th32processid))))
        (close-handle hprocess-snap)))))
