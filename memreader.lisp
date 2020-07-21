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

(defctype handle :pointer)
(defctype dword :uint32)
(defctype bool (:boolean :int))
(defctype long :long)
(defctype wchar :int16)
(defctype tchar wchar)
(defctype ulong-ptr #+32-bit :uint32 #+64-bit :uint64)

(cffi:defcstruct process-entry-32
  (size dword)
  (usage dword)
  (process-id dword)
  (default-heap-id ulong-ptr)
  (module-id dword)
  (count-threads dword)
  (parent-process-id dword)
  (pri-class-base long)
  (flags dword)
  (exe-file wchar :count 260))


(defcfun "CreateToolhelp32Snapshot" handle (flags dword) (process-id dword))

(defcfun (process-32-first "Process32FirstW") bool (snapshot handle) (lppe :pointer (:struct tag-process-entry-32)))

(defcfun (process-32-next "Process32NextW") bool (snapshot handle) (lppe :pointer (:struct tag-process-entry-32)))

(defcfun "CloseHandle" handle (object handle))


(defun string-to-lisp (chars &optional count)
  (values
   (cffi:foreign-string-to-lisp
    chars
    :count (and count (* count (cffi:foreign-type-size 'tchar)))
    :encoding +win32-string-encoding+)))

(defun main ()
  (get-process-list))

(defun get-process-list ()
  (with-foreign-object (pe32 '(:struct process-entry-32))
    (let ((hprocess-snap
            (create-toolhelp-32-snapshot +TH32CS-SNAPPROCESS+ 0)))

      (unwind-protect (progn
                        (when (cffi:pointer-eq hprocess-snap +invalid-handle-value+)
                          (error "CreateToolHelp32Snapshot failed"))

                        (setf (foreign-slot-value pe32 '(:struct process-entry-32) 'size)
                              (foreign-type-size '(:struct process-entry-32)))

                        (unless (process-32-first hprocess-snap pe32)
                          (error "First process unsuccessful"))

                        (loop while (process-32-next hprocess-snap pe32)
                              do (format t "Process Name: ~a ~%"
                                         (string-to-lisp
                                          (foreign-slot-value pe32 '(:struct process-entry-32) 'exe-file)))))

        (close-handle hprocess-snap)))))
