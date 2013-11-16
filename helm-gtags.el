;;; helm-gtags.el --- GNU GLOBAL helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; 纪秀峰 fork version
;; URL: https://github.com/jixiuf/emacs-helm-gtags
;; Author: 纪秀峰 <jixiuf@gmail.com>
;; Version: 2.0
;; Package-Requires: ((helm "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `helm-gtags.el' is GNU GLOBAL `helm' interface.
;; `helm-gtags.el' is not compatible `anything-gtags.el', but `helm-gtags.el'
;; is designed for fast search.

;;
;; To use this package, add these lines to your init.el or .emacs file:
;;     (require 'helm-config)
;;     (require 'helm-gtags)
;;
;;     (add-hook 'c-mode-hook '(lambda()
;;                           (add-to-list 'helm-gtags-tag-location-list "/usr/include/")
;;                           ))
;;     (add-hook 'helm-gtags-mode-hook
;;               '(lambda ()
;;                  (local-set-key [(meta return)] 'helm-gtags-complete)
;;                  (local-set-key (kbd "M-.") 'helm-gtags-find-tag-and-symbol)
;;                  (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
;;                  (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
;;                  (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
;;                  (local-set-key (kbd "C-,") 'helm-gtags-pop-stack)
;;                  (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
;;                  (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)))
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'helm)
(require 'helm-files)
(require 'helm-match-plugin)
(require 'which-func)
(require 'thingatpt)

(defgroup helm-gtags nil
  "GNU GLOBAL for helm"
  :group 'helm)

(defcustom helm-gtags-path-style 'root
  "Style of file path"
  :type '(choice (const :tag "Root of the current project" root)
                 (const :tag "Relative from the current directory" relative)
                 (const :tag "Absolute Path" absolute))
  :group 'helm-gtags)

(defcustom helm-gtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-read-only nil
  "Gtags read only mode."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-auto-update nil
  "*If non-nil, tag files are updated whenever a file is saved."
  :type 'boolean
  :group 'helm-gtags)

(defcustom helm-gtags-tag-location-list nil
  "tag locations."
  :type 'list
  :group 'helm-gtags)
(make-variable-buffer-local 'helm-gtags-tag-location-list)

(defcustom helm-gtags-default-candidate-limit 1000
  "default candidate limit."
  :type 'integer
  :group 'helm-gtags)

(defcustom helm-gtags-global-cmd "global"
  "where global command."
  :type 'string
  :group 'helm-gtags)

(defcustom helm-gtags-cmd "gtags"
  "where gtags command."
  :type 'string
  :group 'helm-gtags)

(defcustom helm-gtags-interval-seconds  (* 1 60) ; 1mins,update tag every min
  "in `after-save-hook' current-time - last-time must bigger than this value,
then `helm-gtags-update-tags' will be called,nil means update immidiately"
  :type 'integer
  :group 'helm-gtags)

(defvar helm-gtags-last-update-time (float-time (current-time))
  "`global -u --single-update'")

(defvar helm-gtags-tag-location nil
  "GNU global tag `GTAGS' location")

(defvar helm-gtags-buffer "*helm gtags*")

(defvar helm-gtags-prompt-alist
  '((:tag    . "Find Definition: ")
    (:rtag   . "Find Reference: ")
    (:symbol . "Find Symbol: ")
    (:file   . "Find File: ")))

(defvar helm-gtags-completing-history nil)
(defvar helm-gtags-context-stack nil)
(defvar helm-gtags-saved-context nil)
(defvar helm-gtags-use-otherwin nil)
(defvar helm-gtags-local-directory nil)
(defvar helm-gtags-parsed-file nil)

(defvar helm-gtags-update-tmp-buf " *helm-gtags-update TAGS*")

(defvar helm-gtags-tag-cache nil)
(defvar helm-gtags-rtag-cache nil)
(defvar helm-gtags-symbol-cache nil)
(defvar helm-gtags-file-cache nil)

(defvar helm-gtags-cache-alist
  `((:tag    . ,helm-gtags-tag-cache)
    (:rtag   . ,helm-gtags-rtag-cache)
    (:symbol . ,helm-gtags-symbol-cache)
    (:file   . ,helm-gtags-file-cache)))

(defvar helm-gtags-buf-alist
  '((:tag    . " *helm-gtags-tags*")
    (:rtag   . " *helm-gtags-rtags*")
    (:symbol . " *helm-gtags-symbol*")
    (:file   . " *helm-gtags-files*")
    (:tag-here   . " *helm-gtags-tags-here*")
    (:parse-file   . " *helm-gtags-tags-parse-file*")))

(defvar helm-gtags-command-option-alist
  '((:tag    . "")
    (:rtag   . "-r")
    (:symbol . "-s")
    (:file   . "-Poa")))

(defmacro helm-declare-obsolete-variable (old new version)
  `(progn
     (defvaralias ,old ,new)
     (make-obsolete-variable ,old ,new ,version)))

(helm-declare-obsolete-variable
 'helm-c-gtags-path-style 'helm-gtags-path-style "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-ignore-case 'helm-gtags-ignore-case  "0.8")
(helm-declare-obsolete-variable
 'helm-c-gtags-read-only 'helm-gtags-read-only "0.8")

(defsubst helm-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

(defun helm-gtags-find-tag-directory()
  (with-temp-buffer
    (let ((status (call-process helm-gtags-global-cmd nil  (current-buffer) nil  "-p")))
      (if (zerop status)
          (let ((tagroot (buffer-substring
                          (goto-char (point-min)) (line-end-position))))
            (setq helm-gtags-tag-location (file-truename (file-name-as-directory tagroot))))
        nil))))

(defun helm-gtags-base-directory ()
  ;; (message helm-gtags-local-directory)
  (or helm-gtags-local-directory
      (case helm-gtags-path-style
        (root helm-gtags-tag-location)
        (otherwise default-directory))))

(defun helm-source-gtags-complete-init()
  (let ((dirs (helm-attr 'helm-gtags-tag-location-list (helm-get-current-source)))
        (default-tag-dir (helm-gtags-searched-directory))
        (prefix (helm-gtags-token-at-point)))
    (when default-tag-dir (add-to-list 'dirs default-tag-dir ))
    (with-current-buffer (helm-candidate-buffer 'global)
      (dolist (dir dirs)
        (goto-char (point-max))
        (call-process helm-gtags-global-cmd nil (current-buffer) nil "-c" prefix)))))

(defvar helm-source-gtags-complete
  `((name . "GNU GLOBAL complete")
    (init . helm-source-gtags-complete-init)
    (candidates-in-buffer)
    (get-line . buffer-substring-no-properties)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (action . helm-gtags-complete-insert-action)))


(defun helm-gtags-complete-insert-action(cand)
  "insert candidate at point"
  (helm-gtags-delete-cur-symbol)
  (insert cand))

;;;###autoload
(defun helm-gtags-complete ()
  "Gtags Complete symbol at point"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-complete)
                     (helm-gtags-token-at-point)))

(defun helm-gtags-delete-cur-symbol()
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (delete-region (car bound) (cdr bound))
      ))
  )
(defun helm-gtags-token-at-point ()
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (buffer-substring-no-properties (car bound) (point))
      "")))

(defun helm-gtags-save-current-context ()
  (let ((file (buffer-file-name (current-buffer))))
    (setq helm-gtags-saved-context
          (list :file file :position (point) :readonly buffer-file-read-only))))

(defun helm-gtags-pop-context ()
  (unless helm-gtags-context-stack
    (error "Context stack is empty." ))
  (let* ((context (pop helm-gtags-context-stack))
         (file (plist-get context :file))
         (curpoint (plist-get context :position))
         (readonly (plist-get context :readonly)))
    (helm-gtags-open-file file readonly)
    (goto-char curpoint)))

(defun helm-gtags-open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defun helm-gtags-open-file-other-window (file readonly)
  (setq helm-gtags-use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

(defun helm-gtags-exec-global-cmd(type &optional input)
  (let (cmd-options
        (candidates-buf (get-buffer-create (assoc-default type helm-gtags-buf-alist)))
        (cache (assoc-default type helm-gtags-cache-alist))
        (dirs (helm-attr 'helm-gtags-tag-location-list (helm-get-current-source)))
        (default-tag-dir (helm-gtags-searched-directory))
        (buf-coding buffer-file-coding-system))
    (when default-tag-dir (add-to-list 'dirs default-tag-dir))
    (with-current-buffer candidates-buf
      (erase-buffer)
      (let (begin end (default-directory default-directory)
                  (coding-system-for-read buf-coding)
                  (coding-system-for-write buf-coding))
        (dolist (dir dirs)
          (setq cmd-options (helm-gtags-construct-command type dir input))
          (setq default-directory (helm-gtags-base-directory))
          (goto-char (point-max))
          (setq begin (point))
          (apply 'call-process helm-gtags-global-cmd nil (current-buffer) nil cmd-options)
          (setq end (point))
          (put-text-property begin end 'default-directory default-directory))
        (case type
          (:file  (setf (cdr (assoc type helm-gtags-cache-alist)) dirs))
          (otherwise (setf (cdr (assoc type helm-gtags-cache-alist)) input)))))
    candidates-buf))

(defun helm-gtags-find-tag-from-here-cands-cmd()
  (let ((candidates-buf (get-buffer-create (assoc-default :tag-here helm-gtags-buf-alist)))
        token from-here
        (dir default-directory)
        (buf-filename (file-truename (buffer-file-name helm-current-buffer))))
    (setq helm-gtags-local-directory nil)
    (with-current-buffer helm-current-buffer
      (helm-gtags-save-current-context)
      (setq token (or (thing-at-point 'symbol) ""))
      (setq from-here (format "--from-here=%d:%s" (line-number-at-pos) buf-filename))
      )
    (with-current-buffer candidates-buf
      (setq default-directory dir)
      (erase-buffer)
      (call-process helm-gtags-global-cmd nil (current-buffer) nil
                    "--result=grep" from-here token))
    candidates-buf))

(defun helm-gtags-candidates-in-buffer-tag-from-here()
  (helm-gtags-candidates-in-buffer
   (helm-gtags-find-tag-from-here-cands-cmd)))

(defun helm-gtags-construct-option (type &optional comp)
  (let ((type-opt (assoc-default type helm-gtags-command-option-alist))
        (result-opt (or (and (eq type :file) "") "--result=grep"))
        (abs-opt (or (and (eq helm-gtags-path-style 'absolute) "-a") ""))
        (case-opt (or (and helm-gtags-ignore-case "-i") ""))
        (comp-opt (or (and comp "-c") ""))
        (local-opt (or (and current-prefix-arg
                            (helm-gtags-type-is-not-file-p type) "-l") "")))
    (delete "" (list result-opt comp-opt type-opt abs-opt case-opt local-opt))))

(defun helm-gtags-construct-command (type dir &optional in)
  (setq helm-gtags-local-directory nil)
  (when dir
    (setq helm-gtags-local-directory dir))
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern)))) ;
        (option (helm-gtags-construct-option type)))
    (when (and (string= input "") (helm-gtags-type-is-not-file-p type))
      (error "Input is empty!!"))
    (append option (list input))))


(defun helm-gtags-candidates-in-buffer-parse-file()
  (helm-gtags-candidates-in-buffer
   (helm-gtags-parse-file-cmd)))

(defun helm-gtags-parse-file-cmd()
  (let ((candidates-buf (get-buffer-create (assoc-default :parse-file helm-gtags-buf-alist)))
        (dir default-directory))
    (with-current-buffer helm-current-buffer (helm-gtags-save-current-context))
    (with-current-buffer candidates-buf
      (erase-buffer)
      (setq default-directory dir)
      (unless (zerop (call-process helm-gtags-global-cmd nil (current-buffer) nil
                                   "--result" "cscope" "-f" helm-gtags-parsed-file))
        (error "Failed: global --result cscope -f \"%s\"" helm-gtags-parsed-file)))
    candidates-buf))

(defun helm-gtags-push-context (context)
  (unless(member context helm-gtags-context-stack)
    (push  context helm-gtags-context-stack)))

(defun helm-gtags-select-find-file-func ()
  (if helm-gtags-use-otherwin
      'helm-gtags-open-file-other-window
    'helm-gtags-open-file))

(defun helm-gtags-do-open-file (open-func file line)
  (funcall open-func file helm-gtags-read-only)
  (goto-char (point-min))
  (forward-line (1- line))
  (helm-gtags-push-context helm-gtags-saved-context))

(defun helm-gtags-parse-file-action (cand)
  (let ((line (when (string-match "\\s-+\\([1-9][0-9]*\\)\\s-+" cand)
                (string-to-number (match-string 1 cand))))
        (open-func (helm-gtags-select-find-file-func)))
    (helm-gtags-do-open-file open-func helm-gtags-parsed-file line)))

(defun helm-gtags-action-openfile (_elm)
  (let* ((elm (helm-get-selection nil 'withprop))
         (elems (split-string elm ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (open-func (helm-gtags-select-find-file-func))
         (default-directory (or (get-text-property 0 'default-directory elm) (helm-gtags-base-directory))))
    (helm-gtags-do-open-file open-func filename line)))

(defun helm-gtags-file-content-at-pos (file pos)
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char pos)
      (let ((curfunc (which-function))
            (line (line-number-at-pos))
            (content (or (buffer-substring
                          (line-beginning-position) (line-end-position))
                         "")))
        (format "%s:%d%s:%s\n"
                file line
                (helm-aif curfunc (format "[%s]" it) "")
                content)))))

(defun helm-gtags-show-stack-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (loop for context in (reverse helm-gtags-context-stack)
          for file = (plist-get context :file)
          for pos  = (plist-get context :position)
          do
          (insert (helm-gtags-file-content-at-pos file pos)))))

(defun helm-gtags-tags-persistent-action (_cand)
  (let* ((elems (split-string (helm-get-selection nil 'withprop) ":"))
         (filename (first elems))
         (line (string-to-number (second elems)))
         (default-directory (helm-gtags-base-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-match-line-color-current-line)))

(defun helm-gtags-use-cache-p(type input cache)
  (case type
    (:file
     (let ((buf-filename (buffer-file-name (current-buffer))))
       (or (null buf-filename)
           (and cache
                (some
                 '(lambda (dir)
                    (string-match (regexp-quote dir)
                                  (file-truename buf-filename)))cache)))))
    (otherwise
     (string-equal input cache))))

(defun helm-gtags-get-candidates-buf-with-cache(type &optional in)
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern))))
        (candidates-buf (get-buffer (assoc-default type helm-gtags-buf-alist)))
        (cache (assoc-default type helm-gtags-cache-alist)))
    (with-current-buffer helm-current-buffer (helm-gtags-save-current-context))
    (if (and cache (bufferp candidates-buf)(buffer-live-p candidates-buf)
             (helm-gtags-use-cache-p type input cache))
        candidates-buf
      (helm-gtags-exec-global-cmd type input))))

(defun helm-gtags-candidates-in-buffer-rtag(&optional in)
  (helm-gtags-candidates-in-buffer
   (helm-gtags-get-candidates-buf-with-cache
    :rtag in)))

(defun helm-gtags-candidates-in-buffer-tag(&optional in)
  (helm-gtags-candidates-in-buffer
   (helm-gtags-get-candidates-buf-with-cache :tag in)))

(defun helm-gtags-candidates-in-buffer-symbol(&optional in)
  (helm-gtags-candidates-in-buffer
   (helm-gtags-get-candidates-buf-with-cache :symbol in)))


(defun helm-gtags-candidates-in-buffer-file(&optional in)
  (helm-gtags-candidates-in-buffer
   (helm-gtags-get-candidates-buf-with-cache :file in)))

(defun helm-gtags-candidates-in-buffer(buf)
  (helm-candidates-in-buffer-1
   buf helm-pattern
   (or (assoc-default 'get-line source)
       #'buffer-substring-no-properties)
   ;; use external variable `source'.
   (or (assoc-default 'search source)
       (if (assoc 'search-from-end source)
           '(helm-candidates-in-buffer-search-from-end)
         '(helm-candidates-in-buffer-search-from-start)))
   (helm-candidate-number-limit source)
   (assoc 'search-from-end source)
   (helm-attr 'match-part)))


(defvar helm-source-gtags-tags
  '((name . "tag")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-tag)
    (get-line . buffer-substring)
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-gsyms
  '((name . "symbol")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-symbol)
    (get-line . buffer-substring)
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-rtags
  '((name . "rtags")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-rtag)
    (get-line . buffer-substring)
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defun helm-gtags-files-candidate-transformer (file)
  (let ((removed-regexp (format "^%s" helm-gtags-tag-location)))
    (replace-regexp-in-string removed-regexp "" file)))

(defun helm-gtags-parse-file-candidate-transformer (file)
  (let ((removed-file (replace-regexp-in-string "\\`\\S-+ " "" file)))
    (when (string-match "\\`\\(\\S-+\\) \\(\\S-+\\) \\(.+\\)\\'" removed-file)
      (format "%-25s %-5s %s"
              (match-string 1 removed-file)
              (match-string 2 removed-file)
              (match-string 3 removed-file)))))

(defvar helm-source-gtags-files
  `((name . "gnu global files")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-file)
    (get-line . buffer-substring-no-properties)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (type . file)))

(defvar helm-source-gtags-find-tag-from-here
  `((name . "tags")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-tag-from-here)
    (get-line . buffer-substring)
    (persistent-action . helm-gtags-tags-persistent-action)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (action . helm-gtags-action-openfile)))

(defvar helm-source-gtags-parse-file
  `((name . "GNU GLOBAL Parsed File")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-parse-file)
    (get-line . buffer-substring-no-properties)
    (real-to-display . helm-gtags-parse-file-candidate-transformer)
    (action . helm-gtags-parse-file-action)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)))

(defvar helm-source-gtags-show-stack
  `((name . "Show Context Stack")
    (init . helm-gtags-show-stack-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (real-to-display . helm-gtags-files-candidate-transformer)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

;;;###autoload
(defun helm-gtags-select ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-select) "") )

(defun helm-source-gtags-select-tag (candidate)
  `((name . "tags")
    (candidates .  (lambda ()
                     (helm-gtags-candidates-in-buffer-tag ,candidate)) )
    (volatile);;candidates
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile))
  )


(defun helm-source-gtags-select-rtag (candidate)
  `((name . "rtags")
    (candidates . helm-gtags-candidates-in-buffer-rtag)
    (volatile);;candidates
    (delayed)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defun helm-source-gtags-select-tag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-tag ,c)) ,c))))

(defun helm-source-gtags-select-rtag-action (c)
  (helm-run-after-quit
   `(lambda ()
      (helm-gtags-common (list (helm-source-gtags-select-rtag ,c)) ,c))))

(defun helm-source-gtags-select-init()
  (let (candidates
        ;; (cmd "global -c")
        (dirs (helm-attr 'helm-gtags-tag-location-list (helm-get-current-source)))
        (default-tag-dir (helm-gtags-searched-directory))
        (buf-coding buffer-file-coding-system)
        )
    (when default-tag-dir (add-to-list 'dirs default-tag-dir ))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let (begin end
                  (default-directory default-directory)
                  (coding-system-for-read buf-coding)
                  (coding-system-for-write buf-coding)
                  )
        (dolist (dir dirs)
          (setq default-directory (helm-gtags-base-directory))
          (goto-char (point-max))
          (setq begin (point))
          (call-process helm-gtags-global-cmd nil (current-buffer) nil "-c")
          ;; (call-process-shell-command cmd nil t)
          (setq end (point))
          (put-text-property begin end 'default-directory default-directory)
          )))))

(defvar helm-source-gtags-select
  `((name . "GNU GLOBAL SELECT")
    (init . helm-source-gtags-select-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (candidate-number-limit . ,helm-gtags-default-candidate-limit)
    (action . (("Goto the location" . helm-source-gtags-select-tag-action)
               ("Goto the location(other buffer)" .
                (lambda (c)
                  (setq helm-gtags-use-otherwin t)
                  (helm-source-gtags-select-tag-action c)))
               ("Move to the referenced point" .
                helm-source-gtags-select-rtag-action)))))

(defun helm-gtags-searched-directory ()
  (case (prefix-numeric-value current-prefix-arg)
    (4 (let ((dir (read-directory-name "Input Directory: ")))
         (setq helm-gtags-local-directory (file-name-as-directory (file-truename dir)))))
    (16 (file-name-directory (file-truename (buffer-file-name))))
    (t
     (let ((dir (helm-gtags-find-tag-directory)))
       (if dir
           (file-name-directory dir)
         nil)))))

(defsubst helm-gtags--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))


(defun helm-gtags-common (srcs &optional input)
  (let ((helm-quit-if-no-candidate #'(lambda() (message "gtags:not found")))
        (helm-execute-action-at-once-if-one t)
        (buf (get-buffer-create helm-gtags-buffer))
        (dir (helm-gtags-searched-directory))
        (custom-dirs (mapcar (lambda(Dir) (file-name-as-directory Dir))
                             helm-gtags-tag-location-list))
        (src (car srcs)))
    (when dir (add-to-list 'custom-dirs dir))
    (when (helm-gtags--using-other-window-p) (setq helm-gtags-use-otherwin t))
    (dolist (src srcs)
      (when (symbolp src) (setq src (symbol-value src)))
      (unless (helm-attr 'init-name src) (helm-attrset 'init-name  (helm-attr 'name src) src))
      (helm-attrset 'helm-gtags-tag-location-list custom-dirs src)
      (helm-attrset 'name
                    (format "Searched %s at %s" (or (helm-attr 'init-name src) "")
                            (mapconcat 'identity custom-dirs "  "))
                    src))
    (helm :sources srcs
          :input (or input (thing-at-point 'symbol))
          :buffer buf)))

;;;###autoload
(defun helm-gtags-find-tag-and-symbol ()
  "Jump to definition"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags
                       helm-source-gtags-gsyms)))

;;;###autoload
(defun helm-gtags-find-tag ()
  "Jump to definition"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags)))

;;;###autoload
(defun helm-gtags-find-rtag ()
  "Jump to referenced point"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-rtags)))

;;;###autoload
(defun helm-gtags-find-symbol ()
  "Jump to the symbol location"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-gsyms)))

;;;###autoload
(defun helm-gtags-find-files ()
  "Find file with gnu global
you could add `helm-source-gtags-files' to `helm-for-files-preferred-list'"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-files) ""))

;;;###autoload
(defun helm-gtags-find-tag-from-here()
  "Find from here with gnu global"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-find-tag-from-here)))

(defun helm-gtags-set-parsed-file ()
  (let* ((this-file (file-name-nondirectory (buffer-file-name)))
         (file (if current-prefix-arg
                   (read-file-name "Parsed File: " nil this-file)
                 this-file)))
    (setq helm-gtags-parsed-file (file-truename (expand-file-name file)))))

;;;###autoload
(defun helm-gtags-parse-file ()
  "parse file with gnu global"
  (interactive)
  ;; (helm-gtags-find-tag-directory)
  (helm-gtags-save-current-context)
  (when (helm-gtags--using-other-window-p)
    (setq helm-gtags-use-otherwin t))
  (helm-gtags-set-parsed-file)
  (helm-attrset 'name
                (format "Parsed File: %s"
                        helm-gtags-parsed-file)
                helm-source-gtags-parse-file)
  (helm :sources '(helm-source-gtags-parse-file)
        :buffer (get-buffer-create helm-gtags-buffer)))

;;;###autoload
(defun helm-gtags-pop-stack ()
  "Jump to previous point on the stack"
  (interactive)
  (helm-gtags-pop-context))

;;;###autoload
(defun helm-gtags-show-stack ()
  "Show context stack"
  (interactive)
  (helm-other-buffer 'helm-source-gtags-show-stack
                     (get-buffer-create helm-gtags-buffer)))

;;;###autoload
(defun helm-gtags-clear-stack ()
  "Clear jumped point stack"
  (interactive)
  (setq helm-gtags-context-stack nil))

(defsubst helm-gtags--update-tags-params ( &optional current-prefix-arg)
  (case (prefix-numeric-value current-prefix-arg)
    (4                                  ;C-u
     (cons helm-gtags-global-cmd  (list "-u")))
    (16                                 ;C-uC-u
     (let* ((tagdir-with-slash
             (file-truename (expand-file-name
                             (read-directory-name "Generate GTAGS at directory:"))))
            (len-of-tagdir (length tagdir-with-slash))
            (tagdir-without-slash-appended (substring tagdir-with-slash 0 (1- len-of-tagdir))))
       ;; on windows "gtags  d:/.emacs.d"  works , but "gtags d:/.emacs.d/" doesn't
       (cons helm-gtags-cmd
             (list tagdir-without-slash-appended))))
    (t
     (cons helm-gtags-global-cmd
           (list "-u" (format "--single-update=%s" (file-truename (buffer-file-name))))))))

;;;###autoload
(defun helm-gtags-update-tags ()
  "Update TAG file. Update All files with `C-u' prefix,
Generate new TAG file in selected directory with `C-uC-u'"
  (interactive)
  (when (and (not (get-buffer helm-gtags-update-tmp-buf)) ;not already running
             (or (called-interactively-p 'interactive) ;if call interactively, update immidiately
                 (and (buffer-file-name)                    ;update current file
                      (or (null helm-gtags-interval-seconds)   ;nil means update immidiately
                          (> (- (float-time (current-time)) ;
                                helm-gtags-last-update-time)
                             helm-gtags-interval-seconds))
                      )))
    (let* ((cmd-and-params (helm-gtags--update-tags-params current-prefix-arg))
           (cmd (car cmd-and-params))
           (params (cdr cmd-and-params))
           (proc (apply 'start-process ;;
                        "helm-gtags-update TAGS" helm-gtags-update-tmp-buf
                        cmd params)))
      (set-process-query-on-exit-flag proc nil) ;neednot query when quit emacs
      (setq helm-gtags-last-update-time (float-time (current-time)));;update time
      (unless proc (message "Failed to update GNU Global TAGS" )
              (kill-buffer helm-gtags-update-tmp-buf))
      (set-process-sentinel proc 'helm-gtags--update-tags-sentinel))))

(defun helm-gtags--update-tags-sentinel (process state)
  (when (eq (process-status process) 'exit)
    (kill-buffer helm-gtags-update-tmp-buf)
    (if (zerop (process-exit-status process))
        (message "Update TAGS successfully")
      (message "Failed to update TAGS"))))


(defvar helm-gtags-mode-name " HGtags")
(defvar helm-gtags-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-gtags-mode ()
  "Enable for helm-gtags"
  :group      'helm-gtags
  :init-value nil
  :global     nil
  :keymap     helm-gtags-mode-map
  :lighter    helm-gtags-mode-name
  (if helm-gtags-mode
      (progn
        (run-hooks 'helm-gtags-mode-hook)
        (when helm-gtags-auto-update
          (add-hook 'after-save-hook 'helm-gtags-update-tags nil t)))
    (progn
      (when helm-gtags-auto-update
        (remove-hook 'after-save-hook 'helm-gtags-update-tags t)))))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
