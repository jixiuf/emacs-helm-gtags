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
;;     you neednot set this if you just use one tag
;;     (setq helm-gtags-tag-location-alist
;;       '((c-mode  "/usr/include/" "/usr/kernel/")
;;         (c++-mode  "/path/of/tag/2/" "/path/of/tag/3/")))

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
(require 'helm-match-plugin)
(require 'helm-utils)
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

(defcustom helm-gtags-tag-location-alist nil
  "tag locations for differnt major-mode.
eq: '((c-mode . '(\"/usr/include/\" \"/usr/kernel/\"))
  (c++-mode . '(\"/path/of/tag/2/\" \"/path/of/tag/3/\"))),
you neednot set this if you just use one tag"
  :type 'alist
  :group 'helm-gtags)


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

(defcustom helm-gtags-select-before-hook nil
  ""
  :group 'helm-gtags
  :type 'hook)

(defcustom helm-gtags-goto-line-before-hook nil
  ""
  :group 'helm-gtags
  :type 'hook)

(defcustom helm-gtags-quit-or-no-candidates-hook nil
  ""
  :group 'helm-gtags
  :type 'hook)

(defvar helm-gtags-last-update-time (float-time (current-time))
  "`global -u --single-update'")

(defvar helm-gtags-buffer "*helm gtags*")

(defvar helm-gtags-prompt-alist
  '((:tag    . "Find Definition: ")
    (:rtag   . "Find Reference: ")
    (:symbol . "Find Symbol: ")
    (:file   . "Find File: ")))

(defvar helm-gtags-use-otherwin nil)
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
  '((:tag        . " *helm-gtags-tags*")
    (:rtag       . " *helm-gtags-rtags*")
    (:symbol     . " *helm-gtags-symbol*")
    (:file       . " *helm-gtags-files*")
    (:tag-here   . " *helm-gtags-tags-here*")
    (:parse-file . " *helm-gtags-tags-parse-file*")))

(defvar helm-gtags-command-option-alist
  '((:tag    . "")
    (:rtag   . "-r")
    (:symbol . "-s")
    (:file   . "-Poa")))

(defun helm-gtags-set-tag-location-alist(key value)
  (let ((kv (assoc key  helm-gtags-tag-location-alist)))
    (if kv
        (setf (cdr (assoc key  helm-gtags-tag-location-alist)) value)
      (add-to-list 'helm-gtags-tag-location-alist `(,key . ,value)))))

(defun helm-gtags-get-tag-location-alist(mode)
  (mapcar (lambda(tmp-dir)
            (file-name-as-directory
             (file-truename (expand-file-name tmp-dir))))
          (assoc-default major-mode helm-gtags-tag-location-alist)))

(defun helm-gtags-delete-cur-symbol()
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (delete-region (car bound) (cdr bound)))))

(defun helm-gtags-token-at-point ()
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound (buffer-substring-no-properties
               (car bound) (point))
      "")))

(defsubst helm-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

(defun helm-gtags-find-tag-directory()
  (let((mode major-mode)
       (dirs (helm-gtags-get-tag-location-alist major-mode)))
    (with-temp-buffer
      (let ((status (call-process helm-gtags-global-cmd nil  (current-buffer) nil  "-p"))
            tag-location)
        (if (zerop status)
            (let ((tagroot (buffer-substring
                            (goto-char (point-min)) (line-end-position))))
              (setq tag-location (file-truename (file-name-as-directory tagroot)))
              (add-to-list 'dirs  tag-location)
              (helm-gtags-set-tag-location-alist mode dirs)
              tag-location
              )nil)))))

(defun helm-gtags-searched-directory(&optional nosearch default)
  (let((buf-filename (buffer-file-name))
       (dirs (helm-gtags-get-tag-location-alist major-mode))
       (i 0) tagroot dir found)
    (if (null buf-filename)
        (or default (file-truename (expand-file-name default-directory)))
      (while (and (< i (length dirs))
                  (null tagroot))
        (setq dir (nth i dirs))
        (when (string-match (regexp-quote dir) (file-truename buf-filename))
          (setq tagroot dir))
        (setq i (1+ i)))
      (if tagroot tagroot
        (if nosearch (or default (file-truename (expand-file-name default-directory)))
          (or (helm-gtags-find-tag-directory)
              (or default (file-truename (expand-file-name default-directory)))))))))

(defun helm-source-gtags-complete-init()
  (let ((dirs (helm-gtags-get-tag-location-alist major-mode))
        (dir (helm-gtags-searched-directory nil nil))
        (prefix (helm-gtags-token-at-point))
        (mode major-mode)
        (default-directory default-directory)
        begin)
    (when dir (add-to-list 'dirs dir))
    (helm-gtags-set-tag-location-alist major-mode dirs)

    (with-current-buffer (helm-candidate-buffer 'global)
      (dolist (dir dirs)
        (setq default-directory dir)
        (goto-char (point-max))
        (setq begin (point))
        (unless (zerop (call-process helm-gtags-global-cmd nil (current-buffer) nil "-c" prefix))
          (when (string-match "global: GTAGS not found." (buffer-substring-no-properties begin (point)))
            (helm-gtags-set-tag-location-alist mode (delete dir dirs)))
          (delete-region begin (point)))))))

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

(defun helm-gtags-find-tag-from-here-cands-cmd()
  (let ((candidates-buf (get-buffer-create (assoc-default :tag-here helm-gtags-buf-alist)))
        token from-here
        (dir default-directory)
        (buf-filename (file-truename (buffer-file-name helm-current-buffer))))
    (with-current-buffer helm-current-buffer
      (run-hooks 'helm-gtags-select-before-hook)
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
    (with-current-buffer candidates-buf
      (erase-buffer)
      (setq default-directory dir)
      (unless (zerop (call-process helm-gtags-global-cmd nil (current-buffer) nil
                                   "--result" "cscope" "-f" helm-gtags-parsed-file))
        (error "Failed: global --result cscope -f \"%s\"" helm-gtags-parsed-file)))
    candidates-buf))

(defun helm-gtags-parse-file-action (cand)
  (let ((line (when (string-match "\\s-+\\([1-9][0-9]*\\)\\s-+" cand)
                (string-to-number (match-string 1 cand))))
        (open-func (helm-gtags-select-find-file-func)))
    (helm-gtags-do-open-file open-func helm-gtags-parsed-file line)))

(defun helm-gtags-action-openfile (_elm)
  (let* ((elm (helm-get-selection nil 'withprop))
         (elems (helm-gtags-split-line elm))
         (filename (or (get-text-property 0 'filename elm) (first elems)))
         (line (string-to-number (second elems)))
         (open-func (helm-gtags-select-find-file-func))
         (default-directory (or (get-text-property 0 'default-directory elm) (helm-gtags-searched-directory))))
    (helm-gtags-do-open-file open-func filename line)))

(defun helm-gtags-select-find-file-func()
  (if helm-gtags-use-otherwin
      'helm-gtags-open-file-other-window
    'helm-gtags-open-file))

(defun helm-gtags-do-open-file (open-func file line)
  (run-hooks 'helm-gtags-goto-line-before-hook)
  (funcall open-func file helm-gtags-read-only)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation))

(defun helm-gtags-open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defsubst helm-gtags--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defun helm-gtags-open-file-other-window (file readonly)
  (setq helm-gtags-use-otherwin nil)
  (if readonly
      (find-file-read-only-other-window file)
    (find-file-other-window file)))

(defun helm-gtags-split-line (line)
  "Split a output line."
  (when (string-match "^\\([a-zA-Z]?:?.*?\\):\\([0-9]+\\)" line)
    ;; Don't use split-string because buffer/file name or string
    ;; may contain a ":".
    (loop for n from 1 to 3 collect (match-string n line))))

(defun helm-gtags-parse-file-candidate-transformer (file)
  (let ((removed-file (replace-regexp-in-string "\\`\\S-+ " "" file)))
    (when (string-match "\\`\\(\\S-+\\) \\(\\S-+\\) \\(.+\\)\\'" removed-file)
      (format "%-25s %-5s %s"
              (match-string 1 removed-file)
              (match-string 2 removed-file)
              (match-string 3 removed-file)))))


(defun helm-gtags-set-parsed-file ()
  (let* ((this-file (file-name-nondirectory (buffer-file-name)))
         (file (if current-prefix-arg
                   (read-file-name "Parsed File: " nil this-file)
                 this-file)))
    (setq helm-gtags-parsed-file (file-truename (expand-file-name file)))))

(defun helm-gtags-tags-persistent-action (_cand)
  (let* ((c (helm-get-selection nil 'withprop))
         (elems (helm-gtags-split-line c))
         (filename (or (get-text-property 0 'filename c) (first elems)))
         (line (string-to-number (second elems)))
         (default-directory (or (get-text-property 0 'default-directory c)
                                (helm-gtags-searched-directory))))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))


(defun helm-gtags-common (srcs &optional input)
  (let ((helm-quit-if-no-candidate #'(lambda()
                                       (with-current-buffer helm-current-buffer
                                         (run-hooks 'helm-gtags-quit-or-no-candidates-hook))
                                       (message "gtags:not found")))
        (helm-execute-action-at-once-if-one t)
        (buf (get-buffer-create helm-gtags-buffer))
        (custom-dirs (helm-gtags-get-tag-location-alist major-mode))
        (src (car srcs)))
    (when (helm-gtags--using-other-window-p) (setq helm-gtags-use-otherwin t))
    (dolist (src srcs)
      (when (symbolp src) (setq src (symbol-value src)))
      (unless (helm-attr 'init-name src) (helm-attrset 'init-name  (helm-attr 'name src) src))
      (helm-attrset 'name
                    (format "Searched %s at %s" (or (helm-attr 'init-name src) "")
                            (mapconcat 'identity custom-dirs "  "))
                    src))
    (run-hooks 'helm-gtags-select-before-hook)
    (helm :sources srcs
          :input (or input (thing-at-point 'symbol))
          :buffer buf)
    (when (eq 1 helm-exit-status)
      (run-hooks 'helm-gtags-quit-or-no-candidates-hook))))

(defun helm-gtags-exec-global-cmd(type &optional input)
  (let ((candidates-buf (get-buffer-create (assoc-default type helm-gtags-buf-alist)))
        ;; (cache-info (assoc-default type helm-gtags-cache-alist))
        (buf-coding buffer-file-coding-system)
        mode cmd-options dirs dir)
    (with-current-buffer helm-current-buffer
      (setq mode major-mode)
      (setq dir (helm-gtags-searched-directory nil nil))
      (setq dirs (helm-gtags-get-tag-location-alist mode))
      (when dir (add-to-list 'dirs dir))
      (helm-gtags-set-tag-location-alist mode dirs)
      (case type
        (:file  (setf (cdr (assoc type helm-gtags-cache-alist)) (list mode input dirs)))
        (otherwise (setf (cdr (assoc type helm-gtags-cache-alist)) (list mode input)))))

    (with-current-buffer candidates-buf
      (erase-buffer)
      (let (begin end (default-directory default-directory)
                  (coding-system-for-read buf-coding)
                  (coding-system-for-write buf-coding))
        (dolist (dir dirs)
          (setq cmd-options (helm-gtags-construct-command type dir input))
          (setq default-directory dir)
          (goto-char (point-max))
          (setq begin (point))
          (if (zerop (apply 'call-process helm-gtags-global-cmd nil (current-buffer) nil cmd-options))
              (progn
                (setq end (point))
                (put-text-property begin end 'default-directory default-directory))
            (when (string-match "global: GTAGS not found." (buffer-substring-no-properties begin (point)))
              (helm-gtags-set-tag-location-alist mode (delete dir dirs)))
            (delete-region begin (point))))))
    candidates-buf))

(defun helm-gtags-use-cache-p(type input cache-info)
  (with-current-buffer helm-current-buffer
    (let ((mode (car cache-info))
          (cache-input (nth 1 cache-info))
          (buf-filename (buffer-file-name (current-buffer))))
      (case type
        (:file
         (let((cache-dirs (nth 2 cache-info)))
           (or (null buf-filename)
               (and (equal major-mode mode)
                    (string-equal input cache-input)
                    (some
                     '(lambda (dir)
                        (string-match (regexp-quote dir)
                                      (file-truename buf-filename))) cache-dirs)))))
        (otherwise
         (and (string-equal input cache-input)
              (equal major-mode mode)))))))


(defun helm-gtags-get-candidates-buf-with-cache(type &optional in)
  (let ((input (or in (car (helm-mp-split-pattern helm-pattern))))
        (candidates-buf (get-buffer (assoc-default type helm-gtags-buf-alist)))
        (cache-info (assoc-default type helm-gtags-cache-alist)))
    (if (and cache-info (bufferp candidates-buf)(buffer-live-p candidates-buf)
             (helm-gtags-use-cache-p type input cache-info))
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


(defvar helm-source-gtags-files
  `((name . "gnu global files")
    (candidates-in-buffer . helm-gtags-candidates-in-buffer-file)
    (get-line . buffer-substring-no-properties)
    ;; (real-to-display . helm-gtags-files-candidate-transformer)
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

(defun helm-source-gtags-select-tag (candidate)
  `((name . "tags")
    (candidates-in-buffer . (lambda ()
                              (helm-gtags-candidates-in-buffer-tag ,candidate)))
    (get-line . buffer-substring)
    (delayed)
    (persistent-action . helm-gtags-tags-persistent-action)
    (action . helm-gtags-action-openfile)))

(defun helm-source-gtags-select-rtag (candidate)
  `((name . "rtags")
    (candidates-in-buffer . (lambda ()
                              (helm-gtags-candidates-in-buffer-rtag ,candidate)))
    (get-line . buffer-substring)
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
        (dirs (helm-gtags-get-tag-location-alist major-mode))
        (buf-coding buffer-file-coding-system))
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((begin)( end)
            (default-directory default-directory)
            (coding-system-for-read buf-coding)
            (coding-system-for-write buf-coding))
        (dolist (dir dirs)
          (setq default-directory dir)
          (goto-char (point-max))
          (setq begin (point))
          (call-process helm-gtags-global-cmd nil (current-buffer) nil "-c")
          (setq end (point))
          (put-text-property begin end 'default-directory default-directory))))))

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
(defun helm-gtags-find-symbol()
  "Jump to the symbol location"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-gsyms)))

;;;###autoload
(defun helm-gtags-find-files(&optional input)
  "Find file with gnu global
you could add `helm-source-gtags-files' to `helm-for-files-preferred-list'"
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm-gtags-common '(helm-source-gtags-files) (or input ""))))

;;;###autoload
(defun helm-gtags-select ()
  (interactive)
  (helm-gtags-common '(helm-source-gtags-select) "") )

;;;###autoload
(defun helm-gtags-find-tag-from-here()
  "Find from here with gnu global"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-find-tag-from-here)))

;;;###autoload
(defun helm-gtags-parse-file()
  "parse file with gnu global"
  (interactive)
  (run-hooks 'helm-gtags-select-before-hook)
  (when (helm-gtags--using-other-window-p)
    (setq helm-gtags-use-otherwin t))
  (helm-gtags-set-parsed-file)
  (helm-attrset 'name
                (format "Parsed File: %s"
                        helm-gtags-parsed-file)
                helm-source-gtags-parse-file)
  ;; (helm-execute-action-at-once-if-one t)
  (let ((helm-quit-if-no-candidate #'(lambda()
                                       (with-current-buffer helm-current-buffer
                                         (run-hooks 'helm-gtags-quit-or-no-candidates-hook))
                                       (message "gtags:no candidates"))))
    (helm :sources '(helm-source-gtags-parse-file)
          :buffer (get-buffer-create helm-gtags-buffer))
    (when (eq 1 helm-exit-status)
      (run-hooks 'helm-gtags-quit-or-no-candidates-hook))))

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
