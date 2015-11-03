;;; helm-gtags.el --- GNU GLOBAL helm interface

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; 纪秀峰 fork version
;; URL: https://github.com/jixiuf/emacs-helm-gtags
;; Author: 纪秀峰 <jixiuf@gmail.com>
;; Version: 2.3
;; Package-Requires: ((helm "1.8"))

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
;;                  (local-set-key (kbd "M-g M-p") 'helm-gtags-parse-file)
;;                  (local-set-key (kbd "C-c C-f") 'helm-gtags-find-files)))
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'helm)
(require 'helm-multi-match)
(require 'helm-types)
(require 'helm-source)
(require 'helm-utils)
(require 'thingatpt)
(require 'tramp)

(declare-function pulse-momentary-highlight-one-line "pulse")
(declare-function pulse-momentary-highlight-region "pulse")
(declare-function cl-some "cl-extra")

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

(defcustom helm-gtags-auto-update t
  "*If non-nil, tag files are updated whenever a file is saved.
you need enable `helm-gtags-mode'"
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

(defcustom helm-gtags-input-idle-delay helm-input-idle-delay ;
  "`helm-input-idle-delay' for helm-gtags source."
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
(defcustom helm-gtags-goto-line-after-hook nil
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
(defvar helm-gtags-eldoc-tmp-buf " *helm-gtags-eldoc*")

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
  (remq nil (mapcar (lambda(tmp-dir)
                      (if (file-directory-p tmp-dir)
                          (file-name-as-directory
                           (file-truename (expand-file-name tmp-dir)))nil))
                    (assoc-default major-mode helm-gtags-tag-location-alist))))

(defun helm-gtags-delete-cur-symbol()
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound
        (delete-region (car bound) (cdr bound)))))

(defun helm-gtags-token-at-point ()
  "token before point"
  (let ((bound (bounds-of-thing-at-point 'symbol)))
    (if bound (buffer-substring-no-properties
               (car bound) (point))
      "")))

(defsubst helm-gtags-type-is-not-file-p (type)
  (not (eq type :file)))

(defun helm-gtags-local-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (if (file-remote-p buffile)
        (tramp-file-name-localname (tramp-dissect-file-name buffile))
      (file-truename buffile))))

(defsubst helm-gtags-read-gtagslabel ()
  (let ((labels '("--gtagslabel=default" "--gtagslabel=native"
                 "--gtagslabel=ctags"  "--gtagslabel=pygments")))
    (completing-read "GTAGS LABEL(Default: default): "
                     labels nil t nil nil "--gtagslabel=default")))

(defun helm-gtags-check-tags-exists()
  (or (helm-gtags-find-tagroot)
      (if (not (yes-or-no-p "File GTAGS not found. generate now? "))
          (user-error "Abort")
        (let* ((tagroot (read-directory-name "Generate tags at Directory: "))
               (label (helm-gtags-read-gtagslabel))
               (default-directory tagroot))
          (message "gtags is generating tags....")
          (if (zerop (process-file "gtags" nil nil nil "-q" label))
              (message "generating tags done!!!")
            (error "Faild: 'gtags -q'"))
          tagroot))))

(defun helm-gtags-find-tagroot(&optional with-process-p)
  "return nil or tag root dir.
if `with-process-p' not nil then use global -p find gtagsroot"
  (helm-aif (or (getenv "GTAGSROOT") (locate-dominating-file default-directory "GTAGS"))
      (file-truename it)
    (and with-process-p
         (with-temp-buffer
           (when (zerop (process-file helm-gtags-global-cmd nil (current-buffer) nil "-p"))
             (let (tag-location
                   (tagroot (buffer-substring-no-properties
                             (goto-char (point-min)) (line-end-position))))
               (setq tag-location (file-name-as-directory tagroot))
               (helm-aif (file-remote-p default-directory)
                   (setq tag-location (concat it tag-location)))
               tag-location))))))

(defun helm-gtags-move-tagroot-first(&optional majormode)
  "make sure current tagroot is the first element of dirs if exists.
and return the dirs"
  (let*((mode (or majormode major-mode))
       (dirs (helm-gtags-get-tag-location-alist mode))
       (tagroot (helm-gtags-find-tagroot t)))
    (when tagroot
      (setq dirs (delete tagroot dirs))
      (add-to-list 'dirs  tagroot)      ;insert at header
      (helm-gtags-set-tag-location-alist mode dirs))
    dirs))

(defun helm-source-gtags-complete-init()
  (let ((dirs (helm-gtags-get-tag-location-alist major-mode))
        (prefix (helm-gtags-token-at-point))
        (mode major-mode)
        (default-directory default-directory)
        begin)
    (with-current-buffer (helm-candidate-buffer 'global)
      (dolist (dir dirs)
        (setq default-directory dir)
        (goto-char (point-max))
        (setq begin (point))
        (unless (zerop (process-file helm-gtags-global-cmd nil (current-buffer) nil "-c" prefix))
          (when (string-match "global: GTAGS not found." (buffer-substring-no-properties begin (point)))
            (helm-gtags-set-tag-location-alist mode (delete dir dirs)))
          (delete-region begin (point)))))))

(defvar helm-source-gtags-complete
    (helm-build-in-buffer-source "GNU GLOBAL complete"
    :init 'helm-source-gtags-complete-init
    :candidate-number-limit helm-gtags-default-candidate-limit
    :action 'helm-gtags-complete-insert-action))

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
      (process-file helm-gtags-global-cmd nil (current-buffer) nil
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
  (let ((input (or in (car (helm-mm-split-pattern helm-pattern)))) ;
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
      (unless (zerop (process-file helm-gtags-global-cmd nil (current-buffer) nil
                                   "--result" "cscope" "-f" helm-gtags-parsed-file))
        (error "Failed: global --result cscope -f \"%s\"" helm-gtags-parsed-file)))
    candidates-buf))

(defun helm-gtags-goto-tag-pos()
  (require 'pulse)
  (let ((old-pos (point)))
    (if (search-forward
         (car (helm-mm-split-pattern helm-pattern)) (point-at-eol) t)
        (progn
          (goto-char (match-beginning 0))
          (pulse-momentary-highlight-region
           (match-beginning 0) (match-end 0)))
      (goto-char old-pos)
      (pulse-momentary-highlight-one-line old-pos))))

(defun helm-gtags-parse-file-action (cand)
  (let ((line (when (string-match "\\s-+\\([1-9][0-9]*\\)\\s-+" cand)
                (string-to-number (match-string 1 cand))))
        (open-func (helm-gtags-select-find-file-func)))
    (helm-gtags-do-open-file open-func helm-gtags-parsed-file line)
    (helm-gtags-goto-tag-pos)))


(defun helm-gtags-action-openfile (_elm)
  (let* ((elm (helm-get-selection nil 'withprop))
         (elems (helm-gtags-split-line elm))
         (filename (or (get-text-property 0 'filename elm) (car elems)))
         (line (string-to-number (cadr elems)))
         (open-func (helm-gtags-select-find-file-func))
         (default-directory (or (get-text-property 0 'default-directory elm) (helm-gtags-find-tagroot))))
    (helm-gtags-do-open-file open-func filename line)
    (helm-gtags-goto-tag-pos)))

(defun helm-gtags-select-find-file-func()
  (if helm-gtags-use-otherwin
      'helm-gtags-open-file-other-window
    'helm-gtags-open-file))

(defun helm-gtags-do-open-file (open-func file line)
  (run-hooks 'helm-gtags-goto-line-before-hook)
  (funcall open-func file helm-gtags-read-only)
  (goto-char (point-min))
  (forward-line (1- line))
  (back-to-indentation)
  (run-hooks 'helm-gtags-goto-line-after-hook))

(defun helm-gtags-open-file (file readonly)
  (if readonly
      (find-file-read-only file)
    (find-file file)))

(defsubst helm-gtags--using-other-window-p ()
  (= (prefix-numeric-value current-prefix-arg) 4))
(defsubst helm-gtags--read-input-p ()
  (= (prefix-numeric-value current-prefix-arg) 16))

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
    (cl-loop for n from 1 to 3 collect (match-string n line))))

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
         (filename (or (get-text-property 0 'filename c) (car elems)))
         (line (string-to-number (cadr elems)))
         (default-directory (or (get-text-property 0 'default-directory c)
                                (helm-gtags-find-tagroot))))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))


(defun helm-gtags-common (srcs &optional input)
  (let ((helm-quit-if-no-candidate #'(lambda()
                                       (with-current-buffer helm-current-buffer
                                         (run-hooks 'helm-gtags-quit-or-no-candidates-hook))
                                       (message "gtags:not found")))
        (helm-input-idle-delay helm-gtags-input-idle-delay)
        (helm-execute-action-at-once-if-one t)
        (buf (get-buffer-create helm-gtags-buffer))
        (custom-dirs (helm-gtags-move-tagroot-first major-mode))
        (src (car srcs)))
    (when (helm-gtags--using-other-window-p) (setq helm-gtags-use-otherwin t))
    (when (helm-gtags--read-input-p) (setq helm-execute-action-at-once-if-one nil))
    (dolist (src srcs)
      (when (symbolp src) (setq src (symbol-value src)))
      (unless (helm-attr 'init-name src) (helm-attrset 'init-name  (helm-attr 'name src) src))
      (helm-attrset 'name
                    (format "Searched %s at %s" (or (helm-attr 'init-name src) "")
                            (mapconcat 'identity custom-dirs "  "))
                    src))
    (helm-gtags-check-tags-exists)
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
        mode cmd-options dirs)
    (with-current-buffer helm-current-buffer
      (setq mode major-mode)
      (setq dirs (helm-gtags-get-tag-location-alist mode))
      (cl-case type
        (:file  (setf (cdr (assoc type helm-gtags-cache-alist)) (list mode input dirs)))
        (otherwise (setf (cdr (assoc type helm-gtags-cache-alist)) (list mode input)))))

    (with-current-buffer candidates-buf
      (erase-buffer)
      (let (tramp-remote-base
            (default-directory default-directory)
            (coding-system-for-read buf-coding)
            (coding-system-for-write buf-coding))
        (dolist (dir dirs)
          (setq cmd-options (helm-gtags-construct-command type dir input))
          (setq default-directory dir)
          (setq tramp-remote-base (file-remote-p dir))
          (insert
           (with-temp-buffer
             (cl-case (apply 'process-file helm-gtags-global-cmd nil (current-buffer) nil cmd-options)
               (0 (when (and tramp-remote-base (equal type :file))
                    (helm-gtags-insert-at-each-bol tramp-remote-base))
                  (put-text-property (point-min) (point-max) 'default-directory default-directory))
               (t
                (when (string-match "global: GTAGS not found." (buffer-string))
                  (helm-gtags-set-tag-location-alist mode (delete dir dirs)))
                (erase-buffer)))
             (buffer-string))))))
    candidates-buf))

(defun helm-gtags-insert-at-each-bol(content &optional buf)
  (with-current-buffer (or buf (current-buffer))
    (goto-char (point-max))
    (goto-char (line-beginning-position))
    (while (not (bobp))
      (when (not (equal (line-beginning-position) (line-end-position)))
        (insert content))
      (forward-line -1)
      (when (bobp)
        (when (not (equal (line-beginning-position) (line-end-position)))
          (insert content)))
      (goto-char (line-beginning-position)))))



(defun helm-gtags-use-cache-p(type input cache-info)
  (with-current-buffer helm-current-buffer
    (let ((mode (car cache-info))
          (cache-input (nth 1 cache-info))
          (buf-filename (buffer-file-name (current-buffer))))
      (cl-case type
        (:file
         (let((cache-dirs (nth 2 cache-info)))
           (or (null buf-filename)
               (and (equal major-mode mode)
                    (string-equal input cache-input)
                    (cl-some
                     '(lambda (dir)
                        (string-match (regexp-quote dir)
                                      (file-truename buf-filename))) cache-dirs)))))
        (otherwise
         (and (string-equal input cache-input)
              (equal major-mode mode)))))))


(defun helm-gtags-get-candidates-buf-with-cache(type &optional in)
  (let ((input (or in (car (helm-mm-split-pattern helm-pattern))))
        (candidates-buf (get-buffer (assoc-default type helm-gtags-buf-alist)))
        (cache-info (assoc-default type helm-gtags-cache-alist)))
    (if (and cache-info (bufferp candidates-buf)(buffer-live-p candidates-buf)
             (helm-gtags-use-cache-p type input cache-info))
        ;; (helm-gtags-exec-global-cmd type input) ;
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
  "similiar to `helm-candidates-in-buffer'"
  (let ((src (helm-get-current-source)))
    (helm-candidates-in-buffer-1
     buf
     helm-pattern
     (or (assoc-default 'get-line src)
         #'buffer-substring-no-properties)
     (or (assoc-default 'search src)
         '(helm-candidates-in-buffer-search-default-fn))
     (helm-candidate-number-limit src)
     (helm-attr 'match-part)
     src)))

(defvar helm-source-gtags-tags
  (helm-build-in-buffer-source "GNU Global tag"
    :init  nil
    :candidates 'helm-gtags-candidates-in-buffer-tag
    :get-line 'buffer-substring
    :persistent-action 'helm-gtags-tags-persistent-action
    :action 'helm-gtags-action-openfile
    )
  )

(defvar helm-source-gtags-gsyms
  (helm-build-in-buffer-source "GNU Global symbol"
    :init  nil
    :candidates 'helm-gtags-candidates-in-buffer-symbol
    :get-line 'buffer-substring
    :persistent-action 'helm-gtags-tags-persistent-action
    :action 'helm-gtags-action-openfile
    )
  )


(defvar helm-source-gtags-rtags
  (helm-build-in-buffer-source "GNU Global rtags"
    :init  nil
    :candidates 'helm-gtags-candidates-in-buffer-rtag
    :get-line 'buffer-substring
    :persistent-action 'helm-gtags-tags-persistent-action
    :action 'helm-gtags-action-openfile))


(defvar helm-source-gtags-files
  (helm-build-in-buffer-source "GNU Global files"
    :init  nil
    :candidates 'helm-gtags-candidates-in-buffer-file
    :candidate-number-limit helm-gtags-default-candidate-limit
    :action (helm-actions-from-type-file)))


(defvar helm-source-gtags-find-tag-from-here
  (helm-build-in-buffer-source "GNU Global tags"
    :init nil
    :candidates 'helm-gtags-candidates-in-buffer-tag-from-here
    :get-line 'buffer-substring
    :persistent-action 'helm-gtags-tags-persistent-action
    :candidate-number-limit helm-gtags-default-candidate-limit
    :action  'helm-gtags-action-openfile
    )
  )


(defvar helm-source-gtags-parse-file
  (helm-build-in-buffer-source "GNU GLOBAL Parsed File"
    :init nil
    :candidates 'helm-gtags-candidates-in-buffer-parse-file
    :real-to-display 'helm-gtags-parse-file-candidate-transformer
    :action 'helm-gtags-parse-file-action
    :candidate-number-limit helm-gtags-default-candidate-limit))


;;;###autoload
(defun helm-gtags-find-tag-and-symbol()
  "Jump to definition"
  (interactive)
  (helm-gtags-common '(helm-source-gtags-tags
                       helm-source-gtags-gsyms)))

;;;###autoload
(defun helm-gtags-find-tag()
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
  (helm-gtags-check-tags-exists)
  (let ((helm-quit-if-no-candidate #'(lambda()
                                       (with-current-buffer helm-current-buffer
                                         (run-hooks 'helm-gtags-quit-or-no-candidates-hook))
                                       (message "gtags:no candidates"))))
    (helm :sources '(helm-source-gtags-parse-file)
          :buffer (get-buffer-create helm-gtags-buffer))
    (when (eq 1 helm-exit-status)
      (run-hooks 'helm-gtags-quit-or-no-candidates-hook))))



(defun helm-gtags-read-tag-directory ()
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst helm-gtags-how-to-update-tags ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (otherwise 'single-update)))


(defun helm-gtags-update-tags-command (how-to)
  (cl-case how-to
    (entire-update '("global" "-u"))
    (generate-other-directory (list "gtags" (helm-gtags-read-gtagslabel) (helm-gtags-read-tag-directory)))
    (single-update (list "global" "--single-update" (helm-gtags-local-file-name)))))

(defun helm-gtags-update-tags-p (how-to interactive-p current-time)
  (or interactive-p
      (and (eq how-to 'single-update)
           (buffer-file-name)
           (or (not helm-gtags-interval-seconds)
               (>= (- current-time helm-gtags-last-update-time)
                   helm-gtags-interval-seconds)))))

;;;###autoload
(defun helm-gtags-update-tags ()
  "Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'"
  (interactive)
  (let ((how-to (helm-gtags-how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (helm-gtags-update-tags-p how-to interactive-p current-time)
      (let* ((cmds (helm-gtags-update-tags-command how-to))
             (proc (apply 'start-file-process "helm-gtags-update-tag"
                          (get-buffer-create helm-gtags-update-tmp-buf) cmds)))
        (if (not proc)
            (message "Failed: %s" (mapconcat 'identity cmds " "))
          (set-process-sentinel proc 'helm-gtags-update-gtags-sentinel)
          (setq helm-gtags-last-update-time current-time))))))

(defun helm-gtags-update-gtags-sentinel(proc _event)
  (when (eq (process-status proc) 'exit)
    (if (zerop (process-exit-status proc))
        (message "Success: update GNU Global TAGS" )
      (message "Failed: update GNU Global TAGS(%s)"
               (with-current-buffer (process-buffer proc)(buffer-string))))
    (set-process-query-on-exit-flag proc nil)
    (kill-buffer (process-buffer proc))))


(defvar-local helm-gtags-eldoc-cache nil)  ;(tagname . taginfo)
(declare-function eldoc-message "eldoc")

(defsubst helm-gtags-print-definition-function(token s)
  (setq helm-gtags-eldoc-cache (list token s))
  (eldoc-message s))
(defsubst helm-gtags-build-eldoc-tmp-buf-name(token)
  (concat helm-gtags-eldoc-tmp-buf "^" token))
(defsubst helm-gtags-get-token-from-tmp-buf-name(buffer)
  (nth 1 (split-string (buffer-name buffer) "\\^")))
;; line =/Users/jixiuf/repos/emacs/src/w32term.h:82:struct w32_display_info
;; (helm-gtags-get-taginfo-from-grep-result "c:\\a:82:struct w32_display_info")
;; return struct w32_display_info
(defsubst helm-gtags-get-taginfo-from-grep-result(line)
  (let ((tokens (split-string line ":")))
    (if (> (length tokens) 2)
        (nth (1- (length tokens)) tokens)
      "")))

(defun helm-gtags-eldoc-function()
  "A function suitable for `eldoc-documentation-function' (which see)."
  (let ((token (thing-at-point 'symbol)))
    (if (equal token (car helm-gtags-eldoc-cache))
        (progn
          (cadr helm-gtags-eldoc-cache)
          )
      (when token
        ;; Prevent multiple runs of ggtags-show-definition
        ;; for the same tag.
        (setq helm-gtags-eldoc-cache (list token))

        (let* ((proc (apply 'start-file-process "helm-gtags-eldoc"
                            (get-buffer-create (helm-gtags-build-eldoc-tmp-buf-name token))
                            helm-gtags-global-cmd (list "--result=grep" token))))
          (set-process-sentinel proc 'helm-gtags-eldoc-sentinel)
          (set-process-query-on-exit-flag proc nil))))))


(defun helm-gtags-eldoc-sentinel(proc _event)
  (when (eq (process-status proc) 'exit)
    (when (zerop (process-exit-status proc))
      (let ((token (helm-gtags-get-token-from-tmp-buf-name (current-buffer)))
            (tag-eldoc))
        (with-current-buffer (process-buffer proc)
          (goto-char (point-min))
          (setq tag-eldoc  (helm-gtags-get-taginfo-from-grep-result
                            (buffer-substring (point-min) (line-end-position)))))
        (helm-gtags-print-definition-function token tag-eldoc)))
    (kill-buffer (process-buffer proc))))

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
        ;; Work around http://debbugs.gnu.org/19324
        (or eldoc-documentation-function
            (setq-local eldoc-documentation-function #'ignore))
        (add-function :after-until (local 'eldoc-documentation-function)
                      #'helm-gtags-eldoc-function '((name . helm-gtags-eldoc-function)
                                                    (depth . -100)))
        (run-hooks 'helm-gtags-mode-hook)
        (when helm-gtags-auto-update
          (add-hook 'after-save-hook 'helm-gtags-update-tags nil t)))
    (remove-function (local 'eldoc-documentation-function) 'helm-gtags-eldoc-function)
    (when helm-gtags-auto-update
      (remove-hook 'after-save-hook 'helm-gtags-update-tags t))))

(provide 'helm-gtags)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-gtags.el ends here
