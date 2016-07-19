;;; License: GPLv3
;; Description: This package enhancees current usability of xwidget browser,
;; making it behaves more like a normal browser, with Emacs key bindings!
;;
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Version: 0.3
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/tuhdo/xwidgete
;; Doc URL:
;; Keywords: xwidgete, tools

(require 'eieio)
(require 'cl-lib)
(require 'xwidget)
(require 'subr-x)

(defvar *xwidgete-total-spaces-pressed* nil
  "Check whether a space key is pressed. This variable is a
  workaround for entering space character, since trailing spaces
  are always trimmed when writing back to textbox.")

(defvar *xwidgete-current-active-element* nil
  "This varaible caches the retrieved DOM element in action.")

(defvar xwidgete-get-html-selection-js "
function extract_html_selection () {
    var selection = window.getSelection();
    if (selection.rangeCount > 0) {
        var range = selection.getRangeAt(0);
        var clonedSelection = range.cloneContents();
        var div = document.createElement('div');
        div.appendChild(clonedSelection);
        /*
         * replace newline with <br /> tag for proper newline rendering
         * this is necessary when the copied HTML content uses newlines
         * for line breaks.
         */
        return div.innerHTML.replace(/\\r?\\n/g,'<br/>');
    }
}
"
  "JS to retrieve HTML content of a selected region.")

(defvar xwidgete-set-caret "
function set_caret(pos) {
    var el = findactiveelement(document);
    el.setSelectionRange(pos,pos)
}
")

(defvar xwidgete-delete-selected-text "
function delete_selected_text() {
    var ele  = findactiveelement(document);
    var text = ele.value;
    var start = ele.selectionStart;
    var end = ele.selectionEnd;

    if (start == end) start = start - 1;

    text = text.slice(0, start) + text.slice(end);
    ele.value = text;
    ele.selectionStart = start;
    ele.selectionEnd = start;
}
")


(defclass xwidgete-js-dom ()
  ((id
    :initargs :id
    :accessor id
    :initform "")
   (value
    :initargs :value
    :reader value
    :writer set-value
    :initform "")
   (type
    :initargs :type
    :accessor type
    :initform nil
    )
   (selectionStart
    :initargs :selectionStart
    :reader selectionStart
    :writer set-selectionStart
    :initform 0
    )
   (selectionEnd
    :initargs :selectionEnd
    :accessor selectionEnd
    :initform 0
    )))

;;;;;;;;;;;;
;; Macros ;;
;;;;;;;;;;;;

(defmacro xwidgete-interactive-get-char (c)
  `(interactive
    (let* ((xww (xwidget-webkit-current-session))
           (field-value (value (xwidgete-find-active-element) xww)))
      (list xww field-value ,c))))

;;;;;;;;;;;;;
;; Methods ;;
;;;;;;;;;;;;;

(cl-defmethod initialize-instance ((elem xwidgete-js-dom) &rest args)
  "Constructor for `xwidgete-js-dom'"
  (let ((xww (xwidget-webkit-current-session)))
    (message "called after creating webkikt-js-element")
    (when xww
      (xwidget-webkit-execute-script xww xwidget-webkit-activeelement-js)
      (message "xwidget created creating webkikt-js-element")
      (oset elem id (xwidget-webkit-execute-script-rv
                     xww
                     "findactiveelement(document).id;"))
      (oset elem value (xwidget-webkit-execute-script-rv
                        xww
                        "findactiveelement(document).value;"))
      (oset elem type (xwidget-webkit-execute-script-rv
                       xww
                       "findactiveelement(document).type;"))
      (oset elem selectionStart (xwidget-webkit-execute-script-rv
                                 xww
                                 "findactiveelement(document).selectionStart;"))
      (oset elem selectionEnd (xwidget-webkit-execute-script-rv
                               xww
                               "findactiveelement(document).selectionEnd;")))))

;; getter and setter for selectionStart
(cl-defmethod selectionStart ((elem xwidgete-js-dom) xw)
  (xwidget-webkit-execute-script xw xwidget-webkit-activeelement-js)
  (let ((caret-pos (xwidget-webkit-execute-script-rv
                    xw
                    "findactiveelement(document).selectionStart;")))
    (oset elem selectionStart caret-pos)

    (string-to-int caret-pos)))

(cl-defmethod set-selectionStart ((elem xwidgete-js-dom) xw pos)
  (xwidget-webkit-execute-script xw xwidgete-set-caret)
  (xwidget-webkit-execute-script xw
                                 (format "set_caret(%s);"
                                         (number-to-string pos)))
  (oset elem selectionStart pos))

;; getter and setter for value
(cl-defmethod value ((elem xwidgete-js-dom) xw)
  (xwidget-webkit-execute-script xw xwidget-webkit-activeelement-js)
  (let ((field-value (xwidget-webkit-execute-script-rv
                      xw
                      "findactiveelement(document).value;")))
    (oset elem value field-value)
    field-value))

(cl-defmethod set-value ((elem xwidgete-js-dom) xw str)
  (oset elem value str)
  (xwidget-webkit-execute-script xw (format "findactiveelement(document).value='%s';" str)))


;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun xwidgete-find-active-element ()
  "Returns current active DOM element.

If the element is the same in the cache `*xwidgete-current-active-element*',
then simply returns *xwidgete-current-active-element*'. Otherwise create new instance that maps to the new active DOM element."
  (let* ((xw (xwidget-webkit-current-session))
         (cur-id nil))
    (xwidget-webkit-execute-script xw xwidget-webkit-activeelement-js)
    (setq cur-id (xwidget-webkit-execute-script-rv xw "findactiveelement(document).id;"))
    (if (and (not (null *xwidgete-current-active-element*))
             (string-equal (id *xwidgete-current-active-element*) cur-id))
        *xwidgete-current-active-element*
      (setq *xwidgete-current-active-element* (make-instance 'xwidgete-js-dom)))))

(defun xwidgete-self-insert-command (xw value c)
  "Self-insert command for ASCII char from 31 to 126."
  (xwidgete-interactive-get-char (this-command-keys))
  (let* ((space-pressed *xwidgete-total-spaces-pressed*)
         (caret-pos (selectionStart (xwidgete-find-active-element) xw))
         (value-len (length value))
         (splice-index (if (> caret-pos value-len)
                           value-len
                         caret-pos))
         (str1 (if (string-empty-p value)
                   ""
                 (substring value 0 splice-index)) )
         (str2 (if (string-empty-p value)
                   ""
                 (substring value splice-index value-len))))
    (setq *xwidgete-total-spaces-pressed* nil)
    (set-value *xwidgete-current-active-element*
               xw
               (concat str1 (if space-pressed " " "") c str2))
    (set-selectionStart (xwidgete-find-active-element) xw (+ caret-pos (if space-pressed 1 0) 1))))

(defun xwidgete-self-insert-command-backspace ()
  "Self-insert command for backspace."
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos nil))
    (xwidget-webkit-execute-script xw xwidgete-delete-selected-text)
    (xwidget-webkit-execute-script xw "delete_selected_text();")
    (setq caret-pos (selectionStart (xwidgete-find-active-element) xw))
    (set-selectionStart (xwidgete-find-active-element) xw caret-pos)))

(defun xwidgete-kill ()
  "Kill command that kills from the caret to end."
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos (selectionStart (xwidgete-find-active-element) xw))
         (value (value (xwidgete-find-active-element) xw))
         (str1 (substring value 0 caret-pos)))
    (setq *xwidgete-total-spaces-pressed* 0)
    (set-value (xwidgete-find-active-element) xw str1)
    (setq caret-pos (selectionStart (xwidgete-find-active-element) xw))
    (set-selectionStart (xwidgete-find-active-element) xw caret-pos)))

(defun xwidgete-beginning-of-line ()
  (interactive)
  (set-selectionStart (xwidgete-find-active-element)
                      (xwidget-webkit-current-session)
                      0))

(defun xwidgete-end-of-line ()
  (interactive)
  (let ((value-length (length (value (xwidgete-find-active-element)
                                     (xwidget-webkit-current-session)))))
    (set-selectionStart (xwidgete-find-active-element)
                        (xwidget-webkit-current-session)
                        value-length)))

(defun xwidgete-caret-move-left ()
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos (selectionStart (xwidgete-find-active-element) xw)))
    (set-selectionStart (xwidgete-find-active-element)
                        xw
                        (1- caret-pos))))

(defun xwidgete-caret-move-right ()
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos (selectionStart (xwidgete-find-active-element) xw)))
    (set-selectionStart (xwidgete-find-active-element)
                        xw
                        (1+ caret-pos))))

(defun xwidgete-self-insert-command-space (xw value c)
  (xwidgete-interactive-get-char nil)
  (let* ((caret-pos (selectionStart (xwidgete-find-active-element) xw))
         (value-len (length value))
         (splice-index (if (> caret-pos value-len)
                           value-len
                         caret-pos))
         (str1 (if (string-empty-p value)
                   ""
                 (substring value 0 splice-index)) )
         (str2 (if (string-empty-p value)
                   ""
                 (substring value splice-index value-len))))
    (setq *xwidgete-total-spaces-pressed* t)
    (set-value *xwidgete-current-active-element*
               xw
               (concat str1 " " str2))
    (set-selectionStart (xwidgete-find-active-element) xw (+ caret-pos 1))))

;; by default, xwidget reuses previous xwidget window,
;; thus overriding your current website, unless a prefix argument
;; is supplied
;;
;; This function always opens a new website in a new window
(defun xwidget-browse-url-no-reuse (url &optional sessoin)
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidgete URL: "
                                             )))
  (xwidget-webkit-browse-url url t))

(defun xwidgete-define-keys ()
  (mapcar (lambda (c)
            (define-key xwidget-webkit-mode-map (kbd c) 'xwidgete-self-insert-command))
          (mapcar (lambda (char-code)
                    (char-to-string char-code))
                  (number-sequence 33 126 1))))

(defun xwidgete-page-up ()
  "Scroll webkit up."
  (interactive)
  (xwidget-set-adjustment (xwidgete-last-session) 'vertical t -300))

(defun xwidgete-page-down ()
  "Scroll webkit down."
  (interactive)
  (xwidget-set-adjustment (xwidgete-last-session) 'vertical t 300))

(defun xwidgete-get-selection ()
  "Get the webkit selection."
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 xwidgete-get-html-selection-js)
  (let* ((html-content (xwidget-webkit-execute-script-rv (xwidget-webkit-current-session)
                                                         "extract_html_selection();"))
         (html-content-sexp (with-temp-buffer
                              (insert html-content)
                              (libxml-parse-html-region (point-min) (point-max))))
         (rendered-content (with-temp-buffer
                             (shr-insert-document html-content-sexp)
                             (buffer-substring-no-properties (point-min) (point-max)))))
    rendered-content))

;; make these keys behave like normal browser
(define-key xwidget-webkit-mode-map [mouse-4] 'xwidgete-page-up)
(define-key xwidget-webkit-mode-map [mouse-5] 'xwidgete-page-down)
(define-key xwidget-webkit-mode-map (kbd "<next>") 'xwidgete-page-down)
(define-key xwidget-webkit-mode-map (kbd "<prior>") 'xwidgete-page-up)
(define-key xwidget-webkit-mode-map (kbd "C-v") 'xwidgete-page-down)
(define-key xwidget-webkit-mode-map (kbd "M-v") 'xwidgete-page-up)
(define-key xwidget-webkit-mode-map (kbd "C-a") 'xwidgete-beginning-of-line)
(define-key xwidget-webkit-mode-map (kbd "C-e") 'xwidgete-end-of-line)
(define-key xwidget-webkit-mode-map (kbd "C-k") 'xwidgete-kill)
(define-key xwidget-webkit-mode-map (kbd "<home>") 'xwidgete-beginning-of-line)
(define-key xwidget-webkit-mode-map (kbd "<end>") 'xwidgete-end-of-line)
(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidgete-scroll-down)
(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidgete-scroll-up)
(define-key xwidget-webkit-mode-map (kbd "<left>") 'xwidgete-caret-move-left)
(define-key xwidget-webkit-mode-map (kbd "C-b") 'xwidgete-caret-move-left)
(define-key xwidget-webkit-mode-map (kbd "C-f") 'xwidgete-caret-move-right)

(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidgete-copy-selection-as-kill)
;; (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidgete-copy-selection-as-kill)
(define-key xwidget-webkit-mode-map (kbd "M-b") 'xwidgete-back)
(define-key xwidget-webkit-mode-map (kbd "M-r") 'xwidgete-reload)
(define-key xwidget-webkit-mode-map (kbd "M-g") 'xwidget-webkit-browse-url)
(define-key xwidget-webkit-mode-map (kbd "M-u") 'xwidgete-current-url)

(xwidgete-define-keys)
(define-key xwidget-webkit-mode-map (kbd "<backspace>") 'xwidgete-self-insert-command-backspace)
(define-key xwidget-webkit-mode-map (kbd "SPC") 'xwidgete-self-insert-command-space)

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
                                              (when (equal major-mode 'xwidgete-mode)
                                                (xwidgete-adjust-size-dispatch))))

(provide 'xwidgete)
