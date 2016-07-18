;; Author: Tu Do
;; URL: https://github.com/tuhdo/xwidgete
;;
;; This file is not part of GNU Emacs.
;;
;; Description: This package enhancees current usability of xwidget browser,
;; making it behaves more like a normal browser, with Emacs key bindings!
;;
;;; License: GPLv3

(require 'eieio)
(require 'cl-lib)
(require 'xwidget)
(require 'subr-x)

(defvar *xwidget-webkit-total-spaces-pressed* nil
  "Check whether a space key is pressed. This variable is a
  workaround for entering space character, since trailing spaces
  are always trimmed when writing back to textbox.")

(defvar *xwidget-webkit-current-active-element* nil
  "This varaible caches the retrieved DOM element in action.")

(defvar xwidget-webkit-get-html-selection-js "
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

(defvar xwidget-webkit-set-caret "
function set_caret(pos) {
    var el = findactiveelement(document);
    el.setSelectionRange(pos,pos)
}
")

(defvar xwidget-webkit-delete-selected-text "
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


(defclass xwidget-webkit-js-dom ()
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

(defmacro xwidget-webkit-interactive-get-char (c)
  `(interactive
    (let* ((xww (xwidget-webkit-current-session))
           (field-value (value (xwidget-webkit-find-active-element) xww)))
      (list xww field-value ,c))))

;;;;;;;;;;;;;
;; Methods ;;
;;;;;;;;;;;;;

(cl-defmethod initialize-instance ((elem xwidget-webkit-js-dom) &rest args)
  "Constructor for `xwidget-webkit-js-dom'"
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
(cl-defmethod selectionStart ((elem xwidget-webkit-js-dom) xw)
  (xwidget-webkit-execute-script xw xwidget-webkit-activeelement-js)
  (let ((caret-pos (xwidget-webkit-execute-script-rv
                    xw
                    "findactiveelement(document).selectionStart;")))
    (oset elem selectionStart caret-pos)

    (string-to-int caret-pos)))

(cl-defmethod set-selectionStart ((elem xwidget-webkit-js-dom) xw pos)
  (xwidget-webkit-execute-script xw xwidget-webkit-set-caret)
  (xwidget-webkit-execute-script xw
                                 (format "set_caret(%s);"
                                         (number-to-string pos)))
  (oset elem selectionStart pos))

;; getter and setter for value
(cl-defmethod value ((elem xwidget-webkit-js-dom) xw)
  (xwidget-webkit-execute-script xw xwidget-webkit-activeelement-js)
  (let ((field-value (xwidget-webkit-execute-script-rv
                      xw
                      "findactiveelement(document).value;")))
    (oset elem value field-value)
    field-value))

(cl-defmethod set-value ((elem xwidget-webkit-js-dom) xw str)
  (oset elem value str)
  (xwidget-webkit-execute-script xw (format "findactiveelement(document).value='%s';" str)))


;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun xwidget-webkit-find-active-element ()
  "Returns current active DOM element.

If the element is the same in the cache `*xwidget-webkit-current-active-element*',
then simply returns *xwidget-webkit-current-active-element*'. Otherwise create new instance that maps to the new active DOM element."
  (let* ((xw (xwidget-webkit-current-session))
         (cur-id nil))
    (xwidget-webkit-execute-script xw xwidget-webkit-activeelement-js)
    (setq cur-id (xwidget-webkit-execute-script-rv xw "findactiveelement(document).id;"))
    (if (and (not (null *xwidget-webkit-current-active-element*))
             (string-equal (id *xwidget-webkit-current-active-element*) cur-id))
        *xwidget-webkit-current-active-element*
      (setq *xwidget-webkit-current-active-element* (make-instance 'xwidget-webkit-js-dom)))))

(defun xwidget-webkit-self-insert-command (xw value c)
  "Self-insert command for ASCII char from 31 to 126."
  (xwidget-webkit-interactive-get-char (this-command-keys))
  (let* ((space-pressed *xwidget-webkit-total-spaces-pressed*)
         (caret-pos (selectionStart (xwidget-webkit-find-active-element) xw))
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
    (setq *xwidget-webkit-total-spaces-pressed* nil)
    (set-value *xwidget-webkit-current-active-element*
               xw
               (concat str1 (if space-pressed " " "") c str2))
    (set-selectionStart (xwidget-webkit-find-active-element) xw (+ caret-pos (if space-pressed 1 0) 1))))

(defun xwidget-webkit-self-insert-command-backspace ()
  "Self-insert command for backspace."
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos nil))
    (xwidget-webkit-execute-script xw xwidget-webkit-delete-selected-text)
    (xwidget-webkit-execute-script xw "delete_selected_text();")
    (setq caret-pos (selectionStart (xwidget-webkit-find-active-element) xw))
    (set-selectionStart (xwidget-webkit-find-active-element) xw caret-pos)))

(defun xwidget-webkit-kill ()
  "Kill command that kills from the caret to end."
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos (selectionStart (xwidget-webkit-find-active-element) xw))
         (value (value (xwidget-webkit-find-active-element) xw))
         (str1 (substring value 0 caret-pos)))
    (setq *xwidget-webkit-total-spaces-pressed* 0)
    (set-value (xwidget-webkit-find-active-element) xw str1)
    (setq caret-pos (selectionStart (xwidget-webkit-find-active-element) xw))
    (set-selectionStart (xwidget-webkit-find-active-element) xw caret-pos)))

(defun xwidget-webkit-beginning-of-line ()
  (interactive)
  (set-selectionStart (xwidget-webkit-find-active-element)
                      (xwidget-webkit-current-session)
                      0))

(defun xwidget-webkit-end-of-line ()
  (interactive)
  (let ((value-length (length (value (xwidget-webkit-find-active-element)
                                     (xwidget-webkit-current-session)))))
    (set-selectionStart (xwidget-webkit-find-active-element)
                        (xwidget-webkit-current-session)
                        value-length)))

(defun xwidget-webkit-caret-move-left ()
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos (selectionStart (xwidget-webkit-find-active-element) xw)))
    (set-selectionStart (xwidget-webkit-find-active-element)
                        xw
                        (1- caret-pos))))

(defun xwidget-webkit-caret-move-right ()
  (interactive)
  (let* ((xw (xwidget-webkit-current-session))
         (caret-pos (selectionStart (xwidget-webkit-find-active-element) xw)))
    (set-selectionStart (xwidget-webkit-find-active-element)
                        xw
                        (1+ caret-pos))))

(defun xwidget-webkit-self-insert-command-space (xw value c)
  (xwidget-webkit-interactive-get-char nil)
  (let* ((caret-pos (selectionStart (xwidget-webkit-find-active-element) xw))
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
    (setq *xwidget-webkit-total-spaces-pressed* t)
    (set-value *xwidget-webkit-current-active-element*
               xw
               (concat str1 " " str2))
    (set-selectionStart (xwidget-webkit-find-active-element) xw (+ caret-pos 1))))

;; by default, xwidget reuses previous xwidget window,
;; thus overriding your current website, unless a prefix argument
;; is supplied
;;
;; This function always opens a new website in a new window
(defun xwidget-browse-url-no-reuse (url &optional sessoin)
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             )))
  (xwidget-webkit-browse-url url t))

(defun xwidget-webkit-define-keys ()
  (mapcar (lambda (c)
            (define-key xwidget-webkit-mode-map (kbd c) 'xwidget-webkit-self-insert-command))
          (mapcar (lambda (char-code)
                    (char-to-string char-code))
                  (number-sequence 33 126 1))))

(defun xwidget-webkit-page-up ()
  "Scroll webkit up."
  (interactive)
  (xwidget-set-adjustment (xwidget-webkit-last-session) 'vertical t -300))

(defun xwidget-webkit-page-down ()
  "Scroll webkit down."
  (interactive)
  (xwidget-set-adjustment (xwidget-webkit-last-session) 'vertical t 300))

(defun xwidget-webkit-get-selection ()
  "Get the webkit selection."
  (xwidget-webkit-execute-script (xwidget-webkit-current-session)
                                 xwidget-webkit-get-html-selection-js)
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
(define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-page-up)
(define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-page-down)
(define-key xwidget-webkit-mode-map (kbd "<next>") 'xwidget-webkit-page-down)
(define-key xwidget-webkit-mode-map (kbd "<prior>") 'xwidget-webkit-page-up)
(define-key xwidget-webkit-mode-map (kbd "C-v") 'xwidget-webkit-page-down)
(define-key xwidget-webkit-mode-map (kbd "M-v") 'xwidget-webkit-page-up)
(define-key xwidget-webkit-mode-map (kbd "C-a") 'xwidget-webkit-beginning-of-line)
(define-key xwidget-webkit-mode-map (kbd "C-e") 'xwidget-webkit-end-of-line)
(define-key xwidget-webkit-mode-map (kbd "C-k") 'xwidget-webkit-kill)
(define-key xwidget-webkit-mode-map (kbd "<home>") 'xwidget-webkit-beginning-of-line)
(define-key xwidget-webkit-mode-map (kbd "<end>") 'xwidget-webkit-end-of-line)
(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
(define-key xwidget-webkit-mode-map (kbd "<left>") 'xwidget-webkit-caret-move-left)
(define-key xwidget-webkit-mode-map (kbd "C-b") 'xwidget-webkit-caret-move-left)
(define-key xwidget-webkit-mode-map (kbd "C-f") 'xwidget-webkit-caret-move-right)

(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
;; (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)
(define-key xwidget-webkit-mode-map (kbd "M-b") 'xwidget-webkit-back)
(define-key xwidget-webkit-mode-map (kbd "M-r") 'xwidget-webkit-reload)
(define-key xwidget-webkit-mode-map (kbd "M-g") 'xwidget-webkit-browse-url)
(define-key xwidget-webkit-mode-map (kbd "M-u") 'xwidget-webkit-current-url)

(xwidget-webkit-define-keys)
(define-key xwidget-webkit-mode-map (kbd "<backspace>") 'xwidget-webkit-self-insert-command-backspace)
(define-key xwidget-webkit-mode-map (kbd "SPC") 'xwidget-webkit-self-insert-command-space)

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
                                              (when (equal major-mode 'xwidget-webkit-mode)
                                                (xwidget-webkit-adjust-size-dispatch))))

(provide 'xwidgete)
