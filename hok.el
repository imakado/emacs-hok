;;; hok.el --- Hit a hint for emacs

;; Copyright (C) 2014 by IMAKADO

;; This is my fork of ace-jump-mode.el wrote by winterTTr.

;; Prefix: hok/
;; Author: Kenji Imakado <ken.imakado -at- gmail.com>
;; Maintainer: imakado
;; Created: :2014-11-06
;; Keywords: 
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((imakado "0.12"))

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

;;; Usage

;; (require 'hok)
;; (global-set-key (kbd "C-S-H") 'hok)
;; (global-set-key (kbd "C-S-W") 'hok/after-move-press-enter)


;;; Code:

(require 'rx)
(require 'cl)
(require 'imakado)

(defgroup hok nil
  "hit on key."
  :prefix "hok/"
  :group 'convenience)

;; Customize
(defvar hok/hint-keys "jkllasdfghyuiopqwervn")
(defvar hok/keys-char (mapcar #'identity hok/hint-keys))
(defvar hok/show-hint-as-single-key nil)
(defface hok/hint-face
  '((t
     (:background "darkgreen"
                  :foreground "red")
     
     ))
  ""
  :group 'hok)


(defface hok/hint-face2
  '((t
     (
      :background "gray13"
                  :foreground "green"
                  )))
  ""
  :group 'hok)

(defvar hok/hint-faces '(hok/hint-face hok/hint-face2))





(defvar hok/after-hook nil)
(defvar hok/forward-word-functions nil)

(defvar hok/ace-jump-background-overlays nil)
(defface hok/ace-jump-face-background
  '((t (:foreground "gray60")))
  "Face for background of AceJump motion"
  :group 'hok)


(defvar hok/cycle-hint-face
  (imakado-cycle-list-gen
   :get-list-fn (imakado-fn () hok/hint-faces)
   :cache nil))

(defun hok/cycle-hint-face ()
  (funcall hok/cycle-hint-face))

(defstruct (hok/$hint
            (:constructor nil)
            (:constructor hok/make-$hint)
            (:copier hok/copy-$hint)
            :named)
  key overlay window point
  $result)
(defstruct (hok/$forward-result
            (:constructor nil)
            (:constructor hok/make-$forward-result)
            :named)
  match-beg
  match-end
  (cb-when-used 'ignore)
  (cb-when-selected nil)
  (hint-start nil)
  (match-string ""))
(imakado-define-with-struct-macro hok/with-$forward-result hok/$forward-result-)

(defsubst hok/$forward-result-get-hint-start ($result)
  (hok/with-$forward-result (match-beg hint-start) $result
    (if hint-start
        hint-start
      match-beg)))

(defun hok/make-key (index)
  (let* ((key-length (length hok/keys-char))
         (excess (/ index key-length))
         (n (% index key-length)))
    (concat (if (zerop excess)
                ""
              (hok/make-key (1- excess)))
            (char-to-string (nth n hok/keys-char)))))

(defun hok/to ($hint)
  (select-window (hok/$hint-window $hint))
  (goto-char
   (hok/$forward-result-get-hint-start
    (hok/$hint-$result $hint))))


(defvar hok/$hints nil)


(defvar hok/overlays nil)

(defun hok/cleanup-overlays ()
  (mapc 'delete-overlay hok/overlays)
  (setq hok/overlays nil))

(add-hook  'hok/after-hook
           'hok/cleanup-overlays)

(defun* hok/make-overlay ($result key &key (buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((key (format "%s" key)))
      (let ((point (hok/$forward-result-get-hint-start $result)))
        (save-excursion
          (goto-char point)
          (let* ((width (length key))
                 (rest width)
                 (begin point)
                 overlay)
            (while (and (> rest 0) (not (eolp)))
              (setq rest (- rest (char-width (char-after))))
              (forward-char))
            
            (if (and (eq (logand width 1) 1)
                     (= 2 (char-width
                           (or (char-before) 0))))
                (setq key (concat key " ")))
            (setq overlay
                  (make-overlay begin (point) (current-buffer)))
            (overlay-put overlay 'display (propertize key 'face (hok/cycle-hint-face)))
            (overlay-put overlay 'window (get-buffer-window buffer))
            (overlay-put overlay 'priority 1000)
            (push overlay hok/overlays)
            overlay))))))

(defun hok/delete-overlays ()
  (loop for $hint in hok/$hints
        do (when (overlayp (hok/$hint-overlay $hint))
             (delete-overlay
              (hok/$hint-overlay $hint)))))

(defvar hok/$windows-infos nil)
(defun hok/remove-hints ()
  (with-silent-modifications
    (hok/delete-overlays)
    (setq hok/$hints nil)
    (loop for ol in hok/ace-jump-background-overlays
          do (delete-overlay ol))
    (setq hok/$windows-infos nil
          hok/ace-jump-background-overlays nil)
    ))

(defun hok/put-background-face ()
  (loop for $windows-info in hok/$windows-infos
        when (buffer-live-p (hok/$windows-info-buffer $windows-info))
        do (let ((ol (make-overlay (hok/$windows-info-window-start $windows-info)
                                   (hok/$windows-info-window-end $windows-info)
                                   (hok/$windows-info-buffer $windows-info))))
             (overlay-put ol 'face 'hok/ace-jump-face-background)
             (push ol hok/ace-jump-background-overlays))))

(defsubst hok/do-forward-word-functions (window-end)
  (imakado-aand
   (loop for imakado-fn in hok/forward-word-functions
         nconc (save-excursion
                 (imakado-make-list (funcall imakado-fn window-end))))
   (imakado-!remif 'hok/$forward-result-p it)))

(defsubst hok/forward-word (window-end)
  "Move to beginning of a forward word, and return point."
  (interactive)
  (let* ((hook-$forward-result (hok/do-forward-word-functions window-end))
         ;; XXX hook
         ($forward-results `(
                             ,(hok/make-$forward-result
                               :match-beg (point-max)
                               :match-end (point-max))
                             ,@hook-$forward-result))
         (min-$forward-result (first
                               (sort* (delq nil $forward-results)
                                      '<
                                      :key 'hok/$forward-result-match-beg))))
    min-$forward-result))


(defsubst hok/forward-word-widget (window-end)
  "Move to beginning of a forward word, and return point."
  (interactive)
  (let* ((hook-$forward-result (hok/do-forward-word-functions window-end))
         ;; XXX hook
         ($forward-results `(
                             ,(hok/make-$forward-result
                               :match-beg (point-max)
                               :match-end (point-max))
                             ,@hook-$forward-result))
         (min-$forward-result (first
                               (sort* (delq nil $forward-results)
                                      '<
                                      :key 'hok/$forward-result-match-beg))))
    min-$forward-result))

(defstruct (hok/$buffer-info)
  window-start
  window-end )

(defvar hok/local-$buffer-info nil)
(make-variable-buffer-local 'hok/local-$buffer-info)



(defsubst hok/put-hint-keys ($hints)
  (let ((len (length $hints))
        ($hints (imakado-sort-by-average $hints 
                                         :key 'hok/$hint-point 
                                         :average (point))))
    (imakado-aand (hok/make-hints len)
                  (mapcar* (lambda ($hint key)
                             (setf (hok/$hint-key $hint) key)
                             (setf (hok/$hint-overlay $hint)
                                   (let ((show-hint (if hok/show-hint-as-single-key
                                                        (substring key 0 1)
                                                      key)))
                                     (hok/make-overlay (hok/$hint-$result $hint)
                                                       show-hint
                                                       :buffer (window-buffer (hok/$hint-window $hint))))))
                           $hints
                           it))))

(defun* hok/show-hints
    (&key (window-list (window-list)))
  (with-silent-modifications
    (condition-case err
        (loop for w in (imakado-make-list window-list)
              with index = 0
              with hints
              do
              (save-excursion
                (save-window-excursion
                  (select-window w)
                  (with-current-buffer (window-buffer w)
                    (move-to-window-line 0)
                    (let ((point (point))
                          (window-start (point))
                          (window-end (window-end w))
                          (key nil)     ;(hok/make-key index))
                          (first-$result
                           (hok/make-$forward-result
                            :match-beg (point)
                            :match-end (point))))
                      (setq hok/local-$buffer-info
                            (make-hok/$buffer-info
                             :window-start window-start
                             :window-end window-end))
                      (loop with $result = first-$result
                            while (< point (hok/$buffer-info-window-end
                                            hok/local-$buffer-info))
                            do (funcall (hok/$forward-result-cb-when-used $result))
                            do (push (hok/make-$hint
                                      :key nil ;(hok/make-key index)
                                      :overlay nil ;(hok/make-overlay $result key)
                                      :window w
                                      :point point
                                      :$result $result)
                                     hints)
                            do (imakado-awhen (hok/forward-word (hok/$buffer-info-window-end
                                                                 hok/local-$buffer-info))
                                 (hok/with-$forward-result (match-beg match-end hint-start) it
                                   (setq $result it)
                                   (let ((hint-start-point
                                          (hok/$forward-result-get-hint-start it)))
                                     (incf index)
                                     (setq point match-beg)
                                        ;key (hok/make-key index))
                                     (goto-char match-end)))))))))
              finally (progn (setq hok/$hints (nreverse hints))
                             (hok/put-hint-keys hok/$hints)
                             ;; (hok/put-overlays hok/$hints)
                             ))
      (error
       (hok/remove-hints)
       (error (error-message-string err))
       ))))


(defun* hok/hint-match (re-or-str hint-key &key (check-as-string nil))
  (let ((case-fold-search nil))
    (cond
     (check-as-string
      (string= hint-key re-or-str))
     (t
      (let ((re (concat "^" re-or-str)))
        (let ((ret (string-match re hint-key)))
          (prog1 ret

            )))))))


(defun hok/search-make-single-hint-key ($hint hint-key key)
  (cond
   (hok/show-hint-as-single-key
    (let ((hint-key hint-key))
      (let ((ret (substring hint-key
                            (length key)
                            (if (= (length hint-key)
                                   (length key))
                                (length key)
                              (1+ (length key))))))
        (prog1 ret
          ;; (message "hint-key: %s key: %s return: %s"
          ;;          hint-key key ret)
          ))))
   (t hint-key)))

(defun hok/search (key &optional check-as-string)
  (let ((res (loop with ret
                   for $hint in hok/$hints
                   do (cond
                       ((hok/hint-match key
                                        (hok/$hint-key $hint)
                                        :check-as-string check-as-string)
                        (delete-overlay (hok/$hint-overlay $hint))
                        (let ((str (hok/search-make-single-hint-key
                                    $hint
                                    (hok/$hint-key $hint)
                                    key)))
                          (setf (hok/$hint-overlay $hint)
                                (hok/make-overlay (hok/$hint-$result $hint)
                                                  str
                                                  :buffer (window-buffer (hok/$hint-window $hint)))))
                        (push $hint ret))
                       (t
                        (delete-overlay (hok/$hint-overlay $hint))
                                        ;(message "%s" (hok/$hint-overlay $hint))
                                        ;(move-overlay (hok/$hint-overlay $hint) (point-max) (point-max))
                        ;;(hok/make-overlay (hok/$hint-$result $hint) "hoge" :buffer (window-buffer (hok/$hint-window $hint)))
                        ;;(redraw-display)
                        ;; (message "deleted ovaerlay : %s" $hint)
                        ))
                   finally return (nreverse ret))))
    (setq hok/$hints res)))

(defstruct (hok/$windows-info
            (:type list)
            (:constructor hok/make-$windows-info)
            :named)
  window buffer window-start window-end)

(defun hok/set-window-list-data (window-list)
  (loop for w in (imakado-make-list window-list)
        do (push (hok/make-$windows-info
                  :window w
                  :buffer (window-buffer w)
                  :window-start (window-start w)
                  :window-end (window-end w))
                 hok/$windows-infos)))


(defvar hok/before-hook nil)
(defun hok (&optional arg)
  (interactive "P")
  (let ((window-list (cond
                      (arg (window-list))
                      (t (first (window-list))))))
    (unwind-protect
        (let ((k nil)
              (key nil))  
          (hok/set-window-list-data window-list)
          (run-hooks 'hok/before-hook)
          (hok/put-background-face)
          (hok/show-hints :window-list window-list)
          (while (not (null hok/$hints))
            (setq k (read-event (concat "HOK: " key)))
            (cond
             ((and (not (null key))
                   (not (member (char-to-string k) (hok/hint-keys-list))))
              (hok/search key t))
             (t
              (setq key (concat key (char-to-string k)))
              (hok/search key)))
            ;; Just one hint
            (when (= 1 (length hok/$hints))
              (unwind-protect
                  (let (($hint (first hok/$hints)))
                    (hok/action $hint))
                (hok/remove-hints)))))
      (hok/remove-hints)
      (run-hooks 'hok/after-hook)
      (loop for w in (imakado-make-list window-list)
            do (with-current-buffer (window-buffer w)
                 (setq hok/local-$buffer-info nil))))))

(defun hok/action ($hint)
  (cond
   ((imakado-when-let ($result (hok/$hint-$result $hint))
      (imakado-aand (hok/$forward-result-cb-when-selected $result)
                    (and (functionp it)
                         (funcall it $result $hint)
                         t)))
    'donothing)
   (t (hok/to $hint))))


(defun hok/double-hok ()
  (interactive)
  (let ((p1
         (save-excursion
           (hok)
           (push-mark (point) t t)
           (point)
           ))
        (p2
         (save-excursion
           (hok)(point)
           )))
    (values p1 p2)))

(defun hok/simple-selection (cb)
  (interactive)
  (imakado-dbind (s e) (hok/double-hok)
    (funcall cb s e)))

(defun hok/get-hok-point ()
  (save-excursion
    (hok)
    (point)))

(defun hok/copy-selection ()
  (interactive)
  (hok/simple-selection
   (imakado-fn (s e) (kill-new
                      (imakado-positions->string s e)))))

(defun hok/kill-selection ()
  (interactive)
  (hok/simple-selection
   (imakado-fn (s e)
     (kill-region  s e)
     (goto-char s))))

(defun hok/occur ()
  (interactive)
  (goto-char (hok/get-hok-point))
  (hok/occur-aux))

(defun hok/occur-aux ()
  (cond
   ((functionp 'anything-c-moccur-occur-by-moccur)
    (call-interactively 'anything-c-moccur-occur-by-moccur))
   (t
    (occur (thing-at-point 'symbol)))))

(defsubst hok/hint-word (window-end)
  (let ((re (rx (+ word))))
    (cond 
     ((re-search-forward re window-end t)
      (imakado-with-anaphoric-match-utilities nil
        (goto-char $MB-0)
        (hok/make-$forward-result
         :match-beg (point)
         :match-end $ME-0
         :match-string $m)))
     (t
      nil))))

(defsubst hok/hint-word-action-press-returnkey (window-end)
  (let ((re (rx (+ graph))))
    (cond 
     ((re-search-forward re window-end t)
      (imakado-with-anaphoric-match-utilities nil
        (goto-char $MB-0)
        (hok/make-$forward-result
         :match-beg (point)
         :match-end $ME-0
         :match-string $m
         :cb-when-selected 'hok/forward-and-press-ret-action)))
     (t
      nil))))

(defun hok/simple (&optional arg)
  (interactive "P")
  (let ((hok/forward-word-functions
         (list 'hok/hint-word)))
    (hok)))

(defun hok/press-return (&optional arg)
  (interactive "P")
  (cond
   (arg 
    (let ((hok/forward-word-functions
           (list 'hok/forward-widget)))
      (hok)))
   (t
    (let ((hok/forward-word-functions
           (list 'hok/hint-word-action-press-returnkey)))
      (hok t)))))

(defun hok/widget (&optional arg)
  (interactive "P")
  (let ((hok/forward-word-functions
         (list 'hok/forward-widget)))
    (hok)))

(defun hok/press-enter (&optional arg)
  (interactive "P")
  (let ((hok/forward-word-functions
         (list 'hok/forward-and-press-ret)))
    (hok)))
(defalias 'hok/after-move-press-enter 'hok/press-enter)

(defvar hok/ace-jump-mode-hint-re nil)

(defun hok/ace-jump-mode (&optional arg)
  (interactive "P")
  (cond
   (arg (hok/ace-jump-mode-string))
   (t
    (let* ((hint-char (read-char "Head Char:"))
           (hint-str (char-to-string hint-char)))
      (cond 
       ((equal " " hint-str)
        (hok/ace-jump-mode-string))
       (t
        (let ((re (rx-to-string `(seq word-start
                                      (group
                                       ,hint-str
                                       (* (or (syntax word)
                                              (syntax symbol)))
                                       word-end)))))
          (setq hok/ace-jump-mode-hint-re re)
          (let ((hok/forward-word-functions
                 (list 'hok/forward-ace-jump-mode)))
            (hok t)))))))))

(defun hok/ace-jump-mode-string ()
  (interactive)
  (let* ((hint-str (read-string "string:"))
         (re (rx-to-string `(seq (group
                                  ,hint-str
                                  )))))
    (setq hok/ace-jump-mode-hint-re re)
    (let ((hok/forward-word-functions
           (list 'hok/forward-ace-jump-mode)))
      (hok t))))

(defun hok/ace-jump-mode-single-string ()
  (interactive)
  (let* ((hint-str (char-to-string (read-char "string:")))
         (re (rx-to-string `(seq (group
                                  ,hint-str
                                  )))))
    (setq hok/ace-jump-mode-hint-re re)
    (let ((case-fold-search t)
          (hok/forward-word-functions
           (list 'hok/forward-ace-jump-mode)))
      (hok t))))

(defun hok/ace-jump-mode-string-space (&optional arg)
  (interactive "P")
  (let* ((re (rx-to-string `(seq (group
                                  (or (seq anything (syntax string-quote))
                                      (or (+ space)
                                          (seq graphic eol)
                                          (syntax string-delimiter)
                                          (syntax paired-delimiter)
                                          (seq (syntax string-quote)
                                               not-newline)
                                          (seq not-newline
                                               (syntax string-quote))))
                                  )))))
    (setq hok/ace-jump-mode-hint-re re)
    (let ((hok/forward-word-functions
           (list 'hok/forward-ace-jump-mode)))
      (hok arg))))


    ;;;; XXX
(defun hok/forward-ace-jump-mode (window-end)
  (save-excursion
    (and (re-search-forward hok/ace-jump-mode-hint-re window-end t)
         (imakado-with-anaphoric-match-utilities nil
           (hok/make-$forward-result
            :match-beg $MB-1
            :match-end $ME-1
            :match-string $m)))))

;;;; XXX
(defvar hok/forward-word-point-regexps
  (list (rx "</")
        ;;(rx (and "(" (+ (syntax word))))
                                        ;(rx (>= 2 word))
                                        ;(rx (>= 5 word))
                                        ;"[[:word:]]\\{5,\\}"
        "\\w\\{3,\\}"
        ))

(defun hok/forward-word-point (window-end)
  (loop for re in hok/forward-word-point-regexps
        when (save-excursion
               (re-search-forward re window-end t))
        collect 
        (imakado-with-anaphoric-match-utilities nil
          (hok/make-$forward-result
           :match-beg $MB-0
           :match-end $ME-0
           :match-string $m))))


(add-hook  'hok/forward-word-functions
           'hok/forward-word-point)



(defun hok/forward-widget (window-end)
  (let ((prop 'button))
    (save-excursion
      (loop 
       for next-change = (or (next-char-property-change (point) (point-max))
                             (point-max))
       for current-point = (point)
       until (eobp)
       do (goto-char next-change)
       do (cond
           ((imakado-inq (get-char-property current-point 'face)
              widget-field )
            (goto-char next-change)
            (return (hok/make-$forward-result
                     :match-beg current-point
                     :match-end next-change
                     :match-string (buffer-substring current-point
                                                     next-change))))
           ((memq 'w3m-anchor (imakado-make-list
                               (get-char-property current-point 'face)))
            (goto-char next-change)
            (return (hok/make-$forward-result
                     :match-beg current-point
                     :match-end next-change
                     :match-string (buffer-substring current-point
                                                     next-change)
                     :cb-when-selected (imakado-fn ($result $hint)
                                         (goto-char (hok/$forward-result-match-beg
                                                     $result))
                                         (call-interactively 'w3m-view-this-url)))))
           ((eq (get-char-property current-point 'button) t)
            (return (hok/make-$forward-result
                     :match-beg current-point
                     :match-end next-change
                     :match-string (buffer-substring current-point
                                                     next-change)
                     :cb-when-selected (imakado-fn ($result $hint)
                                         (goto-char (hok/$forward-result-match-beg
                                                     $result))
                                         (call-interactively 'push-button))
                     )))
           ;; w3m-anchor
           ((get-char-property current-point 'button)
            (return (hok/make-$forward-result
                     :match-beg current-point
                     :match-end next-change
                     :match-string (buffer-substring current-point
                                                     next-change)
                     :cb-when-selected (imakado-fn ($result $hint)
                                         (goto-char (hok/$forward-result-match-beg
                                                     $result))
                                         (call-interactively 'widget-button-press))
                     )))
           (t (goto-char next-change)))))))

(defun hok/forward-and-press-ret (window-end)
  (let ((prop 'button))
    (save-excursion
      (loop 
       for next-change = (or (next-char-property-change (point) (point-max))
                             (point-max))
       for current-point = (point)
       until (eobp)
       do (goto-char next-change)
       do (progn
            (goto-char next-change)
            (return (hok/make-$forward-result
                     :match-beg current-point
                     :match-end next-change
                     :match-string (buffer-substring current-point
                                                     next-change)
                     :cb-when-selected 'hok/forward-and-press-ret-action)))))))
(defun hok/forward-and-press-ret-action ($result $hint)
  (hok/to $hint)
  (let ((cmd (key-binding (kbd "RET")))
        (case-fold-search t))
    (when (or (imakado-=~ "custom" (format "%s" cmd))
              (not (imakado-=~ "newline" (format "%s" cmd))))
      (call-interactively cmd))))

;;;; Hooks


(defun hok/hint-double-quote (window-end)
  (let ((re (rx
             (or "\"" "(" "{" "[")
             (group
              (* space)
              (or "\"" ")" "}" "]"
                  )))))
    (when (re-search-forward re window-end t)
      (hok/make-$forward-result
       :match-beg (match-beginning 0)
       :match-end (match-end 0)
       :hint-start (match-beginning 1)
       :match-string (match-string-no-properties 0)))))
(add-hook  'hok/forward-word-functions
           'hok/hint-double-quote)


;;;; hint-blank-line
(defvar hok/hint-blank-line-spacer-str "_______")
(imakado-defcacheable hok/hint-blank-line-spacer-str ()
  (imakado-aand (imakado-pput hok/hint-blank-line-spacer-str :hok/spacer t)
                it))

(defun hok/hint-blank-line (window-end)
  (unless buffer-read-only
    (let ((re (rx bol (* space) eol)))
      (when (re-search-forward re window-end t)
        ;;(forward-line -1)
        (hok/make-$forward-result
         :cb-when-used (lambda ()
                         (let ((s (hok/hint-blank-line-spacer-str)))
                           (insert s)
                           (incf (hok/$buffer-info-window-end
                                  hok/local-$buffer-info)
                                 (length s))
                           (search-backward s)))
         :match-beg (point)
         :match-end (point-at-eol)
         :match-string (match-string-no-properties 0))))))


;; (add-hook  'hok/forward-word-functions
;;            'hok/hint-blank-line)
;; (add-hook  'hok/after-hook
;;            'hok/hint-blank-line-remove-spacers)

(defun* hok/hint-blank-line-remove-spacers
    (&key (property :hok/spacer)
          (windows (window-list)))
  (loop for w in windows
        do
        (save-excursion
          (save-window-excursion
            (select-window w)
            (with-current-buffer (window-buffer w)
              (unless buffer-read-only
                (when hok/local-$buffer-info
                  (save-excursion
                    (with-silent-modifications
                      (loop with re = (concat "^" "\\(?:[[:space:]]*\\)?"  hok/hint-blank-line-spacer-str )
                            initially (goto-char (hok/$buffer-info-window-start hok/local-$buffer-info))
                            while (re-search-forward
                                   re
                                   nil
                                   t)
                            do (delete-region (match-beginning 0) (match-end 0))))))))))))

(defun hok/bm ()
  (interactive)
  (require 'elp)
  (elp-instrument-package "hok/"))


(defun hok/reset-bm ()
  (interactive)
  (require 'elp)
  (with-no-warnings
    (elp-restore-all)))



(imakado-defcacheable hok/hint-keys-list ()
  (delete-dups
   (split-string hok/hint-keys "" t)))

(defvar hok/reverse-hints nil)

(defun* hok/make-next-hint 
    (hint-str &key (unique-only t))
  (declare (special reverse-hints num-hints))
  (let ((case-fold-search nil))
    (let ((hint-length (length hint-str))
          (hint-keys-len (length  hok/hint-keys)))
      (cond
       ((equal hint-str "")
        (nth 0 (hok/hint-keys-list)))
       (t
        (let ((hint-but-last (substring hint-str 0 (- hint-length 1)))
              (n-point (progn (string-match (substring hint-str (- hint-length 1) hint-length)
                                            hok/hint-keys)
                              (1+ (match-beginning 0)))))
          (cond
           ((= n-point hint-keys-len)
            (let ((next (hok/make-next-hint hint-but-last)))
              (when unique-only
                (setq reverse-hints (delete next reverse-hints))
                (decf num-hints))
              (concat next (nth 0 (hok/hint-keys-list)))))
           (t
            (concat hint-but-last (nth n-point (hok/hint-keys-list)))))))))))

(defun hok/make-hints (amount)
  (let ((hint "")
        (reverse-hints nil)
        (num-hints 0))
    (while (< num-hints amount)
      (setq hint (hok/make-next-hint hint))
      (push hint reverse-hints)
      (incf num-hints))
    (nreverse reverse-hints)))

(provide 'hok)

;;; hok.el ends here

;; Local variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
