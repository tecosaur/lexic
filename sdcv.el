;;; sdcv.el --- major mode to do dictionary query through sdcv -*- lexical-binding: t; -*-

;; Copyright 2006~2008 pluskid,
;;           2011~2012 gucong
;;           2020 tecosaur
;;
;; Author: pluskid <pluskid@gmail.com>,
;;         gucong <gucong43216@gmail.com>,
;;         tecosaur <tec@tecosaur.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a major mode to view output of dictionary search of sdcv.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;   (require 'sdcv-mode)
;;   (global-set-key (kbd "C-c d") 'sdcv-search)

;;; Changelog:

;; 2020/07/28
;;     * New variable: `sdcv-dictionary-specs', allows for
;;       - Dictionary display names
;;       - Custom dictionary entry formatters
;;       - Dictionary sorting
;;     * Update outline function calls to replace depreciated names.'
;;     * Tweak sdcv-mode
;;       - Remove font-locking
;;       - Change `outline-regexp' to ZERO WIDTH SPACE
;;       - Add `outline-heading-end-regexp', a PUNCTUATION SPACE
;;       - Expand the mode map, to bind
;;         - Two modes of entry navigation
;;         - History navigation
;;         - TAB for toggling an entry
;;     * Expand popup window
;;     * Add linear history navigation
;;     * Revise behaviour of `sdcv-next-entry' and `sdcv-previous-entry'
;;     * New function: `sdcv-get-outline-path' which gives the structural path
;;       to the current position in buffer, e.g. dict → word v. t. → 1. (Chem.)
;;     * Remove (now unused) custom face vars, could do with adding some
;;       new face vars in the future
;;     * Change the default of `sdcv-program-path' to be an absolute path
;;     * New functions: `sdcv-format-result', `sdcv-failed-p',
;;       and `sdcv-format-failure' to handle the upgraded entry prodessing
;;     * New functions: `sdcv-format-webster', `sdcv-format-online-etym',
;;       `sdcv-format-element', and `sdcv-format-soule' to format
;;       the dictionaries recognised by default in `sdcv-dictionary-specs'.
;;       - with helper functions `sdcv-format-webster-diacritics', and
;;         `sdcv-format-expand-abbreviations' for nicer content.

;; 2012/01/02
;;     * New variable: `sdcv-word-processor'
;;     * Breaking change:
;;       for `shttps://github.com/gucong/emacs-sdcv/blob/master/sdcv-mode.elhttps://github.com/gucong/emacs-sdcv/blob/master/sdcv-mode.eldcv-dictionary-list' and `sdcv-dictionary-alist',
;;       non-list (non-nil) value now means full dictionary list
;;     * Rewrite `sdcv-search' for both interactive and non-interactive use
;;     * `sdcv-dictionary-list' is left for customization use only
;;     * Better highlighting.
;;
;; 2011/06/30
;;     * New feature: parse output for failed lookup
;;     * Keymap modification
;;
;; 2008/06/11
;;     * sdcv-mode v 0.1 init (with background process)

;;; Code:

(require 'outline)
(require 'dash)
(require 'cl)
(provide 'sdcv)

;;; ==================================================================
;;; Frontend, search word and display sdcv buffer
(defun sdcv-search (word &optional dict-list-name dict-list interactive-p no-history-p)
  "Search WORD through the command-line tool sdcv.
The result will be displayed in buffer named with
`sdcv-buffer-name' with `sdcv-mode' if called interactively.

When provided with DICT-LIST-NAME, query `sdcv-dictionary-alist'
to get the new dictionary list before search.
Alternatively, dictionary list can be specified directly
by DICT-LIST.  Any non-list value of it means using all dictionaries.

When called interactively, prompt for the word.
Prefix argument have the following meaning:
If `sdcv-dictionary-alist' is defined,
use prefix argument to select a new DICT-LIST-NAME.
Otherwise, prefix argument means using all dictionaries.

Word may contain some special characters:
    *       match zero or more characters
    ?       match zero or one character
    /       used at the beginning, for fuzzy search
    |       used at the beginning, for data search
    \       escape the character right after"
  (interactive
   (let* ((dict-list-name
           (and current-prefix-arg sdcv-dictionary-alist
                (completing-read "Select dictionary list: "
                                 sdcv-dictionary-alist nil t)))
          (dict-list
           (and current-prefix-arg (not sdcv-dictionary-alist)))
          (guess (or (and transient-mark-mode mark-active
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))
                     (current-word nil t)))
          (word (read-string (format "Search dict (default: %s): " guess)
                             nil nil guess)))
     (list word dict-list-name dict-list t)))
  ;; init current dictionary list
  (when (null sdcv-current-dictionary-list)
    (setq sdcv-current-dictionary-list sdcv-dictionary-list))
  ;; dict-list-name to dict-list
  (when (and (not dict-list) dict-list-name)
    (if (not sdcv-dictionary-alist)
        (error "`sdcv-dictionary-alist' not defined"))
    (setq dict-list
          (cdr (assoc dict-list-name sdcv-dictionary-alist))))
  ;; prepare new dictionary list
  (when (and dict-list (not (equal sdcv-current-dictionary-list dict-list)))
    (setq sdcv-current-dictionary-list dict-list)
    ;; kill sdcv process
    (and (get-process sdcv-process-name)
         (kill-process (get-process sdcv-process-name)))
    (while (get-process sdcv-process-name)
      (sleep-for 0.01)))
  (let ((result
          (mapconcat
           (lambda (w) (sdcv-do-lookup w))
           (if sdcv-word-processor
               (let ((processed (funcall sdcv-word-processor word)))
                 (if (listp processed) processed (list processed)))
             (list word))
           "")))
    (unless no-history-p
      (setq sdcv--search-history
            (append (subseq sdcv--search-history
                            0 sdcv--search-history-position)
                    (list word))
            sdcv--search-history-position (length sdcv--search-history)))
    (if (not interactive-p)
        result
      (with-current-buffer (get-buffer-create sdcv-buffer-name)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert result))
      (sdcv-goto-sdcv)
      (sdcv-mode)
      (sdcv-mode-reinit)
      (let* ((window (get-buffer-window (sdcv-get-buffer)))
             (height (window-height window))
             (min-height (pcase (count-lines (point-min) (point-max))
                           ((pred (> 50)) 12)
                           ((pred (> 100)) 16)
                           (_ 20))))
        (when (> min-height height)
          (window-resize window (- 12 height)))))))

(defun sdcv-search-word-at-point ()
  (interactive)
  (sdcv-search
   (downcase
    (or (and transient-mark-mode mark-active
             (buffer-substring-no-properties
              (region-beginning) (region-end)))
        (current-word nil t))) nil nil t))

(defun sdcv-list-dictionary ()
  "Show available dictionaries."
  (interactive)
  (let (resize-mini-windows)
    (shell-command "sdcv -l" sdcv-buffer-name)))

(defvar sdcv-current-dictionary-list nil)

(defun sdcv-generate-dictionary-argument ()
  "Generate dictionary argument for sdcv from `sdcv-current-dictionary-list'
and `sdcv-dictionary-path'."
  (append
   (and sdcv-dictionary-path (list "--data-dir" sdcv-dictionary-path))
   (and (listp sdcv-current-dictionary-list)
        (mapcan (lambda (dict)
                  (list "-u" dict))
                sdcv-current-dictionary-list))))

(defvar sdcv--search-history nil)
(defvar sdcv--search-history-position 0)

(defun sdcv-search-history-backwards ()
  (interactive)
  (if (> sdcv--search-history-position 0)
      (sdcv-search (nth (setq sdcv--search-history-position
                              (1- sdcv--search-history-position))
                        sdcv--search-history)
                   nil nil t t)
    (message "At start of search history.")))

(defun sdcv-search-history-forwards ()
  (interactive)
  (if (> (length sdcv--search-history) sdcv--search-history-position)
      (sdcv-search (nth (setq sdcv--search-history-position
                              (1+ sdcv--search-history-position))
                        sdcv--search-history)
                   nil nil t t)
    (message "At end of search history.")))

;;; ==================================================================
;;; utilities to switch from and to sdcv buffer
(defvar sdcv-previous-window-conf nil
  "Window configuration before switching to sdcv buffer.")
(defun sdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (interactive)
  (unless (eq (current-buffer)
	      (sdcv-get-buffer))
    (setq sdcv-previous-window-conf (current-window-configuration)))
  (let* ((buffer (sdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun sdcv-return-from-sdcv ()
  "Bury sdcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p sdcv-previous-window-conf)
      (progn
        (set-window-configuration sdcv-previous-window-conf)
        (setq sdcv-previous-window-conf nil)
        (bury-buffer (sdcv-get-buffer)))
    (bury-buffer)))

(defun sdcv-get-buffer ()
  "Get the sdcv buffer. Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    buffer))

;;; ==================================================================

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'sdcv-return-from-sdcv)
    (define-key map (kbd "RET") 'sdcv-search-word-at-point)
    (define-key map "a" 'outline-show-all)
    (define-key map "h" 'outline-hide-body)
    (define-key map "o" 'sdcv-toggle-entry)
    (define-key map (kbd "TAB") 'sdcv-toggle-entry)
    (define-key map "n" 'sdcv-next-entry)
    (define-key map "N" (lambda () (interactive) (sdcv-next-entry t)))
    (define-key map "p" 'sdcv-previous-entry)
    (define-key map "P" (lambda () (interactive) (sdcv-previous-entry t)))
    (define-key map "b" 'sdcv-search-history-backwards)
    (define-key map "f" 'sdcv-search-history-forwards)
    map)
  "Keymap for `sdcv-mode'.")

(define-derived-mode sdcv-mode text-mode "sdcv"
  "Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'."
  (setq buffer-read-only t)
  (setq-local outline-regexp "\u200B+")
  (setq-local outline-heading-end-regexp "\u2008"))

(defun sdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entrys but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only nil)
    (sdcv-parse-failed)
    (setq buffer-read-only t)
    (when (< 30 (count-lines (point-min) (point-max)))
        (outline-hide-sublevels 3))
    (goto-char (point-min))
    (search-forward "\u200B\u200B")
    (left-char 1)))

(defun sdcv-parse-failed ()
  (goto-char (point-min))
  (let (save-word)
    (while (re-search-forward "^[0-9]+).*-->\\(.*\\)$" nil t)
      (let ((cur-word (match-string-no-properties 1)))
        (unless (string= save-word cur-word)
          (setq save-word cur-word)
          (re-search-backward "^\\(.\\)" nil t)
          (match-string 1)
          (insert (format "\n==>%s\n" save-word)))))))

(defun sdcv-expand-entry ()
  "Show the children of the current entry, or subtree if there are none."
  (outline-show-children)
  (when ; no children
      (<= 0 (- (save-excursion (outline-next-heading) (point))
                (save-excursion (outline-end-of-subtree) (point))))
    (outline-show-subtree)))

(defun sdcv-next-entry (&optional linear)
  "Move to the next entry, targeting the same level unless LINEAR is set."
  (interactive)
  (when (< 1 (sdcv-outline-level))
    (outline-hide-subtree))
  (if linear
      (outline-next-heading)
    (condition-case nil
        (outline-forward-same-level 1)
      (error
       (condition-case nil
           (progn
             (outline-up-heading 1 t)
             (outline-forward-same-level 1))
         (error (progn (outline-next-heading)
                       (sdcv-expand-entry)))))))
  (sdcv-expand-entry)
  (recenter-top-bottom 1)
  (message "%s" (sdcv-get-outline-path)))

(defun sdcv-previous-entry (&optional linear)
  "Move to the previous entry, targeting the same level unless LINEAR is set."
  (interactive)
  (outline-hide-subtree)
  (if (= 2 (line-number-at-pos))
      (recenter-top-bottom -1)
    (if linear
        (outline-previous-heading)
      (condition-case nil
          (outline-backward-same-level 1)
        (error
         (condition-case nil
             (outline-up-heading 1 t)
           (error (outline-previous-heading))))))
    (sdcv-expand-entry)
    (recenter-top-bottom 2))
  (message "%s" (sdcv-get-outline-path)))

(defun sdcv-toggle-entry ()
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (save-excursion
               (outline-end-of-heading)
               (outline-invisible-p (line-end-position))))
        (outline-hide-subtree)
      (outline-show-subtree))))

;;; ==================================================================
;;; Support for sdcv process in background
(defun sdcv-do-lookup (word &optional raw-p)
  "Send the word to the sdcv process and return the result."
  (let ((process (sdcv-get-process)))
    (process-send-string process (concat word "\n"))
    (with-current-buffer (process-buffer process)
      (let ((i 0) result done)
	(while (and (not done)
		    (< i sdcv-wait-timeout))
	  (when (sdcv-match-tail sdcv-word-prompts)
	    (setq result (buffer-substring-no-properties (point-min)
						      (point-max)))
	    (setq done t))
	  (when (sdcv-match-tail sdcv-choice-prompts)
	    (process-send-string process "-1\n"))
	  (unless done
	    (sleep-for sdcv-wait-interval)
	    (setq i (+ i sdcv-wait-interval))))
	(unless (< i sdcv-wait-timeout)
	  ;; timeout
	  (kill-process process)
	  (error "ERROR: timeout waiting for sdcv"))
	(erase-buffer)
  (if raw-p result
    (sdcv-format-result result))))))

(defun sdcv-oneshot-lookup (word &optional raw-p args)
  (let ((result (shell-command-to-string
                 (concat sdcv-program-path " -n " args
                         " '" (replace-regexp-in-string "'" "\\'" word) "'"))))
    (if raw-p result
      (sdcv-format-result result))))

(defvar sdcv-wait-timeout 2
  "The max time (in seconds) to wait for the sdcv process to
produce some output.")
(defvar sdcv-wait-interval 0.01
  "The interval (in seconds) to sleep each time to wait for
sdcv's output.")

(defconst sdcv-process-name "%sdcv-mode-process%")
(defconst sdcv-process-buffer-name "*sdcv-mode-process*")

(defvar sdcv-word-prompts '("Enter word or phrase: ")
  "A list of prompts that sdcv use to prompt for word.")

(defvar sdcv-choice-prompts '("Your choice[-1 to abort]: ")
  "A list of prompts that sdcv use to prompt for a choice
of multiple candicates.")

(defvar sdcv-result-patterns '("^Found [0-9]+ items, similar to [*?/|]*\\(.+?\\)[*?]*\\.")
  "A list of patterns to extract result word of sdcv. Special
characters are stripped.")

(defun sdcv-get-process ()
  "Get or create the sdcv process."
  (let ((process (get-process sdcv-process-name)))
    (when (null process)
      (with-current-buffer (get-buffer-create
                            sdcv-process-buffer-name)
        (erase-buffer)
        (setq process (apply 'start-process
                             sdcv-process-name
                             sdcv-process-buffer-name
                             sdcv-program-path
                             (sdcv-generate-dictionary-argument)))
        ;; kill the initial prompt
        (let ((i 0))
          (message "starting sdcv...")
          (while (and (not (sdcv-match-tail sdcv-word-prompts))
                      (< i sdcv-wait-timeout))
            (sit-for sdcv-wait-interval t)
            (setq i (+ i sdcv-wait-interval)))
          (unless (< i sdcv-wait-timeout)
            ;; timeout
            (kill-process process)
            (error "ERROR: timeout waiting for sdcv"))
          (erase-buffer))))
    process))

(defun sdcv-buffer-tail (length)
  "Get a substring of length LENGTH at the end of
current buffer."
  (let ((beg (- (point-max) length))
	(end (point-max)))
    (if (< beg (point-min))
	(setq beg (point-min)))
    (buffer-substring-no-properties beg end)))

(defun sdcv-match-tail (prompts)
  (let ((done nil)
	(prompt nil))
    (while (and (not done)
		prompts)
      (setq prompt (car prompts))
      (setq prompts (cdr prompts))
      (when (string-equal prompt
                          (sdcv-buffer-tail (length prompt)))
        (delete-region (- (point-max) (length prompt))
                       (point-max))
        (setq done t)))
    done))

;;;;##################################################################
;;;;  Output Processing
;;;;##################################################################

(defun sdcv-format-result (result)
  "For a given RESULT from sdcv, test for failure and format accordingly.
Entries are sorted by their :priority in `sdcv-dictionary-specs', and formatted by
`sdcv-format-result' in the case of success; `sdcv-format-failure' otherwise."

  (if (sdcv-failed-p result)
      (sdcv-format-failure result)

    (let* ((entries
            (sort (sdcv-parse-results result)
                  (lambda (a b)
                    (< (or (sdcv-dictionary-spec (plist-get a :dict) :priority) 1)
                       (or (sdcv-dictionary-spec (plist-get b :dict) :priority) 1)))))
           (word (save-match-data
                   (string-match "\\`Found.* similar to \\(\\w+\\)\\." result)
                   (downcase (match-string 1 result)))))
      (concat
       "\u200B"
       (propertize (capitalize word) 'face 'outline-1)
       "\u2008"
       (apply #'concat
              (mapcar (lambda (e)
                        (sdcv-format-entry
                         e word))
                      entries))))))

(defun sdcv-parse-results (result)
  "TODO"
  (let (entries latest-match last-match dict word)
    (with-temp-buffer
      (insert result)
      (goto-char (point-min))
      (while
          (setq latest-match (re-search-forward
                              "-->\\([^\n]+\\)\n-->\\(.+\\)\n\n" nil t))
        (when last-match
          (forward-line -3)
          (setq entries
                (append entries
                        `((:word ,word
                           :dict ,dict
                           :info ,(buffer-substring last-match (point))))))
          (forward-line 3))
        (setq last-match latest-match)
        (setq dict (match-string 1))
        (setq word (match-string 2)))
      (when last-match
        (setq entries
              (append entries
                      `((:word ,word
                         :dict ,dict
                         :info ,(buffer-substring last-match (point-max)))))))))
  )

(defun sdcv-failed-p (results)
  "Whether the RESULTS match the hardcoded failure pattern."
  (if (string-match-p "Found [0-9]+ items, similar to [^.]+\\.\n0)" results) t nil))

(defun sdcv-format-failure (results)
  "When sdcv failed to match the word, format the suggestions in RESULTS."
  (let (suggestions last-match)
    (while (setq last-match
                 (string-match "^[0-9]+)\\(.*\\)-->\\([A-Za-z]+\\)"
                               results
                               (when last-match (1+ last-match))))
      (let ((dict (match-string 1 results))
            (word (match-string 2 results)))
        (if (assoc dict suggestions)
            (setcdr (assoc dict suggestions)
                    (list (append (cadr (assoc dict suggestions)) (list word))))
          (setq suggestions (append suggestions `((,dict . ((,word)))))))))
    (concat
     (propertize
      (replace-regexp-in-string
       "items" "entries"
       (substring results 0 (string-match "\n" results)))
      'face 'warning)
     "\n"
     (mapconcat (lambda (dict-suggestions)
                  (format "\u200B\u200B%s\n\u200B\u200B\u200B%s"
                          (propertize (or
                                       (sdcv-dictionary-spec (car dict-suggestions) :short)
                                       (car dict-suggestions))
                                      'face 'outline-3)
                          (propertize (s-join "\n\u200B\u200B\u200B"
                                              (cadr dict-suggestions))
                                      'face 'font-lock-keyword-face)))
                (sort suggestions
                      (lambda (a b)
                        (< (or (sdcv-dictionary-spec (car a) :priority) 1)
                           (or (sdcv-dictionary-spec (car b) :priority) 1))))
                "\n"))))

(defun sdcv-format-entry (entry &optional expected-word)
  "Format a given ENTRY, a plist with :word :dict and :info.
If the DICT has a :short value in `sdcv-dictionary-specs' that is used as
the display name. Likewise if present, :formatter is used to generate the
entry."
  (let ((dict (plist-get entry :dict)))
    (concat
     "\n\u200B\u200B"
     (propertize (or (sdcv-dictionary-spec dict :short)
                     dict) 'face 'outline-3)
     "\n\u2008\n"
     (if-let* ((formatter (sdcv-dictionary-spec dict :formatter)))
         (let ((case-fold-search nil))
           (string-trim (funcall formatter entry expected-word)))
       entry)
     "\n")))

(defun sdcv-get-outline-path ()
  "Return a string giving the structural path to the current position."
  (let ((outline-path "")
        (last-pos 0)
        outline-level-current substring level-regexp)
    (save-excursion
      (outline-back-to-heading)
      (setq outline-level-current (sdcv-outline-level))
      (while (/= (point) last-pos)
        (setq outline-level-current (sdcv-outline-level))
        (setq substring
              (buffer-substring
               (point)
               (save-excursion (search-forward "\u2008") (point))))
        (setq level-regexp
              (case outline-level-current
                (1 "^\\([^\n]+\\)")
                (2 "^\\([^ \n]+\\)")
                (3 "^\u200B\u200B*\\([^,]+\\(?:, [ &.;a-z]+\\)?\\)")
                (4 "\\([0-9]+\\.\\(  ?([^)]+)\\)?\\( \\w+\\)\\{0,4\\}\\)")
                (5 "\\(([a-z])\\(  ?([^)]+)\\)?\\( \\w+\\)\\{0,4\\}\\)")
                (t "^\u200B\u200B*\\([^ ]+\\)")))
        (setq outline-path
              (concat
               (propertize " → " 'face 'bold)
               (save-match-data
                 (string-match level-regexp substring)
                 (match-string 1 substring))
               outline-path))
        (setq last-pos (point))
        (ignore-errors
          (outline-up-heading 1)))
     (substring outline-path 2))))

(defun sdcv-outline-level ()
  "It seems that while (outline-level) should work, it has issues."
  (- (save-excursion (outline-back-to-heading)
                     (search-forward-regexp "\u200B+"))
     (point)))

(defun sdcv-dictionary-spec (dict spec)
  "Helper function to get a :SPEC of a given DICT."
  (plist-get (cdr (assoc dict sdcv-dictionary-specs)) spec))

(defvar sdcv-dictionary-specs
  '(("Webster's Revised Unabridged Dictionary (1913)"
     :formatter sdcv-format-webster
     :priority 1)
    ("Elements database"
     :short "Element"
     :formatter sdcv-format-element
     :priority 2)
    ("Hitchcock's Bible Names Dictionary"
     :short "Hitcchcock's Bible Names"
     :priority 3)
    ("Online Etymology Dictionary"
     :short "Etymology"
     :formatter sdcv-format-online-etym
     :priority 4)
    ("Soule's Dictionary of English Synonyms"
     :short "Synonyms"
     :formatter sdcv-format-soule
     :priority 5))
  "Alist of dictionary information, where the car is the name
according to sdcv, and the cdr is a plist whith the following options:
  :short - a (usually) shorter display name for the dictionary
  :formatter - a function with signature (ENTRY WORD) that returns a string
  :priority - sort priority, defaults to 1")

(defun sdcv-format-webster (entry &optional expected-word)
  "Make a Webster's dictionary ENTRY for WORD look nice.
Designed for Webster's Revised Unabridged Dictionary (1913),
as found at http://download.huzheng.org/dict.org/stardict-dictd-web1913-2.4.2.tar.bz2.

This should also work nicely with GCIDE."
    (->> (plist-get entry :info)
         (sdcv-format-webster-diacritics)
         (replace-regexp-in-string ; entry dividors
          (format "\n\n\\(%s\\)" (plist-get entry :word))
          "\n                     ━━━━━━━━━ ■ ━━━━━━━━━\n\n\\1")
         (replace-regexp-in-string ; entry headline
          (rx line-start
              (group-n 1 ; word
                       (any "A-Z")
                       (+ (any "a-z")))
              (optional " \\" ; word2
                        (group-n 2 (+ (not (any "\\"))))
                        "\\")
              (optional " (" ; pronounciation
                        (group-n 3 (+ (not (any ")"))))
                        ")")
              ", "
              (group-n 4 ; part of speech
                       (+ (any "A-Z" "a-z" ".;&" " ")))
              (optional "[" ; etymology / alternative forms
                        (group-n 5
                                 (+ (or (+ (not (any "][")))
                                        (and "[" (+ (not (any "]["))) "]"))))
                        "]")
              (optional ; definately etymology
               (+ (any "\n" " ")) "["
               (group-n 6
                        (+ (or (+ (not (any "][")))
                               (and "[" (+ (not (any "]["))) "]"))))
               "]")
              (optional " (" ; category
                        (group-n 7 (+ (not (any ")"))))
                        ")"))
          (lambda (match)
            (let* ((word2 (match-string 2 match))
                   (pronounciation (match-string 3 match))
                   (part-of-speech (sdcv-format-expand-abbreviations
                                    (replace-regexp-in-string " \\'" ""
                                                              (match-string 4 match))))
                   (alternative-forms (when (match-string 6 match)
                                        (sdcv-format-expand-abbreviations (match-string 5 match))))
                   (etymology (sdcv-format-expand-abbreviations (match-string (if alternative-forms 6 5) match)))
                   (category (sdcv-format-expand-abbreviations (match-string 7 match)))
                   (last-newline (lambda (text) (- (length text)
                                              (or (save-match-data
                                                    (string-match "\n[^\n]*\\'" text)) 0)))))
              (concat
               "\u200B\u200B\u200B"
               (propertize word2
                           'face 'bold)
               (when pronounciation
                 (propertize (format " %s" pronounciation)
                             'face 'font-lock-type-face))
               ", "
               (propertize part-of-speech
                           'face '(bold font-lock-keyword-face))
               (when alternative-forms
                 (setq alternative-forms
                       (sdcv-format-reflow-text
                        (format " [%s]" alternative-forms)
                        80 10
                        (+ 3 (if pronounciation 1 0)
                           (funcall last-newline
                                    (concat word2 pronounciation part-of-speech)))
                        "   "))
                 (propertize alternative-forms
                             'face 'diff-context))
               (when etymology
                 (setq etymology
                       (sdcv-format-reflow-text
                        (format " [%s]" etymology)
                        80 10
                        (+ 3 (if pronounciation 1 0)
                           (funcall last-newline
                                    (concat word2 pronounciation part-of-speech alternative-forms)))
                        "   "))
                 (propertize etymology
                             'face 'font-lock-comment-face))
               (when category
                 (propertize (format " (%s)" category)
                             'face 'font-lock-constant-face))
               "\u2008"))))
         (replace-regexp-in-string ; categorised terms
          "{\\([^}]+?\\)}\\(.?\\) (\\([^)]+?\\))"
          (lambda (match)
            (let ((term (match-string 1 match))
                  (punct (match-string 2 match))
                  (category (match-string 3 match)))
              (concat
               (propertize term 'face 'font-lock-keyword-face)
               punct
               (propertize (format " (%s)"
                                   (if sdcv-expand-abbreviations
                                       (sdcv-format-expand-abbreviations category)
                                     category))
                           'face 'font-lock-constant-face)))))
         (replace-regexp-in-string ; other terms
          "{\\([^}]+?\\)}"
          (lambda (match)
            (let ((term (match-string 1 match)))
              (concat
               (propertize term 'face 'font-lock-keyword-face)))))
         (replace-regexp-in-string ; quotations
          "^\n            +\\(\\w[[:ascii:]]+?\\)\\(\n? *--[A-Za-z0-9. ]+\n? *[A-Za-z0-9. ]*\\)"
          (lambda (match)
            (let ((body (match-string 1 match))
                  (author (match-string 2 match)))
              (concat
               "\n           "
               (propertize (format "❝%s❞" body)
                           'face 'font-lock-doc-face)
               author "\n"))))
         (replace-regexp-in-string ; attributions
          " --\\([A-Z][A-Za-z. ]+\n? *[A-Za-z0-9. ]*\\)"
          (lambda (match)
            (propertize (concat " ──" (match-string 1 match))
                        'face '(italic font-lock-type-face))))
         (replace-regexp-in-string ; inline quotations (1)
          "``" "“")
         (replace-regexp-in-string ; inline quotations (1)
          "''" "”")
         (replace-regexp-in-string ; em dash approximation
          " -- " " ─── ")
         (replace-regexp-in-string ; lists
          "   \\(?:\\([0-9]+\\.\\)\\|\\(   ([a-z])\\)\\) \\(?: ?(\\([^)]+\\)) \\)?\\(.*\\)"
          (lambda (match)
            (let ((number (match-string 1 match))
                  (letter (match-string 2 match))
                  (category (match-string 3 match))
                  (rest-of-line (match-string 4 match)))
              (concat
               (when letter "\u200B")
               "\u200B\u200B\u200B\u200B   "
               (when number
                 (propertize number 'face '(bold font-lock-string-face)))
               (when letter
                 (propertize letter 'face 'font-lock-string-face))
               (when category
                 (propertize (format " (%s)"
                                     (if sdcv-expand-abbreviations
                                         (sdcv-format-expand-abbreviations category)
                                       category))
                             'face 'font-lock-constant-face))
               " "
               rest-of-line
               "\u2008"))))
         (replace-regexp-in-string ; note
          "  Note: "
          (concat "     "
                  (propertize " " 'display '(space . (:width 0.55)))
                  (propertize "☞" 'face 'font-lock-function-name-face)
                  " "))
         (replace-regexp-in-string ; subheadings
          "  \\(\\w+\\): "
          (lambda (match)
            (propertize  (concat "  "(match-string 1 match) ": ")
                         'face 'bold)))))

(defvar sdcv-expand-abbreviations t
  "Whether or not to try to expand abbreviations, where they are expected.")

(defun sdcv-format-expand-abbreviations (content &optional force)
  (when content
    (when (or sdcv-expand-abbreviations force)
      (let ((abbreviations
             '(; A
               ("adj"                     "adjective")
               ("a"                       "adjective")
               ("abbrev"                  "abbreviated")
               ("abl"                     "ablative")
               ("Abp"                     "Archbishop")
               ("acc"                     "Acoustics")
               ("act"                     "active")
               ("adv"                     "adverb")
               ("Agric"                   "Agriculture")
               ("Alban"                   "Albanian")
               ("Alg"                     "Algebra")
               ("Am"                      "America")
               ("Amer"                    "American")
               ("Am"                      "Amos")
               ("Am\\. Cyc"               "Appleton's American Cyclopedia")
               ("Anal. Geom"              "Analytical Geometry")
               ("Anat"                    "Anatomy")
               ("Anc"                     "Ancient")
               ("Angl\\. Ch"              "Anglican Church")
               ("aor"                     "aorist")
               ("Ar"                      "Arabic")
               ("Arch"                    "Architecture")
               ("Arch\\. Pub\\. Soc"      "Architectural Pub. Society")
               ("Arith"                   "Arithmetic")
               ("Arm\\., Armor"           "Armorican")
               ("AS"                      "Anglo-Saxon")
               ("Astrol"                  "Astrology")
               ("Astron"                  "Astronomy")
               ("aug"                     "augmentative")
               ;; B
               ("Bank"                    "Banking")
               ("Beau\\. & Fl"            "Beaumont & Fletcher")
               ("B\\. & Fl"               "Beaumont & Fletcher")
               ("Bib\\. Sacra"            "Bibliotheca Sacra")
               ("Bib"                     "Biblical")
               ("Bibliog"                 "Bibliography")
               ("Biol"                    "Biology")
               ("Bisc"                    "Biscayan")
               ("B\\. Jon"                "Ben Jonson")
               ("Bk\\. of Com\\. Prayer " "Book of Common Prayer")
               ("Blackw\\. Mag"           "Blackwood's Magazine")
               ("Bohem"                   "Bohemian")
               ("Bot"                     "Botany")
               ("Bp"                      "Bishop")
               ("Brande & C"              "Brande & Cox")
               ("Braz"                    "Brazilian")
               ("Brit\\. Critic"          "British Critic")
               ("Brit\\. Quar\\. Rev"     "British Quarterly Review")
               ("Burl"                    "Burlesque")
               ;; C
               ("C"                       "Centigrade")
               ("Cant"                    "Canticles")
               ("Carp"                    "Carpentry")
               ("Catal"                   "Catalan")
               ("Cath\\. Dict"            "Catholic Dictionary")
               ("Celt"                    "Celtic")
               ("cf"                      "confer")
               ("Ch"                      "Church")
               ("Chald"                   "Chaldee")
               ("Chem"                    "Chemistry")
               ("Ch\\. Hist"              "Church History")
               ("Chron"                   "Chronology, Chronicles")
               ("Civ"                     "Civil")
               ("Class"                   "Classical")
               ("Class\\. Myth"           "Classical Mythology")
               ("Col"                     "Colossians")
               ("colloq\\., coll"         "colloquial, colloquially")
               ("Com"                     "Commerce, Common")
               ("comp"                    "compound, compounded, composition")
               ("compar"                  "comparative")
               ("conj"                    "conjunction")
               ("Con\\. Sect"             "Conic Sections")
               ("contr"                   "contracted, contraction")
               ("Copt"                    "Coptic")
               ("Corn"                    "Cornish")
               ("corrupt"                 "corrupted, corruption")
               ("Cotgr"                   "Cotgrave")
               ("Cyc\\. Med"              "Cyclopedia of Practical Medicine")
               ("Crim\\. Law"             "Criminal Law")
               ("Crystallog"              "Crystallography")
               ("Cyc"                     "Cyclopedia")
               ;; D
               ("D"                       "Dutch (sometimes Daniel)")
               ("Dan"                     "Danish")
               ("dat"                     "dative")
               ("def"                     "definitions")
               ("Deut"                    "Deuteronomy")
               ("Dial"                    "Dialectic")
               ("dim"                     "diminutive")
               ("Diosc"                   "dioscorides")
               ("Disp"                    "Dispensatory")
               ("Disus"                   "Disused")
               ("Dom\\. Econ"             "Domestic Economy")
               ("Dublin Univ\\. Mag"      "Dublin University Magazine")
               ("Dyn"                     "Dynamics")
               ;; E
               ("E"                       "English")
               ("Eccl"                    "Ecclesiastical, Ecclesiastes")
               ("Eccl\\. Hist"            "Ecclesiastical History")
               ("Ecclus"                  "Ecclesiasticus")
               ("Eclec\\. Rev"            "Eclectic Review")
               ("Ed\\. Rev"               "Edinburgh Review")
               ;; ("e\\. g"                  "exempli gratia (for example)")
               ("Egypt"                   "Egyptian")
               ("Elect"                   "Electricity")
               ("Elec"                    "Electrical")
               ("emph"                    "emphatic")
               ("Encyc\\. Amer"           "Encyclopedia Americana")
               ("Encyc\\. Crit"           "Encyclopedia Britannica")
               ("Encyc\\. Dict"           "Hunter's Encyclopedic Dictionary")
               ("Encyc"                   "Encyclopedia")
               ("Eng\\. Cyc"              "English Cyclopedia")
               ("Eng"                     "English")
               ("Engin"                   "Engineering")
               ("Eol"                     "Eolic")
               ("Eph\\., Ephes"           "Ephesians")
               ("equiv"                   "equivalent")
               ("Esd"                     "Esdras")
               ("esp"                     "especially")
               ("Etch\\. & Eng"           "Etching & Engraving")
               ("Ethnol"                  "Ethnology")
               ("etym\\., etymol"         "etymology")
               ("Ex\\., Exod"             "Exodus")
               ("Ezek"                    "Ezekiel")
               ;; F
               ("F"                       "French")
               ("f"                       "feminine")
               ("fem"                     "feminine")
               ("Fahr"                    "Fahrenheit")
               ("Far"                     "Farriery")
               ("Feud"                    "Feudal")
               ("Fig"                     "Figurative, figuratively")
               ("Fin"                     "Finnish")
               ("For\\. Quart\\. Rev"     "Foreign Quarterly Review")
               ("Fort"                    "Fortification")
               ("Fr"                      "French")
               ("fr"                      "from")
               ("freq"                    "frequentative")
               ("Fries"                   "Friesic")
               ("fut"                     "future")
               ;; G
               ("G"                       "German")
               ("Gael"                    "Gaelic")
               ("Gal"                     "Galen")
               ("Gal"                     "Galatians")
               ("Galv"                    "Galvanism")
               ("gen"                     "generally, genitive")
               ("Geneal"                  "Genealogy")
               ("Gent\\. Mag"             "Gentleman's Magazine")
               ("Geog"                    "Geography")
               ("Geol"                    "Geology")
               ("Geom"                    "Geometry")
               ("Ger"                     "Germanic or German")
               ("Gk"                      "Greek")
               ("Goth"                    "Gothic")
               ("Gov\\. of Tongue"        "Government of the Tongue")
               ("Gr"                      "Greek")
               ("Gram"                    "Grammar")
               ("Gris"                    "Grisons")
               ("Gun"                     "Gunnery")
               ;; H
               ("H"                       "High")
               ("Hab"                     "Habakkuk")
               ("Hag"                     "Haggai")
               ("Ham\\. Nav\\. Encyc"     "Hamersly's Naval Encyclopedia")
               ("Heb"                     "Hebrew")
               ("Her"                     "Heraldry")
               ("Hind"                    "Hindostanee")
               ("Hipp"                    "Hippocrates")
               ("Hist"                    "History")
               ("Horol"                   "Horology")
               ("Hort"                    "Horticulture")
               ("Hung"                    "Hungarian")
               ("Hydraul"                 "Hydraulics")
               ("Hydros"                  "Hydrostatics")
               ("hypoth"                  "hypothetical")
               ;; I
               ("Icel"                    "Icelandic")
               ;; ("i\\. e"                  "id est (that is)")
               ("Illust"                  "Illustration, Illustrated")
               ("imp"                     "imperfect")
               ("Imp\\. Dict"             "Imperial Dictionary")
               ("incho"                   "inchoative")
               ("ind"                     "indicative")
               ("indef"                   "indefinite")
               ("inf"                     "infinitive")
               ("intens"                  "intensive")
               ("interj"                  "interjection")
               ("Internat\\. Cyc"         "International Cyclopeia")
               ("Ion"                     "Ionic")
               ("i\\. q"                  "idem quod")
               ("Ir"                      "Irish")
               ("Is"                      "Isaiah")
               ("Isa"                     "Isaiah")
               ("It"                      "Italian")
               ;; J
               ("Jap"                     "Japanese")
               ("Jas"                     "James")
               ("Jav"                     "Javanese")
               ("Jer"                     "Jeremiah")
               ("Join"                    "Joinery")
               ("Josh"                    "Joshua")
               ("Judg"                    "Judges")
               ;; K
               ("K"                       "Kings")
               ;; L
               ("L"                       "Latin")
               ("Lam"                     "Lamentations")
               ("Lapp"                    "Lappish")
               ("Lat"                     "Latin")
               ("LD"                      "Low Dutch")
               ("Lett"                    "Lettish")
               ("Lev"                     "Leviticus")
               ("LG"                      "Low German")
               ("LGr"                     "Low Greek")
               ("Linn"                    "Linnæus")
               ("Lit"                     "Literally")
               ("lit"                     "literally")
               ("Lit"                     "Literature")
               ("Lith"                    "Lithuanian")
               ("LL"                      "Late Latin")
               ;; M
               ("M"                       "Middle")
               ("m"                       "masculine")
               ("masc"                    "masculine")
               ("Maced"                   "Macedonian")
               ("Mach"                    "Machinery")
               ("Mad"                     "Madam")
               ("Mag"                     "Magazine")
               ("Mal"                     "Malachi")
               ("Malay"                   "Malayan")
               ("Man"                     "Manège")
               ("Manuf"                   "Manufacturing")
               ("Mar"                     "Maritime")
               ("Math"                    "Mathematics")
               ("Matt"                    "Matthew")
               ("ME"                      "Middle English")
               ("Mech"                    "Mechanic")
               ("Med"                     "Medicine")
               ("Metal"                   "Metallurgy")
               ("Metaph"                  "Metaphysics")
               ("Meteor"                  "Meteorolgy")
               ("mgr"                     "milligrams")
               ("MHG"                     "Middle High German")
               ("Micros"                  "Microscopy")
               ("Mil"                     "Military")
               ("Min"                     "Mineralogy")
               ("Mir\\. for Mag"          "Mirror for Magistrates")
               ("MLG"                     "Middle Low German")
               ("Moham"                   "Mohammedan")
               ("Mozley & W"              "Mozley & Whiteley")
               ("Mus"                     "Music")
               ("Myst"                    "Mysteries")
               ("Myth"                    "Mythology")
               ;; N
               ("Nat\\. Hist"             "Natural History")
               ("Nat\\. ord"              "Natural order")
               ("Naut"                    "Nautical")
               ("Nav"                     "Navy")
               ("Navig"                   "Navigation")
               ("N\\. Brit\\. Rev"        "North British Review")
               ("Neh"                     "Nehemiah")
               ("neut"                    "neuter")
               ("New Am\\. Cyc"           "New American Cyclopedia")
               ("New Month\\. Mag"        "New Monthly Magazine")
               ("NF"                      "New French")
               ("NGr"                     "Mew Greek")
               ("NHeb"                    "New Hebrew")
               ("NL"                      "New Latin")
               ("nom"                     "nominative")
               ("Norm\\. F"               "Norman French")
               ("North Am\\. Rev"         "North American Review")
               ("Norw"                    "Norwegian")
               ("Num"                     "Numbers")
               ("Numis"                   "Numismatics")
               ("N"                       "New")
               ;; O
               ("O"                       "Old")
               ("Ob"                      "Obadiah")
               ("obs"                     "obsolete")
               ("Obsoles"                 "Obsolescent")
               ("OCelt"                   "Old Celtic")
               ("OD"                      "Old Dutch")
               ("ODan"                    "Old Danish")
               ("OE"                      "Old English")
               ("OF"                      "Old French")
               ("OFelm"                   "Old Flemish")
               ("OFris"                   "Old Frisian")
               ("OFries"                  "Old Friesic")
               ("OGael"                   "Old Gaelic")
               ("OGr"                     "Old Greek")
               ("OHG"                     "Old High German")
               ("OIcel"                   "Old Icelandic")
               ("OIt"                     "Old Italian")
               ("OL"                      "Old Latin")
               ("OIr"                     "Old Irish")
               ("ON"                      "Old Norse")
               ("OLG"                     "Old Low German")
               ("OPer"                    "Old Persian")
               ("OPg"                     "Old Portuguese")
               ("OPol"                    "Old Polish")
               ("Opt"                     "Optics")
               ("orig"                    "original")
               ("Ornith"                  "Ornithology")
               ("OS"                      "Old Saxon")
               ("OSlav"                   "Old Slavic")
               ("OSp"                     "Old Spanish")
               ("Oxf\\. Gloss"            "Oxford Glossary of Architecture")
               ;; P
               ("p\\.[\n ]*a"             "participial adjective")
               ("Paint"                   "Painting")
               ("Paleon"                  "Paleontology")
               ("pass"                    "passive")
               ("Pathol"                  "Pathology")
               ("P\\. Cyc"                "Penny Cyclopedia")
               ("Per"                     "Persian")
               ("perh"                    "perhaps")
               ("pers"                    "person")
               ("Persp"                   "Perspective")
               ("Pert"                    "Pertaining")
               ("Peruv"                   "Peruvian")
               ("Pet"                     "Peter")
               ("Pg"                      "Portuguese")
               ("Pharm"                   "Pharmacy, Pharmacopœia")
               ("Phil"                    "Phillipians")
               ("Philem"                  "Philemon")
               ("Philol"                  "Philology")
               ("Philos"                  "Philosophy")
               ("Phon"                    "Phonetics")
               ("Photog"                  "Photography")
               ("Photom"                  "Photometry")
               ("Phren"                   "Phrenology")
               ("Phys"                    "Physics")
               ("Phys\\. Sci"             "Physical Science")
               ("Physiol"                 "Physiology")
               ("pl"                      "plural")
               ("Poet"                    "Poetry, Poetical")
               ("Pol"                     "Polish")
               ("Pol\\. Econ"             "Political Economy")
               ("Polit\\. Econ"           "Political Economy")
               ("Pop\\. Sci\\. Monthly"   "Polular Science Monthly")
               ("Poss"                    "Possessive")
               ("pp"                      "pages")
               ("P\\. Plowman"            "Piers Plowman")
               ("p\\.[\n ]*p"             "past participle")
               ("p\\.[\n ]*pr"            "present participle")
               ("p\\.[\n ]*ple"           "present participle")
               ("Pr"                      "Provençal")
               ("Pref"                    "Preface")
               ("pref"                    "prefix")
               ("prep"                    "preposition")
               ("pres"                    "present")
               ("pret"                    "preterit")
               ("prin"                    "principally")
               ("Print"                   "Printing")
               ("priv"                    "privative")
               ("prob"                    "probably")
               ("pron"                    "pronoun")
               ("prop"                    "properly")
               ("Pros"                    "Prosody")
               ("prov"                    "provincial")
               ("Prov"                    "Provincial")
               ("Prov"                    "Proverbs")
               ("Ps\\., Psa"              "Psalms")
               ("Pyro\\.-elect"           "Pyro-electricity")
               ("p"                       "participle")
               ;; Q
               ("Quart\\. Rev"            "Quarterly Review")
               ("q\\. v"                  "quod vide (which see)")
               ;; R
               ("R\\. C"                  "Roman Catholic")
               ("R\\. C\\. Ch"            "Roman Catholic Church")
               ("Rep\\. Sec\\. of War"    "Report of Secretary of War")
               ("Rev"                     "Revelation")
               ("Rev"                     "Review")
               ("Rev\\. Ver"              "Revised Version (of the Bible)")
               ("Rhet"                    "Rhetoric")
               ("R\\. of Brunne"          "Robert of Brunne")
               ("R\\. of Gl"              "Robert of Gloucester")
               ("Rom"                     "Roman, Romans")
               ("Rom\\. Cath"             "Roman Catholic")
               ("Rom\\. of R"             "Romaunt of the Rose")
               ("Rpts"                    "reports")
               ("Russ"                    "Russian")
               ("R"                       "Rare")
               ;; S
               ("Sam"                     "Samaritan")
               ("Sam"                     "Samuel")
               ("Sat\\. Rev"              "Saturday Review")
               ("Sax"                     "Saxon")
               ("sc"                      "scilicet (being understood)")
               ("Scand"                   "Scandinavian")
               ("Sci"                     "Science")
               ("Sci\\. Am"               "Scientific American")
               ("Scot"                    "Scotland, Scottish")
               ("Script"                  "Scripture, Scriptural")
               ("Sculp"                   "Sculpture")
               ("Serb"                    "Serbian")
               ("Serv"                    "Servian")
               ("Shak"                    "Shakespeare")
               ("sing"                    "singular")
               ("Skr"                     "Sanskrit")
               ("Slav"                    "Slavonic")
               ("Sp"                      "Spanish")
               ("Specif"                  "Specifically")
               ("Stat"                    "Statuary")
               ("subj"                    "subjunctive")
               ("superl"                  "superlative")
               ("Surg"                    "Surgery")
               ("Surv"                    "Surveying")
               ("Sw"                      "Swedish")
               ("Syd\\. Soc\\. Lex"       "Sydenham Society Lexicon")
               ("Syn"                     "Synonyms")
               ("Synop"                   "Synopsis")
               ("Syr"                     "Syriac")
               ;; T
               ("Tart"                    "Tartaric")
               ("Teleg"                   "Telegraphy")
               ("term"                    "termination")
               ("Test"                    "Testament")
               ("Theol"                   "Theology")
               ("Thes"                    "Thessalonians")
               ("Tim"                     "Timothy")
               ("Todd & B"                "Todd & Bowman")
               ("Trans"                   "Translation")
               ("Treas"                   "Treasury")
               ("Trig"                    "Trigonometry")
               ("Turk"                    "Turkish")
               ("Typog"                   "Typography")
               ;; U
               ("Univ"                    "University")
               ("Up"                      "Upper")
               ("U\\. ?S"                 "United States")
               ("U\\. ?S\\. Disp"         "United States Dispensatory")
               ("U\\. ?S\\. Pharm"        "United States Pharmacopœia")
               ("U\\. ?S\\. Int\\. Rev\\. Statutes" "United States Internal Revenue Statutes")
               ("usu"                     "usually")
               ;; V
               ("v\\.[\n ]*i"             "intransitive verb")
               ("v\\.[\n ]*t"             "transitive verb")
               ("var"                     "variety")
               ("vb\\.[\n ]*n"            "verbal noun")
               ("Veter"                   "Veterinary")
               ("Vitr"                    "Vitruvius")
               ;; W
               ("W"                       "Welsh")
               ("Wall"                    "Wallachian")
               ("Westm\\. Cat"            "Westminster Catechism")
               ("Westm\\. Rev"            "Westminster Review")
               ;; Z
               ("Zech"                    "Zechariah")
               ("Zeph"                    "Zephaniah")
               ("Zoöl"                    "Zoölogy")
               ;; Reordered for correctness
               ("n"                       "noun")
               ("v"                       "verb")
               )))
        (dolist (abbrev abbreviations)
          (setq content
                (replace-regexp-in-string
                 (concat "\\b" (car abbrev) "\\.")
                 (cadr abbrev)
                 content t)))))
    content))

(defun sdcv-format-webster-diacritics (pronunciation)
  (let ((diacritics
         '(("[,C]" "Ç")
           ("\"u" "ü") ; uum
           ("'e" "é") ; eacute
           ("\\^a" "â") ; acir
           ("\"a" "ä") ; aum
           ("`a" "à") ; agrave
           ("\\*a" "å") ; aring
           ("\\*u" "ů") ; uring
           (",c" "ç") ; ccedil
           ("cced" "ç")
           ("\\^e" "ê") ; ecir
           ("\"e" "ë") ; eum
           ("`e" "è") ; egrave
           ("\"i" "ï") ; ium
           ("\\^i" "î") ; icir
           ("`i" "ì") ; igrave
           ("\"A" "Ä") ; Aum
           ("\\*A" "Å") ; Aring
           ("'E" "È") ; Eacute
           ("ae" "æ") ; ae
           ("AE" "Æ") ; AE
           ("\\^o" "ô") ; ocir
           ("\"o" "ö") ; oum
           ("`o" "ò") ; ograve
           ("'o" "ó") ; oacute
           ("Oacute" "Ó")
           ("\\^u" "û") ; ucir
           ("`u" "ù") ; ugrave
           ("'u" "ú") ; uacute
           ("\"y" "ÿ") ; yum
           ("\"O" "Ö") ; Oum
           ("\"U" "Ü")
           ("pound" "£")
           ("'a" "á") ; aacute
           ("'i" "í") ; iacute
           ("frac23" "⅔")
           ("frac13" "⅓")
           ("frac12" "½")
           ("frac14" "¼")
           ("\\?" "�")  ; Place-holder for unknown or illegible character.
           ("hand" "☞")  ; pointing hand (printer's u"fist") ; hand ; and
           ("\\.\\.\\." "…")
           ("\\*\\*\\*\\*\\*\\*\\*\\*" "✶")
           ("sect" "§") ; sect
           ("=a" "ā") ; amac
           ("ng" "ṉ")  ; u"n sub-macron" ; nsm
           ("sharp" "♯") ; sharp
           ("flat" "♭") ; flat
           ("th" "th") ; th
           ("=i" "ī") ; imac
           ("imac" "ī") ; imac
           ("=e" "ē") ; emac
           ("\\.d" "ḍ")  ; Sanskrit/Tamil d dot
           ("\\.n" "ṇ")  ; Sanskrit/Tamil n dot ; nsdot
           ("\\.t" "ṭ")  ; Sanskrit/Tamil t dot ; tsdot
           ("a\\^" "ă") ; acr
           ("e\\^" "ĕ") ; ecr
           ("i\\^" "ĭ") ; icr
           ("o\\^" "ŏ") ; ocr
           ("!o" "ǒ")
           ("OE" "Œ") ; OE
           ("oe" "œ") ; oe
           ("=O" "Ō") ; Omac
           ("=o" "ō") ; omac
           ("=u" "ū") ; umac
           ("ocar" "ǒ") ; o hacek
           ("=ae" "ǣ") ; aemac
           ("u\\^" "ŭ") ; ucr
           ("a\\^" "ă")
           ("=y" "ȳ") ; ymac
           ("asl" "a")  ; FIXME: a u"semilong" (has a macron above with a short
           ("-e" "e")  ; FIXME: e u"semilong" ; esl
           ("-i" "i")  ; FIXME: i u"semilong" ; isl
           ("-o" "o")  ; FIXME: o u"semilong" ; osl
           ("-u" "u")  ; FIXME: u u"semilong" ; usl
           ("-n" "ṉ") ; nsmac
           ("\\.a" "ȧ")  ; a with dot above ; adot
           ("\\.c" "ċ")  ; cdot
           ("\\.h" "ḥ")  ; hsdot
           ("\\.m" "ṃ")  ; msdot
           ("\\.u" "ụ")  ; usdot
           ("\\.z" "ẓ")  ; zsdot
           ("Eth" "Ð") ; EDH
           ("eth" "ð") ; edh
           ("thorn" "þ") ; thorn
           ("~a" "ã") ; atil
           ("~e" "ẽ") ; etil
           ("itil" "ĩ")
           ("otil" "õ")
           ("util" "ũ")
           ("~n" "ñ") ; ntil
           ("Atil" "Ã")
           ("Etil" "Ẽ")
           ("Itil" "Ĩ")
           ("Otil" "Õ")
           ("Util" "Ũ")
           ("~N" "Ñ") ; Ntil
           ("\\.n" "ṅ") ; ndot
           ("\\.r" "ṛ") ; rsdot
           ("yogh" "ȝ") ; yogh
           ("deg" "°")
           ("middot" "•")
           ("root" "√")
           ;; Greek letters
           ("alpha" "α")
           ("beta" "β")
           ("gamma" "γ")
           ("delta" "δ")
           ("epsilon" "ε")
           ("zeta" "ζ")
           ("eta" "η")
           ("theta" "θ")
           ("iota" "ι")
           ("approx" "κ") ; ap
           ("lambda" "λ")
           ("mu" "μ")
           ("nu" "ν")
           ("xi" "ξ")
           ("omicron" "ο")
           ("pi" "π")
           ("rho" "ρ")
           ("sigma" "σ")
           ("sigmat" "ς")
           ("tau" "τ")
           ("upsilon" "υ")
           ("phi" "φ")
           ("chi" "χ")
           ("psi" "ψ")
           ("omega" "ω")
           ("digamma" "ϝ")
           ("ALPHA" "Α")
           ("BETA" "Β")
           ("Gamma" "Γ") ; GAMMA
           ("Delta" "Δ") ; DELTA
           ("EPSILON" "Ε")
           ("ZETA" "Ζ")
           ("ETA" "Η")
           ("Theta" "Θ") ; THETA
           ("IOTA" "Ι")
           ("KAPPA" "Κ")
           ("Lambda" "Λ") ; LAMBDA
           ("MU" "Μ")
           ("NU" "Ν")
           ("XI" "Ξ")
           ("Omicron" "Ο") ; OMICRON
           ("Pi" "Π") ; PI
           ("RHO" "Ρ")
           ("Sigma" "Σ") ; SIGMA
           ("Tau" "Τ") ; TAU
           ("Upsilon" "Υ") ; UPSILON
           ("PHI" "Φ")
           ("Chi" "Χ") ; CHI
           ("PSI" "Ψ")
           ("Omega" "Ω") ; OMEGA
           ;; FIXME: Vowels with a double dot below. There`s nothing suitable in the Unicode
           ("add" "a")
           ("udd" "u")
           ("ADD" "A")
           ("UDD" "U")
           ;; Quotes
           ("dagger" "†")
           ("dag" "†")
           ("u\\^" "§") ; par
           ("and" "and")
           ("or" "or")
           ("sec" "˝")
           ("[,C]" "Ç")
           ("\"u" "ü") ; uum
           ("'e" "é") ; eacute
           ("\\^a" "â") ; acir
           ("\"a" "ä") ; aum
           ("`a" "à") ; agrave
           ("\\*a" "å") ; aring
           ("\\*u" "ů") ; uring
           (",c" "ç") ; ccedil
           ("cced" "ç")
           ("\\^e" "ê") ; ecir
           ("\"e" "ë") ; eum
           ("`e" "è") ; egrave
           ("\"i" "ï") ; ium
           ("\\^i" "î") ; icir
           ("`i" "ì") ; igrave
           ("\"A" "Ä") ; Aum
           ("\\*A" "Å") ; Aring
           ("'E" "È") ; Eacute
           ("ae" "æ") ; ae
           ("AE" "Æ") ; AE
           ("\\^o" "ô") ; ocir
           ("\"o" "ö") ; oum
           ("`o" "ò") ; ograve
           ("'o" "ó") ; oacute
           ("Oacute" "Ó")
           ("\\^u" "û") ; ucir
           ("`u" "ù") ; ugrave
           ("'u" "ú") ; uacute
           ("\"y" "ÿ") ; yum
           ("\"O" "Ö") ; Oum
           ("\"U" "Ü")
           ("pound" "£")
           ("'a" "á") ; aacute
           ("'i" "í") ; iacute
           ("frac23" "⅔")
           ("frac13" "⅓")
           ("frac12" "½")
           ("frac14" "¼")
           ;; ("\\?" "�")  ; Place-holder for unknown or illegible character.
           ("hand" "☞")  ; pointing hand (printer's u"fist") ; hand ; and
           ("sect" "§") ; sect
           ("=a" "ā") ; amac
           ("ng" "ṉ")  ; u"n sub-macron" ; nsm
           ("sharp" "♯") ; sharp
           ("flat" "♭") ; flat
           ("th" "th") ; th
           ("=i" "ī") ; imac
           ("=e" "ē") ; emac
           ("\\.d" "ḍ")  ; Sanskrit/Tamil d dot
           ("\\.n" "ṇ")  ; Sanskrit/Tamil n dot ; nsdot
           ("\\.t" "ṭ")  ; Sanskrit/Tamil t dot ; tsdot
           ("a\\^" "ă") ; acr
           ("e\\^" "ĕ") ; ecr
           ("i\\^" "ĭ") ; icr
           ("o\\^" "ŏ") ; ocr
           ("!o" "ǒ")
           ("OE" "Œ") ; OE
           ("oe" "œ") ; oe
           ("=O" "Ō") ; Omac
           ("=o" "ō") ; omac
           ("=u" "ū") ; umac
           ("ocar" "ǒ") ; o hacek
           ("=ae" "ǣ") ; aemac
           ("u\\^" "ŭ") ; ucr
           ("a\\^" "ă")
           ("=y" "ȳ") ; ymac
           ("asl" "a")  ; FIXME: a u"semilong" (has a macron above with a short
           ("-e" "e")  ; FIXME: e u"semilong" ; esl
           ("-i" "i")  ; FIXME: i u"semilong" ; isl
           ("-o" "o")  ; FIXME: o u"semilong" ; osl
           ("-u" "u")  ; FIXME: u u"semilong" ; usl
           ("-n" "ṉ") ; nsmac
           ("\\.a" "ȧ")  ; a with dot above ; adot
           ("\\.c" "ċ")  ; cdot
           ("\\.h" "ḥ")  ; hsdot
           ("\\.m" "ṃ")  ; msdot
           ("\\.u" "ụ")  ; usdot
           ("\\.z" "ẓ")  ; zsdot
           ("Eth" "Ð") ; EDH
           ("eth" "ð") ; edh
           ("thorn" "þ") ; thorn
           ("~a" "ã") ; atil
           ("~e" "ẽ") ; etil
           ("itil" "ĩ")
           ("otil" "õ")
           ("util" "ũ")
           ("~n" "ñ") ; ntil
           ("Atil" "Ã")
           ("Etil" "Ẽ")
           ("Itil" "Ĩ")
           ("Otil" "Õ")
           ("Util" "Ũ")
           ("~N" "Ñ") ; Ntil
           ("\\.n" "ṅ") ; ndot
           ("\\.r" "ṛ") ; rsdot
           ("yogh" "ȝ") ; yogh
           ("deg" "°")
           ("middot" "•")
           ("root" "√")
           ;; Greek letters
           ("alpha" "α")
           ("beta" "β")
           ("gamma" "γ")
           ("delta" "δ")
           ("epsilon" "ε")
           ("zeta" "ζ")
           ("eta" "η")
           ("theta" "θ")
           ("iota" "ι")
           ("approx" "κ") ; ap
           ("lambda" "λ")
           ("mu" "μ")
           ("nu" "ν")
           ("xi" "ξ")
           ("omicron" "ο")
           ("pi" "π")
           ("rho" "ρ")
           ("sigma" "σ")
           ("sigmat" "ς")
           ("tau" "τ")
           ("upsilon" "υ")
           ("phi" "φ")
           ("chi" "χ")
           ("psi" "ψ")
           ("omega" "ω")
           ("digamma" "ϝ")
           ("ALPHA" "Α")
           ("BETA" "Β")
           ("Gamma" "Γ") ; GAMMA
           ("Delta" "Δ") ; DELTA
           ("EPSILON" "Ε")
           ("ZETA" "Ζ")
           ("ETA" "Η")
           ("Theta" "Θ") ; THETA
           ("IOTA" "Ι")
           ("KAPPA" "Κ")
           ("Lambda" "Λ") ; LAMBDA
           ("MU" "Μ")
           ("NU" "Ν")
           ("XI" "Ξ")
           ("Omicron" "Ο") ; OMICRON
           ("Pi" "Π") ; PI
           ("RHO" "Ρ")
           ("Sigma" "Σ") ; SIGMA
           ("Tau" "Τ") ; TAU
           ("Upsilon" "Υ") ; UPSILON
           ("PHI" "Φ")
           ("Chi" "Χ") ; CHI
           ("PSI" "Ψ")
           ("Omega" "Ω") ; OMEGA
           ;; FIXME: Vowels with a double dot below. There`s nothing suitable in the Unicode
           ("add" "a")
           ("udd" "u")
           ("ADD" "A")
           ("UDD" "U")
           ;; Quotes
           ("dagger" "†")
           ("dag" "†")
           ("u\\^" "§") ; par
           ("and" "and")
           ("or" "or")
           ("times" "×")
           ("sec" "˝"))))
    (setq pronunciation
          (replace-regexp-in-string
           "\\.\\.\\." "…"
           (replace-regexp-in-string
            "\\*\\*\\*\\*\\*\\*\\*\\*" "✶"
            pronunciation
            )))
    (dolist (dcrt diacritics)
      (setq pronunciation (replace-regexp-in-string
             (concat "\\[" (car dcrt) "\\]")
             (cadr dcrt)
             pronunciation t)))
    pronunciation))

(defun sdcv-format-reflow-text (text max-width &optional min-width initial-colunm indent sepregex)
  (let* ((initial-col (or initial-colunm 0))
         (min-width (or min-width 1))
         (indent (or indent ""))
         (sepregex (or sepregex "[ ,.]"))
         (line-regex (format "\\(\\`.\\{%d,%d\\}\\(?:%s\\(?:.\\{1,%d\\}\\'\\)?\\|\\'\\)\\|.\\{%d\\}\\)\\(.*\\)"
                             min-width (- max-width (length indent))
                             sepregex min-width (- max-width (length indent))))
         reflowed-text)
    (setq text (replace-regexp-in-string "[[:space:]]+" " " text))
    (setq text
          (if (> initial-col max-width)
              (replace-regexp-in-string "\\` " "" text)
            (replace-regexp-in-string
             (format "\\`.\\{%d,%d\\}%s\\(?:.\\{1,%d\\}\\'\\)?\\|\\`"
                     (min min-width (- max-width initial-col)) (- max-width initial-col)
                     sepregex min-width)
             (lambda (match)
               (setq reflowed-text match)
               "")
             text)))
    (while (not (string-empty-p text))
      (setq text
            (if (<= (length text) max-width)
                (progn (setq reflowed-text (concat reflowed-text
                                                   (unless (string-empty-p reflowed-text)
                                                     (concat "\n" indent))
                                                   text)) "")
              (replace-regexp-in-string
               line-regex
               (lambda (match)
                 (setq reflowed-text (concat reflowed-text "\n" indent (match-string 1 match)))
                 (match-string 2 match))
               text))))
    reflowed-text))

(defun sdcv-format-online-etym (entry &optional expected-word)
  "Make an html ENTRY for WORD look nice.
Designed for an export of Douglas Harper's Online Etymology Dictionary,
sourced from http://tovotu.de/data/stardict/etymonline.zip."
  (->> (string-join
        (mapcar (lambda (e) (plist-get e :info))
                (-filter (lambda (e) (string= (plist-get e :dict)
                                         (plist-get entry :dict)))
                         (sdcv-parse-results
                          (sdcv-oneshot-lookup
                           (replace-regexp-in-string " ?(.*)" " (*)" (plist-get entry :word)) ; sdcv accepts a glob
                           t (format "-0 -u '%s'" (replace-regexp-in-string "'" "\\'" (plist-get entry :dict))))))))
       (replace-regexp-in-string
        "\\(?:\\`\\|\n\n\\)<b>\\(.+?\\) (\\(.+?\\)\\([0-9]+\\)?)</b> ?"
        (lambda (match)
          (let ((word (match-string 1 match))
                (pos (sdcv-format-expand-abbreviations (match-string 2 match)))
                (index (match-string 3 match)))
            (concat "\n\n\u200B\u200B\u200B"
                    (propertize word 'face 'bold)
                    " "
                    (propertize pos 'face '(bold font-lock-keyword-face))
                    (when index
                      (propertize (concat " " index) 'face '(italic font-lock-doc-face)))
                    "\u2008\n\n"))))
       (replace-regexp-in-string
        "<i>\\(.*?\\)</i>"
        (lambda (match) (propertize (match-string 1 match) 'face 'italic)))
       (replace-regexp-in-string
        "<b>\\(.*?\\)</b>"
        (lambda (match) (propertize (match-string 1 match) 'face 'bold)))
       (replace-regexp-in-string
        "<strong>\\(.*?\\)</strong>"
        (lambda (match) (propertize (match-string 1 match) 'face 'font-lock-constant-face)))
       (replace-regexp-in-string
        "<a href=\".*?\">\\(.*?\\)</a>"
        (lambda (match) (propertize (match-string 1 match) 'face 'font-lock-keyword-face)))
       (replace-regexp-in-string
        "<span style=\".*?\">\\(.*?\\)</span>"
        (lambda (match) (propertize (match-string 1 match) 'face 'font-lock-doc-face)))
       (replace-regexp-in-string
        "[0-9]\\{4\\}s?\\|[0-9]\\{2\\}c"
        (lambda (match) (propertize match 'face 'font-lock-type-face)))
       (replace-regexp-in-string
        "<span>\\(.*?\\)</span>\\( (.+?)\\)?"
        (lambda (match)
          (let ((linked (match-string 1 match))
                (pos (sdcv-format-expand-abbreviations (match-string 2 match))))
            (concat
             (propertize linked 'face 'font-lock-keyword-face)
             (when pos (propertize (replace-regexp-in-string "\\([0-9]+\\))" " \\1)" pos)
                                   'face '(bold diff-context)))))))
       (replace-regexp-in-string "<br/><br/>" "")
       (replace-regexp-in-string
        "<p>\\(.*?\\)</p>"
        (lambda (match)
          (concat
           (sdcv-format-reflow-text (match-string 1 match)
                                    80 5)
           "\n")))
       (replace-regexp-in-string
        "^.\\{86,\\}"
        (lambda (match)
           (sdcv-format-reflow-text match 80 5)))
       ))

(defun sdcv-format-element (entry element &optional expected-word)
  "Make an ENTRY for ELEMENT look nice.
Based on http://download.huzheng.org/dict.org/stardict-dictd_www.dict.org_elements-2.4.2.tar.bz2."
  (replace-regexp-in-string
        "^\\([a-z]+\\)
Symbol: \\([A-Za-z]+\\)
Atomic number: \\([0-9]+\\)
Atomic weight: \\((?[0-9.]+)?\\)"
        (lambda (match)
          (let ((element (match-string 1 match))
                (symbol (match-string 2 match))
                (number (match-string 3 match))
                (weight (match-string 4 match)))
            (format
             "┌────────────────┐
│ %-3s %10s │
│ %s %11s │
└────────────────┘
"
             (propertize number 'face 'font-lock-function-name-face)
             (propertize weight 'face 'font-lock-comment-face)
             (propertize symbol 'face '(bold font-lock-keyword-face))
             (propertize element 'face 'font-lock-string-face))))
        (plist-get entry :info)))

(defun sdcv-format-soule (entry &optional expected-word)
  "Format an ENTRY for WORD in Soule's Dictionary of English Synonyms.
Designed using http://download.huzheng.org/bigdict/stardict-Soule_s_Dictionary_of_English_Synonyms-2.4.2.tar.bz2."
  (->> (plist-get entry :info)
       (replace-regexp-in-string ; categories
        "^\\([IVX]+\\. \\)?\\([a-z.;& ]+\\)"
        (lambda (match)
          (concat
           "\u200B\u200B\u200B"
           (when case-fold-search
             (propertize
              'face '(bold font-lock-constant-face)))
           (propertize (sdcv-format-expand-abbreviations (match-string 2 match))
                       'face '(bold font-lock-keyword-face))
           "\u2008")))
       (replace-regexp-in-string
        "^\\([0-9]+\\.\\) \n\\([^,.]*,?\\)"
        (lambda (match)
          (concat
           "\u200B\u200B\u200B\u200B"
           (propertize (match-string 1 match)
                       'face '(bold font-lock-string-face))
           " "
           (match-string 2 match)
           "\u2008")))
       (replace-regexp-in-string
        "^\\(.\\{81\\}\\)"
        (lambda (match)
          (sdcv-format-reflow-text match 80 1 0 "   " "[, ]")))
       (replace-regexp-in-string
        ","
        (propertize "," 'face 'font-lock-type-face))))

;;;;##################################################################
;;;;  User Options, Variables
;;;;##################################################################

(defvar sdcv-buffer-name "*sdcv*"
  "The name of the buffer of sdcv.")
(defvar sdcv-dictionary-list t
  "A list of dictionaries to use.
Each entry is a string denoting the name of a dictionary, which
is then passed to sdcv through the '-u' command line option.
Any non-list value means using all the dictionaries.")
(defvar sdcv-dictionary-alist nil
  "An alist of dictionaries, used to interactively form
dictionary list. It has the form:
   ((\"full\" . t)
    (\"group1\" \"dict1\" \"dict2\" ...)
    (\"group2\" \"dict2\" \"dict3\"))
Any cons cell here means using all dictionaries.
")

(defvar sdcv-program-path "/usr/bin/sdcv"
  "The path of sdcv program.")

(defvar sdcv-dictionary-path nil
  "The path of dictionaries.")

(defvar sdcv-word-processor nil
  "This is the function that take a word (stirng)
and return a word or a list of words for lookup by `sdcv-search'.
All lookup result(s) will finally be concatenated together.

`nil' value means do nothing with the original word.

The following is an example.  This function takes the original word and
compare whether simplified and traditional form of the word are the same.
If not, look up both of the words.

      (lambda (word)
        (let ((sim (chinese-conv word \"simplified\"))
              (tra (chinese-conv word \"traditional\")))
          (if (not (string= sim tra))
              (list sim tra)
            word)))
")

;;; sdcv.el ends here
