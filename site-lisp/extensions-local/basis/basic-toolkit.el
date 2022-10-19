;; -*- coding: utf-8; -*-
;;; Require:

;;; Code:

(defvar cursor-position-stack nil
  "Cursor position stack.")

(defun unmark-all-buffers ()
  "Unmark all have marked buffers."
  (interactive)
  (let ((current-element (current-buffer)))
    (save-excursion
      (dolist (element (buffer-list))
        (set-buffer element)
        (deactivate-mark)))
    (switch-to-buffer current-element)
    (deactivate-mark)))

(defun insert-line-number (beg end &optional start-line)
  "Insert line numbers into buffer."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d " line))
        (beginning-of-line 2)
        (cl-incf line)
        (cl-incf counter)))))

(defun insert-line-number+ ()
  "Insert line number into buffer."
  (interactive)
  (if mark-active
      (insert-line-number (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-number (point-min) (point-max))))

(defun strip-blank-lines()
  "Strip all blank lines in select area of buffer,
if not select any area, then strip all blank lines of buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n")
  (message "Blanks line striped. ^_^"))

(defun strip-line-number()
  "Strip all line number in select area of buffer,
if not select any area, then strip all line number of buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+ ")
  (message "Line number striped. ^_^"))

(defun strip-regular-expression-string (regular-expression)
  "Strip all string that match REGULAR-EXPRESSION in select area of buffer.
If not select any area, then strip current buffer"
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (save-excursion
      (goto-char end)
      (while (and (> (point) begin)
                  (re-search-backward regular-expression nil t))
        (replace-match "" t t)))))

(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'python-mode)
    (message "Don't indent python buffer. It will mess up the code syntax."))
   (t
    (save-excursion
      (indent-region (point-min) (point-max) nil)
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))))))

(defun indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (indent-comment-region (point-min) (point-max)))

(defun indent-comment-region (start end)
  "Indent region."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))

(defun capitalize-one-char (arg)
  "Change the letter pointed by the cursor to uppercase."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun lowercase-one-char (arg)
  "Change the letter pointed by the cursor to lowercase."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun mark-line ()
  "Mark one whole line, similar to `mark-paragraph'."
  (interactive)
  (beginning-of-line)
  (if mark-active
      (exchange-point-and-mark)
    (push-mark nil nil t))
  (forward-line)
  (exchange-point-and-mark))

(defun kill-and-join-forward (&optional arg)
  "Delete empty line in select region."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (forward-char 1)
        (just-one-space 0)
        (backward-char 1)
        (kill-line arg))
    (kill-line arg)))

(defun delete-chars-hungry-forward (&optional reverse)
  "Delete chars forward use `hungry' style.
Optional argument REVERSE default is delete forward, if reverse is non-nil delete backward."
  (delete-region
   (point)
   (progn
     (if reverse
         (skip-chars-backward " \t\n\r")
       (skip-chars-forward " \t\n\r"))
     (point))))

(defun delete-chars-hungry-backward ()
  "Delete chars backward use `hungry' style."
  (delete-chars-hungry-forward t))

(defun reverse-chars-in-region (start end)
  "Reverse the region character by character without reversing lines."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (dolist (line (split-string str "\n"))
      (let ((chars (mapcar (lambda (c)
                             (or (matching-paren c)
                                 c))
                           (reverse (append line nil)))))
        (when chars
          (apply 'insert chars))
        (newline)))))

(defun underline-line-with (char)
  "Insert some char below at current line."
  (interactive "cType one char: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

(defun prettyfy-string (string &optional after)
  "Strip starting and ending whitespace and pretty `STRING'.
Replace any chars after AFTER with '...'.
Argument STRING the string that need pretty."
  (let ((replace-map (list
                      (cons "^[ \t]*" "")
                      (cons "[ \t]*$" "")
                      (cons (concat "^\\(.\\{"
                                    (or (number-to-string after) "10")
                                    "\\}\\).*")
                            "\\1..."))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(defun forward-button-with-line-begin ()
  "Move to next button with line begin."
  (interactive)
  (call-interactively 'forward-button)
  (while (not (bolp))
    (call-interactively 'forward-button)))

(defun backward-button-with-line-begin ()
  "Move to previous button with line begin."
  (interactive)
  (call-interactively 'backward-button)
  (while (not (bolp))
    (call-interactively 'backward-button)))

(defun goto-column (number)
  "Untabify, and go to a column NUMBER within the current line (0 is beginning of the line)."
  (interactive "nColumn number: ")
  (move-to-column number t))

(defun only-comment-p ()
  "Return t if only comment in current line.
Otherwise return nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward comment-start (line-end-position) t)
        (progn
          (backward-char (length comment-start))
          (equal (point)
                 (progn
                   (back-to-indentation)
                   (point))))
      nil)))

(defun cursor-position-1-store ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun cursor-position-1-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun cursor-position-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq cursor-position-stack (cons (list (current-buffer) (point)) cursor-position-stack)))

(defun cursor-position-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null cursor-position-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar cursor-position-stack))
    (goto-char (cadar cursor-position-stack))
    (setq cursor-position-stack (cdr cursor-position-stack))))

(defun count-words ()
  "Count the number of word in buffer, include Chinese."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (count-ce-words begin end)))

(defun count-ce-words (beg end)
  "Count Chinese and English words in marked region."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

(defun goto-percent-text (percent)
  "Move the cursor to the character,
  which is <percent>% far from the top character."
  (interactive "n(text) Goto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun goto-percent-line (percent)
  "Move the cursor to the line,
  which is <percent>% far from the top line."
  (interactive "n(line) Goto percent: ")
  (goto-line (/ (* percent (count-lines (point-min) (point-max)))
                100)))

(defun replace-match+ (object match-str replace-str)
  "Replace `MATCH-STR' of `OBJECT' with `REPLACE-STR'."
  (string-match match-str object)
  (replace-match replace-str nil nil object 0))

(defun scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun cycle-buffer-in-special-mode (special-mode)
  "Cycle in special mode."
  (require 'cycle-buffer)
  (catch 'done
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p special-mode)
          (throw 'done (switch-to-buffer buffer)))))))

(defun find-file-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (require 'tramp)
  (tramp-cleanup-all-connections)
  (find-file (concat "/sudo:root@localhost:" file)))

(defun find-file-smb(file)
  "Access file through samba protocol."
  (interactive "fFind file as samba: ")
  (find-file (concat "/smb:" file)))

(defun kill-unused-buffers ()
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (and (string-prefix-p "*" (buffer-name)) (string-suffix-p "*" (buffer-name)))
          (kill-buffer buf))
        ))))

(defun join-lines (n)
  "Join N lines."
  (interactive "p")
  (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
    (dotimes (_ (abs n))
      (delete-indentation (natnump n)))))

(defun rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-file-name "New name: " (file-name-directory filename)))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Sure to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(provide 'basic-toolkit)

;;; basic-toolkit.el ends here
