;;; twitel.el --- Simple Emacs-based client for Twitter

;; Author: Neil Roberts
;; Keywords: twitter

;; Copyright 2008, 2009, 2010  Neil Roberts
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; A Twitter client for emacs that can view your friends timeline and
;; publish new statuses.

;; This requires the oauth package from here:
;;    http://www.emacswiki.org/emacs/OAuthLibrary

;;; You should add the following to your Emacs configuration file:

;; (autoload 'twitel-get-friends-timeline "twitel" nil t)
;; (autoload 'twitel-status-edit "twitel" nil t)
;; (global-set-key "\C-xt" 'twitel-get-friends-timeline)
;; (add-hook 'twitel-status-edit-mode-hook 'longlines-mode)

;; You can view the statuses by pressing C-x t. While in the timeline
;; buffer you can press C-c C-s to post a new status or C-c C-r to
;; reply to the status at point. Once the message is finished press
;; C-c C-c to publish.

;; To use Twitel you need to specify a consumer key and consumer
;; secret for OAuth authentication. Twitter's idea is that an
;; application developer would hardcode these keys into an application
;; and then try to hide them. However that's not really possible with
;; an open source application so instead they are left blank here. To
;; get keys you could either register your own Twitter application or
;; possibly steal another key from another application. Once you have
;; the value you can customize the twitel group to set them.

;; The first time you use Twitel it will use OAuth to get an access
;; token from Twitter. This will require you to login to a web page
;; and copy a code. The access token is saved so this should only be
;; needed once.

;;; Code:
(require 'cl)
(require 'url)
(require 'url-http)
(require 'xml)
(require 'oauth)

(defgroup twitel nil "Twitter status viewer"
  :group 'applications)

(defgroup twitel-faces nil "Faces for displaying Twitter statuses"
  :group 'twitel)

(defface twitel-header-face
  '((t (:background "light gray")))
  "base face for headers"
  :group 'twitel-faces)

(defface twitel-user-name-face
  '((t (:weight bold :inherit twitel-header-face)))
  "face for user name headers"
  :group 'twitel-faces)

(defface twitel-time-stamp-face
  '((t (:slant italic :inherit twitel-header-face)))
  "face for time stamp headers"
  :group 'twitel-faces)

(defface twitel-status-overlong-face
  '((t (:foreground "red")))
  "face used for characters in overly long Twitter statuses.
The face is also used in the mode line if the character count
remaining drops to negative.")

(defface twitel-new-tweets-sep-face
  '((t (:background "black" :foreground "white")))
  "Used when printing the separator between tweets already seen,
and tweets newly arrived."
  :group 'twitel-faces)

(defconst twitel-friends-timeline-url
  "http://twitter.com/statuses/friends_timeline.xml"
  "URL used to receive the friends timeline")

(defconst twitel-replies-timeline-url
  "http://twitter.com/statuses/replies.xml"
  "URL used to receive the replies timeline")

(defconst twitel-status-update-url
  "http://twitter.com/statuses/update.xml"
  "URL used to update Twitter status")

(defconst twitel-month-map
  '(("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Nov" . 11)
    ("Dec" . 12))
  "Assoc list mapping month abbreviations to month numbers")

(defcustom twitel-consumer-key
  ""
  "The consumer key for Twitel to gain an OAuth access token."
  :type 'string
  :group 'twitel)

(defcustom twitel-consumer-secret
  ""
  "The consumer secret for Twitel to gain an OAuth access token."
  :type 'string
  :group 'twitel)

(defvar twitel-access-token nil
  "Access token to authenticate with Twitter.
If nil, twitel will try to read a saved access token. If there
isn't one, it will try to fetch a new token from Twitter.")

(defconst twitel-access-token-file
  "~/.twitel-access-token"
  "Name of a file to store the access token in.")

(defconst twitel-request-url
  "https://api.twitter.com/oauth/request_token")

(defconst twitel-access-url
  "https://api.twitter.com/oauth/access_token")

(defconst twitel-authorize-url
  "https://api.twitter.com/oauth/authorize")

(defcustom twitel-maximum-status-length 140
  "Maximum length to allow in a Twitter status update."
  :type 'integer
  :group 'twitel)

(defcustom twitel-fetch-status-count nil
  "Number of status to retrieve when displaying a timeline.
If nil, it will be left up to the Twitter server to choose a default."
  :type '(choice (const :tag "Default" nil) (integer))
  :group 'twitel)

(defcustom twitel-include-replies nil
  "Whether to include the replies list in your friends timeline.
If t, the replies list will be merged and sorted with your
friends timeline."
  :type 'boolean
  :group 'twitel)

(defcustom twitel-status-source "twitel"
  "What to send as the source of status updates.
The Twitter website will use this to display a message like:

about 3 minutes ago from twitel."
  :type 'string
  :group 'twitel)

(defcustom twitel-time-format 'twitel-format-time-for-display
  "Function or string describing the format to display time stamps in.
If the value is a string it should be a format string with %
characters and it will be passed to format-time-string.

If the value is a function it will be called with a single
argument which will be a two element list containing the high and
low part of the number of seconds since epoch. The value can be
converted to broken down time using decode-time.

Otherwise the variable can be nil in which case the time string
from Twitter will be displayed directly."
  :type '(choice (const :tag "No translation" nil)
                 string
                 function)
  :group 'twitel)

(defcustom twitel-new-tweets-sep
  (propertize
   "----[ NEW ]--------------------------------------------------------------"
   'face 'twitel-new-tweets-sep-face)
  "Will be printed in between the last seen tweet, and the newly
received tweets."
  :type 'string
  :group 'twitel)

(defconst twitel-default-status-format
  (concat (propertize "%-32n"
                      'face 'twitel-user-name-face)
          (propertize "%33t"
                      'face 'twitel-time-stamp-face)
          (propertize " %r"
                      'face 'twitel-header-face)
          "\n%M\n\n")
  "The default status format.
This can be set as the value for twitel-status-format to make it
display the tweets with a long header line with the user's full
name, time of posting and a reply button followed by the content
of the tweet on a new line.")

(defconst twitel-web-status-format
  (concat (propertize "%u"
                      'face 'twitel-user-name-face)
          " %M\n"
          (propertize "%t from %s"
                      'face 'twitel-time-stamp-face)
          "\n\n")
  "A status format to appear more like the twitter website.
This can be set as the value for twitel-status-format to make it
display the tweets in a style similar to the twitter website. The
screen name of the tweeter preceeds the message and the time and
source is given on the next line.")

(defcustom twitel-status-format
  twitel-default-status-format
  "Format string describing how to display twitter statuses
It should be a string containing '%' characters followed by one
of the following commands:

%n - the full name of the person posting the tweet
%u - the screen name of the person posting the tweet
%t - the time the tweet was created. This gets formatted
     according to twitel-time-format
%r - a reply button
%m - the tweet's text
%M - the tweet's text but filled with fill-region
%s - the name of the program used to send the tweet

%i - the numeric id of the tweet
%T - whether the tweet was truncated

%U - the screen name of the person who the tweet was a reply to
%R - the status id of the tweet that this is a reply to
%S - the user id of the tweet that this is a reply to

%I - the id of the user posting the tweet
%l - the location of the user posting the tweet
%d - a description of the user posting the tweet
%A - a URL to the image for the person posting the tweet
%L - a URL to the home page for the person posting the tweet
%F - the number of followers of the person posting the tweet
%P - whether posts from this user are protected

%% - a literal percent character

Any other text is copied directly into the buffer. Text
properties are preserved and the properties of the % markers will
be applied to the resulting string.

The marker can optionally be given a padding value after the %
symbol. If the value is negative, the padding will be added to
the right otherwise it will be added to the left."
  :type `(choice (const :tag "Default" ,twitel-default-status-format)
                 (const :tag "Web" ,twitel-web-status-format)
                 string)
  :group 'twitel)

(defvar twitel-status-edit-remaining-length ""
  "Characters remaining in a Twitter status update.
This is displayed in the mode line.")

(put 'twitel-status-edit-remaining-length 'risky-local-variable t)

(defvar twitel-status-edit-overlay nil
  "Overlay used to highlight overlong status messages.")

(defvar twitel-status-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'twitel-status-post)
    (define-key map "\C-c\C-k" 'twitel-kill-status-buffer)
    map)
  "Keymap for `twitel-status-edit-mode'.")

(defvar twitel-timeline-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-r" 'twitel-reply)
    (define-key map "\C-c\C-s" 'twitel-status-edit)
    map)
  "Keymap for `twitel-timeline-view-mode'.")

(defvar twitel-frame-configuration nil
  "Frame configuration from immediately before a twitel command
is called")

(defvar twitel-last-id-seen nil
  "The last twitter id that was seen while building up the status
  list.")

(defvar twitel-last-id-saved nil
  "The last id that was saved as seen; this tweet will have a
  line above it (or whatever twitel-new-tweets-sep is set to)
  when shown again to demarcate old tweets from new ones.")

(defun twitel-retrieve-url (url cb &optional cbargs)
  "Wrapper around url-retrieve.
Fetches an access token and retains it if we don't already have
one."

  ;; If we don't already have an access token then fetch it now
  (when (null twitel-access-token)
    ;; Check if we saved a key from a previous instance
    (if (file-exists-p twitel-access-token-file)
        (with-temp-buffer
          (insert-file-contents twitel-access-token-file)
          (setq twitel-access-token (read (current-buffer))))
      ;; Otherwise fetch it from twitter
      (setq twitel-access-token
            (oauth-authorize-app twitel-consumer-key
                                 twitel-consumer-secret
                                 twitel-request-url
                                 twitel-access-url
                                 twitel-authorize-url))
      ;; Save the token for next time
      (with-temp-file twitel-access-token-file
        (prin1 twitel-access-token (current-buffer)))))

  (oauth-url-retrieve twitel-access-token url cb cbargs))

(defun twitel-retrieve-timeline-url (url cb &optional cbargs)
  "Wrapper around twitel-retrieve-url which sets the count parameter.
This can be used for Twitter API methods that fetch a
timeline. It sets the count parameter based on the
twitel-fetch-status-count variable."
  (when twitel-fetch-status-count
     (setq url (concat url
                       "?count="
                       (number-to-string twitel-fetch-status-count))))
  (twitel-retrieve-url url cb cbargs))

(defun twitel-get-friends-timeline ()
  "Fetch and display the friends timeline.
The results are formatted and displayed in a buffer called
*Twitter friends timeline*

If the variable `twitel-include-replies' is non-nil, the replies
timeline will also be merged into the friends timeline and
displayed."
  (interactive)
  (twitel-retrieve-timeline-url twitel-friends-timeline-url
                                 'twitel-fetched-friends-timeline
                                 (list (if twitel-include-replies
                                           (list twitel-replies-timeline-url)
                                         nil)
                                       ;; next arg is list of status to merge
                                       nil)))

(defun twitel-fetched-friends-timeline (status other-urls status-list)
  "Callback handler for fetching the Twitter friends timeline."
  (let ((result-buffer (current-buffer)) doc)
    ;; Make sure the temporary results buffer is killed even if the
    ;; xml parsing raises an error
    (unwind-protect
        (progn
          ;; Skip the mime headers
          (goto-char (point-min))
          (re-search-forward "\n\n")
          ;; Parse the rest of the document
          (setq doc (xml-parse-region (point) (point-max))))
      (kill-buffer result-buffer))
    ;; Merge the new list with the current list of statuses
    (setq status-list (twitel-merge-status-lists status-list
                                                  (xml-get-children (car doc)
                                                                    'status)))
    ;; If there's more URLs then start fetching those
    (if other-urls
        (twitel-retrieve-timeline-url (car other-urls)
                                       'twitel-fetched-friends-timeline
                                       (list (cdr other-urls) status-list))
      ;; Otherwise display the results
      ;; Get a clean buffer to display the results
      (let ((buf (get-buffer-create "*Twitter friends timeline*"))
            (compiled-format (twitel-compile-format-string
                              twitel-status-format)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (kill-all-local-variables)
            ;; If the GET failed then display an error instead
            (if (plist-get status :error)
                (twitel-show-error doc)
              ;; Otherwise process each status node
	      (progn
		(setq twitel-last-id-seen 'nil)
		(while status-list
		  (twitel-format-status-node (car status-list)
					      compiled-format)
		  (setq status-list (cdr status-list)))
		(setq twitel-last-id-saved twitel-last-id-seen))))
          (goto-char (point-min))
          (twitel-timeline-view-mode))
        (view-buffer buf 'kill-buffer)))))

;; Angle brackets ("<" and ">") are entity-encoded.
;; See Question 7) "Encoding affects status character count" at
;; http://apiwiki.twitter.com/Things-Every-Developer-Should-Know
(defun twitel-decode-entity-encoding (str)
  (let (result)
    (setq result (replace-regexp-in-string "&gt;" ">" str))
    (setq result (replace-regexp-in-string "&lt;" "<" result))))

(defun twitel-get-node-text (node)
  "Return the text of XML node NODE.
All of the text elements are concatenated together and returned
as a single string."
  (let (text-parts)
    (dolist (part (xml-node-children node))
      (when (stringp part)
        (push (twitel-decode-entity-encoding part) text-parts)))
    (apply 'concat (nreverse text-parts))))

(defun twitel-get-attrib-node (node attrib)
  "Get the text of a child attribute node.
If the children of XML node NODE are formatted like
<attrib1>data</attrib1> <attrib2>data</attrib2> ... then this
fuction will return the text of the child node named ATTRIB or
nil if it isn't found."
  (let ((child (xml-get-children node attrib)))
    (if (consp child)
        (twitel-get-node-text (car child))
      nil)))

(defun twitel-reply-button-pressed (button)
  "Calls twitel-reply for the position where BUTTON is."
  (twitel-reply (overlay-start button)))

(defun twitel-reply (pos)
  "Sets up a status edit buffer to reply to the message at POS.
twitel-reply-status-id is set to the id of the status
corresponding to the status so that it will be marked as a
reply. The status' screen name is initially entered into the
buffer.

When called interactively POS is set to point."
  (interactive "d")
  (let ((status-screen-name (get-text-property pos 'twitel-status-screen-name))
        (status-id (get-text-property pos 'twitel-status-id)))
    (when (null status-screen-name)
      (error "Missing screen name in status"))
    (when (null status-id)
      (error "Missing status id"))
    (twitel-status-edit)
    (setq twitel-reply-status-id status-id)
    (insert "@" status-screen-name " ")))

(defun twitel-show-error (doc)
  "Show a Twitter error message.
DOC should be the XML parsed document returned in the error
message. If any information about the error can be retrieved it
will also be displayed."
  (insert "An error occured while trying to process a Twitter request.\n\n")
  (let (error-node)
    (if (and (consp doc)
             (consp (car doc))
             (eq 'hash (caar doc))
             (setq error-node (xml-get-children (car doc) 'error)))
        (insert (twitel-get-node-text (car error-node)))
      (xml-print doc))))

(defun twitel-format-time-for-display (time)
  "Convert TIME to a friendly human readable string.
TIME should be a high/low pair as returned by encode-time."
  ;; This is based on a similar function from Tweet
  (let* ((now (current-time))
         (age (subtract-time now time))
         (age-days (- (time-to-days now) (time-to-days time))))
    (if (or (< (car age) 0)
            (>= (car age) 16) ; more than about 12 days
            (>= age-days 7))
        (format-time-string "%x at %H:%M" time)
      (let* ((age-seconds (logior (lsh (car age) 16) (cadr age)))
             (age-minutes (/ age-seconds 60))
             (age-hours (/ age-minutes 60)))
        (cond ((< age-seconds 60)
               "Less than a minute ago")
              ((<= age-minutes 1)
               "About a minute ago")
              ((< age-minutes 60)
               (format "About %d minutes ago" age-minutes))
              ((<= age-hours 1)
               "About an hour ago")
              ((< age-minutes 360)
               (format "About %d hours ago" age-hours))
              ((<= age-days 0)
               (format-time-string "Today at %H:%M" time))
              ((<= age-days 1)
               (format-time-string "Yesterday at %H:%M" time))
              (t
               (format-time-string "Last %A at %H:%M" time)))))))

(defun twitel-compile-format-string (format-string)
  "Converts FORMAT-STRING into a list that is easier to scan.
See twitel-status-format for a description of the format. The
returned list contains elements that are one of the following:

- A string. This should be inserted directly into the buffer.

- A four element list like (RIGHT-PAD WIDTH COMMAND
  PROPERTIES). RIGHT-PAD is t if the - flag was specified or nil
  otherwise. WIDTH is the amount to pad the string to or nil if
  no padding was specified. COMMAND is an integer representing
  the character code for the command. PROPERTIES is a list of
  text properties that should be applied to the resulting
  string."
  (let (parts last-point)
    (with-temp-buffer
      (insert format-string)
      (goto-char (point-min))
      (setq last-point (point))
      (while (re-search-forward "%\\(-?\\)\\([0-9]*\\)\\([a-zA-Z%]\\)" nil t)
        ;; Push the preceeding string (if any) to copy directly into
        ;; the buffer
        (when (> (match-beginning 0) last-point)
          (push (buffer-substring last-point (match-beginning 0)) parts))
        ;; Make the three element list describing the command
        (push (list (> (match-end 1) (match-beginning 1)) ; is - flag given?
                    (if (> (match-end 2) (match-beginning 2)) ; is width given?
                        (string-to-number (match-string 2)) ; extract the width
                      nil) ; otherwise set to nil
                    ;; copy the single character for the command number directly
                    (char-after (match-beginning 3))
                    ;; extract all of the properties so they can be
                    ;; copied into the final string
                    (text-properties-at (match-beginning 0)))
              parts)
        ;; Move last point to the end of the last match
        (setq last-point (match-end 0)))
      ;; Add any trailing text
      (when (< last-point (point-max))
        (push (buffer-substring last-point (point-max)) parts)))
    (nreverse parts)))

(defconst twitel-status-commands
  '((?i . id)
    (?R . in_reply_to_status_id)
    (?S . in_reply_to_user_id)
    (?U . in_reply_to_screen_name)
    (?T . truncated))
  "Alist mapping format commands to XML nodes in the status element.")

(defconst twitel-user-commands
  '((?n . name)
    (?u . screen_name)
    (?I . id)
    (?l . location)
    (?d . description)
    (?A . profile_image_url)
    (?L . url)
    (?F . followers_count)
    (?P . protected))
  "Alist mapping format commands to XML nodes in the user element.")

(defun twitel-insert-status-part-for-command (status-node command)
  "Extract the string for COMMAND from STATUS-NODE and insert.
The command should be integer representing one of the characters
supported by twitel-status-format."
  (let ((user-node (car (xml-get-children status-node 'user))))
    (cond ((= command ?t)
           (let ((val (twitel-get-attrib-node status-node 'created_at)))
             (when val
               (cond ((stringp twitel-time-format)
                      (insert (format-time-string twitel-time-format
                                                  (twitel-time-to-time val))))
                     ((functionp twitel-time-format)
                      (insert (funcall twitel-time-format
                                       (twitel-time-to-time val))))
                     ((null twitel-time-format)
                      (insert val))
                     (t (error "Invalid value for twitel-time-format"))))))
          ((= command ?r)
           (insert-button "reply"
                          'action 'twitel-reply-button-pressed))
          ((or (= command ?m) (= command ?M))
           (let ((val (twitel-get-attrib-node status-node 'text)))
             (when val
               (if (= command ?M)
                   (fill-region (prog1 (point) (insert val)) (point))
                 (insert val)))))
          ((= command ?s)
           (let ((val (twitel-get-attrib-node status-node 'source)))
             (when val
               (with-temp-buffer
                 (insert val)
                 (setq val (twitel-get-node-text
                            (car (xml-parse-region (point-min) (point-max))))))
               (when val
                 (insert val)))))
          ((= command ?%)
           (insert ?%))
          (t
           (let (val elem)
             (cond ((setq elem (assoc command twitel-user-commands))
                    (setq val (twitel-get-attrib-node
                               user-node (cdr elem))))
                   ((setq elem (assoc command twitel-status-commands))
                    (setq val (twitel-get-attrib-node
                               status-node (cdr elem)))))
             (when val
               (insert val)))))))

(defun twitel-format-status-node (status-node format)
  "Insert the contents of a Twitter status node.
The status is formatted with text properties according to FORMAT
and insterted into the current buffer. FORMAT should be a
compiled format string as returned by
twitel-compile-format-string."
  (let ((status-begin (point)))
    (let (elem this-id)
      (setq elem (assoc ?i twitel-status-commands))
      (setq this-id (twitel-get-attrib-node
		     status-node (cdr elem)))

      (if (string= twitel-last-id-saved this-id)
	  (progn (insert twitel-new-tweets-sep)
		 (newline)))

      (if (not twitel-last-id-seen)
	  (setq twitel-last-id-seen this-id)))

    (while format
      (if (stringp (car format))
          (insert (car format))
        (let ((part-start (point))
              (right-pad (caar format))
              (padding (cadar format))
              (command (caddar format))
              (properties (nth 3 (car format))))
          (twitel-insert-status-part-for-command status-node command)
          (when (and padding
                     (< (- (point) part-start) padding))
            (setq padding (make-string
                           (+ padding (- part-start (point))) ? ))
            (if right-pad
                (insert padding)
              (let ((part-end (point)))
                (goto-char part-start)
                (insert padding)
                (goto-char (+ part-end (length padding))))))
          (add-text-properties part-start (point) properties)))
      (setq format (cdr format)))
    (let ((user-node (car (xml-get-children status-node 'user))))
      (add-text-properties status-begin (point)
                           `(twitel-status-screen-name
                             ,(twitel-get-attrib-node user-node 'screen_name)
                             twitel-status-id
                             ,(twitel-get-attrib-node status-node 'id))))))

(defun twitel-remove-duplicate-statuses (a b)
  "Destructively modifies A to removes statuses that are also in B.
The new head of A is returned."
  (let (last (na a) nb)
    (while na
      (setq nb b)
      ;; Looking for a matching node in b
      (if (catch 'found
            (while nb
              (if (string-equal (twitel-get-attrib-node (car na) 'id)
                                (twitel-get-attrib-node (car nb) 'id))
                  (throw 'found t))
              (setq nb (cdr nb)))
            nil)
          ;; If we found one then skip this node
          (if last
              (setcdr last (cdr na))
            (setq a (cdr na)))
        (setq last na))
      (setq na (cdr na))))
  a)

(defun twitel-merge-status-lists (a b)
  "Merge the two twitter status lists.
The lists should be just the status nodes from the parsed XML
output. They are interleaved so that the resulting list is still
sorted by time. Duplicate entries are removed. The resulting list
is then returned."
  ;; Remove duplicates from a
  (setq a (twitel-remove-duplicate-statuses a b))

  (let (result)
    (while (cond ((null a) ; have we reached the end of a?
                  ;; return result + b
                  (setq result (nconc (nreverse result) b))
                  nil)
                 ((null b) ; have we reached the end of b?
                  ;; return result + a
                  (setq result (nconc (nreverse result) a))
                  nil)
                 ((twitel-status-time-lessp (car a) (car b))
                  ;; choose b
                  (push (car b) result)
                  (setq b (cdr b))
                  t)
                 (t
                  ;; choose a
                  (push (car a) result)
                  (setq a (cdr a))
                  t)))
    result))

(defun twitel-status-time-lessp (a b)
  "Return whether the time stamp of status node A is < B."
  (time-less-p (twitel-time-to-time (twitel-get-attrib-node
                                      a 'created_at))
               (twitel-time-to-time (twitel-get-attrib-node
                                      b 'created_at))))

(defun twitel-time-to-time (time)
  "Convert TIME to a number of seconds since some epoch."
  (let ((case-fold-search t))
    (if (null (string-match (concat "\\`[a-z]\\{3\\} "
                                    "\\([a-z]\\{3\\}\\) "
                                    "\\([0-9]\\{1,2\\}\\) "
                                    "\\([0-9]\\{2\\}\\):"
                                    "\\([0-9]\\{2\\}\\):"
                                    "\\([0-9]\\{2\\}\\) "
                                    "\\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\) "
                                    "\\([0-9]\\{4\\}\\)\\'") time))
        (error "Invalid time string: %s" time))
    (encode-time (string-to-number (match-string 5 time))
                 (string-to-number (match-string 4 time))
                 (string-to-number (match-string 3 time))
                 (string-to-number (match-string 2 time))
                 (cdr (assoc (match-string 1 time) twitel-month-map))
                 (string-to-number (match-string 8 time))
                 (concat (match-string 6 time) ":" (match-string 7 time)))))

(defun twitel-status-get-string ()
   "Get the contents of the current buffer as a string.
All groups of spaces in the string are replaced with a single
space."
   (let ((other-buffer (current-buffer)))
     (with-temp-buffer
       (insert-buffer-substring-no-properties other-buffer)
       (goto-char (point-min))
       (while (re-search-forward "[\n\t ]+" nil t)
         (replace-match " " t t))
       (buffer-substring (point-min) (point-max)))))

(defun twitel-status-post ()
  "Update your Twitter status.
The contents of the current buffer are used for the status. The
current buffer is then killed. If there is too much text in the
buffer then you will be asked for confirmation.

If the twitel-reply-status-id variable is not nil then this will
be sent to mark the status as a reply. The reply button on the
status list automatically sets that varaible."
  (interactive)
  (when (or (<= (buffer-size) twitel-maximum-status-length)
            (y-or-n-p (format (concat "The message is %i characters long. "
                                      "Are you sure? ") (buffer-size))))
    (message "Sending status...")
    (let ((url-request-method "POST")
          (url-request-data (concat "status="
                                    (url-hexify-string
                                     (twitel-status-get-string))
                                    "&source="
                                    (url-hexify-string
                                     twitel-status-source))))
      (when twitel-reply-status-id
        (setq url-request-data (concat url-request-data
                                       "&in_reply_to_status_id="
                                       twitel-reply-status-id)))
      (twitel-retrieve-url twitel-status-update-url
                            'twitel-status-callback))))

(defun twitel-status-callback (status)
  "Function called after Twitter status has been sent."
  (let ((errmsg (plist-get status :error)))
    (when errmsg
      (signal (car errmsg) (cdr errmsg)))
    (twitel-kill-status-buffer)
    (message "Succesfully updated Twitter status.")))

(defun twitel-kill-status-buffer ()
  "Kill the *Twitter Status* buffer and restore the previous
frame configuration."
  (interactive)
  (kill-buffer "*Twitter Status*")
  (set-frame-configuration twitel-frame-configuration))

(defun twitel-status-edit ()
  "Edit your twitter status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[twitel-status-post] when you are finished editing to send the
message."
  (setq twitel-frame-configuration (current-frame-configuration))
  (interactive)
  (pop-to-buffer "*Twitter Status*")
  (twitel-status-edit-mode))

(defun twitel-status-edit-update-length ()
  "Updates the character count in Twitter status buffers.
This should be run after the text in the buffer is changed. Any
characters after the maximum status update length are
hightlighted in the face twitel-status-overlong-face and the
character count on the mode line is updated."
  ;; Update the remaining characters in the mode line
  (let ((remaining (- twitel-maximum-status-length
                      (buffer-size))))
    (setq twitel-status-edit-remaining-length
          (concat " "
                  (if (>= remaining 0)
                      (number-to-string remaining)
                    (propertize (number-to-string remaining)
                                'face 'twitel-status-overlong-face))
                  " ")))
  (force-mode-line-update)
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) twitel-maximum-status-length)
      (let ((start (+ (point-min) twitel-maximum-status-length)))
        (if (null twitel-status-edit-overlay)
            (overlay-put (setq twitel-status-edit-overlay
                               (make-overlay start (point-max)))
                         'face 'twitel-status-overlong-face)
          (move-overlay twitel-status-edit-overlay
                        start (point-max))))
    ;; Buffer is not too long so just hide the overlay
    (when twitel-status-edit-overlay
      (delete-overlay twitel-status-edit-overlay))))

(defun twitel-status-edit-after-change (begin end old-size)
  (twitel-status-edit-update-length))

(define-derived-mode twitel-status-edit-mode text-mode "Twitter Status Edit"
  "Major mode for updating your Twitter status."
  ;; Schedule to update the character count after altering the buffer
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'twitel-status-edit-after-change)
  ;; Add the remaining character count to the mode line
  (make-local-variable 'twitel-status-edit-remaining-length)
  ;; Copy the mode line format list so we can safely edit it without
  ;; affecting other buffers
  (setq mode-line-format (copy-sequence mode-line-format))
  ;; Add the remaining characters variable after the mode display
  (let ((n mode-line-format))
    (catch 'found
      (while n
        (when (eq 'mode-line-modes (car n))
          (setcdr n (cons 'twitel-status-edit-remaining-length
                          (cdr n)))
          (throw 'found nil))
        (setq n (cdr n)))))
  ;; Make a buffer-local reference to the overlay for overlong
  ;; messages
  (make-local-variable 'twitel-status-edit-overlay)
  ;; A buffer local variable for the reply id. This is filled in when
  ;; the reply button is pressed
  (make-local-variable 'twitel-reply-status-id)
  (setq twitel-reply-status-id nil)
  ;; Update the mode line immediatly
  (twitel-status-edit-update-length))

(define-derived-mode twitel-timeline-view-mode fundamental-mode
  "Twitter Timeline"
  "Major mode for viewing timelines from Twitter.")

(provide 'twitel)

;;; twitel.el ends here
