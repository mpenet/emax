(autoload 'mu4e "mu4e" "" t)

;; general
(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; incoming
(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder   "/[Gmail].Sent Mail"
      mu4e-trash-folder  "/[Gmail].Trash"
      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
      mu4e-sent-messages-behavior 'delete

      ;; you can quickly switch to your Inbox -- press ``ji''
      ;; then, when you want archive some messages, move them to
      ;; the 'All Mail' folder by pressing ``ma''.
      mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/chiara" . ?c)
        ;; ("/clojure"     . ?c)
        ;; ("/github"      . ?g)
        ("/[Gmail].Sent Mail"   . ?s)
        ("/[Gmail].All Mail"    . ?a))
      ;; allow for updating mail using 'U' in the main view:
      mu4e-get-mail-command "offlineimap"
      mu4e-headers-leave-behavior 'apply
      mu4e-show-images t
      mu4e-maildir (expand-file-name "~/mail")
      mu4e-update-interval 300
      mu4e-html2text-command "html2text -utf8 -width 72")

;; outgoing
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      user-mail-address "m@qbits.cc"
      user-full-name  "Max Penet"
      message-kill-buffer-on-exit t)

(eval-after-load 'mu4e-headers
  '(defun mu4e-headers-mark-and-next (mark)
     "Set mark MARK on the message at point or on all messages in the
region if there is a region, then move to the next message."
     (interactive)
     (mu4e-mark-set mark)
     (forward-line)
     ;; ugh; no!
     ;; (mu4e-headers-next)
     ))
