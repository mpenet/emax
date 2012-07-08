(require 'pgg)

;; verify/decrypt only if mml knows about the protocl used
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

;; Automatically sign when sending mails
(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

;; Enough explicit settings
;(setq pgg-passphrase-cache-expiry 300)
;(setq pgg-default-user-id max.penet@gmail.com::primary-key)

;; standard way of getting imap going
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; set up smtp so we can send from gmail too:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 25 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 25 "zcamster@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 25)

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-home-directory "~/.gnus"
      gnus-directory "~/.gnus/news"
      mfolder-directory "~/.gnus/mail"
      message-directory "~/.gnus/mail"
      gnus-large-newsgroup 'nil)

;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads are nice!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

(setq user-full-name "Max Penet")
(setq user-mail-address "zcamster@gmail.com")
