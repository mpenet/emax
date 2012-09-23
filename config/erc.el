(load "~/.erc" t)

(setq erc-modules '(netsplit fill track completion ring button autojoin
                             services match stamp track page scrolltobottom
                             hl-nicks)
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
      erc-autojoin-mode t
      erc-timestamp-format "%H:%M "
      erc-interpret-mirc-color t
      erc-input-line-position -2
      erc-prompt ">"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      ;; erc-current-nick-highlight-type 'nick
      erc-prompt-for-nickserv-password nil)

(eval-after-load 'erc
  '(progn
     (require 'erc-services)
     (require 'erc-hl-nicks)
     (erc-services-mode 1)
     (erc-track-mode t)
     (set-face-foreground 'erc-input-face "dim gray")))

(defun erc-connect/freenode ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (erc :server "irc.freenode.net"
       :port 6667
       :nick "mpenet"
       :full-name "mpenet"))

(defun erc-connect/groveio ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (add-to-list 'erc-networks-alist '(grove "irc.grove.io"))
  (add-to-list 'erc-nickserv-alist
               '(grove "NickServ!NickServ@services."
                       "This nickname is registered."
                       "NickServ" "IDENTIFY" nil))
  (erc-tls :server "shore.irc.grove.io" :port 6697
           :nick "mpenet" :password grove-connect-password))

(defun erc-connect/quakenet ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (erc :server "irc.quakenet.org"
       :port 6667
       :nick "zcam"
       :full-name "zcam"))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure" "#haskell")
        ("quakenet.org" "#ratatouil")))

(global-set-key (kbd "C-c e f") 'erc-connect/freenode)
(global-set-key (kbd "C-c e g") 'erc-connect/groveio)
(global-set-key (kbd "C-c e q") 'erc-connect/quakenet)
