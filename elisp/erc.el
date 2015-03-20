(setq erc-modules '(netsplit fill track completion ring button autojoin
                             services match stamp track page scrolltobottom
                             hl-nicks)
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
      erc-ignore-list '("chord" "chare")
      erc-autojoin-mode t
      erc-timestamp-format "%H:%M "
      erc-interpret-mirc-color t
      erc-input-line-position -2
      erc-prompt ">"
      erc-keywords '("\\bcassandra\\b" "\\balia\\b" "\\bhayt\\b")
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      ;; erc-current-nick-highlight-type 'nick
      erc-prompt-for-nickserv-password nil)

(eval-after-load 'erc
  '(progn
     (erc-services-mode 1)
     (erc-track-mode t)))

(require 'erc-services)

(defun erc-connect/freenode ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (erc :server "kornbluth.freenode.net"
       :port 6667
       :nick "mpenet"
       :full-name "mpenet"))

(defun erc-connect/slack ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (add-to-list 'erc-networks-alist '(slack "irc.slack.com"))
  (add-to-list 'erc-nickserv-alist
               '(slack "NickServ!NickServ@services."
                       "This nickname is registered."
                       "NickServ" "IDENTIFY" nil))
  (erc-tls :server "shoreware.irc.slack.com" :port 6667
           :nick "mpenet" :password slack-connect-password))

(defun erc-connect/quakenet ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (erc :server "irc.quakenet.org"
       :port 6667
       :nick "zcam"
       :full-name "zcam"))

(defun erc-connect/mozilla ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (erc :server "irc.mozilla.org"
       :port 6667
       :nick "mpenet"
       :full-name "mpenet"))

(defun erc-connect/gitter ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (add-to-list 'erc-networks-alist '(gitter "irc.gitter.im"))
  (erc-tls :server "irc.gitter.im" :port 6667
           :nick "mpenet" :password gitter-connect-password))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#clojure" "#cassandra" "#datastax-drivers" "#leiningen"
         "#pixie-lang")
        ("irc.gitter.im" "#mpenet/alia" "#mpenet/jet" "#mpenet/hayt" "#MichaelDrogalis/onyx")
        ("irc.mozilla.org" "#rust")
        ("quakenet.org" "#ratatouil")))

(global-set-key (kbd "C-c e f") 'erc-connect/freenode)
(global-set-key (kbd "C-c e q") 'erc-connect/quakenet)
(global-set-key (kbd "C-c e s") 'erc-connect/slack)
(global-set-key (kbd "C-c e m") 'erc-connect/mozilla)
(global-set-key (kbd "C-c e g") 'erc-connect/gitter)
