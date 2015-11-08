(progn
  (load-file "serge.el")
  (load-file "serge-naive-hyperprint.el")
  
  (serge-start-process "tree-server" nil
                       "/home/def/Work/emacs-serge/tree_server" nil
                       'serge-hyperprint-handler)
)
