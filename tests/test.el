(progn
  (load-file "../sturgeon.el")
  
  (sturgeon-start-process "tree-server" nil
                          "/home/def/Work/emacs-serge/tests/tree_server" nil
                          'sturgeon-ui-handler))
