# My Emacs config

This Emacs config is inspired by David Wilsons Emacs from scratch series on youtube.
You can find the configuration for the series [here](https://github.com/daviwil/emacs-from-scratch)

## Init.org

This configuration uses the org-mode file ~/.emacs.d/Init.org for configuration.
All changes to the configuration must be made in the Init.org file. After saving Init.org, 
a new init.el will be created.
In order to make it nicer to manage the configuration with git, the init.el file is ignored by git

For more information read the contents in Init.org

## Initialize the config

After you have cloned the repo and started emacs you need to run

```emacs-lisp
M-x load-file RET
~/.emacs.d/bootstrap.el
```

This will initialize the package management, tangle Init.org and loads the created init.el
