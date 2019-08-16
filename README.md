Note: this file is auto converted from org-picklink.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [org-picklink's README](#org25afe21)


<a id="org25afe21"></a>

# org-picklink's README

This package contains the command \`org-picklink' which pops
up a org-agenda window as link chooser, user can
pick a headline in this org-agenda window, then insert
its link to origin org-mode buffer.

![img](./snapshots/org-picklink.gif)

The simplest installation method is to call:

    (org-picklink-enable)

This will bind "C-c l" in org-mode buffer to \`org-picklink'.

This can also be done manually, e.g.:

    (define-key org-agenda-mode-map "q" 'org-picklink-quit-window)
    (define-key org-agenda-mode-map "\C-c\C-c" 'org-picklink-push-link)
    (define-key org-agenda-mode-map (kbd "RET") 'org-picklink-push-link-and-quit-window)
    (define-key org-mode-map "\C-cl" 'org-picklink)

