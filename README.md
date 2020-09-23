# Mark Highlight ![wftpl](http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl-badge-4.png)

Mark text in buffer, with colors!

![Mark Highlight preview](screenshot.png)

    (use-package mark-highlight
      :bind (("<f5>" . mark-highlight-toggle)
             ("S-<f5>" . mark-highlight-reset-universe)))

`mark-highlight-toggle` will search selected text in the whole file and mark/unmark them.

`mark-highlight-reset-universe` will clear everything.
