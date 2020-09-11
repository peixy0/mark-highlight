# Mark Highlight

Mark text in buffer, with colors!

![Mark Highlight preview](screenshot.png)

    (use-package mark-highlight
      :bind (("<f3>" . mark-highlight-toggle)
             ("S-<f3>" . mark-highlight-reset-universe)))

`mark-highlight-toggle` will search selected text in the whole file and mark/unmark them.

`mark-highlight-reset-universe` will clear everything.