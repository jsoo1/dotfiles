;;; haskell-prettify.el --- Prettify Haskell code

;; Copyright (C) 2015 Clément Pit--Claudel
;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>

;; Created: 10 Jul 2015
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; To use the bindings defined in this file, add the following to your .emacs:
;; (haskell-prettify-setup 'haskell-mode)

;;; Code:

(defvar haskell-prettify-symbols-alist
  '(;; Double-struck letters
    ("|A|" . ?𝔸)
    ("|B|" . ?𝔹)
    ("|C|" . ?ℂ)
    ("|D|" . ?𝔻)
    ("|E|" . ?𝔼)
    ("|F|" . ?𝔽)
    ("|G|" . ?𝔾)
    ("|H|" . ?ℍ)
    ("|I|" . ?𝕀)
    ("|J|" . ?𝕁)
    ("|K|" . ?𝕂)
    ("|L|" . ?𝕃)
    ("|M|" . ?𝕄)
    ("|N|" . ?ℕ)
    ("|O|" . ?𝕆)
    ("|P|" . ?ℙ)
    ("|Q|" . ?ℚ)
    ("|R|" . ?ℝ)
    ("|S|" . ?𝕊)
    ("|T|" . ?𝕋)
    ("|U|" . ?𝕌)
    ("|V|" . ?𝕍)
    ("|W|" . ?𝕎)
    ("|X|" . ?𝕏)
    ("|Y|" . ?𝕐)
    ("|Z|" . ?ℤ)
    ("|gamma|" . ?ℽ)
    ("|Gamma|" . ?ℾ)
    ("|pi|" . ?ℼ)
    ("|Pi|" . ?ℿ)

    ;; Types
    ("::" . ?∷)

    ;; Quantifiers
    ("forall" . ?∀)
    ("exists" . ?∃)

    ;; Arrows
    ("->" . ?→)
    ("-->" . ?⟶)
    ("<-" . ?←)
    ("<--" . ?⟵)
    ("<->" . ?↔)
    ("<-->" . ?⟷)

    ("=>" . ?⇒)
    ("==>" . ?⟹)
    ("<==" . ?⟸)
    ("<=>" . ?⇔)
    ("<==>" . ?⟺)

    ("|->" . ?↦)
    ("|-->" . ?⟼)
    ("<-|" . ?↤)
    ("<--|" . ?⟻)

    ("|=>" . ?⤇)
    ("|==>" . ?⟾)
    ("<=|" . ?⤆)
    ("<==|" . ?⟽)

    ("~>" . ?⇝)
    ("<~" . ?⇜)

    (">->" . ?↣)
    ("<-<" . ?↢)
    ("->>" . ?↠)
    ("<<-" . ?↞)

    (">->>" . ?⤖)
    ("<<-<" . ?⬻)

    ("<|-" . ?⇽)
    ("-|>" . ?⇾)
    ("<|-|>" . ?⇿)

    ("<-/-" . ?↚)
    ("-/->" . ?↛)

    ("<-|-" . ?⇷)
    ("-|->" . ?⇸)
    ("<-|->" . ?⇹)

    ("<-||-" . ?⇺)
    ("-||->" . ?⇻)
    ("<-||->" . ?⇼)

    ("-o->" . ?⇴)
    ("<-o-" . ?⬰)

    ;; Boolean operators
    ("not" . ?¬)
    ("&&" . ?∧)
    ("||" . ?∨)

    ;; Relational operators
    ("==" . ?≡)
    ("/=" . ?≠)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("/<" . ?≮)
    ("/>" . ?≯)

    ;; Containers / Collections
    ("++" . ?⧺)
    ("+++" . ?⧻)
    ("|||" . ?⫴)
    ("empty" . ?∅)
    ("elem" . ?∈)
    ("notElem" . ?∉)
    ("member" . ?∈)
    ("notMember" . ?∉)
    ("union" . ?∪)
    ("intersection" . ?∩)
    ("isSubsetOf" . ?⊆)
    ("isProperSubsetOf" . ?⊂)

    ;; Other
    ("<<" . ?≪)
    (">>" . ?≫)
    ("<<<" . ?⋘)
    (">>>" . ?⋙)
    ("<|" . ?⊲)
    ("|>" . ?⊳)
    ("><" . ?⋈)
    ("mempty" . ?∅)
    ("mappend" . ?⊕)
    ("<*>" . ?⊛)
    ("undefined" . ?⊥)
    (":=" . ?≔)
    ("=:" . ?≕)
    ("=def" . ?≝)
    ("=?" . ?≟)
    ("..." . ?…)))

;;;###autoload
(defun haskell-prettify-enable ()
  "Enable prettification for Haskell symbols."
  (prettify-symbols-mode -1)
  (setq-local prettify-symbols-alist (append prettify-symbols-alist
                                             haskell-prettify-symbols-alist))
  (prettify-symbols-mode))

(provide 'haskell-prettify)
;;; haskell-prettify.el ends here
