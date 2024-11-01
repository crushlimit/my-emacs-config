(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(modus-operandi-tritanopia))
 '(custom-safe-themes
   '("99d1e29934b9e712651d29735dd8dcd431a651dfbe039df158aa973461af003e" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "e1da45d87a83acb558e69b90015f0821679716be79ecb76d635aafdca8f6ebd4" "9a977ddae55e0e91c09952e96d614ae0be69727ea78ca145beea1aae01ac78d2" "1594eb8fc081be254c7df7b2a37e9808f79c94863366da6d34bbe735519a30f5" "372905daccda4574b28e5d5b64d5a4059da9e3c5548abc274af04fe63adc1372" "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983" "cb8eb6d80c3908a53b0c8a98ab0cedd007c1f10593a5f0f1e2ee24eec241e3e0" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" "64045b3416d83e5eac0718e236b445b2b3af02ff5bcd228e9178088352344a92" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "eab123a5ed21463c780e17fc44f9ffc3e501655b966729a2d5a2072832abd3ac" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "bbfccff82c1d35611cdf25339401a483875b32472fae7fcdaf14bd12c3a05b07" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "0cf95236abcf59e05b1ea69b4edd53d293a5baec4fe4c3484543fee99bfd2204" "bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" default))
 '(desktop-path '("~/.emacs.d/" "~" "~/.emacs.d/sessions/"))
 '(doom-one-light-brighter-comments t)
 '(doom-one-light-brighter-modeline t)
 '(doom-one-light-padded-modeline t)
 '(elpy-rpc-python-command "python3.11")
 '(global-display-line-numbers-mode t)
 '(highlight-indent-guides-method 'character)
 '(highlight-indentation-blank-lines nil)
 '(org-modern-block-fringe t)
 '(org-modern-block-name t)
 '(org-modern-star 'fold)
 '(org-modern-timestamp '(" %Y-%m-%d " . " %H:%M "))
 '(package-selected-packages
   '(org-mode-modern org-modern nerd-icons-completion all-the-icons-dired all-the-icons-nerd-fonts all-the-icons-completion all-the-icons modus-themes auctex pdf-tools vterm corfu anaconda-mode doom-themes company-quickhelp elpy go-mode rustic racket-mode slime-company slime zenburn-theme yasnippet-snippets which-key wfnames vertico undo-tree spacemacs-theme shrink-path paredit ox-hugo org-roam orderless nerd-icons markdown-preview-mode marginalia magit leuven-theme helpful git-timemachine flycheck-inline flycheck-color-mode-line exec-path-from-shell elisp-demos elfeed easy-hugo delight dap-mode consult-flycheck cmake-mode async))
 '(pdf-view-incompatible-modes nil)
 '(python-interpreter "python")
 '(python-shell-completion-native-enable t)
 '(python-shell-completion-setup-code
   "\12def __PYTHON_EL_get_completions(text):\12    completions = []\12    completer = None\12\12    try:\12        import gnureadline as readline\12\12        try:\12            import __builtin__\12        except ImportError:\12            # Python 3\12            import builtins as __builtin__\12        builtins = dir(__builtin__)\12\12        is_ipython = ('__IPYTHON__' in builtins or\12                      '__IPYTHON__active' in builtins)\12        splits = text.split()\12        is_module = splits and splits[0] in ('from', 'import')\12\12        if is_ipython and is_module:\12            from IPython.core.completerlib import module_completion\12            completions = module_completion(text.strip())\12        elif is_ipython and '__IP' in builtins:\12            completions = __IP.complete(text)\12        elif is_ipython and 'get_ipython' in builtins:\12            completions = get_ipython().Completer.all_completions(text)\12        else:\12            # Try to reuse current completer.\12            completer = readline.get_completer()\12            if not completer:\12                # importing rlcompleter sets the completer, use it as a\12                # last resort to avoid breaking customizations.\12                import rlcompleter\12                completer = readline.get_completer()\12            if getattr(completer, 'PYTHON_EL_WRAPPED', False):\12                completer.print_mode = False\12            i = 0\12            while True:\12                completion = completer(text, i)\12                if not completion:\12                    break\12                i += 1\12                completions.append(completion)\12    except:\12        pass\12    finally:\12        if getattr(completer, 'PYTHON_EL_WRAPPED', False):\12            completer.print_mode = True\12    return completions")
 '(python-shell-interpreter "/Users/jacky_goodluck/.virtualenvs/scrapingEnv/bin/python")
 '(python-shell-interpreter-args "-i")
 '(python-shell-interpreter-interactive-arg "-i")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-indentation-current-column-face ((t (:inherit nil :background "dark gray")))))
