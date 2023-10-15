# goldendict-ng: Search GoldenDict from within Emacs

`goldendict-ng` supports searching GoldenDict dictionaries from within Emacs. The package makes use of the command-line facilities provided by [goldendict-ng](https://github.com/xiaoyifang/goldendict-ng), a GoldenDict fork.

## Requirements

As noted, the package requires `goldendict-ng`. [Install it](https://xiaoyifang.github.io/goldendict-ng/install/) if you haven't done so.

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

``` emacs-lisp
(add-to-list 'load-path "path/to/goldendict-ng")
```

where `"path/to/goldendict-ng"` is the path to the local repository you just cloned.

### Elpaca

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, you just need to add this your `init.el` file:

``` emacs-lisp
(use-package goldendict-ng
  :elpaca (goldendict-ng
           :host github
	   :repo "benthamite/goldendict-ng")
  :demand t)
```

## Configuration

Here is a sample configuration:

``` emacs-lisp
(use-package goldendict-ng
  :elpaca (goldendict-ng
	   :host github
	   :repo "benthamite/goldendict-ng")
  :demand t
  :config
  (setq goldendict-ng-executable "/Applications/GoldenDict.app/Contents/MacOS/GoldenDict")
  (setq goldendict-ng-groups
        '(("English dictionaries" . "en")
          ("English encyclopedias" . "en")
          ("French dictionaries" . "fr")
          ("French encyclopedias" . "fr"))
	
  ;; all user options below are set to their default values, for illustration purposes
  (setq goldendict-ng-groups-enforce nil)
  (setq goldendict-ng-narrow-groups-to-matching-langs nil)
  (setq goldendict-ng-show-all-group t)
  (setq goldendict-ng-use-active-region 'initial-input)
  (setq goldendict-ng-use-word-at-point 'initial-input)
  (setq goldendict-ng-main-window nil)
  (setq goldendict-ng-scanpopup nil)
  (setq goldendict-ng-reset-window-state nil)
  (setq goldendict-ng-disable-tts nil)
  
  :general
  ("A-y" 'goldendict-ng-search))
```

## Usage

`M-x goldendict-ng-search`.
