# goldendict-ng: Search GoldenDict from within Emacs

`goldendict-ng` is a very simple Emacs package that lets the user perform a search of GoldenDict dictionaries from within Emacs. The package makes use of the command-line facilities provided by [goldendict-ng](https://github.com/xiaoyifang/goldendict-ng), a GoldenDict fork.

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

``` emacs-lisp
(add-to-list 'load-path "path/to/goldendict-ng"
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
  (setq goldendict-ng-group-prompt t)
  (setq goldendict-ng-groups '("EN" "ES"))
  (setq goldendict-ng-groups-enforce nil)
  (setq goldendict-ng-disable-tts nil)

  :general
  ("A-y" 'goldendict-ng-search))
```

## Usage

`M-x goldendict-ng-search`.
