# goldendict-ng: Search GoldenDict from within Emacs

`goldendict-ng` is a very simple Emacs package that lets the user perform a search of GoldenDict dictionaries from within Emacs. The package makes use of the command-line facilities provided by [goldendict-ng](https://github.com/xiaoyifang/goldendict-ng), a GoldenDict fork.

## Requirements

The package requires `goldendict-ng`. [Install it](https://xiaoyifang.github.io/goldendict-ng/install/) if you haven't done so.

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
  (setq goldendict-ng-groups '("EN" "ES"))
  (setq goldendict-ng-groups-prompt t)
  (setq goldendict-ng-groups-enforce nil)
  (setq goldendict-ng-disable-tts nil)
  (setq goldendict-ng-initial-input-use-active-region t)
  (setq goldendict-ng-initial-input-use-thing-at-point t)
  
  :general
  ("A-g" 'goldendict-ng-search))
```

(Note that most of the above user options are set to their default values. They are included for the sake of illustration.)

## Usage

`M-x goldendict-ng-search`.
