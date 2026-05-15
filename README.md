# css-parser

This is a modern CSS parser and printer written in pure Haskell with Alex/Happy stack.
CSS libraries test set includes: bootstrap, carbon, patternfly, uikit, primer, uswds, etc.

css-parser is based on [css-selectors](https://hackage.haskell.org/package/css-selectors).

The work is started to provide info about style sheets structure to
[miso-css](https://github.com/yaitskov/miso-css) library.

## Dev

``` shell
$ nix develop
$ emacs src/CssParser.hs &
$ cabal test
$ cabal repl
ghci> import CssParser
ghci> parseCss "p {x: 1px;}"
CssFile {rules = [CssRule (Selector Nothing (TagSelector {tagNs = NoBar, tagName = TagName "p", tagSubSelectors = []}) [] :| []) [CssLeafRule (KnownDescriptor XT) (PropVals (IntVal (TypedNum "1" Px) :| []) Nothing)]]}
```

``` shell
nix build
./result file.css
./result < file.css
```

### Integration tests

``` shell
cd itest
nix develop
intest
```

Links to successfully parsed CSS files are stored in `.css-hashes`
folder to exclude them from consequent `intest` reruns.
