# comapny-symbol-after-symbol

WIP: Simple-minded omni completion for company

![screencast](./screencast.gif)

``` emacs-lisp
(require 'company-symbol-after-symbol)
(push 'company-symbol-after-symbol company-backends)
```

## implementation
### BOL 2-gram completion

example (consider "|" as the cursor):

```javascript
FooComponent.propTypes = {
  foo: PropTypes.bool.isRequired,
  bar: PropTypes.func,
  baz: PropTypes.arrayOf(PropTypes.number)
}

...

BarComponent.propTypes = {
  foo: |
}
```

Here we have `foo: `, which is the first word in the line, before the
cursor. Then `company-symbol-after-symbol` searches for `^\\W*foo: `
through the buffer, and finds the following snippet.

```javascript
  foo: PropTypes
```

So `company-symbol-after-symbol` completes `PropTypes` here.

```javascript
BarComponent.propTypes = {
  foo: PropTypes|
}
```

### 3-gram completion

Now we have two words before the cursor.

```javascript
BarComponent.propTypes = {
  foo: PropTypes.|
}
```

Then `company-symbol-after-symbol` searches for `foo: PropTypes.`
through the buffer and finds the following snippet.

```javascript
foo: PropTypes.bool
```

So `company-symbol-after-symbol` completes `bool` here.

```javascript
BarComponent.propTypes = {
  foo: PropTypes.bool|
}
```

### 2-gram completion

Consider the following situation:

```javascript
BarComponent.propTypes = {
  foo: PropTypes.bool,
  qux: PropTypes.|
}
```

Here `company-symbol-after-symbol` first searches for `qux:
PropTypes.`, which finds nothing. Then `company-symbol-after-symbol`
falls back to "2-gram" completion. `company-symbol-after-symbol`
shorten the query to `PropTypes.` and finds following snippets.

```javascript
PropTypes.bool
PropTypes.func
PropTypes.arrayOf
```

So `company-symbol-after-symbol` shows three completion candidates
(`bool`, `func`, `arrayOf`) and completes the selected one.

Note that this fallbac behavior may give less accurate candidates than
other two methods. So this can be disabled by setting
`company-symbol-after-symbol-fallback-to-2gram` to `nil`.

### symbols from other buffers

`company-symbol-after-symbol` can also complete symbols from other
buffers in the same major-mode.

Internally, `company-symbol-after-symbol` caches symbols in inactive
buffers into an efficient data structure called radix-tree, so that
`company-symbol-after-symbol` can find completion candidates
efficiently.
