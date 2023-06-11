# dp
**D**ynamic **P**arser with extensible PEG grammar. Reads as *deep*

# Transparency Rules
* Single unnamed pattern is passed as is:
  `X: x` ~ `X: <value: x> => value`
* Sequence without named arguments results in `{}`:
  `X: x y` ~ `X: x y => {}`
* Sequence with named arguments is wrapped with rule's name
  `X: <value: x> y` ~ `X: <value: x> y => X { value }`

# Rules overriding
You may override rules:
```
X: a
X: b

Y: X // Same as Y: b
```

## Root rule
Parser starts at `Root` rule.
If you want to extend parsers syntax, you must override it
```
>>> EmptyTuple: '(' ')' => "EmptyTuple"
>>> Root: Rule | EmptyTuple
// >>> Root: Root.clone() | EmptyTuple // TODO: copy rule
>>> ()
"EmptyTuple"

```

# Errors
Create error rules for better errors description. By convention, they should start with `Invalid`.
`@` means at current location,
`@lparen` means at location of `lparen`
```
Tuple: '(' ')' | InvalidTuple
InvalidTuple: <lparen: '('> =>
	throw ExpectedMatching {
		expected: ')',
		at: @,
		to_match: '(',
		unmatched_at: @lparen,
	}
```

# Debugging
To debug library itself call with env `RUST_LOG=debug` or `RUST_LOG=trace`

# Todo
* [X] Array constructors
* [ ] Remove `on_parse` function from rule
* [ ] Move logic to syntax
* [ ] Export/Import rules
* [ ] Add syntax highlighting
* [ ] Add documentation
* [ ] Simplify separated patterns
* [ ] Add `#[derive(Rule)]`
      * [ ] Add `#[rule]` attribute
	  * [ ] Add `Self::rule()` method
	  * [ ] Add `Self::parse(str: &str)` method
      * [ ] Implement deserialize
	  * [ ] On enums every variant is alternative
* [ ] Comments
* [ ] Simplify rules