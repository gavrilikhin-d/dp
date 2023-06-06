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

# Todo
* [ ] Remove parse tree
* [ ] Array constructors
* [ ] Add a way to obtain syntax information from variable name
* [ ] Store source code in context
* [ ] Remove `on_parse` function from rule
* [ ] Move logic to syntax
* [ ] Export/Import rules
* [ ] Add syntax highlighting
* [ ] Support left-recursion
* [ ] Packrat parsing
* [ ] Add documentation
* [ ] Simplify separated patterns
* [ ] Add `#[derive(Rule)]`
      * [ ] Add `#[rule]` attribute
	  * [ ] Add `Self::rule()` method
	  * [ ] Add `Self::parse(str: &str)` method
      * [ ] Implement deserialize
* [ ] Comments