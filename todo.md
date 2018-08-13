
- could do something like the workflow list for bootstrap rows
- could add a config type with a default value for the workflow list steps
  - the ctx, pre and post would have defaults
- abstracting the bits and pieces around the workflow list would be good

- probably want error events to come out of the widgets
  - or use Validation for that path
  - especially if errors change to have ids coupled with them
    - then we can do some interesting things on workflow pages etc...

- radio button validation needs some special handling with classes etc...

- work out how to ask for validation of forms, and how to work with the information that returns
  - probably work on fallback validation for things like dates and times, package these up
  - this is coming together nicely

- we could add ids to the error messages, and then move from Identity to Validation (NonEmpty e)
  - would probably want to make id propagation a bit harder to get wrong

- better layout options would be good
