
- add the ability for custom attributes for all of the bootstrap widgets
- complete HTML5 inputs

- the ability to separate out the workflow controls from the workflow forms
  - so we can have multiple dropdowns in a row for nested workflows

need to gather up headers and footers
we recursively use workflowWidget or fieldWidget, depending on if we have more steps to go

- if validation is required to move forward, we need to validate everything between the current index and the new index, and stop at the first one that fails to validate
  - either that or we can't use dropdowns for control like that

- check the ability to store lists of Foo (Last a) so that we can look at the history of changes over time
  - possibly have a save button, or link this to the validation button / steps?
  - possibly only on a save button or a workflow change?

- look at adding setValue to Select so we can work with reflex-dom-storage
  - we probably need dropdownView for this to work
  - or just write our own version of dropdown

- would be good to have validation information displayed across tabs when one of them does something with invalid data












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







What do we want here?

- a widget for displaying and editing an `f Maybe`, via something like `Dynamic t (f Maybe) -> Dynamic t (NonEmpty (WithId e)) -> m (Event t (Endo (f Maybe)))`
  - a continuation based way of composing that out of its subparts with lenses, if this can drop layered ids into place along the way, great
  - do we punt on the continuation based strategy and just use maps of components, and maybe wrappers to alter display?

- the ability to go from `f Maybe` to `f (Validation (NonEmpty (WithId e)))`, where the pieces are composable
- the ability to pull out an `Maybe (f Identity)` in the case where there is no error
- the ability to use a `NonEmpty (WithId e)` to display the errors in a form etc...
  - if we had Id a = Id a | Nest a (Id a), would that help us with filtering errors down to the right spots?
    - do we even need to add ids to the DOM in that case?  we might have enough structure to put the right thing in the right place and be done with it

- the ability to combine these pieces together (ideally using lenses and prisms)
- the ability to use this with collections
- the ability to use this with workflows
  - this means the ability to 
    - block / not block the "next" action if the current page doesn't validate 
    - allow you to save / not save the current page on the "back" action
    - do we want to generalize that so those choices are available regardless of the action?
  - we probably want arbitrary workflow support, which doesn't presuppose that we have "next" and "back" actions

What are the pieces that we will need for this?

- it would be nice to build up the tools to combine the `Id -> ctx -> f Maybe -> f (Validation (NonEmpty (WithId e)))` or `ctx -> f Maybe -> f (Validation (NonEmpty e))` bits on their own
- then our widgets can display their widgets, and on changes, can validate the sub parts and then the whole


- do we want a context, or do we want to generate contextual errors at the level where we have the context, and 
  possibly have mechanism to hint which subcomponent the context error should be attached to?

