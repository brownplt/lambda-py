# see "A Monotonic Superclass Linearization for Dylan",
# by Kim Barrett et al. (OOPSLA 1996)
# Testing consistency with EPG...
class Pane(object): pass
class ScrollingMixin(object): pass
class EditingMixin(object): pass
class ScrollablePane(Pane,ScrollingMixin): pass
class EditablePane(Pane,EditingMixin): pass
class EditableScrollablePane(ScrollablePane,EditablePane): pass

___assertEqual(EditableScrollablePane.__mro__,
      (EditableScrollablePane, ScrollablePane, EditablePane, Pane,
        ScrollingMixin, EditingMixin, object))
