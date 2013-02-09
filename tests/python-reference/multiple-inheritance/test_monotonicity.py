# see "A Monotonic Superclass Linearization for Dylan",
# by Kim Barrett et al. (OOPSLA 1996)
# Testing MRO monotonicity...
class Boat(object): pass
class DayBoat(Boat): pass
class WheelBoat(Boat): pass
class EngineLess(DayBoat): pass
class SmallMultihull(DayBoat): pass
class PedalWheelBoat(EngineLess,WheelBoat): pass
class SmallCatamaran(SmallMultihull): pass
class Pedalo(PedalWheelBoat,SmallCatamaran): pass

___assertEqual(PedalWheelBoat.__mro__,
      (PedalWheelBoat, EngineLess, DayBoat, WheelBoat, Boat, object))
___assertEqual(SmallCatamaran.__mro__,
      (SmallCatamaran, SmallMultihull, DayBoat, Boat, object))
___assertEqual(Pedalo.__mro__,
      (Pedalo, PedalWheelBoat, EngineLess, SmallCatamaran,
       SmallMultihull, DayBoat, WheelBoat, Boat, object))
