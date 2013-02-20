# uk-gridref

A Clojure library to convert between UK Ordinance Survey Grid references
and Latitude Longitude

Heavily based on the [javascript by Chris Veness](http://www.movable-type.co.uk/scripts/latlong-gridref.html) 

## Usage

```clojure
(use 'uk-gridref.core)
(e-n-to-wgs84 (gridref-to-e-n "TG5140913177"))
```

## License

Copyright © 2013 Michael Bauer

Distributed under the 2-Clause BSD License, see LICENSE for details
