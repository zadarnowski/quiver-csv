Quiver combinators for cellular CSV data processing
========================================

    Copyright © 2015 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

This library provides an efficient Quiver-based implementation
of a cellular CSV codec designed for fast streaming of data
with guaranteed constant memory usage.

The decoder accepts precisely the CSV syntax defined in RFC 4180,
with the following extensions:

* arbitrary characters, including ASCII control codes and non-ASCII code points
  are accepted anywhere in the input,
* CR and LF are accepted as row separators in addition to the standard CR+LF,
* rows can have varying number of fields,
* final row is not required to end with CR+LF, and
* within quoted field, a quote character that is not followed by another quote,
  comma or line break is accepted literally.
