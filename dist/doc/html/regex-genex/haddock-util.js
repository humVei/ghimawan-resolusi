
// Haddock JavaScript utilities

var rspace = /\s\s+/g,
	  rtrim = /^\s+|\s+$/g;

function spaced(s) { return (" " + s + " ").replace(rspace, " "); }
function trim(s)   { return s.replace(rtrim, ""); }

function hasClass(elem, value) {
  var className = spaced(elem.className || "");
  return className.indexOf( " " + value + " " ) >= 0;
}

function addClass(elem, value) {
  var className = spaced(elem.className || "");
  if ( className.indexOf( " " + value + " " ) < 0 ) {
    elem.className = trim(className + " " + value);
  }
}

function removeClass(elem, value) {
  var className = spaced(elem.className || "");
  className = className.replace(" " + value + " ", " ");
  elem.className = trim(className);
}

function toggleClass(elem, valueOn, valueOff, bool) {
  if (bool == null) { bool = ! hasClass(elem, valueOn); }
  if (bool) {
    removeClass(elem, valueOff);
    addClass(elem, valueOn);
  }
  else {
    removeClass(elem, valueOn);
    addClass(elem, valueOff);
  }
  return bool;
}