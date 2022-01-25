
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


function makeClassToggle(valueOn, valueOff)
{
  return function(elem, bool) {
    return toggleClass(elem, valueOn, valueOff, bool);
  }
}

toggleShow = makeClassToggle("show", "hide");
toggleCollapser = makeClassToggle("collapser", "expander");

function toggleSection(id)
{
  var b = toggleShow(document.getElementById("section." + id));
  toggleCollapser(document.getElementById("control." + id), b);
  rememberCollapsed(id, b);
  return b;
}

var collapsed = {};
function rememberCollapsed(id, b)
{
  if(b)
    delete collapsed[id]
  else
    collapsed[id] = null;

  var sections = [];
  for(var i in collapsed)
  {
    if(collapsed.hasOwnProperty(i))
      sections.push(i);
  }
  // cookie specific to this page; don't use setCookie which sets path=/
  document.cookie = "collapsed=" + escape(sections.join('+'));
}

function restoreCollapsed()
{
  var cookie = getCookie("collapsed");
  if(!cookie)
    return;

  var ids = cookie.split('+');
  for(var i in ids)
  {
    if(document.getElementById("section." + ids[i]))
      toggleSection(ids[i]);
  }
}

function setCookie(name, value) {
  document.cookie = name + "=" + escape(value) + ";path=/;";
}

function clearCookie(name) {
  document.cookie = name + "=;path=/;expires=Thu, 01-Jan-1970 00:00:01 GMT;";
}

function getCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) {
      return unescape(c.substring(nameEQ.length,c.length));
    }
  }
  return null;
}



var max_results = 75; // 50 is not enough to search for map in the base libraries
var shown_range = null;
var last_search = null;

function quick_search()
{
    perform_search(false);
}

function full_search()
{
    perform_search(true);
}


function perform_search(full)
{
    var text = document.getElementById("searchbox").value.toLowerCase();
    if (text == last_search && !full) return;
    last_search = text;
    
    var table = document.getElementById("indexlist");
    var status = document.getElementById("searchmsg");
    var children = table.firstChild.childNodes;
    
    // first figure out the first node with the prefix
    var first = bisect(-1);
    var last = (first == -1 ? -1 : bisect(1));

    if (first == -1)
    {
        table.className = "";
        status.innerHTML = "No results found, displaying all";
    }
    else if (first == 0 && last == children.length - 1)
    {
        table.className = "";
        status.innerHTML = "";
    }
    else if (last - first >= max_results && !full)
    {
        table.className = "";
        status.innerHTML = "More than " + max_results + ", press Search to display";
    }
    else
    {
        // decide what you need to clear/show
        if (shown_range)
            setclass(shown_range[0], shown_range[1], "indexrow");
        setclass(first, last, "indexshow");
        shown_range = [first, last];
        table.className = "indexsearch";
        status.innerHTML = "";
    }