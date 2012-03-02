// default search
define_webjump("duck", "http://duckduckgo.com/?q=%s");

// wolfram-alpha
define_webjump("wolfram", "http://www.wolframalpha.com/input/?i=%s");

// the googles
define_webjump("google-scholar", "http://scholar.google.com/scholar?q=%s");
define_webjump("google-books", "http://www.google.com/search?q=%s&tbm=bks");

// amazon
define_webjump("amazon",
               "http://www.amazon.com/exec/obidos/external-search/" +
               "?field-keywords=%s&mode=blended");

// netflix
define_webjump("netflix", "http://www.netflix.com/Search?v1=%s");
