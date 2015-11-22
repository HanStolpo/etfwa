require('./main.css');

var settings = require("./settings");
if (settings.app === 'RoutesExample')
{
    var app = require('./RoutesExample.purs');
    app.runApp();
}
else if (settings.app === 'EchoOnly')
{
    var app = require('./EchoOnly.purs');
    app.runApp();
}
else
{
  console.error("blah");
}
