console.log("Server started");
var Msg = '';
var WebSocketServer = require('ws').Server
    , wss = new WebSocketServer({port: 8010})
    , express = require('express')
    , wsa 
    , app = express();

wss.on('connection', function(ws) {
        app.get('/',function(req,res){
           ws.send('Server send1');
           res.send("DONE");
         });

         app.listen(8083, function(){
            console.log("listening on 8083");
         });

        ws.on('message', function(message) {
            console.log('Received from client: %s', message);
            ws.send('Server received from client: ' + message);
        });
});
