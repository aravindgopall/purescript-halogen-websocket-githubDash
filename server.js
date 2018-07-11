console.log("Server started");
var Msg = '';
var WebSocketServer = require('ws').Server
    , wss = new WebSocketServer({port: 8010})
    , express = require('express')
    , wsa 
    , app = express();

var pg  = require("pg");
var pool = new pg.Pool();
var connectionString = "postgres://aravind.mallapureddy@localhost:5432/template1";
var config = {
      user: 'postgres',
      database: 'booktown', 
      password: 'admin', 
      port: 5432, 
      max: 10, // max number of connection can be open to database
      idleTimeoutMillis: 30000, // how long a client is allowed to remain idle before being closed
};


// wss.on('connection', function(ws) {
        app.get('/', function (req, res, next) {
                pool.connect(function(err,client,done) {
                           if(err){
                                          console.log("not able to get connection "+ err);
                                          res.status(400).send(err);
                                      } 
                           client.query('SHOW search_path',function(err,result) {
                                          done(); // closing the connection;
                                          if(err){
                                                             console.log(err);
                                                             res.status(400).send(err);
                                                         }
                                          console.log(result);
                                          res.status(200).send(result);
                                      });
                        });
        });
         app.listen(8083, function(){
            console.log("listening on 8083");
         });

        // ws.on('message', function(message) {
        //     console.log('Received from client: %s', message);
        //     ws.send('Server received from client: ' + message);
        // });
// });
