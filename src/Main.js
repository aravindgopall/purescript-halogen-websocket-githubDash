// const {Client} = require("pg")
// const client = new Client()

client.connect()

exports["logMe"]= function(val){ console.log(val); return val;}

exports["jsonParse"] = function(val) {console.log(val); return parse(val);}

exports["pushEvent"]= function(msg){
    return function(){
        console.log(msg);
        return msg;
    }
}

exports["getAllEvents"] = function(date){
    return function(sc){
        // client.query('Select Events from EventTable where :
        return "";
    }
}

function parse(json) {
    return json;
}
