

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

exports["getEventData"]= function(year){
    return function(firstIndex){
        return function(month){
            var emptyArray = Array(firstIndex).fill(' ');
            return emptyArray.concat([""]);
        }
    }
}


function parse(json) {
    return json;
}
