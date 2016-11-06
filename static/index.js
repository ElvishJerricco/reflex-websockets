var socket = new WebSocket("ws://localhost:8080/socket");
socket.onopen = function() {
    socket.send("test");
};
socket.onmessage = function(event) {
    console.log(event.data);
};
