function SimpleChatLoginController ($scope, $http, $location) {

    $scope.join = function(login) {
        //
        // Send http request to the /join/username
        //
        $http.post('/join/' + login, []).success(function(data, status, headers, config) {
        	sessionStorage.setItem('login', login);
            window.location.replace("/chat/" + data.chat);
        })
    }
}

function SimpleChatRootController ($scope) {
	var webSocket = new WebSocket('ws://localhost:8800');

    $scope.safeApply = function(fn) {
      var phase = this.$root.$$phase;
      if(phase == '$apply' || phase == '$digest') {
        if(fn && (typeof(fn) === 'function')) {
          fn();
        }
      } else {
        this.$apply(fn);
      }
    };

	webSocket.onopen = function(event) {
    
    };
 
    webSocket.onmessage = function(event) {
        resp = JSON.parse(event.data)
        if ($scope.chat == undefined)
            $scope.chat = '';
        $scope.safeApply(function() {$scope.chat = $scope.chat + resp.id + ':' + resp.message + '\n'})
    };
 
    webSocket.onclose = function(event) {
    	window.location.replace("/");
    };

    $scope.send_message = function(message) {
        var username = sessionStorage.getItem('login');
        webSocket.send(JSON.stringify({'id': username, 'message' : message}));
    }
}