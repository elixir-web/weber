function RootController($scope, $http) {

	$scope.todos = [];
	
	$scope.add_todo = function(todo){
		$http.post('/add/' + todo, []).success(function (data, status, headers, config){
			if (data.response == "ok"){
				$scope.todos.push(todo);
				todo = '';
			}
		})
	}
}