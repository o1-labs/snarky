const child_process = require("child_process");

var shallow_copy = function(obj) {
  var copy = new Object();
  for (var i in obj) {
    copy[i] = obj[i];
  }
  return copy;
};

module.exports = exports = function (name) {
  var args = ["exec", name, "--"].concat(arguments);
  var process = child_process.spawn("dune", args,
      {"stdio": ["pipe", "pipe", "inherit"]});
  var communicate = function (data) {
    return new Promise (function (resolve, reject) {
      process.on("exit", function (code) {
        reject(new Error ("Process exited with code " + code + "."));
      });
      process.stdout.on("error", reject);
      process.stdout.on("data", resolve);
      process.stdin.write(data);
    });
  };
  var prove = function (query) {
    /* Copy the object instead of mutating. */
    var query = shallow_copy(query);
    query.command = "prove";
    var query_json = JSON.stringify(query);
    return communicate(query_json).then(function (response_json) {
        return new Promise (function (resolve, reject) {
          var response = JSON.parse(response_json);
          if (response.name == "error") {
            reject(new Error(response.message));
          } else if (response.name == "proof") {
            resolve(response.proof);
          } else {
            reject(new Error(
              "Expected name 'proof', but received '" + response.name + "' from process."
            ));
          }
        });
      }, function (err) { return err; });
  };
  var verify = function (query) {
    /* Copy the object instead of mutating. */
    var query = shallow_copy(query);
    query.command = "verify";
    var query_json = JSON.stringify(query);
    return communicate(query_json).then(function (response_json) {
        return new Promise (function (resolve, reject) {
          var response = JSON.parse(response_json);
          if (response.name == "error") {
            reject(new Error(response.message));
          } else if (response.name == "verified") {
            resolve(response.verified);
          } else {
            reject(new Error(
              "Expected name 'verify', but received '" + response.name + "' from process."
            ));
          }
        });
      }, function (err) { return err; });
  };
  return {"prove": prove, "verify": verify};
};
