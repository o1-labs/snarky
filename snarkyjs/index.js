const child_process = require("child_process");

const processObject = (obj) => {
  if (typeof obj === 'number') {
    return obj;
  }
  if (typeof obj === 'string') {
    return obj;
  }
  if (typeof obj === 'bigint') {
    return obj.toString();
  }
  if (typeof obj === 'boolean') {
    return obj;
  }
  if (Array.isArray(obj)) {
    return obj.map(processObject);
  }

  const copy = new Object();
  for (var i in obj) {
    copy[i] = processObject(obj[i]);
  }
  return copy;
};

module.exports = exports = function (name) {
  var args = ["exec", "--display=quiet", name, "--"].concat(arguments);
  var snarky_process = child_process.spawn("dune", args,
      {"stdio": ["pipe", "pipe", "inherit"]});
  var communicate = function (data) {
    return new Promise (function (resolve, reject) {
      snarky_process.on("exit", function (code) {
        reject(new Error ("Process exited with code " + code + "."));
      });
      snarky_process.stdout.on("error", reject);
      snarky_process.stdout.on("data", resolve);
      snarky_process.stdin.write(data);
    });
  };
  var prove = function (query) {
    /* Copy the object instead of mutating. */
    var query = processObject(query);
    query.command = "prove";
    var query_json = JSON.stringify(query);
    return communicate(query_json).then(function (response_json) {
        return new Promise (function (resolve, reject) {
          var response = JSON.parse(response_json);
          if (response.name == "error") {
            reject(new Error(response.message + "\n\nBacktrace:\n" + response.backtrace));
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
    var query = processObject(query);
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
  return {
    prove: prove,
    verify: verify,
    kill: function() { snarky_process.kill('SIGINT'); }
  };
};
