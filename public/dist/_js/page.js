"use strict";

/*
 * // _config/path/to/page.js
 * window.config = {
 *   path: "path/to/page.html",
 *   page: "Path.To.Page",
 * }
 */

try {
  var Error = (function(){
    var node = document.getElementById("error").cloneNode(true);
    node.classList.remove("display-none");

    return {
      show: function(){
        document.body.replaceWith(node);
      },
    };
  })();

  var Detect = {
    init: function(){
      GettoDetect({
        version_to_path: function(version){
          return "/"+version+"/"+config.path+location.search;
        }
      }).from_current_version(version,function(path) {
        location.href = path;
      });
    },
  };

  var Auth = (function(){
    var keycloak = Keycloak({
      url: document.getElementById("keycloak").src.replace("/js/keycloak.js",""),
      realm: "getto",
      clientId: "upload",
    });

    var updateToken = {
      interval: 2 * 60 * 1000,
      scope: 2 * 60 + 30,
    };

    return {
      init: function(callback){
        keycloak.init({
          onLoad: "login-required",
          checkLoginIframe: false,
        })
          .success(function(){
            callback(keycloak.token);
          })
          .error(Error.show);
      },

      setUpdateTokenInterval: function(callback){
        var update = function(){
          keycloak.updateToken(updateToken.scope)
            .success(function(refreshed) {
              if (refreshed) {
                callback(keycloak.token);
              }
            })
            .error(function() {
              keycloak.clearToken();
            });
        };

        setTimeout(update,0);
        setInterval(update, updateToken.interval);
      },

      logout: function(){
        keycloak.logout();
      },
    };
  })();

  var Storage = (function(){
    var key = "app";

    return {
      load: function(){
        return localStorage.getItem(key);
      },

      addChangedListener: function(callback){
        window.addEventListener("storage", function(event) {
          if (event.storageArea === localStorage && event.key === key) {
            callback(event.newValue);
          }
        }, false);
      },

      store: function(value){
        if (value === null) {
          localStorage.removeItem(key);
        } else {
          localStorage.setItem(key, JSON.stringify(value));
        }
      },
    };
  })();

  Auth.init(function(token){
    try {
      var modules = config.page.split(".");

      var entry_point = modules.reduce(function(acc,m){return acc[m];},Elm.GettoUpload.App).EntryPoint;

      var app = entry_point.init({
        flags: {
          token: token,
          storage: Storage.load(),
          loadAt: (new Date()).toISOString(),
          project: {
            name:     document.getElementById("project").innerText,
            company:  document.getElementById("company").innerText,
            title:    document.getElementById("title").innerText,
            subTitle: document.getElementById("sub-title").innerText,
          },
        },
      });

      var subscribe = function(name,func) {
        if (app.ports && app.ports[name]) {
          app.ports[name].subscribe(func);
        }
      };

      var send = function(name,data) {
        if (app.ports && app.ports[name]) {
          app.ports[name].send(data);
        }
      };

      subscribe("detectNewVersion", function(_params){
        Detect.init();
      });

      Auth.setUpdateTokenInterval(function(token){
        send("onTokenChanged",token);
      });

      subscribe("logout", function(_params){
        Auth.logout();
      });

      var onStorageChanged = function(value) {
        send("onStorageChanged",value);
      };

      subscribe("store", function(value) {
        Storage.store(value);
        setTimeout(function(){
          onStorageChanged(value);
        }, 0);
      });

      Storage.addChangedListener(function(value){
        onStorageChanged(value);
      });

      subscribe("fixedMidashi", function(_params) {
        setTimeout(function(){
          FixedMidashi.create();
        },300);
      });

    } catch(e) {
      Error.show();
      throw e;
    }
  });
} catch(e) {
  Error.show();
  throw e;
}
