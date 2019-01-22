"use strict";

/*
 * // _config/path/to/page.js
 * window.config = {
 *   path: "path/to/page.html",
 *   page: "Path.To.Page",
 * }
 */

try {
  var DisplayError = (function(){
    // clone "error" node at head of script
    // because all body nodes remove in Elm.init process
    var node = document.getElementById("error").cloneNode(true);
    node.classList.remove("display-none");

    var showError = function(){
      document.body.replaceWith(node);
    };

    return {
      show: function(){
        showError();
      },
    };
  })();

  var Auth = (function(){
    var keycloak = Keycloak({
      url: document.getElementById("keycloak").src.replace("/js/keycloak.js",""),
      realm:    "getto",
      clientId: "upload",
    });

    var updateTokenConfig = {
      interval: 2 * 60 * 1000,
      scope:    2 * 60 + 30,
    };

    var init = function(callback){
      keycloak.init({
        onLoad: "login-required",
        checkLoginIframe: false,
      })
        .success(function(){
          callback(credential());
        })
        .error(DisplayError.show);
    };

    var updateToken = function(callback){
      keycloak.updateToken(updateTokenConfig.scope)
        .success(function(refreshed) {
          if (refreshed) {
            callback(credential());
          }
        })
        .error(function() {
          keycloak.clearToken();
        });
    };

    var credential = function(){
      var roles = function(){
        var resource = keycloak.tokenParsed.resource_access[keycloak.clientId];
        if (resource !== null && resource.roles !== null) {
          return resource.roles;
        } else {
          return [];
        }
      };

      return {
        token: keycloak.token,
        roles: roles(),
      };
    };

    var logout = function(){
      keycloak.logout();
    };

    return {
      /**
       * callback: function(credential){
       *   credential // { token: "access token", roles: ["role"] }
       * }
       */
      init: function(callback){
        init(callback);
      },

      /**
       * callback: function(credential){
       *   credential // { token: "access token", roles: ["role"] }
       * }
       */
      setUpdateCredentialInterval: function(callback){
        setTimeout(function(){ updateToken(callback); }, 0);
        setInterval(function(){ updateToken(callback); }, updateTokenConfig.interval);
      },

      logout: function(){
        logout();
      },
    };
  })();

  var Storage = (function(config){
    var current_path = config.path;
    return function(key){
      var getItem = function(){
        return localStorage.getItem(key);
      };

      var toValue = function(value){
        try {
          return JSON.parse(value);
        } catch(e) {
          return null;
        }
      };

      var update = function(value){
        try {
          localStorage.setItem(key, JSON.stringify(value));
        } catch(e) {
          localStorage.removeItem(key);
        }
      };

      return {
        // returns: obj
        load: function(){
          return toValue(getItem());
        },

        /**
         * callback: function(value){
         *   value // obj
         * }
         */
        addChangedListener: function(callback){
          window.addEventListener("storage", function(event) {
            if (event.storageArea === localStorage && event.key === key) {
              callback(toValue(event.newValue));
            }
          }, false);
        },

        // value: obj
        store: function(value){
          update(value);
        },
      };
    };
  })(config);

  var LayoutStorage = Storage("_layout");
  var AppStorage = Storage(config.path);

  var App = (function(config){
    var current_page = config.page;

    var init = function(credential){
      return current_page.split(".")
        .reduce(function(acc,m){return acc[m];},Elm.GettoUpload.App).Page
        .init({
          flags: {
            'static': {
              project: {
                name:    document.getElementById("project").innerText,
                company: document.getElementById("company").innerText,
                title:   document.getElementById("title").innerText,
                sub:     document.getElementById("sub-title").innerText,
              },
              page: {
                path: config.path,
              },
            },
            credential: credential,
            store: {
              layout: LayoutStorage.load(),
              app:    AppStorage.load(),
            },
          },
        });
    };

    var setupPorts = function(ports){
      var onTokenChanged = function(credential){
        // credential: { token: "access token", roles: ["role"] }
        ports.send("onTokenChanged",credential);
      };

      var onLayoutStorageChanged = function(value) {
        // value: obj
        ports.send("onLayoutStorageChanged",value);
      };

      var onAppStorageChanged = function(value) {
        // value: obj
        ports.send("onAppStorageChanged",value);
      };

      Auth.setUpdateCredentialInterval(function(credential){
        onTokenChanged(credential);
      });

      ports.subscribe("logout", function(_params){
        Auth.logout();
      });

      // value: obj
      ports.subscribe("storeLayout", function(value) {
        LayoutStorage.store(value);
      });

      LayoutStorage.addChangedListener(function(value){
        onLayoutStorageChanged(value);
      });

      // value: obj
      ports.subscribe("storeApp", function(value) {
        AppStorage.store(value);
      });

      AppStorage.addChangedListener(function(value){
        onAppStorageChanged(value);
      });

      ports.subscribe("fixedMidashi", function(_params) {
        setTimeout(function(){
          FixedMidashi.create();
        },300);
      });
    };

    var ports = function(app){
      return {
        subscribe: function(name,func) {
          if (app.ports && app.ports[name]) {
            app.ports[name].subscribe(func);
          }
        },

        send: function(name,data) {
          if (app.ports && app.ports[name]) {
            app.ports[name].send(data);
          }
        },
      };
    };

    return {
      init: function(){
        Auth.init(function(credential){
          try {
            setupPorts(ports(init(credential)));
          } catch(e) {
            DisplayError.show();
            throw e;
          }
        });
      },
    };
  })(config);

  // main entry point
  App.init();

} catch(e) {
  DisplayError.show();
  throw e;
}
