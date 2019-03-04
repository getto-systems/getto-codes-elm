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

  var Store = (function(){
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
        key: key,

        // returns: obj
        load: function(){
          return toValue(getItem());
        },

        // value: obj
        store: function(value){
          update(value);
        },
      };
    };
  })();

  var StoreGC = (function(){
    return function(store,limit){
      var load = function(){
        var list = store.load();
        if (list === null) {
          return [];
        }
        return list;
      };

      var unshiftCurrentPath = function(list,current_path){
        if (list[0] !== current_path) {
          list = list.filter(function(path){ return path !== current_path; });
          list.unshift(current_path);
        }
        return list;
      };

      var removeOlds = function(list){
        list.slice(limit).forEach(function(path){
          localStorage.removeItem(path);
        });
      };

      var update = function(list){
        store.store(list.slice(0,limit));
      };

      return {
        clear: function(current_path){
          var list = unshiftCurrentPath(load(),current_path);
          removeOlds(list);
          update(list);
        },
      };
    };
  })();

  var GCStore     = Store("_gc_list");
  var LayoutStore = Store("_layout");
  var AppStore    = Store(config.path);

  StoreGC(GCStore,10).clear(AppStore.key);

  var Dom = (function(){
    return {
      fill: function(values){
        setTimeout(function(){
          values.forEach(function(obj){
            var element = document.getElementById(obj.id);
            if (element) {
              element.value = obj.value;
            }
          });
        },100);
      },
    };
  })();

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
              layout: LayoutStore.load(),
              app:    AppStore.load(),
            },
          },
        });
    };

    var setupPorts = function(ports){
      /*** Auth ***/

      Auth.setUpdateCredentialInterval(function(credential){
        ports.send("onCredentialChanged",credential);
      });

      ports.subscribe("clearCredential", function(_params){
        Auth.logout();
      });

      /*** Store ***/

      ports.subscribe("storeLayout", function(value) {
        LayoutStore.store(value);
      });

      ports.subscribe("storeApp", function(value) {
        AppStore.store(value);
      });

      /*** Dom ***/

      ports.subscribe("fillFieldValues", function(values) {
        Dom.fill(values);
      });

      /*** FixedMidashi ***/

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
