#!/usr/bin/env node
"use strict";

const commander = require("commander");

const stat = require("getto-code-stat-elm");

commander.version(require("../package.json").version)
  .option("-d, --dir [dir]", "The root of source directory")
  .option("-p, --package-dir [packageDir]", "The root of elm package directory")
  .option("-e, --encode [encode]", "The encode of elm file. Defaults to utf8", "utf8")
  .option("-c, --elm-json [elmJson]", "The path to elm.json. Defaults to ./elm.json", "./elm.json")
  .parse(process.argv);

const fluidity = {
  "Version": 1,
  "Env":     2,

  "View": 3,
  "Href": 3,

  "Command": 4,

  "Html": 5,
  "I18n": 6,
};

const data = stat.dump({
  dir: commander.dir,
  packageDir: commander.packageDir,
  encode: commander.encode,
  elm: commander.elmJson,
  fluidity: (module,entry) => {
    const tips = module.split(".");
    if (tips[0] !== "GettoCodes") {
      return 0;
    } else {
      const module = tips[1];
      if (module === "App" || module === "Layout") {
        const bottom = tips.pop();
        if (fluidity[bottom] !== null) {
          return fluidity[bottom];
        } else {
          return fluidity.I18n;
        }
      } else {
        return fluidity[module];
      }
    }
  },
});

const maximum = (key) => {
  let max = 0;
  Object.keys(data.counts).forEach((module) => {
    const count = data.counts[module][key];
    if (count && count > max) {
      max = count;
    }
  });
  return max;
};

const max = {
  ossification: maximum("ossification"),
  instability: maximum("instability"),
};

const score = (ignores) => {
  let summary = {};

  Object.keys(data.counts).forEach((module) => {
    if (ignores.indexOf(module) === -1) {
      const rate = (key) => {
        const count = data.counts[module][key];
        if (!count) {
          return 0;
        }
        return count / max[key];
      };

      const score = rate("ossification") + rate("instability");
      if (score > 1) {
        summary[module] = score;
      }
    }
  });

  return summary;
};

const over = (key) => {
  let summary = {};

  Object.keys(data.modules.projects).forEach((module) => {
    const count = data.counts[module][key];
    const entry = data.modules.projects[module];

    let overs = [];
    Object.keys(entry.imports).forEach((imported) => {
      if (data.counts[imported][key] > count) {
        overs.push(imported);
      }
    });
    if (overs.length > 0) {
      summary[module] = overs;
    }
  });

  return summary;
};

let hasError = false;

const dump = (title,summary) => {
  const keys = Object.keys(summary);
  if (keys.length == 0) {
    console.log(title + " OK");
  } else {
    hasError = true;

    console.log(title + " fault");
    keys.forEach((module) => {
      console.log(module + " : " + summary[module]);
    });
  }
};


dump("score", score(["Basics"]));
dump("fluidity", over("fluidity"));
dump("instability", over("instability"));

if (hasError) {
  throw "there are error";
}
