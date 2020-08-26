// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_js_exceptions = require("bs-platform/lib/js/caml_js_exceptions.js");

var userInfo = {
  contents: [
    {
      factor: 1.0,
      fcn: (function (prim) {
          return Math.sin(prim);
        }),
      theta: 1.0,
      offset: 0.0
    },
    {
      factor: 1.0,
      fcn: (function (prim) {
          return Math.sin(prim);
        }),
      theta: 1.0,
      offset: 0.0
    }
  ]
};

function getInputValue(id, defaultValue, f) {
  var element = document.getElementById(id);
  if (element == null) {
    return {
            TAG: /* Error */1,
            _0: "No element" + id
          };
  } else {
    return Curry._1(f, element.value);
  }
}

function getNumericValue(id, defaultValue) {
  var element = document.getElementById(id);
  if (element == null) {
    return {
            TAG: /* Error */1,
            _0: "No element " + id
          };
  }
  var s = element.value;
  if (s === "") {
    return {
            TAG: /* Ok */0,
            _0: defaultValue
          };
  }
  var result;
  try {
    result = Caml_format.caml_float_of_string(s);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      return {
              TAG: /* Error */1,
              _0: s + " is not numeric."
            };
    }
    throw exn;
  }
  return {
          TAG: /* Ok */0,
          _0: result
        };
}

function getFormula(suffix) {
  getNumericValue("factor" + suffix, 1.0);
  getNumericValue("theta" + suffix, 1.0);
  getNumericValue("offset" + suffix, 0.0);
  return {
          TAG: /* Error */1,
          _0: "TBD"
        };
}

function draw(evt) {
  var testNumeric = getNumericValue("factor1", 1.0);
  console.log(testNumeric);
  
}

var optButton = document.getElementById("draw");

if (optButton == null) {
  window.alert("Cannot find button");
} else {
  optButton.addEventListener("click", draw);
}

var DOM;

var Doc;

var Elem;

var HtmlElem;

var InputElem;

var EvtTarget;

var optButton$1 = (optButton == null) ? undefined : Caml_option.some(optButton);

exports.DOM = DOM;
exports.Doc = Doc;
exports.Elem = Elem;
exports.HtmlElem = HtmlElem;
exports.InputElem = InputElem;
exports.EvtTarget = EvtTarget;
exports.userInfo = userInfo;
exports.getInputValue = getInputValue;
exports.getNumericValue = getNumericValue;
exports.getFormula = getFormula;
exports.draw = draw;
exports.optButton = optButton$1;
/* optButton Not a pure module */