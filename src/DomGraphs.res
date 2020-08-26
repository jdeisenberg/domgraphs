module DOM = Webapi.Dom  // use all upper case to distinguish from built-in Dom
module Doc = Webapi.Dom.Document
module Elem = Webapi.Dom.Element
module HtmlElem = Webapi.Dom.HtmlElement
module InputElem = Webapi.Dom.HtmlInputElement
module EvtTarget = Webapi.Dom.EventTarget

type trigFcn = (float) => float

type formula = {
  factor: float,
  fcn: trigFcn,
  theta: float,
  offset: float
}

type numError = Belt.Result.t<float, string>
type fcnError = Belt.Result.t<trigFcn, string>
type inputResult<'a> = Belt.Result.t<'a, string>

external unsafeAsHtmlInputElement : Elem.t => InputElem.t = "%identity"

let userInfo = ref (({factor: 1.0, fcn: sin, theta: 1.0, offset: 0.0},
  {factor: 1.0, fcn: sin, theta: 1.0, offset: 0.0}))

let getInputValue = (id: string, defaultValue: 'a, f: (string)=> inputResult<'a>):
  inputResult<'a> => {
  switch (Doc.getElementById(id, DOM.document)) {
    | Some(element) => {
        let s = InputElem.value(unsafeAsHtmlInputElement(element))
        f(s)
      }
    | None => Belt.Result.Error("No element" ++ id)
  }
}

/*
 * Get text from the given id (unless it doesn't exist, in which case
 *   return Error("no such id")
 * If the string is empty, return Ok(default).
 * If non-empty and conversion to float is successful, return Ok(value)
 * otherwise, return Error("non-numeric")
 */

let getNumericValue = (id: string, defaultValue: float): numError => {
  switch (Doc.getElementById(id, DOM.document)) {
    | Some(element) => {
        let s = InputElem.value(unsafeAsHtmlInputElement(element))
        if (s == "") {
          Belt.Result.Ok(defaultValue)
        } else {
          switch (float_of_string(s)) {
            | result => Belt.Result.Ok(result)
            | exception Failure(_) => Belt.Result.Error(s ++ " is not numeric.")
          }
        }
      }
    | None => Belt.Result.Error("No element " ++ id)
  }
}


let getFormula = (suffix: string): Belt.Result.t<formula, string> => {
  let factor = getNumericValue("factor" ++ suffix, 1.0)
  let theta = getNumericValue("theta" ++ suffix, 1.0)
  let offset = getNumericValue("offset" ++ suffix, 0.0)
  // let fcn = getFunctionValue("fcn" ++ suffix)
  Belt.Result.Error("TBD")
}
    
let draw = (evt) => {
  let testNumeric = getNumericValue("factor1", 1.0)
  Js.log(testNumeric)
}

let optButton = Doc.getElementById("draw", DOM.document)
switch (optButton) {
  | Some(button) => {
      EvtTarget.addClickEventListener(draw, Elem.asEventTarget(button))
    }
  | None => DOM.Window.alert("Cannot find button", DOM.window)
}

