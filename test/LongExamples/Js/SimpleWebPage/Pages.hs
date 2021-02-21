{-# LANGUAGE QuasiQuotes #-}
module LongExamples.Js.SimpleWebPage.Pages where
import Text.RawString.QQ

import Test.HUnit
import Parser
import Types
import TestUtils

tests = [
  controller `matchesJs` controllerResult
  ]


controller = [r|import * as React from "react"
import Nav from "./Nav.jsx"
import ShopPreferences from "./ShopPreferences.jsx"
import NameYourShop from "./NameYourShop.jsx"
import StockYourShop from "./StockYourShop.jsx"
import HowYoullGetPaid from "./HowYoullGetPaid.jsx"
import SetupBilling from "./SetupBilling.jsx"

export default class Controller extends React.Component where

constructor props := {
  super(props)
  @state = {
    activeItem: 0
  }
}

navItemClicked navId := {
  @@activeItem = navId
}

onContinue id := guard
  | id < 5 -> @@activeItem = @@activeItem + 1
  | otherwise -> @@activeItem = 0

openForBusiness := {
  div.new({className: "row"}) do
    h2 "Your shop is open for business!"
  end
}

currentStep := guard(@@activeItem)
  | 0 -> ShopPreferences.new({stepId: 0, onContinue: (\id -> @onContinue(id))})
  | 1 -> NameYourShop.new({stepId: 1, onContinue: (\id -> @onContinue(id))})
  | 2 -> StockYourShop.new({stepId: 2, onContinue: (\id -> @onContinue(id))})
  | 3 -> HowYoullGetPaid.new({stepId: 3, onContinue: (\id -> @onContinue(id))})
  | 4 -> SetupBilling.new({stepId: 4, onContinue: (\id -> @onContinue(id))})
  | 5 -> @openForBusiness()
  | _ -> p "no step selected"

render := {
  div.new({
    className: "container pad"
  }) do
    div.new({className: "row"}) do
      Nav.new({
        selected: @@activeItem,
        navItemClicked: (\id -> @navItemClicked(id))})
    end
    div.new({className: "row"}) do
      @currentStep()
    end
  end
}
|]

controllerResult = [r|import * as React from "react";
import Nav from "./Nav.jsx";
import ShopPreferences from "./ShopPreferences.jsx";
import NameYourShop from "./NameYourShop.jsx";
import StockYourShop from "./StockYourShop.jsx";
import HowYoullGetPaid from "./HowYoullGetPaid.jsx";
import SetupBilling from "./SetupBilling.jsx";
 export default class Controller extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      "activeItem": 0
    }
  }
  navItemClicked(navId) {
    this.setState({
        activeItem: navId
    });
  }
  onContinue(id) {
    if (id < 5) {
      this.setState({
          activeItem: this.state.activeItem + 1
      });
    } else {
      this.setState({
          activeItem: 0
      });
    }
  }
  openForBusiness() {
    return (<div className="row"><h2>Your shop is open for business!</h2></div>);
  }
  currentStep() {
    switch (this.state.activeItem) {
      case 0:
        return (<ShopPreferences stepId={0} onContinue={((id) => {
        return this.onContinue(id);
      })}></ShopPreferences>);
      case 1:
        return (<NameYourShop stepId={1} onContinue={((id) => {
        return this.onContinue(id);
      })}></NameYourShop>);
      case 2:
        return (<StockYourShop stepId={2} onContinue={((id) => {
        return this.onContinue(id);
      })}></StockYourShop>);
      case 3:
        return (<HowYoullGetPaid stepId={3} onContinue={((id) => {
        return this.onContinue(id);
      })}></HowYoullGetPaid>);
      case 4:
        return (<SetupBilling stepId={4} onContinue={((id) => {
        return this.onContinue(id);
      })}></SetupBilling>);
      case 5:
        return this.openForBusiness();
      default:
        return (<p>no step selected</p>);
    }
  }
  render() {
    return (<div className="container pad"><div className="row"><Nav selected={this.state.activeItem} navItemClicked={((id) => {
      return this.navItemClicked(id);
    })}></Nav></div>
    <div className="row">{this.currentStep()}</div></div>)
  }
}|]
