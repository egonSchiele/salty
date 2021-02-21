{-# LANGUAGE QuasiQuotes #-}
module LongExamples.Js.SimpleWebPage.Nav where
import Text.RawString.QQ

import Test.HUnit
import Parser
import Types
import TestUtils

tests = [
  salty `matchesJs` expectedJs
  ]


salty = [r|import * as React from "react"

class NavItem extends React.Component {
  getClass := guard
    | @props.selected == true -> "nav-link active"
    | _ -> "nav-link"

  render := {
    li.new({className: "nav-item"}) do
      a.new({
      href: "#",
      className: @getClass(),
      key: @props.navId,
      children: @props.title,
      onClick: (\e -> {
        e.preventDefault()
        @props.navItemClicked(@props.navId)
      })
      })
    end
  }
}

export default class Nav extends React.Component where
constructor props := {
  super(props)
  @navItemClicked = props.navItemClicked
  @state = {
    steps: [
      "Shop preferences",
      "Name your shop",
      "Stock your shop",
      "How you'll get paid",
      "Set up billing"
    ]
  }
}

render := {
  ul.new({className: "nav nav-pills"}) do
    @@steps.map(\step i -> {
      NavItem.new({
        navId: i,
        title: step,
        selected: (i == @props.selected),
        navItemClicked: (\id -> {
          @setState({
            selected: id
          }, (\_ -> @navItemClicked(id))
        )
        })
      })
    })
  end
}
|]

expectedJs = [r|import * as React from "react";
class NavItem extends React.Component {
  getClass() {
    if (this.props.selected === true) {
      return "nav-link active";
    } else {
      return "nav-link";
    }
  }
  render() {
    return (<li className="nav-item"><a href="#" className={this.getClass()} key={this.props.navId} children={this.props.title} onClick={((e) => {
      e.preventDefault();
      return this.props.navItemClicked(this.props.navId);
    })}></a></li>);
  }
}
 export default class Nav extends React.Component {
  constructor(props) {
    super(props);
    this.navItemClicked = props.navItemClicked;
    this.state = {
      "steps": ["Shop preferences", "Name your shop", "Stock your shop", "How you'll get paid", "Set up billing"]
  }
}
render() {
  return (<ul className="nav nav-pills">{this.state.steps.map((step, i) => {
    return <NavItem navId={i} title={step} selected={(i === this.props.selected)} navItemClicked={((id) => {
      return this.setState({
        "selected": id
      }, (() => {
        return this.navItemClicked(id);
      }));
    })}></NavItem>
  })}</ul>);
}
}|]
