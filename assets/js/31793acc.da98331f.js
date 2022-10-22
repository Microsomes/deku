/*! For license information please see 31793acc.da98331f.js.LICENSE.txt */
"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[883],{41535:(e,t)=>{var r=Symbol.for("react.element"),n=Symbol.for("react.portal"),o=Symbol.for("react.fragment"),a=Symbol.for("react.strict_mode"),c=Symbol.for("react.profiler"),u=Symbol.for("react.provider"),s=Symbol.for("react.context"),l=Symbol.for("react.forward_ref"),i=Symbol.for("react.suspense"),f=Symbol.for("react.memo"),p=Symbol.for("react.lazy"),y=Symbol.iterator;var d={isMounted:function(){return!1},enqueueForceUpdate:function(){},enqueueReplaceState:function(){},enqueueSetState:function(){}},b=Object.assign,m={};function v(e,t,r){this.props=e,this.context=t,this.refs=m,this.updater=r||d}function h(){}function g(e,t,r){this.props=e,this.context=t,this.refs=m,this.updater=r||d}v.prototype.isReactComponent={},v.prototype.setState=function(e,t){if("object"!=typeof e&&"function"!=typeof e&&null!=e)throw Error("setState(...): takes an object of state variables to update or a function which returns an object of state variables.");this.updater.enqueueSetState(this,e,t,"setState")},v.prototype.forceUpdate=function(e){this.updater.enqueueForceUpdate(this,e,"forceUpdate")},h.prototype=v.prototype;var O=g.prototype=new h;O.constructor=g,b(O,v.prototype),O.isPureReactComponent=!0;var j=Array.isArray,w=Object.prototype.hasOwnProperty,k={current:null},S={key:!0,ref:!0,__self:!0,__source:!0};function _(e,t,n){var o,a={},c=null,u=null;if(null!=t)for(o in void 0!==t.ref&&(u=t.ref),void 0!==t.key&&(c=""+t.key),t)w.call(t,o)&&!S.hasOwnProperty(o)&&(a[o]=t[o]);var s=arguments.length-2;if(1===s)a.children=n;else if(1<s){for(var l=Array(s),i=0;i<s;i++)l[i]=arguments[i+2];a.children=l}if(e&&e.defaultProps)for(o in s=e.defaultProps)void 0===a[o]&&(a[o]=s[o]);return{$$typeof:r,type:e,key:c,ref:u,props:a,_owner:k.current}}function P(e){return"object"==typeof e&&null!==e&&e.$$typeof===r}var x=/\/+/g;function E(e,t){return"object"==typeof e&&null!==e&&null!=e.key?function(e){var t={"=":"=0",":":"=2"};return"$"+e.replace(/[=:]/g,(function(e){return t[e]}))}(""+e.key):t.toString(36)}function $(e,t,o,a,c){var u=typeof e;"undefined"!==u&&"boolean"!==u||(e=null);var s=!1;if(null===e)s=!0;else switch(u){case"string":case"number":s=!0;break;case"object":switch(e.$$typeof){case r:case n:s=!0}}if(s)return c=c(s=e),e=""===a?"."+E(s,0):a,j(c)?(o="",null!=e&&(o=e.replace(x,"$&/")+"/"),$(c,t,o,"",(function(e){return e}))):null!=c&&(P(c)&&(c=function(e,t){return{$$typeof:r,type:e.type,key:t,ref:e.ref,props:e.props,_owner:e._owner}}(c,o+(!c.key||s&&s.key===c.key?"":(""+c.key).replace(x,"$&/")+"/")+e)),t.push(c)),1;if(s=0,a=""===a?".":a+":",j(e))for(var l=0;l<e.length;l++){var i=a+E(u=e[l],l);s+=$(u,t,o,i,c)}else if(i=function(e){return null===e||"object"!=typeof e?null:"function"==typeof(e=y&&e[y]||e["@@iterator"])?e:null}(e),"function"==typeof i)for(e=i.call(e),l=0;!(u=e.next()).done;)s+=$(u=u.value,t,o,i=a+E(u,l++),c);else if("object"===u)throw t=String(e),Error("Objects are not valid as a React child (found: "+("[object Object]"===t?"object with keys {"+Object.keys(e).join(", ")+"}":t)+"). If you meant to render a collection of children, use an array instead.");return s}function D(e,t,r){if(null==e)return e;var n=[],o=0;return $(e,n,"","",(function(e){return t.call(r,e,o++)})),n}function T(e){if(-1===e._status){var t=e._result;(t=t()).then((function(t){0!==e._status&&-1!==e._status||(e._status=1,e._result=t)}),(function(t){0!==e._status&&-1!==e._status||(e._status=2,e._result=t)})),-1===e._status&&(e._status=0,e._result=t)}if(1===e._status)return e._result.default;throw e._result}var C={current:null},M={transition:null}},27378:(e,t,r)=>{r(41535)},3905:(e,t,r)=>{r.d(t,{Zo:()=>i,kt:()=>y});var n=r(67294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function c(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function u(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var s=n.createContext({}),l=function(e){var t=n.useContext(s),r=t;return e&&(r="function"==typeof e?e(t):c(c({},t),e)),r},i=function(e){var t=l(e.components);return n.createElement(s.Provider,{value:t},e.children)},f={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},p=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,a=e.originalType,s=e.parentName,i=u(e,["components","mdxType","originalType","parentName"]),p=l(r),y=o,d=p["".concat(s,".").concat(y)]||p[y]||f[y]||a;return r?n.createElement(d,c(c({ref:t},i),{},{components:r})):n.createElement(d,c({ref:t},i))}));function y(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=r.length,c=new Array(a);c[0]=p;var u={};for(var s in t)hasOwnProperty.call(t,s)&&(u[s]=t[s]);u.originalType=e,u.mdxType="string"==typeof e?e:o,c[1]=u;for(var l=2;l<a;l++)c[l]=r[l];return n.createElement.apply(null,c)}return n.createElement.apply(null,r)}p.displayName="MDXCreateElement"},37736:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>s,contentTitle:()=>c,default:()=>f,frontMatter:()=>a,metadata:()=>u,toc:()=>l});var n=r(87462),o=(r(27378),r(3905));const a={},c="Glossary",u={unversionedId:"glossary",id:"glossary",title:"Glossary",description:"Under construction!",source:"@site/../docs/glossary.md",sourceDirName:".",slug:"/glossary",permalink:"/docs/glossary",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/../docs/glossary.md",tags:[],version:"current",frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"The Deku-P VM Protocol",permalink:"/docs/Deku-P/vm_protocol"}},s={},l=[],i={toc:l};function f(e){let{components:t,...r}=e;return(0,o.kt)("wrapper",(0,n.Z)({},i,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"glossary"},"Glossary"),(0,o.kt)("p",null,"Under construction!"))}f.isMDXComponent=!0}}]);