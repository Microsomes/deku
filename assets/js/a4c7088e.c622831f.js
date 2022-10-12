"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[735],{3905:(e,t,n)=>{n.d(t,{Zo:()=>s,kt:()=>m});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function o(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var p=a.createContext({}),u=function(e){var t=a.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},s=function(e){var t=u(e.components);return a.createElement(p.Provider,{value:t},e.children)},c={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,i=e.originalType,p=e.parentName,s=o(e,["components","mdxType","originalType","parentName"]),d=u(n),m=r,k=d["".concat(p,".").concat(m)]||d[m]||c[m]||i;return n?a.createElement(k,l(l({ref:t},s),{},{components:n})):a.createElement(k,l({ref:t},s))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=n.length,l=new Array(i);l[0]=d;var o={};for(var p in t)hasOwnProperty.call(t,p)&&(o[p]=t[p]);o.originalType=e,o.mdxType="string"==typeof e?e:r,l[1]=o;for(var u=2;u<i;u++)l[u]=n[u];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},44653:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>l,default:()=>c,frontMatter:()=>i,metadata:()=>o,toc:()=>u});var a=n(87462),r=(n(67294),n(3905));const i={},l="Create your first JS Deku dApp",o={unversionedId:"dApps/dapp_hello_world_tutorial",id:"dApps/dapp_hello_world_tutorial",title:"Create your first JS Deku dApp",description:"In this tutorial, we demonstrate how to create an DApp (using TypeScript) to interact with the Deku-Parametric or Deku-P in short.",source:"@site/../docs/dApps/dapp_hello_world_tutorial.md",sourceDirName:"dApps",slug:"/dApps/dapp_hello_world_tutorial",permalink:"/docs/dApps/dapp_hello_world_tutorial",draft:!1,editUrl:"https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/../docs/dApps/dapp_hello_world_tutorial.md",tags:[],version:"current",frontMatter:{},sidebar:"tutorialSidebar",previous:{title:"vm_protocol",permalink:"/docs/Deku-P/vm_protocol"},next:{title:"dapp_http_tutorial",permalink:"/docs/dApps/dapp_http_tutorial"}},p={},u=[{value:"Prerequisites",id:"prerequisites",level:2},{value:"Write dApp",id:"write-dapp",level:2},{value:"Run our dApp: Hello World",id:"run-our-dapp-hello-world",level:4},{value:"Operation",id:"operation",level:2}],s={toc:u};function c(e){let{components:t,...n}=e;return(0,r.kt)("wrapper",(0,a.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"create-your-first-js-deku-dapp"},"Create your first JS Deku dApp"),(0,r.kt)("p",null,"In this tutorial, we demonstrate how to create an DApp (using TypeScript) to interact with the Deku-Parametric or Deku-P in short."),(0,r.kt)("p",null,"TLDR;\nDeku-P is a private blockchain (it has its own consensus, gossip, network, validators, etc.). Where it has a separate WASM act as the bridge, to communicate between your DApp and the blockchain. The advantage of Deku-P is that you only need to focus on writing your dApp, Deku-P will take care of how to handle the transaction, consensus, etc. for you."),(0,r.kt)("h2",{id:"prerequisites"},"Prerequisites"),(0,r.kt)("ol",{start:0},(0,r.kt)("li",{parentName:"ol"},"You can clone the github directory and use the branch ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/marigold-dev/deku/tree/parametric-develop"},"parametric-develop"),", on your terminal type:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"git clone https://github.com/marigold-dev/deku.git \ngit checkout parametric-develop\n")),(0,r.kt)("p",null,"You can follow this ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/tree/parametric"},"readme")," on how to compile your project."),(0,r.kt)("p",null,"I will use ",(0,r.kt)("inlineCode",{parentName:"p"},"nix")," to build this project as follow:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"cd deku\ndirenv allow\ndune build\n")),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},"We will use Typescript as a main language for our application. Create the folder for your new project inside the folder ",(0,r.kt)("inlineCode",{parentName:"li"},"examples"),".  On your terminal type:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"cd examples\nmkdir tutorial\nnpm init\n")),(0,r.kt)("p",null,(0,r.kt)("inlineCode",{parentName:"p"},"npm init")," to initialize your project."),(0,r.kt)("ol",{start:2},(0,r.kt)("li",{parentName:"ol"},"Run ",(0,r.kt)("inlineCode",{parentName:"li"},"npm install typescript"),", it will create a file ",(0,r.kt)("inlineCode",{parentName:"li"},"package.json"),".\nInside this file you can add the configuration file ",(0,r.kt)("inlineCode",{parentName:"li"},"tsconfig.json")," which contain all the configurations for your typescript project.")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},'"scripts": {\n    "build": "tsc --p tsconfig.json",\n    "test": "echo \\"Error: no test specified\\" && exit 1"\n  },\n')),(0,r.kt)("p",null,"Create a new file ",(0,r.kt)("inlineCode",{parentName:"p"},"tsconfig.json")," at the same level of your typescript project"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"touch tsconfig.json\n")),(0,r.kt)("p",null,"copy the content of this ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/blob/cookie-game/examples/tutorial/tsconfig.json"},(0,r.kt)("inlineCode",{parentName:"a"},"tsconfig.json"))),(0,r.kt)("ol",{start:3},(0,r.kt)("li",{parentName:"ol"},"Now you need to build a WASM so that you have a communication channel between your application and the Deku-P. ")),(0,r.kt)("p",null,"To do that in your ",(0,r.kt)("inlineCode",{parentName:"p"},"package.json")," you can add the ",(0,r.kt)("inlineCode",{parentName:"p"},'"dependecies"')," as follow:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},' "dependencies": {\n    "deku_js_interop": "file:../../sdks/deku_js_interop",\n    "typescript": "^4.7.4"\n  }\n')),(0,r.kt)("p",null,"Or you can install ",(0,r.kt)("inlineCode",{parentName:"p"},"deku_js_interop")," manually by using:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},'npm install "file:../.../sdks/deku_js_interop"\n')),(0,r.kt)("p",null,"Here is the complete ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/blob/cookie-game/examples/tutorial/package.json"},(0,r.kt)("inlineCode",{parentName:"a"},"package.json"))),(0,r.kt)("ol",{start:4},(0,r.kt)("li",{parentName:"ol"},"Now you can start to write your application. To do that create an ",(0,r.kt)("inlineCode",{parentName:"li"},"index.ts")," file. This file will contain all the logic of your application.")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"touch index.ts\n")),(0,r.kt)("ol",{start:5},(0,r.kt)("li",{parentName:"ol"},"To be able to interact between Deku-P and your applcation, you need to have a Deku wallet. You can use the CLI ",(0,r.kt)("inlineCode",{parentName:"li"},"deku-cli")," (for more information you can use ",(0,r.kt)("inlineCode",{parentName:"li"},"deku-cli --help"),") provided by the Deku-P as follow:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"alias deku-cli='nix run github:marigold-dev/deku/parametric#deku-cli --'\ndeku-cli create-wallet\n")),(0,r.kt)("p",null,"This will generate a ",(0,r.kt)("inlineCode",{parentName:"p"},"tz1xxxx.tzsidewallet")," file. Let's rename it to ",(0,r.kt)("inlineCode",{parentName:"p"},"wallet.json")," for the example."),(0,r.kt)("ol",{start:6},(0,r.kt)("li",{parentName:"ol"},"Finally, we can test the dApp with the Deku-P, in this example we are using the CLI provided by Deku-P ",(0,r.kt)("inlineCode",{parentName:"li"},"create-mock-transaction")," to run the example without interact with the Deku-P cluster, to interact with Deku-P cluster use ",(0,r.kt)("inlineCode",{parentName:"li"},"create-custom-transaction"),". ")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"ts index.ts\nnpm i\nnpm run build\ndeku-cli create-mock-transaction ./wallet.json $action node examples/tutorial/index.js\n")),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"ts index.ts"),": build ",(0,r.kt)("inlineCode",{parentName:"li"},"ts")," to ",(0,r.kt)("inlineCode",{parentName:"li"},"js")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"npm i")," and ",(0,r.kt)("inlineCode",{parentName:"li"},"npm run build"),": build your TypeScript project."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"$action"),": is the input from dApp that you want to send to the WASM.")),(0,r.kt)("h2",{id:"write-dapp"},"Write dApp"),(0,r.kt)("p",null,"Let's start to write the logic for our dApp. We want to print out the ",(0,r.kt)("inlineCode",{parentName:"p"},"Hello World"),"."),(0,r.kt)("p",null,"Inside ",(0,r.kt)("inlineCode",{parentName:"p"},"index.ts")," we import the library ",(0,r.kt)("inlineCode",{parentName:"p"},"deku_js_interop")," to guide our dApp on how to communicate with the WASM."),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'import  { main, get, set, transaction } from "deku_js_interop"\n')),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"main"),": is the main function where it will take two parameters: ",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"initial state of your VM "),(0,r.kt)("li",{parentName:"ul"},"state transition when there is a new input"))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"get"),": retrieves the value from the local state, it will take a single parameter ",(0,r.kt)("inlineCode",{parentName:"li"},"key"),", and will return the stored value."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"set"),": it will set a value  in a Deku state for a given key. It takes two parameters:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"key: a key of the state"),(0,r.kt)("li",{parentName:"ul"},"value: a value is a string encoded in json format"))),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"transition"),": is a json received from the chain, it contains")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"interface transaction {\n  source: string;\n  tx_hash: string;\n  op_hash: string;\n  operation: { [key: string]: any };\n}\n")),(0,r.kt)("p",null,"The complete ",(0,r.kt)("inlineCode",{parentName:"p"},"index.ts")," example:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'// @ts-ignore\nimport { main, get, set, transaction } from "deku_js_interop"\n\nconst transition = (tx: transaction) => {\n    // get the current value of `state` field defined in `main` function\n    let source_value = JSON.parse(get("my_state"));\n    // set the new value to `Hello world!`\n    source_value = "Hello World!";\n    // save the new state\n    set("my_state", source_value);\n}\n\nmain({ my_state: "" }, transition);\n')),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The ",(0,r.kt)("inlineCode",{parentName:"li"},"transition")," function")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},"const transition = (tx: transaction) => {\n}\n")),(0,r.kt)("p",null,"The ",(0,r.kt)("inlineCode",{parentName:"p"},"tx")," which is a ",(0,r.kt)("inlineCode",{parentName:"p"},"transaction")," will allow you to retrieve and save the state of your application."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The ",(0,r.kt)("inlineCode",{parentName:"li"},"main")," function declare the initial state:")),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'main({ my_state: "" }, transition);\n')),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},'my_state: ""'),": it is the initial state, it must be a ",(0,r.kt)("inlineCode",{parentName:"li"},"JSON")," object with the ",(0,r.kt)("inlineCode",{parentName:"li"},"tz1xxx")," address as key"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"transition"),": it will update the state.")),(0,r.kt)("h4",{id:"run-our-dapp-hello-world"},"Run our dApp: Hello World"),(0,r.kt)("p",null,"Call your VM transaction:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"npm run build\ndeku-cli create-mock-transaction wallet.json '\"\"' node examples/tutorial/index.js\n")),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"npm run build")," to build your VM."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"$action")," we give an empty string because we just want to call the ",(0,r.kt)("inlineCode",{parentName:"li"},"transition")," with the value ",(0,r.kt)("inlineCode",{parentName:"li"},"Hello World")," that already stored in our value out.")),(0,r.kt)("p",null,"If you want to run on Deku-P cluster use:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},'tilt up -- --mode=local --vm="node examples/tutorial/index.js"\ndeku-cli create-custom-transaction data/0 wallet.json \'""\'\n')),(0,r.kt)("h2",{id:"operation"},"Operation"),(0,r.kt)("p",null,"In this example, instead giving the ",(0,r.kt)("inlineCode",{parentName:"p"},"$action")," as an empty string, we will input the string ",(0,r.kt)("inlineCode",{parentName:"p"},"Hello World")," from the dApp to the Deku-P cluster. To do that, let's modify the code in the function ",(0,r.kt)("inlineCode",{parentName:"p"},"transition")," in ",(0,r.kt)("inlineCode",{parentName:"p"},"index.ts")," as follow:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-typescript"},'const transition = (tx: transaction) => {\n    console.log("Getting source");\n    let source_value = JSON.parse(get("my_state"));\n    console.log("Current value: " + source_value);\n    // tx.operation is the last argument of `deku-cli` command line\n    source_value = tx.operation;\n    console.log("New value: " + source_value);\n    set("my_state", source_value);\n}\n')),(0,r.kt)("p",null,"We will simply write it in the last argument of ",(0,r.kt)("inlineCode",{parentName:"p"},"deku-cli create-custom-transaction"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell",metastring:"script",script:!0},"$ deku-cli create-custom-transaction data/0 wallet.json '\"Hello world!\"'\n")),(0,r.kt)("p",null,"To change the state with another string ",(0,r.kt)("inlineCode",{parentName:"p"},"Something else")," we can use:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell",metastring:"script",script:!0},"$ deku-cli create-custom-transaction data/0 wallet.json '\"Something else\"'\n")),(0,r.kt)("p",null,"And check on tilt, the VM state updating:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"Getting source\nCurrent value:\nNew value: Hello world!\nGetting source\nCurrent value: Hello world!\nNew value: Something else\n")),(0,r.kt)("p",null,"The complete source code of our tutorial can be find at: ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/tree/cookie-game/examples/tutorial"},"tutorial")),(0,r.kt)("p",null,"For more complex examples, you can have a look at ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/tree/cookie-game/examples/ts-counter"},"counter")," or at ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/marigold-dev/deku/tree/cookie-game/examples/cookie-game"},"cookie-game"),"."))}c.isMDXComponent=!0}}]);