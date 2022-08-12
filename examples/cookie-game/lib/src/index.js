"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
const deku_js_interop_1 = require("deku_js_interop");
const state_1 = require("./state");
const actions_1 = require("./actions");
const utils_1 = require("./utils");
const saveState = (source, cookieBaker) => {
    const cookieBakerJSON = JSON.stringify(cookieBaker, utils_1.stringifyReplacer);
    (0, deku_js_interop_1.set)(source, cookieBakerJSON);
    console.log("Successfully saved cookieBaker: " + cookieBakerJSON);
};
const transition = (tx) => {
    // source -> tz1 address
    // op_hash / tx_hash => BLAKE2B => resolved as string
    // operation => any
    const source = tx.source;
    const operation = tx.operation;
    let sourceValue = JSON.parse((0, deku_js_interop_1.get)(source));
    let cookieBaker;
    if (sourceValue === undefined || sourceValue === null) {
        cookieBaker = (0, state_1.createEmptyCookieBaker)();
    }
    else {
        cookieBaker = JSON.parse(sourceValue, utils_1.parseReviver);
        cookieBaker = (0, state_1.initCookieBaker)(cookieBaker);
    }
    console.log("Actual cookieBaker retrieved from state: " + JSON.stringify(cookieBaker, utils_1.stringifyReplacer));
    switch (operation) {
        case actions_1.actions.cookie: {
            //update state
            cookieBaker = (0, state_1.addCookie)(cookieBaker);
            console.log("Successfully minted cookie");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.cursor: {
            //action successful, update state
            cookieBaker = (0, state_1.addCursor)(cookieBaker);
            console.log("Successfully minted cursor");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.grandma: {
            //action successful, update state
            cookieBaker = (0, state_1.addGrandma)(cookieBaker);
            console.log("Successfully minted grandma");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.farm: {
            //action successful, update state
            cookieBaker = (0, state_1.addFarm)(cookieBaker);
            console.log("Successfully minted farm");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.mine: {
            //action successful, update state
            cookieBaker = (0, state_1.addMine)(cookieBaker);
            console.log("Successfully minted mine");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.factory: {
            //action successful, update state
            cookieBaker = (0, state_1.addFactory)(cookieBaker);
            console.log("Successfully minted factory");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.bank: {
            //action successful, update state
            cookieBaker = (0, state_1.addBank)(cookieBaker);
            console.log("Successfully minted bank");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.temple: {
            //action successful, update state
            cookieBaker = (0, state_1.addTemple)(cookieBaker);
            console.log("Successfully minted temple");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.wizard: {
            //action successful, update state
            cookieBaker = (0, state_1.addWizard)(cookieBaker);
            console.log("Successfully minted wizard");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.shipment: {
            //action successful, update state
            cookieBaker = (0, state_1.addShipment)(cookieBaker);
            console.log("Successfully minted shipment");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.alchemy: {
            //action successful, update state
            cookieBaker = (0, state_1.addAlchemy)(cookieBaker);
            console.log("Successfully minted alchemy");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.portal: {
            //action successful, update state
            cookieBaker = (0, state_1.addPortal)(cookieBaker);
            console.log("Successfully minted portal");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.timeMachine: {
            //action successful, update state
            cookieBaker = (0, state_1.addTimeMachine)(cookieBaker);
            console.log("Successfully minted time machine");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.antimatter: {
            //action successful, update state
            cookieBaker = (0, state_1.addAntimatter)(cookieBaker);
            console.log("Successfully minted antimatter");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.prism: {
            //action successful, update state
            cookieBaker = (0, state_1.addPrism)(cookieBaker);
            console.log("Successfully minted prism");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.chanceMaker: {
            //action successful, update state
            cookieBaker = (0, state_1.addChanceMaker)(cookieBaker);
            console.log("Successfully minted chance maker");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.fractal: {
            //action successful, update state
            cookieBaker = (0, state_1.addFractal)(cookieBaker);
            console.log("Successfully minted fractal");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.javaScript: {
            //action successful, update state
            cookieBaker = (0, state_1.addJavaScript)(cookieBaker);
            console.log("Successfully minted javascript");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.idleverse: {
            //action successful, update state
            cookieBaker = (0, state_1.addIdleverse)(cookieBaker);
            console.log("Successfully minted idleverse");
            saveState(source, cookieBaker);
            break;
        }
        case actions_1.actions.cordex: {
            //action successful, update state
            cookieBaker = (0, state_1.addCordex)(cookieBaker);
            console.log("Successfully minted cordex");
            saveState(source, cookieBaker);
            break;
        }
    }
};
(0, deku_js_interop_1.main)({}, transition);
