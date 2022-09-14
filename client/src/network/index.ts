const VERSION = "/api/v1"

type endpoint<T> = {
    uri: string,
    expectedStatus: number,
    parse: (json: JSONValue) => T | null
}

export type endpoints = {
    "GET_CHAIN_INFO": endpoint<{ consensus: string, discovery: string }>,
}

export const makeEndpoints = (root: string): endpoints => ({
    "GET_CHAIN_INFO": {
        uri: `${root}${VERSION}/chain/info`,
        expectedStatus: 200,
        parse: (json: JSONValue) => {
            const consensus = json.at("consensus").as_string();
            const discovery = json.at("discovery").as_string();
            if (consensus === null) return null;
            if (discovery === null) return null;
            return { consensus, discovery }
        }
    },
})

const parse = async <T>(endpoint: endpoint<T>, status: number, json: JSONType): Promise<T> => {
    if (status !== endpoint.expectedStatus) {
        return Promise.reject(json);
    }

    const jsonValue = JSONValue.of(json);
    const parsedResponse = endpoint.parse(jsonValue);
    if (parsedResponse === null) {
        return Promise.reject({ "type": "ERROR", "msg": "please contact the team" });
    }
    return parsedResponse
}

export const get = async <T>(endpoint: endpoint<T>): Promise<T> => {
    const uri = endpoint.uri;
    const response = await fetch(uri);

    const status = response.status;
    const json: JSONType = await response.json();
    return parse(endpoint, status, json);
}

