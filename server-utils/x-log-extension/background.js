/** @type {Map<any, [{xLog: string, timestamp: number}]>} */
let tabs = new Map();

chrome.runtime.onMessage.addListener(function (message, sender) {
    if (message.type === 'x-log-acknowledge') {
        let tab = tabs.get(sender.tab.id);
        if (!tab) return;
        let queue = tab.splice(0, tab.length).filter(x => x.timestamp < message.timestamp);
        if (queue.length == 0) return;
        let timestamp = queue[queue.length - 1].timestamp;
        let xLog = queue.map(x => x.xLog).join("");
        chrome.tabs.sendMessage(sender.tab.id, { xLog, timestamp })
    }
});

chrome.webRequest.onHeadersReceived.addListener(
    async function (details) {
        let xLog = null;

        for (let header of details.responseHeaders) {
            if (header.name.toLowerCase() === 'x-log') {
                xLog = JSON.parse(header.value);
                break;
            }
        }

        if (!xLog) return;
        let tab = tabs.get(details.tabId);
        if (!tab) {
            tab = [];
            tabs.set(details.tabId, tab);
        }
        let msg = {xLog, timestamp: new Date().getTime()};
        tab.push(msg);
        chrome.tabs.sendMessage(details.tabId, msg);
    },
    { urls: ["<all_urls>"] },
    ["responseHeaders"]
);
