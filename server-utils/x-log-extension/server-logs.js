const print = x => console.debug(x);
chrome.runtime.onMessage.addListener(function (message) {
    if (typeof message.xLog === "string") {
        print(message.xLog);
        chrome.runtime.sendMessage({ type: 'x-log-acknowledge', timestamp: message.timestamp });
    }
});
chrome.runtime.sendMessage({ type: 'x-log-acknowledge', timestamp: new Date().getTime() });
const script = document.createElement('script');
script.textContent = 'window["x-log"] = true;';
(document.head || document.documentElement).appendChild(script);
script.remove();