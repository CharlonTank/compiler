// This file needs to be minified into dist for release:
// cd extra
// esbuild live.js --bundle --minify --target=chrome58,firefox57,safari11,edge16 > dist/live.js
// `lamdera live` does this automatically LDEBUG mode

import * as Sockette from 'sockette';
import * as Cookie  from 'js-cookie';

var clientId = ""
const sessionId = getSessionId()
var connected = false
var disconnectedTime = null
var bufferOutbound = []
var bufferInbound = []

var leaderId = null
var nodeType = "f"

// Null checking as we might be on an error page, which doesn't initiate an app
// but we still want the livereload to function
var app = null
var initBackendModel = null

var msgHandler = function(e) {
  const d = JSON.parse(e.data)
  switch(d.t) {
    case "r":
      document.location.reload()
      break;
  }
}

const ws = Sockette.default(((window.location.protocol === "https:") ? "wss://" : "ws://") + window.location.host + "/_w", {
  timeout: 2e3,
  maxAttempts: Infinity,
  onopen: e => {
    if (clientId !== "") {
      connected = true
      // If we've been disconnected longer than 10s, refresh entirely, as it's likely
      // in such a long time we changed live to a new project, or other things may have changed
      if (disconnectedTime !== null && disconnectedTime - new Date() < -10e3) { window.location.reload() }
      disconnectedTime = null
    }
    if (app !== null) { app.ports.setLiveStatus.send(connected) }
    flushOutbound()
  },
  onmessage: e => {
    msgHandler(e)
    if (clientId !== "" && connected === false) {
      connected = true
      flushOutbound()
    }
  },
  onreconnect: e => {}, // Called when connection is already down and a reconnect is attempted
  onmaximum: e => {}, // Will never be hit
  onclose: e => { // Called whenever the connection is terminated
    // console.log(`ws closed`, e)
    connected = false
    disconnectedTime = disconnectedTime || new Date()
    if (app !== null) { app.ports.setLiveStatus.send(connected) }
  },
  onerror: e => { //
    // console.log(`ws error`, e)
    connected = false
    disconnectedTime = disconnectedTime || new Date()
    if (app !== null) { app.ports.setLiveStatus.send(connected) }
  }
})

const flushOutbound = function() {
  if (connected) {
    while(bufferOutbound.length > 0) {
      var out = bufferOutbound.pop()
      if (out.t == "ToBackend") { out.c = clientId }
      ws.json(out)
    }
  }
}

const msgEmitter = function(payload) {
  if (connected) {
    ws.json(payload)
  } else {
    bufferOutbound.unshift(payload)
  }
}

const flushInbound = function() {
  if (app !== null) {
    while(bufferInbound.length > 0) {
      var inbound = bufferInbound.pop()
      app.ports[inbound.n].send(inbound.a)
    }
  }
}

const msgInbound = function(portname, arg) {
  if (app !== null) {
    app.ports[portname].send(arg)
  } else {
    bufferInbound.unshift({ n: portname, a: arg })
  }
}

window.setupApp = function(name, elid) {

  function initApp() {
    if (app !== null) { return } // Don't init when already initialised
    // console.log(`booting with`, { c: clientId, s: sessionId, nt: nodeType, b: initBackendModel })

    if (name !== "LocalDev") {
      console.warn('Not a Lamdera app, loading as normal Elm.')
      app = name.split('.').reduce((o,i)=> o[i], Elm).init({ node: document.getElementById(elid) })
      if (document.getElementById(elid)) {
        document.getElementById(elid).innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.'
      }
      return;
    }

    app = Elm[name].init({
      node: document.getElementById(elid),
      flags: { c: clientId, s: sessionId, nt: nodeType, b: initBackendModel }
    })
    if (document.getElementById(elid)) {
      document.getElementById(elid).innerText = 'This is a headless program, meaning there is nothing to show here.\n\nI started the program anyway though, and you can access it as `app` in the developer console.'
    }
    // window.app = app
    app.ports.send_ToFrontend.subscribe(function (payload) {
      if (payload.b !== null) {
        payload.b = bytesToBase64(payload.b)
      }
      // console.log(`[S] ToFrontend`, payload)
      msgEmitter(payload)
    })

    app.ports.save_BackendModel.subscribe(function (payload) {
      payload.b = bytesToBase64(payload.b)
      payload.f = (payload.f) ? "force" : ""
      msgEmitter(payload)
    })

    app.ports.send_EnvMode.subscribe(function (payload) {
      msgEmitter(payload)
    })

    app.ports.send_ToBackend.subscribe(function (bytes) {
      var b64 = bytesToBase64(bytes)
      // console.log(`[S] ToBackend`, { t:"ToBackend", s: sessionId, c: clientId, b: b64 })
      msgEmitter({ t:"ToBackend", s: sessionId, c: clientId, b: b64 })
    })

    if (app.ports.openDebugWindow) {
      app.ports.openDebugWindow.subscribe(function (debugState) {
        console.log('openDebugWindow called with:', debugState);
        
        try {
          // Try to open the window
          const debugWindow = window.open('', 'LamderaDebugger', 'width=800,height=600,scrollbars=yes,resizable=yes');
          
          if (!debugWindow) {
            alert('Popup blocked! Please allow popups for this site and try again.');
            return;
          }
          
          // Create a standalone HTML page for the debugger
          const debuggerHTML = `
<!DOCTYPE html>
<html>
<head>
    <title>Lamdera Time Travel Debugger</title>
    <style>
        body {
            margin: 0;
            padding: 20px;
            background-color: #1a1a1a;
            color: white;
            font-family: system-ui, -apple-system, sans-serif;
        }
        h1 { margin-top: 0; color: #4CAF50; }
        .container {
            max-width: 1200px;
            margin: 0 auto;
        }
        .history-list {
            background: #2a2a2a;
            border-radius: 8px;
            padding: 16px;
            margin: 16px 0;
            max-height: 400px;
            overflow-y: auto;
        }
        .history-item {
            padding: 8px;
            margin: 4px 0;
            background: #333;
            border-radius: 4px;
            cursor: pointer;
        }
        .history-item:hover {
            background: #444;
        }
        .history-item.active {
            background: #0066cc;
        }
        .model-view {
            background: #2a2a2a;
            border-radius: 8px;
            padding: 16px;
            margin: 16px 0;
        }
        pre {
            margin: 0;
            white-space: pre-wrap;
            font-size: 12px;
        }
        .timeline-container {
            background: #1e1e1e;
            border-radius: 8px;
            padding: 16px;
            margin: 16px 0;
            height: 400px;
            overflow: auto;
            position: relative;
        }
        .timeline-header {
            color: #ccc;
            font-size: 12px;
            margin-bottom: 16px;
        }
        .timeline-svg {
            border: 1px solid #333;
            background: #0a0a0a;
        }
        .lane-label {
            font-size: 11px;
            fill: #ccc;
            font-family: monospace;
        }
        .lane-line {
            stroke: #333;
            stroke-width: 1;
        }
        .message-dot {
            cursor: pointer;
            stroke: #000;
            stroke-width: 1;
        }
        .message-dot:hover {
            stroke: #fff;
            stroke-width: 2;
        }
        .message-dot.selected {
            stroke: #4CAF50;
            stroke-width: 3;
        }
        .connection-line {
            stroke: #666;
            stroke-width: 1;
            fill: none;
            opacity: 0.7;
        }
        .connection-line.selected {
            stroke: #4CAF50;
            stroke-width: 2;
            opacity: 1;
        }
        .kind-init { fill: #9966CC; }
        .kind-frontend { fill: #85BC7A; }
        .kind-backend { fill: #4196AD; }
        .kind-tofrontend { fill: #FFCB64; }
        .kind-tobackend { fill: #E06C75; }
        .test-info {
            background: #333;
            padding: 10px;
            border-radius: 4px;
            margin-bottom: 20px;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🐛 Lamdera Time Travel Debugger</h1>
        <div class="test-info">
            Debug window opened successfully! History count: <span id="historyCount">0</span><br>
            <small>Use ↑/↓ or j/k keys to navigate, Home/End to jump to start/end</small>
        </div>
        <div id="content">Loading...</div>
    </div>
    <script>
        const debugState = ${JSON.stringify(debugState)};
        console.log('Debug state:', debugState);
        
        function generateTimeline() {
            // Extract unique client IDs from the debug state
            const clientIds = new Set();
            
            // Add clients from current frontend models
            if (debugState.frontendModels) {
                debugState.frontendModels.forEach(([clientId]) => clientIds.add(clientId));
            }
            
            // Add clients from history entries
            debugState.history.forEach(entry => {
                if (entry.frontendModels) {
                    entry.frontendModels.forEach(([clientId]) => clientIds.add(clientId));
                }
                if (entry.source) clientIds.add(entry.source.clientId);
                if (entry.target) clientIds.add(entry.target.clientId);
            });
            
            const clients = Array.from(clientIds).sort();
            const lanes = ['Backend', ...clients];
            
            const laneHeight = 50;
            const laneWidth = Math.max(800, debugState.history.length * 15 + 200);
            const svgHeight = lanes.length * laneHeight + 40;
            const messageSpacing = (laneWidth - 200) / Math.max(1, debugState.history.length);
            
            function getLaneY(laneId) {
                if (laneId === 'Backend') return laneHeight / 2;
                const clientIndex = clients.indexOf(laneId);
                return (clientIndex + 1) * laneHeight + laneHeight / 2;
            }
            
            function getMessageX(index) {
                return 150 + index * messageSpacing;
            }
            
            let svg = \`<svg class="timeline-svg" width="\${laneWidth}" height="\${svgHeight}">\`;
            
            // Draw lane lines and labels
            lanes.forEach((lane, i) => {
                const y = (i + 0.5) * laneHeight;
                svg += \`<line class="lane-line" x1="140" y1="\${y}" x2="\${laneWidth - 20}" y2="\${y}"></line>\`;
                svg += \`<text class="lane-label" x="10" y="\${y + 4}">\${lane}</text>\`;
            });
            
            // Draw messages and connections
            debugState.history.forEach((entry, index) => {
                const messageX = getMessageX(index);
                const isSelected = index === debugState.currentIndex;
                const kindClass = 'kind-' + entry.kind.toLowerCase().replace(/([a-z])([A-Z])/g, '$1$2').toLowerCase();
                
                // Determine which lane this message belongs to
                let messageY;
                if (entry.kind === 'Backend' || entry.kind === 'ToBackend') {
                    messageY = getLaneY('Backend');
                } else if (entry.source && entry.source.clientId) {
                    messageY = getLaneY(entry.source.clientId);
                } else if (entry.target && entry.target.clientId) {
                    messageY = getLaneY(entry.target.clientId);
                } else {
                    // Default to first client lane for Frontend messages
                    messageY = clients.length > 0 ? getLaneY(clients[0]) : getLaneY('Backend');
                }
                
                // Draw connection lines for ToFrontend and ToBackend messages
                if (entry.kind === 'ToFrontend' && entry.target) {
                    const sourceY = getLaneY('Backend');
                    const targetY = getLaneY(entry.target.clientId);
                    svg += \`<path class="connection-line \${isSelected ? 'selected' : ''}" 
                             d="M \${messageX} \${sourceY} Q \${messageX + 20} \${(sourceY + targetY) / 2} \${messageX} \${targetY}"></path>\`;
                }
                
                if (entry.kind === 'ToBackend' && entry.source) {
                    const sourceY = getLaneY(entry.source.clientId);
                    const targetY = getLaneY('Backend');
                    svg += \`<path class="connection-line \${isSelected ? 'selected' : ''}" 
                             d="M \${messageX} \${sourceY} Q \${messageX + 20} \${(sourceY + targetY) / 2} \${messageX} \${targetY}"></path>\`;
                }
                
                // Draw the message dot
                const radius = isSelected ? 8 : 5;
                svg += \`<circle class="message-dot \${kindClass} \${isSelected ? 'selected' : ''}" 
                         cx="\${messageX}" cy="\${messageY}" r="\${radius}"
                         onclick="jumpTo(\${index})"
                         title="[\${index}] \${entry.kind}: \${entry.msg.substring(0, 50)}...">
                         </circle>\`;
            });
            
            svg += \`</svg>\`;
            return svg;
        }
        
        function render() {
            document.getElementById('historyCount').textContent = debugState.history.length;
            const content = document.getElementById('content');
            content.innerHTML = \`
                <div class="timeline-container">
                    <div class="timeline-header">
                        Timeline Visualization (${debugState.history.length} messages)
                    </div>
                    \${generateTimeline()}
                </div>
                <div class="history-list">
                    <h3>Message History (\${debugState.history.length} messages)</h3>
                    \${debugState.history.map((item, index) => \`
                        <div class="history-item \${index === debugState.currentIndex ? 'active' : ''}" 
                             onclick="jumpTo(\${index})">
                            [\${index}] \${item.kind}: \${item.msg.substring(0, 100)}...
                        </div>
                    \`).join('')}
                </div>
                <div class="model-view">
                    <h3>Frontend Models</h3>
                    \${debugState.frontendModels && debugState.frontendModels.length > 0 ? 
                        debugState.frontendModels.map(([clientId, frontendModel]) => \`
                            <div class="client-model">
                                <h4 style="color: #4CAF50; margin: 8px 0 4px 0; font-size: 12px;">Client: \${clientId}</h4>
                                <pre style="margin: 0; background: #444; padding: 8px; border-radius: 4px; font-size: 11px;">\${frontendModel}</pre>
                            </div>
                        \`).join('')
                        : '<div style="color: #999; font-style: italic;">No frontend models available</div>'
                    }
                </div>
                <div class="model-view">
                    <h3>Backend Model</h3>
                    <pre>\${debugState.bem}</pre>
                </div>
            \`;
        }
        
        function jumpTo(index) {
            console.log('Jumping to index:', index);
            // Update local state immediately for responsiveness
            debugState.currentIndex = index;
            render();
            
            // Send message back to parent window
            if (window.opener) {
                window.opener.postMessage({
                    type: 'jumpTo',
                    index: index
                }, '*');
            }
        }
        
        function moveUp() {
            const newIndex = Math.max(-1, debugState.currentIndex - 1);
            jumpTo(newIndex);
        }
        
        function moveDown() {
            const maxIndex = debugState.history.length - 1;
            const newIndex = Math.min(maxIndex, debugState.currentIndex + 1);
            jumpTo(newIndex);
        }
        
        // Keyboard navigation
        document.addEventListener('keydown', (event) => {
            switch(event.key) {
                case 'ArrowUp':
                case 'k': // Vim-style navigation
                    event.preventDefault();
                    moveUp();
                    break;
                case 'ArrowDown':
                case 'j': // Vim-style navigation
                    event.preventDefault();
                    moveDown();
                    break;
                case 'Home':
                    event.preventDefault();
                    jumpTo(-1); // Go to initial state
                    break;
                case 'End':
                    event.preventDefault();
                    jumpTo(debugState.history.length - 1); // Go to latest
                    break;
            }
        });
        
        // Listen for updates from parent window
        window.addEventListener('message', (event) => {
            if (event.data.type === 'updateDebugState') {
                Object.assign(debugState, event.data.state);
                render();
            }
        });
        
        // Auto-focus the window for keyboard events
        window.focus();
        
        render();
    </script>
</body>
</html>`;
          
          debugWindow.document.write(debuggerHTML);
          debugWindow.document.close();
          
          // Store reference to debug window for updates
          window.lamderaDebugWindow = debugWindow;
          
          console.log('Debug window created successfully');
        } catch (error) {
          console.error('Error opening debug window:', error);
          alert('Error opening debug window: ' + error.message);
        }
      })
    }

    // Handler for updating debugger state
    if (app.ports.updateDebuggerState) {
      app.ports.updateDebuggerState.subscribe(function (debugState) {
        if (window.lamderaDebugWindow && !window.lamderaDebugWindow.closed) {
          window.lamderaDebugWindow.postMessage({
            type: 'updateDebugState',
            state: debugState
          }, '*');
        }
      });
    }

    // Listen for messages from debug window
    window.addEventListener('message', (event) => {
      if (event.data.type === 'jumpTo' && app.ports.debuggerMessage) {
        app.ports.debuggerMessage.send({
          type: 'jumpTo',
          index: event.data.index
        });
      }
    });

    // Handler for sending frontend model updates
    if (app.ports.sendFrontendModelUpdate) {
      app.ports.sendFrontendModelUpdate.subscribe(function (payload) {
        // Send this as a WebSocket message to the leader
        // Ensure fem is base64 encoded
        payload.fem = bytesToBase64(payload.fem);
        msgEmitter(payload);
      });
    }

    // Handler for sending frontend messages
    if (app.ports.sendFrontendMessage) {
      console.log("[live.js] sendFrontendMessage port found, subscribing");
      app.ports.sendFrontendMessage.subscribe(function (payload) {
        console.log("[live.js] Sending frontend message to leader:", payload);
        // Send this as a WebSocket message to the leader
        msgEmitter(payload);
      });
    } else {
      console.log("[live.js] WARNING: sendFrontendMessage port not found!");
    }

    // Handler for broadcasting time travel state
    if (app.ports.broadcastTimeTravelState) {
      app.ports.broadcastTimeTravelState.subscribe(function (payload) {
        // Broadcast time travel state to all clients
        msgEmitter(payload);
      });
    }

    // Handler for enabling/disabling client interaction
    if (app.ports.setClientInteractionEnabled) {
      app.ports.setClientInteractionEnabled.subscribe(function (enabled) {
        const overlay = document.getElementById('lamdera-time-travel-overlay');
        if (!enabled) {
          // Create overlay to block interaction
          if (!overlay) {
            const div = document.createElement('div');
            div.id = 'lamdera-time-travel-overlay';
            div.style.cssText = `
              position: fixed;
              top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              z-index: 999999;
              background: rgba(0, 0, 0, 0.1);
              cursor: not-allowed;
              pointer-events: all;
            `;
            div.title = 'Time travel mode - interaction disabled. Navigate to latest history to enable.';
            document.body.appendChild(div);
          }
        } else {
          // Remove overlay to enable interaction
          if (overlay) {
            overlay.remove();
          }
        }
      });
    }

    // Auto-generated by extra/Lamdera/Injection.hs
    if (typeof elmPkgJsIncludes !== "undefined") elmPkgJsIncludes.init(app)

    flushInbound()
  }

  // Upgrade the msg handler now that the app is binding
  msgHandler = function(e) {

    // console.log(`got message`,e)
    let d = null;
    try {
      d = JSON.parse(e.data)
    } catch(err) {
      console.log(err, e.data);
      return;
    }

    switch(d.t) {
      case "r":
        document.location.reload()
        break;

      case "s": // setup message, will get called again if websocket drops and reconnects
        clientId = d.c
        if (app !== null) { app.ports.setClientId.send(clientId) }

        leaderId = d.l
        if (clientId == leaderId) {
          nodeType = "l"
          if (app !== null) { app.ports.setNodeTypeLeader.send(true) }
        } else {
          nodeType = "f"
          if (app !== null) { app.ports.setNodeTypeLeader.send(false) }
        }

        initApp()
        break;

      case "e": // leader has been elected
        leaderId = d.l
        if (clientId == leaderId) {
          nodeType = "l"
          if (app !== null) { app.ports.setNodeTypeLeader.send(true) }
        } else {
          nodeType = "f"
          if (app !== null) { app.ports.setNodeTypeLeader.send(false) }
        }
        break;

      case "ToBackend":
        // console.log(`[R] ToBackend`, d)
        app.ports.receive_ToBackend.send([d.s, d.c, base64ToBytes(d.b)])
        break;

      case "ToFrontend":
        // Only process messages for our clientId, or a broadcast
        if (d.c == clientId || d.c == sessionId || d.c == "b") {
          // console.log(`[R] ToFrontend`, d)
          d.c = clientId
          if (d.b !== null) {
            d.b = base64ToBytes(d.b)
          }
          app.ports.receive_ToFrontend.send(d)
        } else {
          // console.log(`dropped message`, d)
        }
        break;

      case "p":
        if (app === null) {
          // We're being given a backend state to boot up with
          initBackendModel = base64ToBytes(d.b)
        } else {
          // We're already live and being given a new backend state
          // @TODO this isn't used currently but needs to adapt for state restore functions?
          app.ports.receive_BackendModel.send(base64ToBytes(d.b))
        }
        break;

      case "q":
        // RPC Query

        try {
          // console.log("got rpc req", d)

          var done = false
          var response = null

          const returnHandler = function(payload) {
            if (payload.r === d.r) {
              // console.log("got rpc resp:", payload)
              response = payload
              done = true
            }
          }

          app.ports.rpcOut.subscribe(returnHandler)

          if (d.i) { d.i = JSON.parse(d.i); }
          if (d.j) { d.j = JSON.parse(d.j); }

          app.ports.rpcIn.send(d)

          // Is there a nicer way to do this?
          waitUntil(() => {
            return done
          }, 10000)
          .then((result) => {
            app.ports.rpcOut.unsubscribe(returnHandler)
            msgEmitter(response)
          })
          .catch((error) => {
            console.log(error)
            app.ports.rpcOut.unsubscribe(returnHandler)
          });

        } catch (error) {
          console.log(error)
          console.log(d)
        }

        break;

      // case "qr":
      //   break;

      case "c":
        msgInbound("onConnection", { s: d.s, c: d.c })
        break;

      case "d":
        msgInbound("onDisconnection", { s: d.s, c: d.c })
        break;

      case "fmu":
        // Frontend Model Update - forward to leader's Elm app
        if (app !== null && app.ports.receiveFrontendModelUpdate) {
          app.ports.receiveFrontendModelUpdate.send({
            s: d.s,
            c: d.c,
            fem: base64ToBytes(d.fem)
          });
        }
        break;

      case "fm":
        // Frontend Message - forward to leader's Elm app  
        console.log("[live.js] Received frontend message from follower:", d);
        if (app !== null && app.ports.receiveFrontendMessage) {
          console.log("[live.js] Forwarding to Elm app");
          app.ports.receiveFrontendMessage.send({
            s: d.s,
            c: d.c,
            msg: d.msg
          });
        } else {
          console.log("[live.js] WARNING: app or receiveFrontendMessage port not available");
        }
        break;

      case "tts":
        // Time Travel State - broadcast from leader to all followers
        if (app !== null && app.ports.receiveTimeTravelState) {
          app.ports.receiveTimeTravelState.send({
            index: d.index,
            frontendModels: d.frontendModels,
            bem: d.bem
          });
        }
        break;

      case "x":
        // Dummy msg to ignore, i.e. for initial backendModel state which is empty
        break;

      default:
        console.warn(`unexpected msg`, d)
    }
  }
}

function getRandomInt(max, min=0) {
  return Math.abs(Math.floor(Math.random() * Math.floor(max)) - min)
}

function getSessionId() {
  let sid = Cookie.get('sid')
  if (typeof sid == 'undefined') {
    // Make the cid look similar to production sec-websocket-key clientIds
    const newSid = getRandomInt(1000000,10000).toString().padEnd(40,"c04b8f7b594cdeedebc2a8029b82943b0a620815")
    Cookie.set('sid', newSid, { sameSite: 'lax' })
    return newSid
  } else {
    return sid
  }
}

var DEFAULT_INTERVAL = 50;
var DEFAULT_TIMEOUT = 5000;

function waitUntil(
  predicate,
  timeout,
  interval
) {
  var timerInterval = interval || DEFAULT_INTERVAL;
  var timerTimeout = timeout || DEFAULT_TIMEOUT;

  return new Promise(function promiseCallback(resolve, reject) {
    var timer;
    var timeoutTimer;
    var clearTimers;
    var doStep;

    clearTimers = function clearWaitTimers() {
      clearTimeout(timeoutTimer);
      clearInterval(timer);
    };

    doStep = function doTimerStep() {
      var result;

      try {
        result = predicate();

        if (result) {
          clearTimers();
          resolve(result);
        } else {
          timer = setTimeout(doStep, timerInterval);
        }
      } catch (e) {
        clearTimers();
        reject(e);
      }
    };

    timer = setTimeout(doStep, timerInterval);
    timeoutTimer = setTimeout(function onTimeout() {
      clearTimers();
      reject(new Error('Timed out after waiting for ' + timerTimeout + 'ms'));
    }, timerTimeout);
  });
}


function bytesToBase64(bytes_) {
  let base64 = '';
  const encodings = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  const bytes = new Uint8Array(bytes_.buffer);
  const byteLength = bytes.byteLength;
  const byteRemainder = byteLength % 3;
  const mainLength = byteLength - byteRemainder;

  let a;
  let b;
  let c;
  let d;
  let chunk;

  // Main loop deals with bytes in chunks of 3
  for (let i = 0; i < mainLength; i += 3) {
    // Combine the three bytes into a single integer
    chunk = (bytes[i] << 16) | (bytes[i + 1] << 8) | bytes[i + 2];

    // Use bitmasks to extract 6-bit segments from the triplet
    a = (chunk & 16515072) >> 18; // 16515072 = (2^6 - 1) << 18
    b = (chunk & 258048) >> 12; // 258048   = (2^6 - 1) << 12
    c = (chunk & 4032) >> 6; // 4032     = (2^6 - 1) << 6
    d = chunk & 63;        // 63       = 2^6 - 1

    // Convert the raw binary segments to the appropriate ASCII encoding
    base64 += encodings[a] + encodings[b] + encodings[c] + encodings[d];
  }

  // Deal with the remaining bytes and padding
  if (byteRemainder === 1) {
    chunk = bytes[mainLength];

    a = (chunk & 252) >> 2; // 252 = (2^6 - 1) << 2

    // Set the 4 least significant bits to zero
    b = (chunk & 3) << 4; // 3   = 2^2 - 1

    base64 += `${encodings[a]}${encodings[b]}==`;
  } else if (byteRemainder === 2) {
    chunk = (bytes[mainLength] << 8) | bytes[mainLength + 1];

    a = (chunk & 64512) >> 10; // 64512 = (2^6 - 1) << 10
    b = (chunk & 1008) >> 4; // 1008  = (2^6 - 1) << 4

    // Set the 2 least significant bits to zero
    c = (chunk & 15) << 2; // 15    = 2^4 - 1

    base64 += `${encodings[a]}${encodings[b]}${encodings[c]}=`;
  }

  return base64;
}

function base64ToBytes(b64) {
  return new DataView(Base64Binary.decodeArrayBuffer(b64))
}

var Base64Binary = {
	_keyStr : "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=",

	/* will return a  Uint8Array type */
  decodeArrayBuffer: function(input) {
    input = this.removePaddingChars(input);
    var bytes = (input.length/4) * 3;
    var ab = new ArrayBuffer(bytes);
    this.decode(input, ab);
    return ab;
  },

  removePaddingChars: function(input){
    var lkey = this._keyStr.indexOf(input.charAt(input.length - 1));
		var lkey2 = this._keyStr.indexOf(input.charAt(input.length - 2));
		if(lkey2 == 64 && lkey == 64){
      return input.substring(0,input.length - 2);
		} else if(lkey == 64){
      return input.substring(0,input.length - 1);
    }
    return input;
  },

	decode: function (input, arrayBuffer) {

		var bytes = parseInt((input.length / 4) * 3, 10);

		var uarray;
		var chr1, chr2, chr3;
		var enc1, enc2, enc3, enc4;
		var i = 0;
		var j = 0;

		if (arrayBuffer)
			uarray = new Uint8Array(arrayBuffer);
		else
			uarray = new Uint8Array(bytes);

		input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");

		for (i=0; i<bytes; i+=3) {
			//get the 3 octects in 4 ascii chars
			enc1 = this._keyStr.indexOf(input.charAt(j++));
			enc2 = this._keyStr.indexOf(input.charAt(j++));
			enc3 = this._keyStr.indexOf(input.charAt(j++));
			enc4 = this._keyStr.indexOf(input.charAt(j++));

			chr1 = (enc1 << 2) | (enc2 >> 4);
			chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
			chr3 = ((enc3 & 3) << 6) | enc4;

			uarray[i] = chr1;
			if (enc3 != 64) uarray[i+1] = chr2;
			if (enc4 != 64) uarray[i+2] = chr3;
		}

		return uarray;
	}
}
