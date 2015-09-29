/*global: pubzub*/

var modScraper = (function() {
	var SUBSCRIBE_DEFAULT_PATH = "~site/mod_scraper/",
	    cache = {},
	    timeSinceCache = {},
		subscribe,
		updateUIStart,
		updateUIEnd,
		initTimeSince,
		updateTimeSince;
	
	/* options:
    id, elemId, subscribeEvents, callbackEventId
    */
	subscribe = function(options) {
	    var id = options.id;
	    if (cache[id]) {
	        return;
	    }
		if (!pubzub) {
			if (console) {
				console.log("pubzub not found");
			}
			return;
		}
		var eventMap = {};
		options.subscribeEvents.forEach(function(ev) {
		    eventMap[ev] = 1;
		});
		cache[id] = {
		    callbackEventId: options.callbackEventId,
		    elemId: options.elemId,
		    subscribeEvents: eventMap,
		    timeSinceEl: options.timeSinceEl
		};
		pubzub.subscribe(SUBSCRIBE_DEFAULT_PATH + id, function(topic, msg) {
			if (cache[id].subscribeEvents[msg.payload]) {
				z_event(cache[id].callbackEventId);

                if (msg.payload === "fetch_completed") {
                    setTimeout(function() {
                        updateUIEnd(id);
                    }, 1000);
                }
            }
		});
	};

	updateUIStart = function(id) {
		var el = document.getElementById(cache[id].elemId);
		if (el) {
			el.classList.add("updating");
		}
	};
	
	updateUIEnd = function(id) {
		var el = document.getElementById(cache[id].elemId);
		if (el) {
			el.classList.remove("updating");
		}
	};
	
	initTimeSince = function(options) {
	    var id = options.id;
	    if (!timeSinceCache[id]) {
	        timeSinceCache[id] = {};
	    }
        if (timeSinceCache[id].timeSinceTimer) {
            clearInterval(timeSinceCache[id].timeSinceTimer);
        }
        var start = moment(options.start, options.dateFormat);
        timeSinceCache[id] = {
            timeSinceTimer: setInterval(function() {
                updateTimeSince(id);
            }, 1000 * 15),
            start: start,
            timeSinceEl: options.timeSinceEl
        };
        updateTimeSince(id);
	};
	
	updateTimeSince = function(id) {
        timeSinceEl = document.getElementById(timeSinceCache[id].timeSinceEl);
        if (timeSinceEl) {
            var start = timeSinceCache[id].start;
            var now = moment();
            var since = start.from(now);
            timeSinceEl.innerHTML = since;
        }
	};
	
	return {
	    /*
	    options:
	    id, elemId, subscribeEvents, callbackEventId
	    */
		init: function(options) {
			subscribe(options);
		},
		
		/*
		options:
		id, timeSinceEl, start, dateFormat
		*/
		initTimeSince: function(options) {
		    initTimeSince(options);
		},
		
		prepareForUpdate: function(id) {
			updateUIStart(id);	
		}
	};
})();
