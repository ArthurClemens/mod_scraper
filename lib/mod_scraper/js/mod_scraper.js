/*global: pubzub*/

var modScraper = (function() {
	var SUBSCRIBE_DEFAULT_PATH = "~site/mod_scraper/",
	    cache = {},
		subscribe,
		startUpdateState,
		endUpdateState;
	
	subscribe = function(id, elemId, subscribeEvents, callbackEventId) {
		if (!pubzub) {
			if (console) {
				console.log("pubzub not found");
			}
			return;
		}
		var eventMap = {};
		subscribeEvents.forEach(function(ev) {
		    eventMap[ev] = 1;
		});
		cache[id] = {
		    callbackEventId: callbackEventId,
		    elemId: elemId,
		    subscribeEvents: eventMap
		};

		pubzub.subscribe(SUBSCRIBE_DEFAULT_PATH + id, function(topic, msg) {
			if (cache[id].subscribeEvents[msg.payload]) {
				z_event(cache[id].callbackEventId);

                if (msg.payload === "fetch_completed") {
                    setTimeout(function() {
                        endUpdateState(id);
                    }, 50);
                }
            }
		});
	};
	
	startUpdateState = function(id) {
		var el = document.getElementById(cache[id].elemId);
		if (el) {
			el.classList.add("updating");
		}
	};
	
	endUpdateState = function(id) {
		var el = document.getElementById(cache[id].elemId);
		if (el) {
			el.classList.remove("updating");
		}
	};
	
	return {
		init: function(id, elemId, subscribeEvents, callbackEventId) {
			subscribe(id, elemId, subscribeEvents, callbackEventId);
		},
		prepareForUpdate: function(id) {
			startUpdateState(id);	
		}
	}
})();
