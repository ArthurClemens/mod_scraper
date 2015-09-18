/*global: pubzub*/

var modScraper = (function() {
	var SUBSCRIBE_DEFAULT_PATH = "~site/mod_scraper/",
		RESULTS_ID = "run_results",
		subscribe,
		startUpdateState,
		endUpdateState;
	
	subscribe = function(id) {
		if (!pubzub) {
			if (console) {
				console.log("pubzub not found");
			}
			return;
		}
		pubzub.subscribe(SUBSCRIBE_DEFAULT_PATH + id, function(topic, msg) {
			if (msg.payload === "fetch_completed") {
				z_event("update_run_results");
				setTimeout(function() {
					endUpdateState();
				}, 50);
			}
		});
	};
	
	startUpdateState = function() {
		var resultsEl = document.getElementById(RESULTS_ID);
		if (resultsEl) {
			resultsEl.classList.add("updating");
		}
	}
	
	endUpdateState = function() {
		var resultsEl = document.getElementById(RESULTS_ID);
		if (resultsEl) {
			resultsEl.classList.remove("updating");
		}
	}
	
	return {
		init: function(id) {
			subscribe(id);
		},
		prepareForUpdate: function() {
			startUpdateState();	
		}
	}
})();
