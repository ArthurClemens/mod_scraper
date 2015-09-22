/*global: pubzub*/

var modScraper = (function() {
	var SUBSCRIBE_DEFAULT_PATH = "~site/mod_scraper/",
		DATA_EL = "scraper_data",
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
		var el = document.getElementById(DATA_EL);
		if (el) {
			el.classList.add("updating");
		}
	}
	
	endUpdateState = function() {
		var el = document.getElementById(DATA_EL);
		if (el) {
			el.classList.remove("updating");
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
