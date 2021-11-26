jQuery.fn.dataTable.render.ellipsis = function ( cutoff, wordbreak, escapeHtml ) {
	return function ( d, type, row ) {
		// Order, search and type get the original data
		if ( type !== 'display' ) {
			return d;
		}

		if ( typeof d !== 'number' && typeof d !== 'string' ) {
			return d;
		}

		d = d.toString(); // cast numbers

		if ( d.length <= cutoff ) {
			return d;
		}

		var shortened = d.substr(0, cutoff-1);

		// Find the last white space character in the string
		if ( wordbreak ) {
			shortened = shortened.replace(/\s([^\s]*)$/, '');
		}

		return '<span class="ellipsis" title="'+d+'">'+shortened+'&#8230;</span>';
	};
};
