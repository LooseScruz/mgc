type tc = {
	mutable last_tab_count : int;
	mutable current_tab_count : int;
}

let create () = { last_tab_count = 0; current_tab_count = 0 }

let incr_current_tab_count tc = 
	{ current_tab_count = tc.current_tab_count + 1; last_tab_count = tc.last_tab_count }
