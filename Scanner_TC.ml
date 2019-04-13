type tc = {
	mutable last_tab_count : int;
	mutable current_tab_count : int;
}

let create () = { last_tab_count = 0; current_tab_count = 0 }

let incr_current_tab_count tc = 
	{ current_tab_count = tc.current_tab_count + 1; last_tab_count = tc.last_tab_count }

let decr_prev_tab_count tc = 
	{ current_tab_count = tc.current_tab_count; last_tab_count = tc.last_tab_count - 1 }

let adv_tab_count tc =
	{ last_tab_count = tc.current_tab_count; current_tab_count = 0 }
