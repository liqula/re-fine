<?php

function randomBG() {
	$bg = ['c_bg_green_background', 'c_bg_note_bubble', 'c_bg_discussion_bubble', 'c_bg_red_background', 'c_bg_interaction_orange'];
	return $bg[array_rand($bg)];
}

