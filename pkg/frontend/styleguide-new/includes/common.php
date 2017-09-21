<?php
#include "svgs/daniel_user.php";

/*
function iconWithNumber($ibuttonSizeClass, $iconName, $number, $moreClasses=[]) {
?>
	<div class="<?= $ibuttonSizeClass?> <?= $iconName?> <?= implode(" ", $moreClasses) ?>">
		<div class="number-top-right"><?= $number?></div>
	</div>					
<?php
}
*/
function iconWithNumber($ibuttonSizeClass, $svgFunction, $svgArgs, $number, $moreClasses=[]) {
?>
	<div class="<?= $ibuttonSizeClass?> <?= implode(" ", $moreClasses) ?>">
		<?= call_user_func_array($svgFunction, $svgArgs) ?>
		<div class="number-top-right"><?= $number?></div>
	</div>					
<?php
}

function icon($ibuttonSizeClass, $svgFunction, $svgArgs, $moreClasses=[]) {
?>
	<div class="<?= $ibuttonSizeClass?> <?= implode(" ", $moreClasses) ?>">
		<?= call_user_func_array($svgFunction, $svgArgs) ?>
	</div>					
<?php
}

function commonHeader() {
?>
		<div class="main-content_header">
			<div class="main-content_header_inner">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_orange', 'c_fill_note_bubble'], ['margin1']) ?>
			</div>
			
			<div class="main-content_header_inner">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_no_color', 'c_fill_interaction_yellow'], ['margin1']) ?>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_note_dark', 'c_fill_note_bubble'], ['margin1']) ?>
				<input class="search-input">
			</div>
		</div>
<?php	
}

