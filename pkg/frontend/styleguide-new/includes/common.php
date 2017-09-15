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
			<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], ['margin1',"float-left"]) ?>
			<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], ['margin1',"float-left"]) ?>
			
			<input class="search-input float-right">
			<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], ['margin1',"float-right"]) ?>
			<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], ['margin1',"float-right"]) ?>
			<div class="clearboth"></div>
		</div>
<?php	
}

