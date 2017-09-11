<?php
#include "svgs/daniel_user.php";

function iconWithNumber($ibuttonSizeClass, $iconName, $number, $moreClasses=[]) {
?>
	<div class="<?= $ibuttonSizeClass?> <?= $iconName?> <?= implode(" ", $moreClasses) ?>">
		<div class="number-top-right"><?= $number?></div>
	</div>					
<?php
}

function iconWithNumberInlineSvg($ibuttonSizeClass, $svgFunction, $svgArgs, $number, $moreClasses=[]) {
?>
	<div class="<?= $ibuttonSizeClass?> <?= implode(" ", $moreClasses) ?>">
		<?= call_user_func_array($svgFunction, $svgArgs) ?>
		<div class="number-top-right"><?= $number?></div>
	</div>					
<?php
}

function commonHeader() {
?>
		<div class="main-content_header">
			<div class="icon-daniel_test3 ibutton_xxlarge margin1 float-left"></div>
			<div class="icon-daniel_test2 ibutton_xxlarge margin1 float-left"></div>
			
			<input class="search-input float-right">
			<div class="test-icon ibutton_xxlarge margin1 float-right"></div>
			<div class="test-icon ibutton_xxlarge margin1 float-right"></div>
			<div class="clearboth"></div>
		</div>
<?php	
}

