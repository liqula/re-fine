<?php 
include "includes/mainMenuGroups-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";
?>
<?php include "includes/page-header-main.php"; ?>
<div class="body-container c_bg_blue_dark">

	<?= icon('ibutton_absolute-topleft', 'svg_close', ['c_fill_blue_night', ' c_fill_note_bubble'], ['ibutton_xxlarge']) ?>
	<div class="platform-title"></div>
	<?= icon('ibutton_absolute-topright', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['ibutton_xxlarge']) ?>

	<div class="main-content">
		<?= commonHeader() ?>
		<div class="hr-div"></div>

		<div class="mainMenuGroups">

			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild.png',       NULL,                                                      'Stadtentwicklungspolitik der SPD', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info',     ["c_fill_interaction_yellow", "c_fill_yes_color"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild2.jpg',      NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info', ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'svgDanielUser',      ['c_fill_interaction_yellow', 'c_fill_note_bubble'],  'Initiative für eine grüne Stadt und so weiter und so fort', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info',      ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild-error.png', NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_interaction_orange", "c_fill_interaction_yellow"], '12', 'svg_info', ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild.png',       NULL,                                                      'Initiative für eine grüne Stadthsajkdhasjkdhaskjdhasjk', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_edit_new',  ["c_fill_yes_color", "c_fill_interaction_yellow", "c_fill_note_bubble"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>

		</div>			
	</div>
</div>