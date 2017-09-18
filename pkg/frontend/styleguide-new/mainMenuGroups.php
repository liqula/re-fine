<?php 
include "includes/mainMenuGroups-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";
?>
<?php include "includes/page-header-main.php"; ?>
		<div class="mainMenuGroup">

			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild.png',       NULL,                                                      'Stadtentwicklungspolitik der SPD', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info',     ["c_fill_interaction_yellow", "c_fill_yes_color"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild2.jpg',      NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info', ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'svgDanielUser',      ['c_fill_interaction_yellow', 'c_fill_note_bubble'],  'Initiative für eine grüne Stadt und so weiter und so fort', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info',      ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild-error.png', NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_interaction_orange", "c_fill_interaction_yellow"], '12', 'svg_info', ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild.png',       NULL,                                                      'Initiative für eine grüne Stadthsajkdhasjkdhaskjdhasjk', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_edit_new',  ["c_fill_yes_color", "c_fill_interaction_yellow", "c_fill_note_bubble"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			
			<?php #mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', '../images/daniel_test1.svg', 'Initiative für eine grüne Stadt', 'icon-daniel_test1', '12', 'icon-daniel_test2', '99', 'icon-daniel_test3', '77')); ?>
			<?php #mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_interaction_orange_light', '../images/daniel_test1.svg', 'Initiative für eine grüne Stadt', 'icon-daniel_test1', '12', 'icon-daniel_test2', '99', 'icon-daniel_test3', '77')); ?>
			<?php #mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_interaction_orange_light', '../images/daniel_test1.svg', 'Initiative für eine grüne Stadtentwicklungsagentur', 'icon-daniel_test1', '12', 'icon-daniel_test2', '99', 'icon-daniel_test3', '77')); ?>
		</div>			
<?php include "includes/page-footer-main.php"; ?>
