<?php 
include "includes/mainMenuGroups-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";
?>
<?php include "includes/page-header-main.php"; ?>
		<div class="mainMenuGroup">

			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_main_menu_blue', 'testbild.png',       NULL,                                                      'Stadtentwicklungspolitik der SPD', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"],         '12', 'svg_info',     ["c_fill_interaction_yellow_neon", "c_fill_form_green"], '99', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_main_menu_blue', 'testbild2.jpg',      NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"],         '12', 'svg_info', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '99', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_main_menu_blue', 'svgDanielUser',      ['c_fill_interaction_yellow_neon', 'c_fill_note_bubble'],  'Initiative für eine grüne Stadt und so weiter und so fort', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"],         '12', 'svg_info',      ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '99', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_main_menu_blue', 'testbild-error.png', NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_interaction_orange", "c_fill_interaction_yellow_neon"], '12', 'svg_info', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '99', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_main_menu_blue', 'testbild.png',       NULL,                                                      'Initiative für eine grüne Stadthsajkdhasjkdhaskjdhasjk', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"],         '12', 'svg_edit_new',  ["c_fill_form_green", "c_fill_interaction_yellow_neon", "c_fill_note_bubble"], '99', 'svgDanielUser', ["c_fill_form_green", "c_fill_interaction_yellow_neon"], '77')); ?>
			
			<?php #mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_main_menu_blue', '../images/daniel_test1.svg', 'Initiative für eine grüne Stadt', 'icon-daniel_test1', '12', 'icon-daniel_test2', '99', 'icon-daniel_test3', '77')); ?>
			<?php #mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_form_orange_light', '../images/daniel_test1.svg', 'Initiative für eine grüne Stadt', 'icon-daniel_test1', '12', 'icon-daniel_test2', '99', 'icon-daniel_test3', '77')); ?>
			<?php #mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_form_orange_light', '../images/daniel_test1.svg', 'Initiative für eine grüne Stadtentwicklungsagentur', 'icon-daniel_test1', '12', 'icon-daniel_test2', '99', 'icon-daniel_test3', '77')); ?>
		</div>			
<?php include "includes/page-footer-main.php"; ?>
