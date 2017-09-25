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
		<div class="main-content__header">
			<div class="main-content__header-inner">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<div class="main-content__header-label">Mein Dashboard</div>
			</div>
			
			<div class="main-content__header-inner">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_no_color', 'c_fill_interaction_yellow'], ['margin1']) ?>
				<div class="main-content__header-label">Profilseite</div>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_note_dark', 'c_fill_note_bubble'], ['margin1']) ?>
				<div class="main-content__header-label">Logout</div>
			</div>
		</div>
		
		<div class="hr-div"></div>

		<!-- TODO: rename css classes to more general names -->
		<div class="groupDetails__submenu">
			<div class="groupDetails__submenu-title">
				Dokumente der Gruppe (css ist eine Ausgeburt der Hölle)
			</div>
			<div class="groupDetails__submenu-icons">
				<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
				<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
			</div>
		</div>

		<div class="mainMenuGroups">

			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild.png',       NULL,                                                      'Stadtentwicklungspolitik der SPD', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info',     ["c_fill_interaction_yellow", "c_fill_yes_color"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild2.jpg',      NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info', ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'svgDanielUser',      ['c_fill_interaction_yellow', 'c_fill_note_bubble'],  'Initiative für eine grüne Stadt und so weiter und so fort', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_info',      ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild-error.png', NULL,                                                      'Initiative für eine grüne Stadt', 'svgDanielUser', ["c_fill_interaction_orange", "c_fill_interaction_yellow"], '12', 'svg_info', ["c_fill_yes_color", "c_fill_interaction_yellow"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>
			<?php mainMenuGroupsSmall(example_mainMenuGroupsSmall('c_bg_blue_dawn', 'testbild.png',       NULL,                                                      'Initiative für eine grüne Stadthsajkdhasjkdhaskjdhasjk', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"],         '12', 'svg_edit_new',  ["c_fill_yes_color", "c_fill_interaction_yellow", "c_fill_note_bubble"], '99', 'svgDanielUser', ["c_fill_yes_color", "c_fill_interaction_yellow"], '77')); ?>

		</div>			
