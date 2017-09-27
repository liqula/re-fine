<?php 
include "includes/mainMenuGroups-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";

function dashboardItem() {
?>
	<div class="dashboard-item c_bg_blue_dawn"> 
		<div class="dashboard-item__image-div">
			<img class="dashboard-item__image" src="testbild.png" alt="" >
		</div>
		<div class="dashboard-item__groupname">Super Groupname</div>
		<div class="dashboard-item__iconlist">
			<?= iconWithNumber('ibutton_xxlarge', 'svgDanielUser', ['c_fill_blue_night', ' c_fill_note_bubble'], '12') ?>
			<?= iconWithNumber('ibutton_xxlarge', 'svgDanielUser', ['c_fill_blue_night', ' c_fill_note_bubble'], '12') ?>
			<?= iconWithNumber('ibutton_xxlarge', 'svgDanielUser', ['c_fill_blue_night', ' c_fill_note_bubble'], '12') ?>
		</div>
	</div>
<?php }

function dashboardItemSVG() {
?>
	<div class="dashboard-item c_bg_blue_dawn"> 
		<div class="dashboard-item__svg-div">
			<?php svgDanielUser('c_fill_blue_night', ' c_fill_note_bubble'); ?>
		</div>
		<div class="dashboard-item__groupname">Super Groupname</div>
		<div class="dashboard-item__iconlist">
			<?= iconWithNumber('ibutton_xxlarge', 'svgDanielUser', ['c_fill_blue_night', ' c_fill_note_bubble'], '12') ?>
			<?= iconWithNumber('ibutton_xxlarge', 'svgDanielUser', ['c_fill_blue_night', ' c_fill_note_bubble'], '12') ?>
			<?= iconWithNumber('ibutton_xxlarge', 'svgDanielUser', ['c_fill_blue_night', ' c_fill_note_bubble'], '12') ?>
		</div>
	</div>
<?php }

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

		<div class="dashboard-item-list-container">
			<?php dashboardItem(); ?>
			<?php dashboardItemSVG(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItemSVG(); ?>
			<?php dashboardItem(); ?>
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

		<div class="dashboard-item-list-container">
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
			<?php dashboardItem(); ?>
		</div>
	</div>
</div>