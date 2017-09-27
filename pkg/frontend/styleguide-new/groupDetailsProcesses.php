<?php 
include "includes/groupDetailsProcesses-components.php";
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


		<div class="groupDetails">
			<?php if (isset($_GET['svg'])) { ?>
			<div class="groupDetails__svg-div">
				<?php svgDanielUser('c_fill_interaction_yellow', ' c_fill_note_bubble'); ?>
			</div>			
			<?php } else { ?>
			<div class="groupDetails__titleimage-div">
				<img class="groupDetails__titleimage-img" src="testbild.png" alt="">
			</div>
			<img class="groupDetails__profile-img" src="testbild.png" alt="">
			<?php } ?>
			<div class="groupDetails__description">
				<div class="groupDetails__description-headline">
					Das ist eine supertolle Gruppe
				</div>
				<div class="groupDetails__description-text">
					Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus.Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus. Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus.
				</div>
			</div>
			<div class="hr-2-div"></div>
			<div class="groupDetails__iconlist">
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], '12', ['margin1']) ?>
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], '12', ['margin1']) ?>
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], '12', ['margin1']) ?>
			</div>


			<div class="groupDetails__submenu">
				<div class="groupDetails__submenu-title">
					Dokumente der Gruppe (css ist eine Ausgeburt der Hölle)
				</div>
				<div class="groupDetails__submenu-icons">
					<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
					<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
				</div>
			</div>

			<?php groupDetailsDocument(); ?>
			<?php groupDetailsDocument(); ?>
			<?php groupDetailsDocument(); ?>
			<?php groupDetailsDocument(); ?>
		</div>
	</div>
</div>
