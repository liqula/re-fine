<?php 
include "includes/groupDetailsProcesses-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";
?>
<?php include "includes/page-header-main.php"; ?>

		<div class="groupDetailsProcesses">
			<?php if (isset($_GET['svg'])) { ?>
			<div class="groupDetailsProcesses__svg-div">
				<?php svgDanielUser('c_fill_interaction_yellow', ' c_fill_note_bubble'); ?>
			</div>			
			<?php } else { ?>
			<div class="groupDetailsProcesses__titleimage-div">
				<img class="groupDetailsProcesses__titleimage-img" src="testbild.png" alt="">
			</div>
			<img class="groupDetailsProcesses__profile-img" src="testbild.png" alt="">
			<?php } ?>
			<div class="groupDetailsProcesses__description">
				<div class="groupDetailsProcesses__description-headline">
					Das ist eine supertolle Gruppe
				</div>
				<div class="groupDetailsProcesses__description-text">
					Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus.Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus. Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus.
				</div>
			</div>
			<div class="hr-2-div"></div>
			<div class="groupDetailsProcesses__iconlist">
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], '12', ['margin1']) ?>
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], '12', ['margin1']) ?>
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], '12', ['margin1']) ?>
			</div>


			<div class="groupDetailsProcesses__submenu">
				<div class="groupDetailsProcesses__submenu-title">
					Dokumente der Gruppe (css ist eine Ausgeburt der Hölle)
				</div>
				<div class="groupDetailsProcesses__submenu-icons">
					<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
					<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
				</div>
			</div>

			<?php groupDetailsProcessesDocument(); ?>
			<?php groupDetailsProcessesDocument(); ?>
			<?php groupDetailsProcessesDocument(); ?>
			<?php groupDetailsProcessesDocument(); ?>
		</div>
<?php include "includes/page-footer-main.php"; ?>
