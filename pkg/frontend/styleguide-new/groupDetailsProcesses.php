<?php 
include "includes/groupDetailsProcesses-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";
?>
<?php include "includes/page-header-main.php"; ?>

		<div class="groupDetailsProcesses">
			<div class="groupDetailsProcesses_titleimage-div">
				<img class="groupDetailsProcesses_titleimage-img" src="testbild.png" alt="">
			</div>
			<img class="groupDetailsProcesses_profile-img" src="testbild.png" alt="">
		
			<div class="groupDetailsProcesses_description">
				<div class="groupDetailsProcesses_description-headline">
				Das ist eine supertolle Gruppe
				</div>
				<div class="groupDetailsProcesses_description-text">
					Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus.Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus. Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus.
				</div>
			</div>
			<div class="hr-2-div"></div>
			<div class="groupDetailsProcesses_iconlist">
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], '12', ['margin1',"float-left"]) ?>
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], '12', ['margin1',"float-left"]) ?>
				<?= iconWithNumber('ibutton_xxxlarge', 'svgDanielUser', ['c_fill_interaction_yellow_neon', ' c_fill_note_bubble'], '12', ['margin1',"float-left"]) ?>
				<div class="clearboth"></div>
			</div>


			<div class="groupDetailsProcesses_submenu">
				<div class="groupDetailsProcesses_submenu-title">
					Dokumente der Gruppe (css ist eine Ausgeburt der Hölle)
				</div>
				<div class="test-icon ibutton_xlarge margin1 float-right"></div>
				<div class="test-icon ibutton_xlarge margin1 float-right"></div>
				<div class="clearboth"></div>
			</div>

			<?php groupDetailsProcessesDocument(); ?>
			<?php groupDetailsProcessesDocument(); ?>
			<?php groupDetailsProcessesDocument(); ?>
			<?php groupDetailsProcessesDocument(); ?>
		</div>
<?php include "includes/page-footer-main.php"; ?>
