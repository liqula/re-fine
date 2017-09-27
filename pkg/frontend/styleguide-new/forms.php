<?php 
include "includes/mainMenuGroups-components.php";
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";

?>
<?php include "includes/page-header-main.php"; ?>
<div class="body-container">

	<div class="form__container">
		<div class="form__header">
			<div class="form__header-inner-div">
				<div class="form__header-label-fat">Ein Formular</div>
			</div>
			
			<div class="form__header-inner-div">
				<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_no_color', 'c_fill_interaction_yellow'], ['margin1']) ?>
				<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_note_dark', 'c_fill_note_bubble'], ['margin1']) ?>
			</div>
		</div>

		<textarea class="form__textarea"></textarea>
		<textarea class="form__textarea border_red"></textarea>
		<textarea class="form__textarea border_blue"></textarea>

		<!-- TODO: rename css classes to more general names -->
		<div class="form__submenu">
			<div class="form__submenu-title">
				Dokumente der Gruppe (css ist eine Ausgeburt der Hölle)
			</div>
			<div class="form__submenu-icons">
				<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
				<?= icon('ibutton_xlarge', 'svgDanielUser', ['c_fill_interaction_yellow', ' c_fill_note_bubble'], ['margin1']) ?>
			</div>
		</div>
		<textarea class="form__textarea border_blue"></textarea>
		<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_note_bubble'], ['margin1']) ?>
	</div>

</div>