<?php 
include "new/common.php";
include "new/daniel_user.php";
//include "includes/svgs/daniel_test2.php";
//include "includes/svgs/svgs1.php";
?>
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<link rel="stylesheet" type="text/css" href="../scss-new/main.css" />
<style type="text/css">
</style>
<title> - </title>
</head>
<body>
<div class="body-container c_bg_blue_dark">

	<?php include "new/menupage-top.php"; ?>


	<div class="menupage-container c_bg_blue_light">
		<?php include "new/menupage-header.php"; ?>
		<div class="menupage-hr m-b-2"></div>
		<div class="group-details">
			<?php if (rand(0,1)) { ?>
			<div class="group-details__svg-div">
				<?php svgDanielUser('c_fill_interaction_yellow', ' c_fill_note_bubble'); ?>
			</div>			
			<?php } else { ?>
			<div class="group-details__titleimage-div">
				<img alt="" src="testbild.png" class="group-details__titleimage-img">
			</div>
			<img class="group-details__profile-img" src="testbild.png" alt="">
			<?php } ?>

			<div class="group-details__description">
				<div class="group-details__description-headline">
					Das ist eine supertolle Gruppe
				</div>
				<div class="group-details__description-text">
					Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus.Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus. Liquidus, nos mo corpore acipidus. Berum unt plia alit voluptat laborio. Ari cumendi aturesciusam quo et quiae. Or alis ped ma cum es eium qui si solor autasped et est recae volupta consequi debis abor suntus.
				</div>
			</div>
			<div class="menupage-hr-inner m-b-2"></div>
			<div class="menupage-iconlist">
					<?php include "new/ibutton-with-number.php"; ?>
					<?php include "new/ibutton-with-number.php"; ?>
					<?php include "new/ibutton-with-number.php"; ?>
			</div>

			<div class="menupage-section-header m-b-1">
				<div class="left-column">
					<div class="inner-column-1 menupage-section-header__label">
						Meine Gruppen
					</div>
				</div>
				<div class="right-column">
					<div class="inner-column-1">
						<div class="ibutton_xlarge">
							<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
						</div>
					</div>
					<div class="inner-column-1">
						<div class="ibutton_xlarge">
							<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
						</div>
					</div>
				</div>
			</div>

			<div class="document-tile-container">
				<?php include "new/document-tile.php"; ?>
				<?php include "new/document-tile.php"; ?>
				<?php include "new/document-tile.php"; ?>
				<?php include "new/document-tile-svg.php"; ?>
			</div>
		</div>
	</div>

</div>
