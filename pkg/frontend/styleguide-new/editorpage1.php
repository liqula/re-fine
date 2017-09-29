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
<div class="body-container">

	<?php include "new/editorpage-header.php" ?>
	

	<div class="editorpage-body">
		<div class="editorpage-left-sidebar <?= randomBG() ?>">
		
			<div class="editorpage-left-sidebar__titlebar">
				<div class="editorpage-left-sidebar__titlebar-title">
					All Discussions
				</div>
				<div class="ibutton_xlarge">
					<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
				</div>
			</div>

			<?php for ($i=0; $i<20; $i++) { ?>

			<div class="editorpage-left-sidebar__preview">
				<div class="editorpage-left-sidebar__preview-icon">
					<div class="ibutton_xlarge">
						<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
					</div>
				</div>
				<div class="editorpage-left-sidebar__preview-inner <?= randomBG() ?>">
					<div class="editorpage-left-sidebar__preview-inner-text">
						It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. 
					</div>
					<?php if (rand(0,100)>50) { ?>
						<div class="editorpage-left-sidebar__preview-inner-bottom"></div>
					<?php } ?>
				</div>
			</div>
			<?php } ?>


		</div>
		<div class="editorpage-center-content">
			<div class="editorpage-center-content__text">
					Ectio. Sed eatibus sitatibus et atus doluptas dolorumet endi bearchil ipidunt pa quia velenimus nis volo denet andias nesci dolute nihic te sant et volorpo sapeliquis ea velles endit volorum quate venda atiorem ea consequis dolupta muscita tempore quam quunto beribus.
Evenis sit as quae mo id estionserum que secum, sam quunt dus, cusa verferendae inverfero officimus derionse dolut aspis voluptis solent id molorum renem rat doloreptur? Entem rercita tquaeri tiandelibus dolor sunt, tem eates sequi corerrumquo tem quia comnimp oriorpos volupta<br><br>
Ectio. Sed eatibus sitatibus et atus doluptas dolorumet endi bearchil ipidunt pa quia velenimus nis volo denet andias nesci dolute nihic te sant et volorpo sapeliquis ea velles endit volorum quate venda atiorem ea consequis dolupta muscita tempore quam quunto beribus.<br><br>
Evenis sit as quae mo id estionserum que secum, sam quunt dus, cusa verferendae inverfero officimus derionse dolut aspis voluptis solent id molorum renem rat doloreptur? Entem rercita tquaeri tiandelibus dolor sunt, tem eates sequi corerrumquo tem quia comnimp oriorpos volupta<br><br>
Ectio. Sed eatibus sitatibus et atus doluptas dolorumet endi bearchil ipidunt pa quia velenimus nis volo denet andias nesci dolute nihic te sant et volorpo sapeliquis ea velles endit volorum quate venda atiorem ea consequis dolupta muscita tempore quam quunto beribus.<br><br>
Evenis sit as quae mo id estionserum que secum, sam quunt dus, cusa verferendae inverfero officimus derionse dolut aspis voluptis solent id molorum renem rat doloreptur? Entem rercita tquaeri tiandelibus dolor sunt, tem eates sequi corerrumquo tem quia comnimp oriorpos volupta Evenis sit as quae mo id estionserum que secum, sam quunt dus, cusa verferendae inverfero officimus derionse dolut aspis vo- luptis solent id molorum renem rat doloreptur? Entem rercita tquaeri tiandelibus dolor sunt, tem eates sequi corer<br><br>
re, quam qui si re venis es atur aligend end- unt rat ulpa quasi senda quamus delis pos es aut ut quae ped quo earum res quam ut re, cum eaquam ut est, comnis aliquam, aditius entin nonsequae nonsequo cus estio bla
			</div>

			<?php for ($i=0; $i<1; $i++) { ?>
			<?php include "new/content-box2.php"; ?>
			<?php } ?>


		</div>
		<div class="editorpage-right-sidebar">

			
			<?php for ($i=0; $i<20; $i++) { ?>

			<div class="editorpage-right-sidebar__preview">
				<div class="editorpage-right-sidebar__preview-icon">
					<div class="ibutton_xlarge">
						<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
					</div>
				</div>
				<div class="editorpage-right-sidebar__preview-inner <?= randomBG() ?>">
					<div class="editorpage-right-sidebar__preview-inner-text">
						It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. 
					</div>
					<?php if (rand(0,100)>50) { ?>
						<div class="editorpage-right-sidebar__preview-inner-bottom"></div>
					<?php } ?>
				</div>
			</div>
			<?php } ?>


		</div>
	</div>
	

</div>
