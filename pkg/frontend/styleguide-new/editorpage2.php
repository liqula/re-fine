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
	<?php include "new/menupage-top-2.php" ?>

	<?php include "new/editorpage-header.php" ?>
	
	<div class="editorpage-body">
		
		<div class="editorpage-left-sidebar <?= randomBG() ?>">

			<div class="editorpage-left-sidebar__titlebar">
				<div class="editorpage-left-sidebar__titlebar-title">
					All Discussions
				</div>
				<div class="editorpage-left-sidebar__titlebar-icon">
					<div class="ibutton_xlarge">
						<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
					</div>
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


		<div class="editorpage-centerright-content">

<?php for ($i=0; $i<1; $i++) { ?>
			<?php include "new/content-box2.php"; ?>
			<?php } ?>

			<div class="discussion-thread-container">

				<div class="discussion-thread-container__text">
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
				</div>

				<div class="discussion-thread-container__tree">

					<div class="discussion-thread-root">
						<?php include "new/tree-node.php"; ?>

					</div>
				</div>




			</div>

		</div>
	</div>
	

</div>
