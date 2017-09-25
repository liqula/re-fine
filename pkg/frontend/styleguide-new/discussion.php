<?php 
include "includes/common.php";
include "includes/svgs/daniel_user.php";
include "includes/svgs/daniel_test2.php";
include "includes/svgs/svgs1.php";

function discussionThreadItem () {
?>
<div class="discussion-thread-item">
	<div class="discussion-thread-item__header">
		<div class="discussion-thread-item__header-inner">
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
		</div>
		<div class="discussion-thread-item__header-inner">
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
		</div>
	</div>
	<div class="discussion-thread-item__body">
		lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
		lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
		lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
		lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
		lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
		lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum lore ipsum 
	</div>
	<div class="discussion-thread-item__footer">
		<div class="discussion-thread-item__footer-inner">
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
		</div>
		<div class="discussion-thread-item__footer-inner">
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
		</div>
	</div>
</div>
<?php }





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

	<div class="discussion-header">
		
		<div class="discussion-header__inner">
			<div class="discussion-header__column1">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
			</div>

			<div class="discussion-header__column2">
				<div class="discussion-header__column2-spacer"></div>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<div class="discussion-header__column2-spacer"></div>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
				<div class="discussion-header__column2-spacer"></div>
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
			</div>
		</div>

		<div class="discussion-header__inner">
			<div class="discussion-header__column3">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
			</div>

			<div class="discussion-header__column4">
				<?= icon('ibutton_xxlarge', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
			</div>
		</div>
	</div>

	<div class="discussion-pagecontent" id="discussion-pagecontent">
		<div class="discussion-sidebar-container" id="discussion-sidebar-container">

			<div class="discussion-sidebar">
				<div class="discussion-sidebar__titlebar">
					<div class="discussion-sidebar__titlebar-title">
						All Discussions
					</div>
					<div class="discussion-sidebar__titlebar-icon">
						<?= icon('ibutton_large', 'svgDanielUser', ['c_fill_interaction_yellow', 'c_fill_blue_dawn'], ['margin1']) ?>
					</div>
				</div>

				<?php for ($i=0; $i<20; $i++) { ?>

				<div class="discussion-sidebar__preview">
					<div class="discussion-sidebar__preview-icon">
						<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
					</div>
					<div class="discussion-sidebar__preview-inner">
						<div class="discussion-sidebar__preview-inner-text">
							It is a long established fact that a reader will be distracted by the readable content of a page when looking at its layout. The point of using Lorem Ipsum is that it has a more-or-less normal distribution of letters, as opposed to using 'Content here, content here', making it look like readable English. 
						</div>
						<?php if (rand(0,100)>50) { ?>
							<div class="discussion-sidebar__preview-inner-bottom"></div>
						<?php } ?>
					</div>
				</div>
				<?php } ?>
			</div>
		</div>

		<div class="discussion-thread-container">
			<div class="discussion-thread-container__text">
			lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum 
			lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum lore isum 
			</div>
			<div class="discussion-thread-container__tree">
				<div class="discussion-thread-item-root">
					<?php discussionThreadItem(); ?>
					<div class="discussion-thread-item-child">
						<div class="discussion-thread-item-child__icon-column">
							<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
						</div>
						<div class="discussion-thread-item-child__node-column">
							<?php discussionThreadItem(); ?>
						</div>
					</div>
					<div class="discussion-thread-item-child">
						<div class="discussion-thread-item-child__icon-column">
							<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
						</div>
						<div class="discussion-thread-item-child__node-column">
							<?php discussionThreadItem(); ?>
							<div class="discussion-thread-item-child">
								<div class="discussion-thread-item-child__icon-column">
									<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
								</div>
								<div class="discussion-thread-item-child__node-column">
									<?php discussionThreadItem(); ?>
								</div>
							</div>
							<div class="discussion-thread-item-child">
								<div class="discussion-thread-item-child__icon-column">
									<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
								</div>
								<div class="discussion-thread-item-child__node-column">
									<?php discussionThreadItem(); ?>
								</div>
							</div>
						</div>
					</div>
					<div class="discussion-thread-item-child">
						<div class="discussion-thread-item-child__icon-column">
							<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
						</div>
						<div class="discussion-thread-item-child__node-column">
							<?php discussionThreadItem(); ?>
							<div class="discussion-thread-item-child">
								<div class="discussion-thread-item-child__icon-column">
									<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
								</div>
								<div class="discussion-thread-item-child__node-column">
									<?php discussionThreadItem(); ?>
								</div>
							</div>
						</div>
					</div>
				</div>
			</div>
		</div>
	</div>
</div>
<script type="text/javascript">
	(function(){
		//var sidebarContainer = document.getElementById("discussion-sidebar-container");
		var sidebarContainer = document.getElementById("discussion-pagecontent");
		window.onresize = function (e) {
			console.log(window.innerHeight - 48);
			sidebarContainer.style.height = (window.innerHeight - 48) + "px";
		};
		var event = document.createEvent('HTMLEvents');
		event.initEvent('resize', true, false);
		window.dispatchEvent(event);
	})();

</script>
