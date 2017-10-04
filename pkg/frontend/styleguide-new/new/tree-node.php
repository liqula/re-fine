<?php 

function treeNode ($depth) {
	include "new/content-box.php";
	$n = rand(0, 3);
	if ($depth > 0) {
		for ($i=0; $i<$n; $i++) {
?>
<div class="discussion-thread-child">
	<div class="discussion-thread-child__icon-column">
		<div class="ibutton_xlarge">
			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
		</div>
	</div>
	<div class="discussion-thread-child__node-column">
		<?php treeNode ($depth - 1); ?>
	</div>
</div>
<?php
		}
	}
}

treeNode(5);