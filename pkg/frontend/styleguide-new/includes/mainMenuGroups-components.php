<?php
#include "svgs/daniel_user.php";

function mainMenuGroupsSmall($data){
?>
			<div class="mainMenuGroupShort <?= $data['background-color']?>"> 
				<?php if ($data['svgArgs']) { ?>
					<div class="mainMenuGroupShort_svg-div"><div class="on_top">
					<?php call_user_func_array($data['image'], $data['svgArgs']) ?>
					</div></div>
				<?php } else { ?>
					<div class="mainMenuGroupShort_image-div">
						<img class="mainMenuGroupShort_image" src="<?= $data['image']?>" alt="" >
					</div>
				<?php } ?>
				<div class="mainMenuGroupShort_groupname"><?= $data['groupname']?></div>
				<div class="mainMenuGroupShort_iconlist">
					<?= iconWithNumberInlineSvg('ibutton_xxxlarge', $data['icons'][0]['name'], $data['icons'][0]['args'], $data['icons'][0]['number']) ?>
					<?= iconWithNumberInlineSvg('ibutton_xxxlarge', $data['icons'][1]['name'], $data['icons'][1]['args'], $data['icons'][1]['number']) ?>
					<?= iconWithNumberInlineSvg('ibutton_xxxlarge', $data['icons'][2]['name'], $data['icons'][2]['args'], $data['icons'][2]['number']) ?>
					
				</div>
			</div>


<?php
}

function example_mainMenuGroupsSmall($backgroundColor, $imageOrSVGFunc, $svgArgs, $groupname, $iconName0, $iconArgs0, $number0, $iconName1, $iconArgs1, $number1, $iconName2, $iconArgs2, $number2) {
	return [
		'background-color' => $backgroundColor,
		'image' => $imageOrSVGFunc,
		'svgArgs' => $svgArgs,
		'groupname' => $groupname,
		'icons' => [
			[
				'name' => $iconName0,
				'args' => $iconArgs0,
				'number' => $number0,
			], 
			[
				'name' => $iconName1,
				'args' => $iconArgs1, 
				'number' => $number1,
			], 
			[
				'name' => $iconName2,
				'args' => $iconArgs2, 
				'number' => $number2,
			], 
		]
	];
}

/*
$mainMenuGroupsSmall_example1 = [
	'background-color' => 'c_bg_main_menu_blue',
	'image' => 'testbild.png',
	'groupname' => 'Initiative für eine grüne Stadt',
	'icons' => [
		[
			'icon-name' => 'icon-daniel_test1',
			'number' => '12'
		], 

		[
			'icon-name' => 'icon-daniel_test2',
			'number' => '59'
		], 

		[
			'icon-name' => 'icon-daniel_test3',
			'number' => '72'
		], 
	]
];

$mainMenuGroupsSmall_example2 = [
	'background-color' => 'c_bg_main_menu_blue',
	'image' => '../images/daniel_test1.svg',
	'groupname' => 'Initiative für eine grüne Stadt',
	'icons' => [
		[
			'icon-name' => 'icon-daniel_test1',
			'number' => '12'
		], 

		[
			'icon-name' => 'icon-daniel_test2',
			'number' => '59'
		], 

		[
			'icon-name' => 'icon-daniel_test3',
			'number' => '72'
		], 
	]
];

$mainMenuGroupsSmall_example3 = [
	'background-color' => 'c_bg_form_orange_light',
	'image' => '../images/daniel_test1.svg',
	'groupname' => 'Initiative für eine grüne Stadt',
	'icons' => [
		[
			'icon-name' => 'icon-daniel_test1',
			'number' => '12'
		], 

		[
			'icon-name' => 'icon-daniel_test2',
			'number' => '59'
		], 

		[
			'icon-name' => 'icon-daniel_test3',
			'number' => '72'
		], 
	]
];
*/
