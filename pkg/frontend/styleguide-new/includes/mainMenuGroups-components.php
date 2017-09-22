<?php
#include "svgs/daniel_user.php";

// FIXME: rename to mainMenuGroupShort (that's the name of the corresponding haskell component.)
function mainMenuGroupsSmall($data){
?>
			<div class="mainMenuGroupShort <?= $data['background-color']?>"> 
				<?php if ($data['svgArgs']) { ?>
					<div class="mainMenuGroupShort__svg-div">
						<?php call_user_func_array($data['image'], $data['svgArgs']) ?>
					</div>
				<?php } else { ?>
					<div class="mainMenuGroupShort__image-div">
						<img class="mainMenuGroupShort__image" src="<?= $data['image']?>" alt="" >
					</div>
				<?php } ?>
				<div class="mainMenuGroupShort__groupname"><?= $data['groupname']?></div>
				<div class="mainMenuGroupShort__iconlist">
					<?= iconWithNumber('ibutton_xxxlarge', $data['icons'][0]['name'], $data['icons'][0]['args'], $data['icons'][0]['number']) ?>
					<?= iconWithNumber('ibutton_xxxlarge', $data['icons'][1]['name'], $data['icons'][1]['args'], $data['icons'][1]['number']) ?>
					<?= iconWithNumber('ibutton_xxxlarge', $data['icons'][2]['name'], $data['icons'][2]['args'], $data['icons'][2]['number']) ?>
					
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
	'background-color' => 'c_bg_blue_dawn',
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
	'background-color' => 'c_bg_blue_dawn',
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
	'background-color' => 'c_bg_interaction_orange_light',
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
