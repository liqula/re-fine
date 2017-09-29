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
		<div class="menu-form m-t-1">
			<div class="menu-form-header">
				<div class="left-column">
					<div class="inner-column-1 menu-form-header__label">
						Neue Gruppe
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
			<div class="menupage-hr-inner m-b-1"></div>

			
			<div class="menu-form__input-div m-b-1">
				<input class="menu-form__input">
			</div>
			
			<div class="menu-form-section m-b-1">
				<div class="left-column">
					<div class="menu-form-section__label">
						Gib hier die Beschreibung ein
					</div>
				</div>
				<div class="right-column">
					<div class="menu-form-section__label">
						10/100
					</div>
				</div>
			</div>

			<div class="menu-form__input-div m-b-1">
				<textarea class="menu-form__textarea"></textarea>
			</div>

			<div class="menupage-hr-inner m-b-1"></div>
			
			<div class="menu-form-section m-b-1">
				<div class="left-column">
					<div class="menu-form-section__label">
						Bilder-Upload
					</div>
				</div>
			</div>

			<div class="menu-form-submenu m-b-1">
			  	<div class="menu-form-submenu__item-container">
			    	<div class="menu-form-submenu__item-ibutton">
			    		<div class="ibutton_xlarge">
			    			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
			    		</div>
			    	</div>
			    	<div class="menu-form-submenu__item-label">
			    		Profilbild
			    	</div>
			    	<div class="menu-form-submenu__item-help">
			    		<div class="ibutton_xlarge">
			    			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
			    		</div>
			    	</div>
			  	</div>

				<div class="menu-form-submenu__item-container">
			    	<div class="menu-form-submenu__item-ibutton">
			    		<div class="ibutton_xlarge">
			    			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
			    		</div>
			    	</div>
			    	<div class="menu-form-submenu__item-label">
			    		Profilbild
			    	</div>
			    	<div class="menu-form-submenu__item-help">
			    		<div class="ibutton_xlarge">
			    			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
			    		</div>
			    	</div>
			  	</div>
			</div>

			<div class="menupage-hr-inner m-b-1"></div>


			<div class="menu-form-submenu m-b-1">
			  	<div class="menu-form-submenu__item-container">
			    	<div class="menu-form-submenu__item-ibutton">
			    		<div class="ibutton_xlarge">
			    			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
			    		</div>
			    	</div>
			    	<div class="menu-form-submenu__item-label">
			    		Nutzer einladen
			    	</div>
			  	</div>
			</div>

			<div class="menu-form__input-div m-b-1">
				<input class="menu-form__input">
			</div>

			<div class="menupage-hr-inner m-b-1"></div>

			<div class="menu-form-submenu m-b-1">
			  	<div class="menu-form-submenu__item-container">
			    	<div class="menu-form-submenu__item-ibutton">
			    		<div class="ibutton_xxlarge">
			    			<?php svgDanielUser('c_fill_interaction_yellow', 'c_fill_blue_dawn'); ?>
			    		</div>
			    	</div>
			  	</div>
			</div>

		</div>	
	</div>

</div>
