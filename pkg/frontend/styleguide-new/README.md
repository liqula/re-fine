New Styleguide in

    pkg/frontend/styleguide-new

New CSS in

    pkg/frontend/scss-new

To compile CSS, run (from /pkg/frontend/):

    sass scss-new/style.scss styleguide-new/style.css

To see the newly styled pages:

    cd pkg/frontend && php -S localhost:8080
    curl http://localhost:8080/styleguide-new/

To generate the SVG files for php:

	cd pkg/frontend
	./scripts/compile-svg.hs pkg/frontend/images pkg/frontend/styleguide-new/svgs/svgs.php