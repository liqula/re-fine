New Styleguide in

    pkg/frontend/styleguide-new

New CSS in

    pkg/frontend/scss-new

To compile CSS, run (from /pkg/frontend/scss-new):

    make once

To watch and re-compile CSS on the fly (from /pkg/frontend/scss-new):

    make loop

To see the newly styled pages (from /pkg/frontend):

    php -S localhost:8080
    curl http://localhost:8080/styleguide-new/
    # (the trailing slash is important!)

To generate the SVG files for php:

    scripts/compile-svg.hs pkg/frontend/images/icon pkg/frontend/styleguide-new/includes/svgs/svgs.php
