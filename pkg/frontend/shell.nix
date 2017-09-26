(import ./build.nix) refine-frontend.env

# FIXME: this file should allow to write
#
#     nix-shell
#
# instead of
#
#     nix-shell --attr refine-frontend.env build.nix
#
# but i get this error:
#
#     nix-shell 'error: undefined variable' at shell.nix
