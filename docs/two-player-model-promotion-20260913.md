# Two-player checkpoint replacement — September 13, 2026

At the user's request, the live 2p/3-ring trainer now resumes the controlled
experiment's control checkpoint at iteration 399, including its optimizer,
replay, RNG, and unfinished games. Its weights match the SHA-256 recorded in
`checkpoints/organism-ablation-20260912/control/evaluation-020/report.json`.
The screen returned 6 wins, 2 losses, and 8 cutoffs in 16 games against the
frozen initial iteration-379 model. This is not a direct comparison against
the displaced production iteration-819 checkpoint or proof of strong play.

The launcher uses the tested 2p recipe: replay capacity 5000, no per-game
sample cap, and neutral length-cutoff value targets. New per-player options
`--buffer-2p`, `--replay-game-cap-2p`, and `--cutoff-value-2p` override the
shared settings; the corresponding `-3p` options are also supported.
Three-player weights, optimizer, replay, evaluations, and recipe are preserved.
The model iteration shown for 2p consequently returns to 400 on restart.

The old 2p directory is preserved intact under
`checkpoints/model-rollbacks/20260913-101341/2p-r3`.
The machine-readable promotion record is
`checkpoints/organism-native/model-promotion.json`.
To roll back, gracefully stop the trainer, preserve the replacement directory,
restore that saved directory to `checkpoints/organism-native/2p-r3`, and remove
the three 2p-specific options from the launcher before restarting.

Validation: release build; CPU integration check
`python3 tests/check_native_player_settings.py` verifies independent settings
and actual cutoff value targets for both player counts; live trainer resumed
iteration 399 and began new self-play at iteration 400.
