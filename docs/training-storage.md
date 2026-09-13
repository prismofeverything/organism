# Training storage

Large compute artifacts live on `/mnt/data`, under
`/mnt/data/archive/organism-training`. Existing paths are preserved as symlinks:

| Existing path | Destination beneath the storage root |
| --- | --- |
| `checkpoints` | `checkpoints` |
| `.venv-training` | `.venv-training` |
| `native/target` | `native/target` |
| `~/.cache/uv` | `uv` |
| `~/.cache/pip` | `pip` |

The source repository and normal browser/site files remain in place. Launchers,
Python interpreter paths, libtorch paths and saved manifests continue using their
existing names. The experiment orchestrators use absolute logical paths without
resolving symlinks, so moving storage does not change persisted experiment
identities. The destination must be mounted before using these paths.

Migration stops production and experiment writers with their STOP files, copies
with `rsync -aH` to preserve shared checkpoint/cache files, verifies file checksums,
and replaces old directories with symlinks. Original copies are removed only after
verification and the links have been checked. The low-space experiment stop now
measures free space on `/mnt/data` through its checkpoint root.
