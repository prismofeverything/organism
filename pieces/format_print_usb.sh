#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# format_print_usb.sh — wipe a USB stick to FAT32 and load the ORGANISM print
# files onto it (for taking to Office Depot). DESTROYS everything on the target.
#
#   bash pieces/format_print_usb.sh            # default target: /dev/sda
#   bash pieces/format_print_usb.sh /dev/sdX   # override target
#
# Run as your normal user (it calls sudo only where needed). It refuses to
# touch anything that isn't a REMOVABLE USB disk, and never the disk that
# holds /, /boot, /home or /mnt/data. You must type the device name to confirm.
# ---------------------------------------------------------------------------
set -euo pipefail

DEV="${1:-/dev/sda}"
LABEL="ORGANISM"
SRC=(
  /home/youdonotexist/code/organism/pieces/prototype
  /home/youdonotexist/code/organism/pieces/prototype-poster
)

red(){ printf '\033[1;31m%s\033[0m\n' "$*"; }
grn(){ printf '\033[1;32m%s\033[0m\n' "$*"; }
die(){ red "ABORT: $*"; exit 1; }

command -v sudo    >/dev/null || die "sudo not found"
command -v mkfs.vfat >/dev/null || die "mkfs.vfat not found"
[[ -b "$DEV" ]] || die "$DEV is not a block device"
base=$(basename "$DEV")

# ---------------- SAFETY CHECKS ----------------
typ=$(lsblk -dno TYPE "$DEV" 2>/dev/null | tr -d '[:space:]' || true)
tran=$(lsblk -dno TRAN "$DEV" 2>/dev/null | tr -d '[:space:]' || true)
rmv=$(lsblk -dno RM   "$DEV" 2>/dev/null | tr -d '[:space:]' || true)
[[ "$typ" == "disk" ]] || die "$DEV is not a whole disk (type=$typ)"
[[ "$tran" == "usb"  ]] || die "$DEV transport is '$tran', not usb — refusing for safety"
[[ "$rmv"  == "1"    ]] || die "$DEV is not removable (RM=$rmv) — refusing for safety"

# nothing critical may live on the target
for m in / /boot /boot/efi /home /mnt/data; do
  s=$(findmnt -no SOURCE "$m" 2>/dev/null || true)
  [[ -n "$s" && "$s" == "$DEV"* ]] && die "critical mount $m is on $DEV ($s)"
done
root_src=$(findmnt -no SOURCE / || true)
root_pk=$(lsblk -no PKNAME "$root_src" 2>/dev/null | head -1 | tr -d '[:space:]' || true)
[[ -n "$root_pk" && "/dev/$root_pk" == "$DEV" ]] && die "$DEV backs the ROOT filesystem"

# ---------------- CONFIRM ----------------
echo; grn "Device to ERASE and reformat:"
lsblk -o NAME,SIZE,TYPE,FSTYPE,LABEL,MOUNTPOINT,MODEL,TRAN,RM "$DEV"
echo; red "!! This PERMANENTLY DESTROYS everything on $DEV (incl. the NixOS installer) !!"
read -rp "Type the device name to confirm (i.e. '$base'), or anything else to abort: " ans
[[ "$ans" == "$base" ]] || die "confirmation did not match '$base'"

# ---------------- DO IT ----------------
grn "[1/5] unmounting any partitions on $DEV ..."
for p in $(lsblk -lno NAME "$DEV" | tail -n +2); do sudo umount "/dev/$p" 2>/dev/null || true; done

grn "[2/5] wiping old signatures + partition table ..."
sudo wipefs -a "$DEV"

grn "[3/5] creating MBR + one full-disk FAT32 partition ..."
sudo parted -s "$DEV" mklabel msdos
sudo parted -s "$DEV" mkpart primary fat32 1MiB 100%
sudo partprobe "$DEV"; sleep 1

part="${DEV}1"; [[ -b "$part" ]] || part="${DEV}p1"
[[ -b "$part" ]] || die "new partition $part did not appear"

grn "[4/5] formatting $part as FAT32 (label $LABEL) ..."
sudo umount "$part" 2>/dev/null || true          # in case the desktop auto-mounted it
sudo mkfs.vfat -F 32 -n "$LABEL" "$part"

grn "[5/5] mounting + copying print files ..."
MNT=$(mktemp -d)
sudo mount -o "uid=$(id -u),gid=$(id -g),dmask=0022,fmask=0133" "$part" "$MNT"
for d in "${SRC[@]}"; do
  if [[ -e "$d" ]]; then echo "   copying $(basename "$d") ..."; cp -r "$d" "$MNT"/; else red "   (skip, not found: $d)"; fi
done
sync
echo; grn "Contents of the stick:"; ls -la "$MNT"
du -sh "$MNT"/* 2>/dev/null || true
sudo umount "$MNT"; rmdir "$MNT"

echo; grn "DONE — USB '$LABEL' is formatted (FAT32) and loaded. Safe to unplug."
