#!/usr/bin/env python3
"""MP4 concatenation v3 - correct box offset handling.

Box structure:
  [size:4][type:4][data...]

Full box structure:
  [size:4][type:4][version:1][flags:3][data...]

find_box returns (size_field_offset, box_size)
Box data starts at size_field_offset + 8
"""
import struct
import os
import sys
import subprocess

WORKDIR = "/Users/zihuidou/0rootbase/0pnbase/kb-main/99ai/1life"
FFMPEG = "/Applications/Trae CN.app/Contents/Resources/app/bin/ffmpeg"

clips = [
    "clip_1_life_slow.mp4",
    "clip_2_wisdom_slow.mp4",
    "clip_3_intelligent_life_slow.mp4",
    "clip_4_evolution_slow.mp4",
    "clip_5_silicon_slow.mp4",
]
output = os.path.join(WORKDIR, "life_wisdom_silicon_1min.mp4")


def parse_atoms(filepath):
    """Returns {atom_type: (offset, size, header_bytes)} for top-level atoms."""
    atoms = {}
    with open(filepath, 'rb') as f:
        f.seek(0, 2)
        file_size = f.tell()
        f.seek(0)
        pos = 0
        while pos < file_size - 7:
            f.seek(pos)
            h = f.read(8)
            if len(h) < 8:
                break
            size, atype = struct.unpack('>I4s', h)
            atype = atype.decode('ascii', errors='replace')
            if size == 1:
                size = struct.unpack('>Q', f.read(8))[0]
                hdr = 16
            elif size == 0:
                size = file_size - pos
                hdr = 8
            else:
                hdr = 8
            atoms[atype] = (pos, size, hdr)
            pos += size
    return atoms


def read_file_range(filepath, offset, size):
    with open(filepath, 'rb') as f:
        f.seek(offset)
        return f.read(size)


def find_box(data, box_type, start=0):
    """Find a box by type string. Returns (size_field_offset, box_size)."""
    needle = box_type.encode('ascii') if isinstance(box_type, str) else box_type
    pos = data.find(needle, start)
    if pos < 0:
        return None, None
    size = struct.unpack('>I', data[pos-4:pos])[0]
    return pos - 4, size


# ============================================================
# Collect data from all clips
# ============================================================

all_mdat = bytearray()
all_stco_offsets = []        # adjusted chunk offsets for combined mdat
all_stsz_sizes = []          # sample sizes
all_stts_entries = []        # (sample_count, sample_delta)
total_samples = 0
mvhd_timescale = 0
mdhd_timescale = 0
total_mvhd_dur = 0
total_mdhd_dur = 0
first_ftyp = None
first_moov = None
has_ctts = False
all_ctts_entries = []        # (sample_count, sample_offset)
stsd_data = None

for i, clip_name in enumerate(clips):
    clip_path = os.path.join(WORKDIR, clip_name)
    print(f"\nClip {i+1}: {clip_name}", file=sys.stderr)

    atoms = parse_atoms(clip_path)
    mdat_off, mdat_sz, mdat_hdr = atoms['mdat']
    moov_off, moov_sz, _ = atoms['moov']

    # Read full moov
    moov = bytearray(read_file_range(clip_path, moov_off, moov_sz))
    moov_box_start = 0        # we read from moov_off, so moov starts at 0
    moov_data_start = 8       # skip moov [size:4][type:4]

    if i == 0:
        first_ftyp = read_file_range(clip_path, atoms['ftyp'][0], atoms['ftyp'][1])
        first_moov = moov

        # --- mvhd (accumulate from each clip) ---
    mvhd_pos, _ = find_box(moov, 'mvhd')
    mvhd_timescale = struct.unpack('>I', moov[mvhd_pos+20:mvhd_pos+24])[0]
    version = moov[mvhd_pos+8]
    if version == 0:
        clip_mvhd_dur = struct.unpack('>I', moov[mvhd_pos+24:mvhd_pos+28])[0]
    else:
        clip_mvhd_dur = struct.unpack('>Q', moov[mvhd_pos+28:mvhd_pos+36])[0]
    total_mvhd_dur += clip_mvhd_dur

    # --- mdhd (accumulate from each clip) ---
    mdhd_pos, _ = find_box(moov, 'mdhd')
    mdhd_timescale = struct.unpack('>I', moov[mdhd_pos+20:mdhd_pos+24])[0]
    total_mdhd_dur += struct.unpack('>I', moov[mdhd_pos+24:mdhd_pos+28])[0]

    print(f"  mvhd: timescale={mvhd_timescale}, dur={clip_mvhd_dur}, mdhd: timescale={mdhd_timescale}", file=sys.stderr)

    # --- Read mdat content ---
    mdat = read_file_range(clip_path, mdat_off + mdat_hdr, mdat_sz - mdat_hdr)
    mdat_content_start = mdat_off + mdat_hdr  # absolute file offset where mdat data begins
    mdat_base = len(all_mdat)                  # offset in combined mdat where this clip's data starts

    # --- Parse stbl ---
    stbl_pos, stbl_sz = find_box(moov, 'stbl')
    stbl = moov[stbl_pos:stbl_pos+stbl_sz]
    # stbl data starts at stbl_pos+8

    # stsd (sample description - keep from first clip)
    if i == 0:
        stsd_pos, stsd_sz = find_box(stbl, 'stsd')
        if stsd_pos is not None:
            stsd_data = bytes(stbl[stsd_pos:stsd_pos+stsd_sz])

    # stsz (sample sizes)
    stsz_pos, stsz_sz = find_box(stbl, 'stsz')
    stsz_data_start = stsz_pos + 8  # skip [size:4][type:4]
    stsz_version = stbl[stsz_data_start]
    stsz_sample_size = struct.unpack('>I', stbl[stsz_data_start+4:stsz_data_start+8])[0]
    stsz_num = struct.unpack('>I', stbl[stsz_data_start+8:stsz_data_start+12])[0]
    if stsz_sample_size == 0:
        for si in range(stsz_num):
            s = struct.unpack('>I', stbl[stsz_data_start+12+si*4:stsz_data_start+16+si*4])[0]
            all_stsz_sizes.append(s)
    else:
        all_stsz_sizes.extend([stsz_sample_size] * stsz_num)
    total_samples += stsz_num
    print(f"  stsz: {stsz_num} samples (total: {total_samples})", file=sys.stderr)

    # stco/co64 (chunk offsets)
    stco_pos, stco_sz = find_box(stbl, 'stco')
    is_64 = False
    if stco_pos is None:
        stco_pos, stco_sz = find_box(stbl, 'co64')
        is_64 = True

    stco_data_start = stco_pos + 8
    stco_num = struct.unpack('>I', stbl[stco_data_start+4:stco_data_start+8])[0]
    for ci in range(stco_num):
        off = stco_data_start + 8 + ci * (8 if is_64 else 4)
        if is_64:
            abs_off = struct.unpack('>Q', stbl[off:off+8])[0]
        else:
            abs_off = struct.unpack('>I', stbl[off:off+4])[0]
        # Adjust: new offset = mdat_base + (original_abs - mdat_content_start)
        adjusted = mdat_base + (abs_off - mdat_content_start)
        all_stco_offsets.append(adjusted)
    print(f"  stco: {stco_num} chunks", file=sys.stderr)

    # stts (time-to-sample)
    stts_pos, stts_sz = find_box(stbl, 'stts')
    stts_data_start = stts_pos + 8
    stts_num = struct.unpack('>I', stbl[stts_data_start+4:stts_data_start+8])[0]
    clip_dur = 0
    for ei in range(stts_num):
        o = stts_data_start + 8 + ei * 8
        count = struct.unpack('>I', stbl[o:o+4])[0]
        delta = struct.unpack('>I', stbl[o+4:o+8])[0]
        all_stts_entries.append((count, delta))
        clip_dur += count * delta
    print(f"  stts: {stts_num} entries, duration={clip_dur}", file=sys.stderr)

    # ctts (composition offsets)
    ctts_pos, _ = find_box(stbl, 'ctts')
    if ctts_pos is not None:
        has_ctts = True

    # (mdhd already accumulated above)

    all_mdat.extend(mdat)


print(f"\n=== Summary ===", file=sys.stderr)
print(f"Combined mdat: {len(all_mdat)} bytes", file=sys.stderr)
print(f"Total samples: {total_samples}", file=sys.stderr)
print(f"Total chunks: {len(all_stco_offsets)}", file=sys.stderr)
print(f"Total stts entries: {len(all_stts_entries)}", file=sys.stderr)
print(f"mvhd timescale: {mvhd_timescale}, duration: {total_mvhd_dur}", file=sys.stderr)
print(f"mdhd timescale: {mdhd_timescale}, duration: {total_mdhd_dur}", file=sys.stderr)
print(f"Estimated seconds: {total_mvhd_dur/mvhd_timescale:.1f}s", file=sys.stderr)

# ============================================================
# Rebuild moov from first clip
# ============================================================

moov = bytearray(first_moov)

# --- Update mvhd ---
mvhd_pos, mvhd_sz = find_box(moov, 'mvhd')
version = moov[mvhd_pos+8]
if version == 0:
    struct.pack_into('>I', moov, mvhd_pos+24, total_mvhd_dur)
else:
    struct.pack_into('>Q', moov, mvhd_pos+28, total_mvhd_dur)
print(f"Updated mvhd duration: {total_mvhd_dur}", file=sys.stderr)

# --- Update tkhd ---
tkhd_pos, _ = find_box(moov, 'tkhd')
tkhd_ver = moov[tkhd_pos+8]
if tkhd_ver == 0:
    struct.pack_into('>I', moov, tkhd_pos+28, total_mvhd_dur)
else:
    struct.pack_into('>Q', moov, tkhd_pos+36, total_mvhd_dur)
print(f"Updated tkhd duration", file=sys.stderr)

# --- Update mdhd ---
mdhd_pos, _ = find_box(moov, 'mdhd')
struct.pack_into('>I', moov, mdhd_pos+24, total_mdhd_dur)
print(f"Updated mdhd duration: {total_mdhd_dur}", file=sys.stderr)

# --- Update stbl ---
stbl_pos, stbl_sz = find_box(moov, 'stbl')
stbl = moov[stbl_pos:stbl_pos+stbl_sz]

# stsz
stsz_pos, stsz_sz = find_box(stbl, 'stsz')
new_sample_table = b''
for s in all_stsz_sizes:
    new_sample_table += struct.pack('>I', s)
new_stsz_data = stbl[stsz_pos+8:stsz_pos+16]  # version+flags+default_size
new_stsz_data += struct.pack('>I', total_samples)  # num_samples (overwriting old)
new_stsz_data += new_sample_table
new_stsz = struct.pack('>I', 8 + len(new_stsz_data)) + b'stsz' + new_stsz_data
stbl[stsz_pos:stsz_pos+stsz_sz] = new_stsz
print(f"Updated stsz: {total_samples} samples ({len(new_stsz)} bytes)", file=sys.stderr)

# stts
stts_pos, stts_sz = find_box(stbl, 'stts')
stts_data = stbl[stts_pos+8:stts_pos+12]  # version+flags
stts_data += struct.pack('>I', len(all_stts_entries))
for count, delta in all_stts_entries:
    stts_data += struct.pack('>II', count, delta)
new_stts = struct.pack('>I', 8 + len(stts_data)) + b'stts' + stts_data
stbl[stts_pos:stts_pos+stts_sz] = new_stts
print(f"Updated stts: {len(all_stts_entries)} entries", file=sys.stderr)

# ============================================================
# Adjust chunk offsets for output file layout
# ============================================================

# Output layout: [ftyp][mdat_header:8][mdat_data...][moov]
output_mdat_data_start = len(first_ftyp) + 8
print(f"Output mdat data starts at file offset: {output_mdat_data_start}", file=sys.stderr)

for i in range(len(all_stco_offsets)):
    all_stco_offsets[i] += output_mdat_data_start

# ============================================================
# Rebuild stco in stbl (now with final offsets)
# ============================================================

stbl_pos, stbl_sz = find_box(moov, 'stbl')
stbl = moov[stbl_pos:stbl_pos+stbl_sz]

stco_pos, stco_sz = find_box(stbl, 'stco')
is_64 = False
if stco_pos is None:
    stco_pos, stco_sz = find_box(stbl, 'co64')
    is_64 = True

entry_fmt = '>Q' if is_64 else '>I'
atom_type = b'co64' if is_64 else b'stco'
stco_header = stbl[stco_pos+8:stco_pos+12]  # version+flags
stco_header += struct.pack('>I', len(all_stco_offsets))
stco_data = b''
for off in all_stco_offsets:
    stco_data += struct.pack(entry_fmt, off)
new_stco_atom = struct.pack('>I', 8 + len(stco_header) + len(stco_data)) + atom_type
new_stco = new_stco_atom + stco_header + stco_data
stbl[stco_pos:stco_pos+stco_sz] = new_stco
print(f"Final stco: {len(all_stco_offsets)} chunks, first offset={all_stco_offsets[0]}, last={all_stco_offsets[-1]}", file=sys.stderr)

# Update stbl size
new_stbl_size = len(stbl)
struct.pack_into('>I', stbl, 0, new_stbl_size)
moov[stbl_pos:stbl_pos+stbl_sz] = stbl

# Recalculate all parent container sizes
# moov -> trak -> mdia -> minf -> stbl
trak_pos, trak_sz = find_box(moov, 'trak')
mdia_pos, _ = find_box(moov, 'mdia')
minf_pos, _ = find_box(moov, 'minf')

# minf: ends where stbl ends
stbl_end = stbl_pos + new_stbl_size
new_minf_size = stbl_end - minf_pos
struct.pack_into('>I', moov, minf_pos, new_minf_size)

# mdia: ends where minf ends
new_mdia_size = stbl_end - mdia_pos
struct.pack_into('>I', moov, mdia_pos, new_mdia_size)

# trak: ends where mdia ends (plus udta if present)
udta_pos, udta_sz = find_box(moov, 'udta')
if udta_pos is not None:
    new_trak_size = udta_pos - trak_pos
else:
    new_trak_size = stbl_end - trak_pos
struct.pack_into('>I', moov, trak_pos, new_trak_size)

# moov: ends where last child ends
if udta_pos is not None:
    new_moov_size = udta_pos + udta_sz
else:
    new_moov_size = stbl_end
struct.pack_into('>I', moov, 0, new_moov_size)

print(f"Updated container sizes: trak={new_trak_size}, mdia={new_mdia_size}, minf={new_minf_size}, moov={new_moov_size}", file=sys.stderr)

# ============================================================
# Write output
# ============================================================

# Update moov size (final)
final_moov_size = len(moov)
struct.pack_into('>I', moov, 0, final_moov_size)
print(f"Final moov size: {final_moov_size}", file=sys.stderr)

mdat_size = 8 + len(all_mdat)
total_size = len(first_ftyp) + new_moov_size + mdat_size

print(f"\nWriting output ({total_size} bytes)...", file=sys.stderr)

with open(output, 'wb') as out:
    out.write(first_ftyp)
    out.write(struct.pack('>I', mdat_size))
    out.write(b'mdat')
    out.write(all_mdat)
    out.write(moov)

print(f"Done: {output} ({os.path.getsize(output)} bytes)", file=sys.stderr)

# Verify
check = subprocess.run([FFMPEG, "-i", output], capture_output=True, text=True)
for line in check.stderr.split('\n'):
    if 'Duration' in line:
        print(f"\n*** {line.strip()} ***")
        break
