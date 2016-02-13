# Copyright 2013 Google Inc. All Rights Reserved.
"""Combine a sbcl image file with an sbcl runtime executable.

While SBCL is capable of producing a combined runtime+image like this
on its own, having a separate script allows the combination of a lisp
C runtime linked with other libraries, with an image dumped from a
vanilla sbcl-only compiler runtime.
"""

import os
import struct
import sys


## The file format handled by this code was determined by looking at
## the code in "src/runtime/coreparse.c" and "src/runtime/save.c" in
## SBCL. As far as I know, it is not documented anywhere. --jyknight

LISPOBJ_SIZE = 8  # Note: 4 on x86.
OFF_T_SIZE = 8  # Note: can be 4 or 8 on x86.
IMAGE_ALIGNMENT = 32768  # Alignment requirement
CORE_MAGIC = 'LCBS\x00\x00\x00\x00'


def FindCoreStart(f):
  """Find the start of a lisp core file.

  It may possibly be the only data in the file, in which case the
  return value will be 0. Or, it may prepended by an ELF binary, in which
  case the return value will be positive.

  If there is no core file present, return None.

  Args:
   f: an open file object.

  Returns:
   Offset to beginning of embedded core file, or None.
  """

  # Possible format 1: core file at beginning of file.
  magic = f.read(LISPOBJ_SIZE)
  if magic == CORE_MAGIC:
    return 0  # Found standalone core header at start.

  # Possible format 2: core file appended to a binary.
  f.seek(-LISPOBJ_SIZE, os.SEEK_END)
  magic = f.read(LISPOBJ_SIZE)
  if magic != CORE_MAGIC:
    return None

  # We found an appended core file.  Immediately prior to the magic
  # number is an offset to the beginning of the core. Read it now.
  f.seek(-(LISPOBJ_SIZE + OFF_T_SIZE), os.SEEK_END)
  core_start = struct.unpack('=q', f.read(OFF_T_SIZE))[0]

  # Verify that the offset actually does look like the beginning of a core file.
  f.seek(core_start, os.SEEK_SET)
  magic = f.read(LISPOBJ_SIZE)
  if magic != CORE_MAGIC:
    return None

  return core_start


def CopyFile(input_file, output_file, nbytes=None):
  """Copy up to nbytes from input to output, starting at current file offset.

  Args:
    input_file: file object open for reading
    output_file: file object open for writing
    nbytes: optionally, number of bytes to copy before stopping.
  """
  while True:
    nread = 131072
    if nbytes is not None:
      nread = min(nread, nbytes)
      nbytes -= nread

    b = input_file.read(nread)
    if not b:
      break
    output_file.write(b)


def main():
  out_file_name, in_runtime_name, in_image_name = sys.argv[1:]

  runtime = open(in_runtime_name, 'rb')
  image = open(in_image_name, 'rb')
  combined = open(out_file_name, 'wb')

  # Determine the start of the core in the image file
  image_core_start = FindCoreStart(image)
  if image_core_start is None:
    sys.exit("Didn't find magic number in image file %r" % (in_image_name,))

  # Check if the runtime had an appended image, so it can be ignored.
  runtime_core_start = FindCoreStart(runtime)

  # Copy the runtime to the output
  runtime.seek(0, os.SEEK_SET)
  CopyFile(runtime, combined, nbytes=runtime_core_start)

  # Determine the start of the image core we're about to write, and ensure
  # it's aligned properly.
  core_start = combined.tell()
  if core_start % IMAGE_ALIGNMENT != 0:
    # Add padding
    combined.write('\0' * (IMAGE_ALIGNMENT - (core_start % IMAGE_ALIGNMENT)))
    core_start = combined.tell()
    assert core_start % IMAGE_ALIGNMENT == 0

  # Copy image to output
  image.seek(image_core_start, os.SEEK_SET)
  CopyFile(image, combined)

  # Fixup file trailer to have new starting offset for image.
  combined.seek(-(OFF_T_SIZE + LISPOBJ_SIZE), os.SEEK_END)
  combined.write(struct.pack('=q', core_start))

if __name__ == '__main__':
  main()
