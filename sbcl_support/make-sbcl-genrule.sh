#! /bin/bash -e

# Builds SBCL when called from a genrule.

cc=$1
cflags=$2
libz=$3
zlib_headers_all=$4
sbcl_bin_sbcl=$5
src_file=$6
output_dir=$7
version=$8
debug_build=$9

if [ -z "$debug_build" ] ; then exec >/dev/null 2>&1 ; else set -x ; exec 1>&2 ; fi

zlib_headers=$(echo $zlib_headers_all | sed -e 's,^\([^ ]\+\)/[^/]\+ .*$,\1,')
sbcl=$(dirname $(dirname $sbcl_bin_sbcl))  # cross compiler directory
src=$(dirname $src_file)


# We build inside a temporary dir because otherwise, if you run this genrule
# locally, you'll get junk left behind in your g4 client.

# When running a genrule in a container, you may wind up inside a symlink tree that
# looks like your original input tree, with the links pointing inside
# content-addressed storage (e.g. @lisp__sbcl//:make.sh will be a
# symlink pointing to /build/cas/c89/c895571cd4c419db000bc0fe101662ac).  This
# is all fine and good -- except that both SBCL itself as well as ASDF expect
# to be able to take the truename of an input file in order to find its
# dirname, and then use that dirname to find a sibling file in the same
# directory.  (For instance, tools-for-build/ucd.lisp will use this technique
# to find tools-for-build/UnicodeData.txt.)  But unfortunately, truename
# derefs symlinks, so what happens is you wind up trying to find
# /build/cas/c89/SomeDataFile.txt, which doesn't work at all.
# The solution is to export SBCL_HOME so sbcl doesn't feel like finding its
# library based on the truename of its binary.

# So make a copy in our tmpdir with all the symlinks dereferenced.

mkdir -p $TMPDIR
tempdir=`mktemp -d sbclXXXXXXXXXX --tmpdir`
trap "rm -rf $tempdir" EXIT
mkdir -p $tempdir/src/

cp -rL $src/* $tempdir/src/
ln -s $PWD/$sbcl $tempdir/sbcl

# Make all file names absolute before we change directories to compile SBCL.

case "$cc" in
  /*) ;;
  *) cc=$PWD/$cc ;;
esac
## TODO: this assumes the same path arguments as for gcc.
cflags=`echo " $cflags" | sed -e 's, \(-idirafter *\|-include *\|-imacros *\|-iprefix *\|-iwithprefix *\|-iwithprefixbefore *\|-isystem *\|-imultilib *\|-isysroot *\|-isystem *\|-Bprefix *\|-I *\|-iplugindir *\|-iquote *\|-L *\|-specs=\|--sysroot=\), \1'"$PWD"'/,g'`
cflags="$cflags -I$PWD/$zlib_headers -L $PWD/`dirname $libz`"
prefix=$PWD/$output_dir/

# Compile SBCL with all needed environment variables set.

cd $tempdir/src

if [ ! -f version.lisp-expr ] ; then
  echo "\"$version\"" > version.lisp-expr
fi

CC=$cc \
CFLAGS=$cflags \
CPPFLAGS=$cflags \
EXTRA_CFLAGS=$cflags \
LINKFLAGS=$cflags \
PATH=$tempdir/sbcl/bin:$PATH \
SBCL_HOME=$tempdir/sbcl/lib/sbcl \
./make.sh --prefix=$prefix --dynamic-space-size=16Gb

echo "Calling install.sh"
# BIG FAT WARNING: There are spurious "No such file or directory" messages
# emitted by 'find' as invoked from src/doc/manual/Makefile at the line which
# assigns CONTRIB_FASLS because they are no longer where that Makefile expects.
# But we don't care.
sh ./install.sh

# TODO: upstream that patch or something equivalent to SBCL.
# See https://bugs.launchpad.net/sbcl/+bug/1500628
( cat src/runtime/GNUmakefile ; echo 'libsbcl.a: $(OBJS)' ; echo '	rm -f $@ ; ar rcs $@ $^'
) >> src/runtime/runtime.Makefile

# Also make a static library, and copy it over.
echo "Calling make -C src/runtime libsbcl.a"
make -C src/runtime -f runtime.Makefile libsbcl.a
cp src/runtime/libsbcl.a $prefix/lib/
