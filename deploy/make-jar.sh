#
# this script depends on class files being in the 'bin' directory
# above us. it's a kludge; you may be able to do the same thing
# with `sbt package` now.
#

rm -r com
rm .DS_Store
rm *jar

cp -r ../bin/com .
jar cvf RandomNoise.jar .

echo ""
echo "Copy this jar file to the right place. Make a .info file too."

