#!/bin/bash -x

git diff-index --quiet HEAD
if [ $? -ne 0 ]; then
    echo "Uncommited changes, aborting"
    exit 1
fi

cd ../scalameta
SCALAMETA_REV="$(git rev-parse HEAD)"
cd ../dotty
cp -r ../scalameta/scalahost/src/main/scala/scala/meta/ src/scala/
mv src/scala/meta/internal/hosts/{scalac,dotty}
sed -i 's/\.scalac/.dotty/g' src/scala/meta/**/*.scala
sed -i 's/object scalac/object dotty/g' src/scala/meta/**/*.scala
rm src/scala/meta/internal/hosts/dotty/{Analyzer,Backend,ConvertPhase,HijackAnalyzer,HijackBackend,ParadiseCompat,Plugin,Settings,reflect/Metadata}.scala
git add src/scala/meta

git commit -F- <<EOF
Create dottyhost from scalameta/scalahost ${SCALAMETA_REV}

Import realized using import-dottyhost.sh
EOF

git checkout "$(git commit-tree -p HEAD "add/meta-trees^{tree}" -m "Initial port to dotty")"
