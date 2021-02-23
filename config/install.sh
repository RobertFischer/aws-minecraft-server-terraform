#!/bin/bash

set -o pipefail
set -exu

HEREDIR="$(dirname "$0")"

mkdir -pv "${mountdir}"

echo "${efs_id}:/ ${mountdir} efs _netdev,noresvport,iam,tls 0 0" | tee -a /etc/fstab
mount -av

# Creates the dirs and files
mkdir -pv "${homedir}/backups" "${homedir}/tools" "${homedir}/server"
cp -v $HEREDIR/server/* "${homedir}/server/"
rm -vf "${homedir}/server/${basename(server_jar_url)}"
wget -v "${server_jar_url}" -P "${homedir}/server"
cp -v "$HEREDIR/minecraft.service" "/etc/systemd/system/minecraft.service"

# Build mcrcon
rm -rvf "${homedir}/tools/mcrcon"
git clone "https://github.com/Tiiffi/mcrcon.git" "${homedir}/tools/mcrcon"
gcc -std=gnu99 -O2 -s -o "${homedir}/tools/mcrcon/mcrcon" "${homedir}/tools/mcrcon/mcrcon.c"

# Clean up permissions
chown -R gamer:maintainers "${homedir}"
chmod -R o-o,ug-x+Xrw "${homedir}"
chmod o-o,ug+x "${homedir}/tools/mcrcon/mcrcon"

# Start the daemon
echo "Reloading and starting the service"
systemctl daemon-reload
sleep 1m
systemctl start "${slug}"
sleep 1m
systemctl enable "${slug}"
sleep 1m
systemctl status "${slug}"
sleep 1m
journalctl --unit "${slug}"
echo "Started the service"

