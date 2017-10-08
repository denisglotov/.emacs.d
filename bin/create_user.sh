#!/bin/bash
#
# Create user with sudo rights and empty password. This script is idempotent -
# duplicate runs make no effect.
#

if [ "$#" -ge 3 ]; then
    echo "Incorrect number of arguments: did you forget to quote your SSH key?"
    echo "Use: create_user.sh [username ['SSH_PUBKEY']]"
    exit -1
fi

[ "$1" ] && NEWUSER="$1" || NEWUSER="dev"
[ "$2" ] && SSH_KEY="$2"

echo "Adding user $NEWUSER..."
sudo adduser --disabled-password --gecos "" $NEWUSER
if sudo grep -qe "^$NEWUSER" /etc/sudoers; then
    echo "(!) $NEWUSER is already among sudoers. Skipping."
else
    printf "$NEWUSER\tALL=(ALL) NOPASSWD:ALL\n" | sudo tee -a /etc/sudoers
fi

read -r -d '' CMD <<-END
if [ ! -d ~/.ssh ]; then
    echo "Creating ~/.ssh..."
    mkdir -m 700 ~/.ssh
fi
if grep -sqe "$SSH_KEY" ~/.ssh/authorized_keys; then
    echo "(!) Key already added."
else
    echo "Adding provided key..."
    echo "$SSH_KEY" >>~/.ssh/authorized_keys
fi
chmod 600 ~/.ssh/authorized_keys
END
if [ "$SSH_KEY" ]; then
    sudo -Hu $NEWUSER bash -c "$CMD"
else
    echo "(!) No key was specified. Skipping."
fi

echo "All done."
