#!/bin/bash
#
# Create user with sudo rights, empty password and given ssh key access. This
# script is idempotent - duplicate runs make no effect.
#
# Run it remorely:
# ssh remote_host [-p 2222] [-l existing_user] \
#    bash -s -- new_user \"$(cat ~/.ssh/id_rsa.pub)\" <bin/create_user.sh
#

if ! sudo -v; then
    echo "Sudo does not work for this user, $USER."
    echo "Maybe it needs a passowd?.."
    exit -2
fi

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

# Commands to be run for new user to set up access key.
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
