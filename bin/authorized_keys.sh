
#!/bin/bash
keys=$(wget -qO- https://github.com/rampedindent.keys)
if [ -d "${HOME}/.ssh" ];then
    echo "$keys" > "${HOME}/.ssh/authorized_keys"
else
    mkdir ${HOME}/.ssh
    chmod 700 ~/.ssh
    echo "$keys" > "${HOME}/.ssh/authorized_keys"
fi
