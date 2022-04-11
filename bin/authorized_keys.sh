
#!/bin/bash
keys=$(wget -qO- https://github.com/rampedindent.keys)
if [ -f "${HOME}/.ssh/authorized_keys" ]
then
    echo "$keys" > "${HOME}/.ssh/authorized_keys"
fi
