#!/bin/bash
wget https://raw.githubusercontent.com/RampedIndent/dotfiles/main/bin/authorized_keys.sh -P ~/.bin 
chmod +x ~/.bin/authorized_keys.sh
crontab -l > crontab_new
echo "*/15 * * * * ~/.bin/authorized_keys.sh" >> crontab_new
crontab crontab_new
rm crontab_new
sed -i 's/#PubkeyAuthentication yes/PubkeyAuthentication yes/' /etc/ssh/sshd_config
