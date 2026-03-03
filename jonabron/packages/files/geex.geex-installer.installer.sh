#!/usr/bin/env sh
# Version 4

if [ $# -eq 0 ]; then
    echo -e \
         "Usage: ENVIRONMENT geex OPTION
Run geex with OPTION, if given.

COMMAND must be one of the sub-commands listed below:

  main commands
    help                         display help message
    git                          display github repository url and developer contact information
    install                      start the interactive installer
    debug                        start application in debug mode
    clean                        clean up all possible leftovers
    live                         enable live preview mode
    mover                        start the mover mode
    features                     get a list of things the installer configures
    bootstrap                    launch interactive ISO bootstrapper
    container                    launch VERY experimental container manager

ENVIRONMENT can be one of the environment variables listed below:

  main environment variables
    GEEX_DEBUG                   start application in debug mode
    GEEX_DEBUG_MODE              start application in debug mode
    GEEX_VERBOSE_MODE            enable verbose mode for more feedback
    GEEX_IGNORE_MISSING          ignore if packages are missing
    GEEX_LIVE_MODE               enable live preview mode for the installer
    GEEX_DEBUG_MISSING_ENABLE    pretend as if packages were missing
    GEEX_PRETEND_FATAL           pretend to have encountered a fatal error
    GEEX_IGNORE_FORCED_DEBUG     ignore forced debug mode if herd was not found
    GEEX_MISSING_HERD_IGNORE     ignore whether herd/shepherd is missing
    GEEX_SKIP_WIFI               ignore if there is no active internet connection
    GEEX_EDITOR                  force the use of a specific editor for editing
                                 the configuration file (if selected)
    GEEX_MANUAL_NET_SETUP_MODE   force the installer to throw you into manual networking
                                 setup mode
    GEEX_FORCE_AGNOSTIC          force distro agnostic mode, which is highly experimental

  experimental environment variables
    GEEX_THE_HURD                force the installer to set your system up with GNU Hurd
    GEEX_THE_HURD_ALLOW          force installer to install with GNU Hurd, warning: this is
                                 a joke variable, do not install GNU Hurd as your systems
                                 main kernel - it will likely not boot, nor support your
                                 hardware
    GEEX_FORCE_THE_HURD          force installation even if installer is set to GNU Hurd

  bootstrapper environment variables
    GEEX_BOOTSTRAPPER_WRITE      switch from pretending to write, to actually writing
                                 your image to your selected disk
    BOOTSTRAPPER_FORCE_FORMAT    force the bootstrapper to format and write image to
                                 selected disk, even if escalation error is encountered

EXAMPLES that you may consider running yourself listed below:

  main examples
    ./geex.sh i                  run installer
    ./geex.sh i d                run installer in debug mode
    ./geex.sh d v i              run installer in debug and verbose mode
    ./geex.sh d v i l            run installer in debug, verbose, and live mode
    ./geex.sh d m                run mover in debug mode
    ./geex.sh c                  clean up any remains
    ./geex.sh feat               list features of the installer

NOTICE for you to consider:

  installer notices
    if you run the installer without debug mode, it will try to install gnu guix on your system or one
    of your disks, please be aware of this and ALWAYS run the installer in DEBUG MODE before deciding
    to actually use it to install an operating system (GNU Guix).

    as a last-resort safety mechanism, the installer forcefully enables DEBUG MODE if it detects that
    shepherd (herd) as a command is not available on your system, automatically assuming you are
    running the installer on a system with which you do not intend to actually install GNU Guix.

  mover notices
    if you run the mover without debug mode, it will try to move and copy files into your system without
    warning. there are backup hooks to try and prevent accidental file deletion, but it is better to check
    and back up your own files FIRST, before running the mover in non-debug mode."
    exit 1
fi

for arg in "$@"; do
    case "$arg" in
        l|-l|--l|live|-live|--live)
            export GEEX_LIVE_MODE=1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        g|-g|--g|git|-git|--git|github|-github|--github)
            echo -e \
                 "Information: REPO

  REPO
    https://github.com/librepup/geex"
            exit 1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        feat|-feat|--feat|features|-features|--features)
            echo -e \
                 "Geex FEATURES:

FEATURES provided by the Geex Installer:

  automations
    network discovery and setup
    bios detection
    driver detection
    firmware necessity detection
    kernel variant detection
    automatic and dynamic dependency resolution

  components
    username
    hostname
    groups
    region
    timezone
    keyboard
    bios
    formatting
    partitioning
    filesystems
    drivers
    kernel
    firmware
    services
    desktops
    package bundles
    custom packages
    system initialization
    system installation
    password setup
    home configuration

  fallbacks
    interactive network config creation
    disk label auto-detection
    automatic debug mode

  extras
    live-preview mode
    legacy mover mode
    leftover cleaner
"
            exit 1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        copyright|-copyright|--copyright|credits|-credits|--credits|credit|-credit|--credit)
            echo -e \
                 "Credits: Geex Installer

CREATOR
  librepup

CONTACT
  librepup@member.fsf.org
"
            exit 1
            ;;
    esac
done

cleanHook() {
    if [ -f "/tmp/geex.self.update.checkfile.dd" ]; then
        rm /tmp/geex.self.update.checkfile.dd
    fi
    if [ -f "/tmp/geex.service.tlp.block.dd" ]; then
        rm /tmp/geex.service.tlp.block.dd
    fi
    if [ -f "/tmp/geex.manual.network.config.conf" ]; then
        rm /tmp/geex.manual.network.config.conf
    fi
    if [ -f "/tmp/geex.bundle.combined.dd" ]; then
        rm /tmp/geex.bundle.combined.dd
    fi
    if [ -f "/tmp/geex.config.scm" ]; then
        rm /tmp/geex.config.scm
    fi
    if [ -f "/tmp/geex.config.custom.template.dd" ]; then
        rm /tmp/geex.config.custom.template.dd
    fi
    if [ -f "/tmp/geex.config.custom.dd" ]; then
        rm /tmp/geex.config.custom.dd
    fi
    if [ -f "/tmp/geex.xorg.optional.dd" ]; then
       rm /tmp/geex.xorg.optional.dd
    fi
    if [ -f "/tmp/geex.compose.block.dd" ]; then
        rm /tmp/geex.compose.block.dd
    fi
    if [ -f "/tmp/geex.xorg.optional.dd" ]; then
        rm /tmp/geex.xorg.optional.dd
    fi
    if [ -f "/tmp/geex.xorg.nvidia.modules.optional.dd" ]; then
        rm /tmp/geex.xorg.nvidia.modules.optional.dd
    fi
    if [ -f "/tmp/geex.kernel.modules.block.dd" ]; then
        rm /tmp/geex.kernel.modules.block.dd
    fi
    if [ -f "/tmp/geex.kernel.block.dd" ]; then
        rm /tmp/geex.kernel.block.dd
    fi
    if [ -f "/tmp/geex.swap.block.dd" ]; then
        rm /tmp/geex.swap.block.dd
    fi
    if [ -f "/tmp/config.scm" ]; then
        rm /tmp/config.scm
    fi
    if [ -f "/tmp/geex.extra.packages.insertable.dd" ]; then
        rm /tmp/geex.extra.packages.insertable.dd
    fi
    if [ -f "/tmp/geex.guix.channel.pull.check.file.dd" ]; then
        rm /tmp/geex.guix.channel.pull.check.file.dd
    fi
    if [ -f "/tmp/geex.guix.system.init.check.file.dd" ]; then
        rm /tmp/geex.guix.system.init.check.file.dd
    fi
    if [ -f "/tmp/geex.guix.style.check.file.dd" ]; then
        rm /tmp/geex.guix.style.check.file.dd
    fi
    if [ -f "/tmp/geex.keyboard.variants.dd" ]; then
        rm /tmp/geex.keyboard.variants.dd
    fi
    if [ -f "/tmp/geex.keyboard.layout.variants.dd" ]; then
        rm /tmp/geex.keyboard.layout.variants.dd
    fi
    if [ -f "/tmp/geex.disk.prefixed.text.block.dd" ]; then
        rm /tmp/geex.disk.prefixed.text.block.dd
    fi
    if [ -f "/tmp/geex.timezone.success.dd" ]; then
        rm /tmp/geex.timezone.success.dd
    fi
    if [ -f "/tmp/geex.timezone.notice.dd" ]; then
        rm /tmp/geex.timezone.notice.dd
    fi
    if [ -f "/tmp/geex.config.desktop.dd" ]; then
        rm /tmp/geex.config.desktop.dd
    fi
    if [ -f "/tmp/geex.config.laptop.dd" ]; then
        rm /tmp/geex.config.laptop.dd
    fi
    if [ -f "/tmp/geex.config.libre.dd" ]; then
        rm /tmp/geex.config.libre.dd
    fi
    if [ -f "/tmp/geex.config.minimal.dd" ]; then
        rm /tmp/geex.config.minimal.dd
    fi
    if [ -f "/tmp/geex.config.desktop.template.dd" ]; then
        rm /tmp/geex.config.desktop.template.dd
    fi
    if [ -f "/tmp/geex.config.laptop.template.dd" ]; then
        rm /tmp/geex.config.laptop.template.dd
    fi
    if [ -f "/tmp/geex.config.libre.template.dd" ]; then
        rm /tmp/geex.config.libre.template.dd
    fi
    if [ -f "/tmp/geex.config.minimal.template.dd" ]; then
        rm /tmp/geex.config.minimal.template.dd
    fi
    if [ -f "/tmp/geex.summary.dd" ]; then
        rm /tmp/geex.summary.dd
    fi
    if [ -f "/tmp/geex.keyboardstatus.dd" ]; then
        rm /tmp/geex.keyboardstatus.dd
    fi
    if [ -f "/tmp/geex.detecteddisks.dd" ]; then
        rm /tmp/geex.detecteddisks.dd
    fi
    if [ -f "/tmp/geex.detectedbios.dd" ]; then
        rm /tmp/geex.detectedbios.dd
    fi
    if [ -f "/tmp/geex.home.scm" ]; then
        rm /tmp/geex.home.scm
    fi
    if [ -f "/tmp/geex.bios.block.dd" ]; then
        rm /tmp/geex.bios.block.dd
    fi
    if [ -f "/tmp/geex.filesystem.block.dd" ]; then
        rm /tmp/geex.filesystem.block.dd
    fi
    if [ -f "/tmp/geex.service.hurd.block.dd" ]; then
        rm /tmp/geex.service.hurd.block.dd
    fi
    if [ -f "/tmp/geex.service.nix.block.dd" ]; then
        rm /tmp/geex.service.nix.block.dd
    fi
    if [ -f "/tmp/geex.group.nix.block.dd" ]; then
        rm /tmp/geex.group.nix.block.dd
    fi
    if [ -f "/tmp/geex.package.doas.block.dd" ]; then
        rm /tmp/geex.package.doas.block.dd
    fi
    if [ -f "/tmp/geex.service.doas.block.dd" ]; then
        rm /tmp/geex.service.doas.block.dd
    fi
    if [ -f "/tmp/geex.wm.i3.packages.dd" ]; then
        rm /tmp/geex.wm.i3.packages.dd
    fi
    if [ -f "/tmp/geex.wm.gnome.service.dd" ]; then
        rm /tmp/geex.wm.gnome.service.dd
    fi
    if [ -f "/tmp/geex.wm.naitre.packages.dd" ]; then
        rm /tmp/geex.wm.naitre.packages.dd
    fi
    if [ -f "/tmp/geex.wm.xmonad.packages.dd" ]; then
        rm /tmp/geex.wm.xmonad.packages.dd
    fi
    if [ -d "/tmp/geex.git.storage" ]; then
        rm -rf /tmp/geex.git.storage
    fi
    if [ -f "/tmp/geex.channels.dd" ]; then
        rm /tmp/geex.channels.dd
    fi
    if [ -f "/tmp/channels.scm" ]; then
        rm /tmp/channels.scm
    fi
    if [ -f "/tmp/geex.config.desktop.template.dd" ]; then
        rm /tmp/geex.config.desktop.template.dd
    fi
    if [ -f "/tmp/geex.config.minimal.template.dd" ]; then
        rm /tmp/geex.config.minimal.template.dd
    fi
    if [ -f "/tmp/geex.config.libre.template.dd" ]; then
        rm /tmp/geex.config.libre.template.dd
    fi
    if [ -f "/tmp/geex.config.laptop.template.dd" ]; then
        rm /tmp/geex.config.laptop.template.dd
    fi
    echo -e "Successfully cleaned up all possible leftovers."
}

for arg in "$@"; do
    case "$arg" in
        c|-c|--c|clean|-clean|--clean)
            cleanHook
            exit 1
            ;;
        ca|-ca|--ca|cleanall|-cleanall|--cleanall)
            cleanHook
            if [ -d "/tmp/geex.bootstrapper.store" ]; then
                rm -rf /tmp/geex.bootstrapper.store
            fi
            if [ -d "/tmp/geex.container.store" ]; then
                rm -rf /tmp/geex.container.store
            fi
            if [ -f "/tmp/geex.outdated.scriptfile.dd" ]; then
                rm /tmp/geex.outdated.scriptfile.dd
            fi
            echo "Cleaned up extra stores as well."
            exit 1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        h|-h|--h|help|-help|--help)
            echo -e \
                 "Usage: ENVIRONMENT geex OPTION
Run geex with OPTION, if given.

COMMAND must be one of the sub-commands listed below:

  main commands
    help                         display help message
    git                          display github repository url and developer contact information
    install                      start the interactive installer
    debug                        start application in debug mode
    clean                        clean up all possible leftovers
    live                         enable live preview mode
    mover                        start the mover mode
    features                     get a list of things the installer configures
    bootstrap                    launch interactive ISO bootstrapper
    container                    launch VERY experimental container manager

ENVIRONMENT can be one of the environment variables listed below:

  main environment variables
    GEEX_DEBUG                   start application in debug mode
    GEEX_DEBUG_MODE              start application in debug mode
    GEEX_VERBOSE_MODE            enable verbose mode for more feedback
    GEEX_IGNORE_MISSING          ignore if packages are missing
    GEEX_LIVE_MODE               enable live preview mode for the installer
    GEEX_DEBUG_MISSING_ENABLE    pretend as if packages were missing
    GEEX_PRETEND_FATAL           pretend to have encountered a fatal error
    GEEX_IGNORE_FORCED_DEBUG     ignore forced debug mode if herd was not found
    GEEX_MISSING_HERD_IGNORE     ignore whether herd/shepherd is missing
    GEEX_SKIP_WIFI               ignore if there is no active internet connection
    GEEX_EDITOR                  force the use of a specific editor for editing
                                 the configuration file (if selected)
    GEEX_MANUAL_NET_SETUP_MODE   force the installer to throw you into manual networking
                                 setup mode
    GEEX_FORCE_AGNOSTIC          force distro agnostic mode, which is highly experimental

  experimental environment variables
    GEEX_THE_HURD                force the installer to set your system up with GNU Hurd
    GEEX_THE_HURD_ALLOW          force installer to install with GNU Hurd, warning: this is
                                 a joke variable, do not install GNU Hurd as your systems
                                 main kernel - it will likely not boot, nor support your
                                 hardware
    GEEX_FORCE_THE_HURD          force installation even if installer is set to GNU Hurd

  bootstrapper environment variables
    GEEX_BOOTSTRAPPER_WRITE      switch from pretending to write, to actually writing
                                 your image to your selected disk
    BOOTSTRAPPER_FORCE_FORMAT    force the bootstrapper to format and write image to
                                 selected disk, even if escalation error is encountered

EXAMPLES that you may consider running yourself listed below:

  main examples
    ./geex.sh i                  run installer
    ./geex.sh i d                run installer in debug mode
    ./geex.sh d v i              run installer in debug and verbose mode
    ./geex.sh d v i l            run installer in debug, verbose, and live mode
    ./geex.sh d m                run mover in debug mode
    ./geex.sh c                  clean up any remains
    ./geex.sh feat               list features of the installer

NOTICE for you to consider:

  installer notices
    if you run the installer without debug mode, it will try to install gnu guix on your system or one
    of your disks, please be aware of this and ALWAYS run the installer in DEBUG MODE before deciding
    to actually use it to install an operating system (GNU Guix).

    as a last-resort safety mechanism, the installer forcefully enables DEBUG MODE if it detects that
    shepherd (herd) as a command is not available on your system, automatically assuming you are
    running the installer on a system with which you do not intend to actually install GNU Guix.

  mover notices
    if you run the mover without debug mode, it will try to move and copy files into your system without
    warning. there are backup hooks to try and prevent accidental file deletion, but it is better to check
    and back up your own files FIRST, before running the mover in non-debug mode."
            exit 1
            ;;
    esac
done

moverFunction() {
    # Create Randomized Backup Name
    export randFunc="$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 12)"
    export randFuncString="$(echo $randFunc)"
    export randEtcName="$(echo guix.backup-etc.$randFuncString)"
    export randCfgName="$(echo guix.backup-cfg.$randFuncString)"

    if [[ -z "$GEEX_DEBUG" ]] || [[ -z "$GEEX_DEBUG_MODE" ]]; then
        if command -v printf >/dev/null; then
            printf "[ Mover ]: You are running the Mover without Debug Mode, this will move files, make backups, and possibly\ntamper with your system configuration. Are you sure you want to proceed? (y/n): "
            read -r moverProceed
            export moverProceedAnswer=$moverProceed
        else
            echo -e "[ Mover ]: You are running the Mover without Debug Mode, this will move files, make backups, and possibly\ntamper with your system configuration. Are you sure you want to proceed? (y/n): "
            read -r moverProceed
            export moverProceedAnswer=$moverProceed
        fi
        if [[ "$moverProceedAnswer" == *n* ]]; then
            export GEEX_DEBUG=1
            export GEEX_DEBUG_MODE=1
            export GEEX_MOVER_FORCE_DEBUG=1
        fi
    fi
    # Check for Debug Mode - if not set, Backup Files - else Pretend
    if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ] || [ -n "$GEEX_MOVER_FORCE_DEBUG" ]; then
        echo "[ Debug ]: Debug Mode Detected, pretending to Backup Files..."
    else
        # Backup '/etc/guix'
        if [ -d /etc/guix ]; then
            cp -r /etc/guix /tmp/$randEtcName
            cp -r /etc/guix $HOME/$randEtcName
            echo "[ Backup ]: Created Backups of your '/etc/guix' at '/tmp/$randEtcName' and '$HOME/$randEtcName'."
            export backedUpEtc="yes"
        else
            echo "[ Status ]: '/etc/guix' not found - not backing up."
        fi
        # Backup '~/.config/guix'
        if [ -d ~/.config/guix ]; then
            cp -r ~/.config/guix /tmp/$randCfgName
            cp -r ~/.config/guix $HOME/$randCfgName
            echo "[ Backup ]: Created Backups of your '~/.config/guix' at '/tmp/$randCfgName' and '$HOME/$randCfgName'."
            export backedUpCfg="yes"
        else
            echo "[ Status ]: '~/.config/guix' not found - not backing up."
        fi
    fi

    # Declaring Escalation Utility
    if command -v doas >/dev/null 2>&1; then
        export escalationUtil="doas"
    elif command -v sudo >/dev/null 2>&1; then
        export escalationUtil="sudo"
    else
        export escalationUtil="su"
    fi
    echo "[ Status ]: Pinned Escalation Utility to '$escalationUtil'..."

    # Pin Username
    export userName="$(echo $USER)"
    if [ "$userName" == "root" ]; then
        echo "[ Warning ]: Cannot create Backups for User 'root'"
        printf "[ Input ]: Please enter Username: "
        read -r manualUserName
        if [ "$manualUserName" == "" ]; then
            export userGuess="$(users | awk '{print $1}')"
            echo "[ Warning ]: Input was Empty, guessing User as '$userGuess'..."
            export manualUserName="$(echo $userGuess)"
        fi
        export userName=$manualUserName
    fi
    echo "[ Status ]: Pinned Username to '$userName'..."

    if [ "$HOME" == "/root" ]; then
        echo "[ Warning ]: Cannot copy Files to 'root' Home"
        printf "[ Input ]: Please enter '$userName' Home Path: "
        read -r manualHomeDirectory
        if [ "$manualHomeDirectory" = "" ]; then
            echo "[ Warning ]: Input was Empty, guessing Home as '/home/$userName'..."
            export manualHomeDirectory="$(echo /home/$userName)"
        fi
        export homeDirectory=$manualHomeDirectory
    else
        export homeDirectory=$HOME
    fi
    echo "[ Status ]: Pinned Home to '$homeDirectory'..."

    # Check for Debug Mode - if not set, Copy Files - else, Pretend
    if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_VAR" ]; then
        echo "[ Debug ]: Mode Detected, pretending to Copy Files..."
        export copyState="pretended to copy"
        echo "[ Debug ]: Copied Geex Files to respective Directories"
    else
        echo "[ Notice ]: Please be ready to enter your Super User/Root Password soon."
        if [ "$escalationUtil" != "su" ]; then
            # Copy Channels
            $escalationUtil cp channels.scm $homeDirectory/.config/guix/channels.scm
            $escalationUtil cp channels.scm /etc/guix/channels.scm
            # Copy Directories
            $escalationUtil cp -r files /etc/guix/files
            $escalationUtil cp -r systems /etc/guix/systems
            $escalationUtil cp -r containers /etc/guix/containers
            # Copy Configs
            $escalationUtil cp home.scm /etc/guix/home.scm
            $escalationUtil cp config.scm /etc/guix/config.scm
            # Set Copy Status Variable
            export copyState="copied"
            echo "[ Creation ]: Copied Geex Files to respective Directories"
        else
            # Copy Channels
            $escalationUtil -c 'cp channels.scm $homeDirectory/.config/guix/channels.scm'
            $escalationUtil -c 'cp channels.scm /etc/guix/channels.scm'
            # Copy Directories
            $escalationUtil -c 'cp -r files /etc/guix/files'
            $escalationUtil -c 'cp -r systems /etc/guix/systems'
            $escalationUtil -c 'cp -r containers /etc/guix/containers'
            # Copy Configs
            $escalationUtil -c 'cp home.scm /etc/guix/home.scm'
            $escalationUtil -c 'cp config.scm /etc/guix/config.scm'
            # Set Copy Status Variable
            export copyState="copied"
            echo "[ Creation ]: Copied Geex Files to respective Directories"
        fi
    fi

    # Evaluate Backup Status
    if [[ "$backedUpEtc" == "yes" ]] && [[ "$backedUpCfg" == "yes" ]]; then
        export backupState="your '/etc/guix', as well as your '~/.config/guix'"
    elif [[ "$backedUpEtc" == "yes" ]] && [[ "$backedUpCfg" != "yes" ]]; then
        export backupState="your '/etc/guix'"
    elif [[ "$backedUpEtc" != "yes" ]] && [[ "$backedUpCfg" == "yes" ]]; then
        export backupState="your '~/.config/guix'"
    else
        export backupState="nothing"
    fi

    # Print Results
    echo -e \
    "
[ Geex ]
We have backed up $backupState, and $copyState over the Geex configuration files to their appropriate destination directories.

[ Configuration ]
To switch the system configuration between 'laptop', 'desktop', 'libre', or 'minimal', change the '%systemchoice' variable in the '/etc/guix/config.scm' file.

[ Commands ]
To rebuild your system, run:
 - guix system reconfigure /etc/guix/config.scm

To rebuild your guix home, run:
 - guix home reconfigure /etc/guix/home.scm

To start a home container, run:
 - guix home container /etc/guix/containers/<home-container>.scm

To start a system container, run:
 - guix system build /etc/guix/containers/<system-container>.scm
 - /gnu/store/<hash>-system
 - guix container exec <PID> /run/current-system/profile/bin/bash --login
"
    if [ -n "$GEEX_DEBUG" ]; then
        echo -e "\n[ Debug Status Report ]: The Mover has been run in Debug mode."
        if [ "$GEEX_MOVER_FORCE_DEBUG" == 1 ]; then
            echo -e "\n[ Mover Status Report ]: You ran the Mover without Debug Mode, but decided to retroactively enable it through the safety question."
        fi
    fi
    export moverFinished=1
}

for arg in "$@"; do
    case "$arg" in
        m|-m|--m|mover|-mover|--mover)
            export GEEX_MOVER_MODE=1
            ;;
    esac
done

# Check if Commands are Missing
export missingCommandCount=0
for cmd in cp awk dialog git grep parted lsblk find ps mke2fs lspci wpa_supplicant curl wget; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "[ Warning ]: Missing required binary: $cmd" >&2
        export missingCommandCount=$(($missingCommandCount + 1))
    fi
done
if ! command -v dhclient >/dev/null 2>&1 && ! command -v dhcpcd >/dev/null 2>&1; then
    echo "[ Warning ]: Missing both 'dhclient' and 'dhcpcd', addressing issue..." >&2
    export missingCommandCount=$(($missingCommandCount + 1))
fi

if command -v herd >/dev/null; then
    echo "[ Status ]: 'herd' found, continuing."
elif [[ "$GEEX_IGNORE_FORCED_DEBUG" == 1 ]] || [[ -n "$GEEX_IGNORE_FORCED_DEBUG" ]]; then
    echo "[ Status ]: Ignoring forced debug mode..."
else
    export GEEX_DEBUG=1
    export GEEX_DEBUG_MODE=1
    export GEEX_FORCED_DEBUG=1
fi

# If Missing Command Debug is Enabled, pretend to be Missing Commands
if [ -n "$GEEX_DEBUG_MISSING_ENABLE" ]; then
    if [ ! -n "$GEEX_IGNORE_MISSING" ]; then
        echo -e "[ Debug ]: Missing Debug Enabled, pretending to have missing Packages.\n[ Warning ]: Commands missing, but found no way to retrieve them temporarily.\nAborting unless Variable 'GEEX_IGNORE_MISSING' is set."
        if [ ! -n "$GEEX_IGNORE_MISSING" ]; then
            echo "[ Status ]: Variable 'GEEX_IGNORE_MISSING' not set, aborting Process..."
            exit 1
        fi
    fi
fi

# If Commands are Missing, Open a Guix Shell with them Present
if [[ "$missingCommandCount" != 0 ]]; then
  if [ -z "$GUIX_ENVIRONMENT" ] && echo "[ Status ]: Checking for Guix, then running shell exec hook..." && command -v guix >/dev/null 2>&1 && guix shell coreutils bash gawk grep parted findutils util-linux git-minimal dialog procps e2fsprogs pciutils wpa-supplicant isc-dhcp curl wget -- true >/dev/null 2>&1; then
      echo "[ Guix ]: Found Guix, running guix shell exec hook..."
      export IN_GUIX_SHELL=1
      export GEEX_RUNNING_IN="guix"
      echo "[ Geex ]: Please wait while the Guix Shell initializes itself..."
      exec guix shell coreutils bash gawk grep parted findutils util-linux git-minimal dialog procps e2fsprogs pciutils wpa-supplicant isc-dhcp curl wget -- bash "$0" "$@"
  elif [ -z "$IN_NIX_SHELL" ] && echo "[ Warning ]: Guix not found, checking for Nix, then running shell exec hook..." && command -v nix-shell >/dev/null 2>&1 && nix-shell -p coreutils gawk bash gnugrep parted findutils util-linux git dialog procps e2fsprogs pciutils wpa_supplicant dhcpcd curl wget --run true >/dev/null 2>&1; then
      echo "[ Nix ]: Found Nix, running nix shell exec hook..."
      export GEEX_RUNNING_IN="nix"
      echo "[ Geex ]: Please wait while the Nix Shell initializes itself..."
      exec nix-shell -p coreutils bash gawk gnugrep parted findutils util-linux git dialog procps e2fsprogs pciutils wpa_supplicant dhcpcd curl wget --run "bash "$0" "$@""
  else
      echo -e "[ Warning ]: Commands missing, but found no way to retrieve them temporarily.\nAborting unless Variable 'GEEX_IGNORE_MISSING' is set."
      if [ ! -n "$GEEX_IGNORE_MISSING" ]; then
          echo "[ Status ]: Variable 'GEEX_IGNORE_MISSING' not set, aborting Process..."
          exit 1
      fi
  fi
else
    echo "[ Status ]: All required Commands present, continuing..."
fi

if [ ! -z "$GUIX_ENVIRONMENT" ]; then
    echo "[ Status ]: Running inside Guix Shell for Command Compatibility"
fi
if [ ! -z "$IN_NIX_SHELL" ]; then
    echo "[ Status ]: Running inside Nix Shell for Command Compatibility"
fi

if leftoverFileReport=$(ls -l -a /tmp | grep "geex") >/dev/null; then
    export verboseFoundLeftoverFiles="Yes"
else
    export verboseFoundLeftoverFiles="No"
fi

cat > /tmp/geex.channels.dd <<'EOF'
(append (list
         (channel
          (name 'jonabron)
          (branch "master")
          (url "https://github.com/librepup/jonabron.git"))
         (channel
          (name 'nonguix)
          (url "https://gitlab.com/nonguix/nonguix")
          (commit "48a8706d44040cc7014f36873dbd834c048aadd3")
          (introduction
           (make-channel-introduction
            "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
            (openpgp-fingerprint
             "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
         (channel
           (name 'guix)
           (url "https://git.guix.gnu.org/guix.git")
           (commit "4baa120b8b298bec155c5b12c8068dda3c07df40")
           (branch "master")
           (introduction
             (make-channel-introduction
               "9edb3f66fd807b096b48283debdcddccfea34bad"
               (openpgp-fingerprint
                "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
         (channel
          (name 'emacs)
          (url "https://github.com/garrgravarr/guix-emacs")
          (commit "6601278b9ec901e20cfe5fd9caee3d9ce6e6d0c9")
          (introduction
           (make-channel-introduction
            "d676ef5f94d2c1bd32f11f084d47dcb1a180fdd4"
            (openpgp-fingerprint
             "2DDF 9601 2828 6172 F10C  51A4 E80D 3600 684C 71BA")))))
        %default-channels)
EOF

cat > /tmp/geex.config.custom.template.dd <<'EOF'
(use-modules (gnu)
             (gnu system)
             (gnu system nss)
             (gnu packages)
             (gnu packages xorg)
             (gnu packages certs)
             (gnu packages shells)
             (gnu packages admin)
             (gnu packages base)
             (gnu services)
             (gnu services xorg)
             (gnu services desktop)
             (gnu services nix)
             (gnu services sound)
             (gnu services audio)
             (gnu services networking)
             (gnu services virtualization)
             (guix)
             (guix utils)
             GEEX_NONFREE_MODULES_OPTIONAL
             GEEX_NVIDIA_MODULES_OPTIONAL
             (jonabron packages wm)
             (jonabron packages terminals)
             (jonabron packages editors)
             (jonabron packages shells)
             (jonabron packages ai)
             (jonabron packages emacs)
             (jonabrok packages entertainment)
             (jonabron packages fonts)
             (jonabron packages games)
             (jonabron packages communication))

(use-service-modules desktop
                     sound
                     audio
                     networking
                     ssh
                     xorg
                     dbus)
(use-package-modules wm
                     bootloaders
                     certs
                     shells
                     version-control
                     xorg)

(define %guix-os
  (operating-system
    GEEX_KERNEL_OPTIONAL
    GEEX_INITRD_OPTIONAL
    GEEX_FIRMWARE_OPTIONAL
    (host-name "GEEX_HOSTNAME")
    (timezone "GEEX_TIMEZONE")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout GEEX_KEYBOARD_LAYOUT))

    ;; Bootloader
    GEEX_BIOS_OPTIONAL

    GEEX_FILESYSTEM_OPTIONAL

    GEEX_SWAP_OPTIONAL

    ;; Users
    (users (cons (user-account
                   (name "GEEX_USERNAME")
                   (comment "GEEX_USERNAME User")
                   (group "users")
                   (home-directory "/home/GEEX_USERNAME")
                   (supplementary-groups '("wheel" "netdev"
                                           "audio"
                                           "video"
                                           "input"
                                           GEEX_NIX_GROUP_OPTIONAL
                                           "tty"))
                   (shell (file-append zsh "/bin/zsh"))) %base-user-accounts))

    ;; Packages
    (packages (append (map specification->package
                           '("eza"
                             "bat"
                             "zoxide"
                             GEEX_DOAS_PACKAGE_OPTIONAL
                             GEEX_I3_PACKAGE_OPTIONAL
                             GEEX_NAITRE_PACKAGE_OPTIONAL
                             GEEX_XMONAD_PACKAGE_OPTIONAL
                             "ripgrep"
                             "grep"
                             "coreutils"
                             "file"
                             "glibc-locales"
                             "ncurses"
                             "zsh"
                             "git-minimal"
                             "emacs-no-x"
                             "usbutils"
                             "pciutils"
                             "procps"
                             "wget"
                             "curl"
                             GEEX_BUNDLE_OPTIONAL
                             "nss-certs"
                             "bash"
                             "sed"
                             GEEX_EXTRA_PACKAGE_LIST_OPTIONAL
                             "kitty"))))

    ;; Services
    (services
     (append (list
              GEEX_NIX_SERVICE_OPTIONAL
              GEEX_HURD_SERVICE_OPTIONAL
              GEEX_GNOME_SERVICE_OPTIONAL
              GEEX_TLP_SERVICE_OPTIONAL
              GEEX_DOAS_SERVICE_OPTIONAL
              GEEX_XORG_SERVICE_OPTIONAL
             )

             (modify-services %desktop-services
               (gdm-service-type config =>
                                 (gdm-configuration (inherit config)
                                                    (wayland? #t)))
               (guix-service-type config =>
                                  (guix-configuration (inherit config)
                                                      (substitute-urls (append
                                                                        (list
                                                                         "https://ci.guix.gnu.org"
                                                                         "https://berlin.guix.gnu.org"
                                                                         "https://bordeaux.guix.gnu.org"
                                                                         "https://substitutes.nonguix.org"
                                                                         "https://hydra-guix-129.guix.gnu.org"
                                                                         "https://substitutes.guix.gofranz.com")
                                                                        %default-substitute-urls))
                                                      (authorized-keys (append
                                                                        (list (local-file
                                                                               "/etc/guix/files/keys/nonguix.pub"))
                                                                        %default-authorized-guix-keys))))
               (mingetty-service-type config =>
                                      (mingetty-configuration (inherit config)
                                                              (auto-login
                                                               "GEEX_USERNAME"))))))))

GEEX_OS_END_CALL_BLOCK
EOF


# Setup Hooks
escalationUtilHook() {
    if [[ "$USER" == "root" ]]; then
        export escalationUtilInfo="root"
        export escalationUtil="root"
        return 0
    fi
    if command -v doas >/dev/null; then
        export escalationUtil="doas"
    elif command -v sudo >/dev/null; then
        export escalationUtil="sudo"
    elif command -v su >/dev/null; then
        export escalationUtil="su"
    else
        export escalationUtil="none"
    fi
}
runWithEscalationUtil() {
    local cmd="$*"
    case "$escalationUtil" in
        "doas")
            export escalationUtilInfo="doas"
            doas sh -c "$cmd"
            ;;
        "sudo")
            export escalationUtilInfo="sudo"
            sudo -v && sudo sh -c "$cmd"
            ;;
        "su")
            export escalationUtilInfo="su"
            su root -c "$cmd"
            ;;
        "none")
            warning=$(dialog --backtitle "Geex" --title "Warning" --yesno "Geex was unable to detect any valid Escalation Utility on your System, and would like to Quit now.\n\nSelect 'Yes' to Quit, and 'No' to ignore this Warning and Continue despite the missing Escalation Tool." 18 75)
            escalationWarning_RESPONSE_CODE=$?
            if [[ ! "$escalationWarning_RESPONSE_CODE" -eq 0 ]]; then
                dialog --clear
                clear
                echo "[ Error ]: No Escalation Tool Available, Aborting..."
                exit 1
            fi
            sh -c "$cmd"
            ;;
        "root")
            export escalationUtilInfo="root"
            sh -c "$cmd"
            ;;
        *)
            export escalationUtilInfo="none"
            sh -c "$cmd"
            ;;
    esac
}
checkMountPointHook() {
    export randomMountNum=$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 12)
    export longRandomString=$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 64)
    if mountpoint -q /mnt; then
        if mountpoint -q /Mount; then
            if mountpoint -q /Geex; then
                if mountpoint -q /UniqueMountPointFromGeex; then
                    export geexMount="$(echo -e "/mnt${randomMountNum}")"
                    if [[ ! -d "$geexMount" ]]; then
                        mkdir -p $geexMount
                    fi
                    if mountpoint -q $geexMount; then
                        unset geexMount
                        export geexMount="$(echo -e "/tmp/geex.emergency.mount-${longRandomString}")"
                        if [[ ! -d "$geexMount" ]]; then
                            mkdir -p $geexMount
                        fi
                    fi
                else
                    if [[ ! -d "/UniqueMountPointFromGeex" ]]; then
                        mkdir -p /UniqueMountPointFromGeex
                    fi
                    export geexMount=/UniqueMountPointFromGeex
                fi
            else
                if [[ ! -d "/Geex" ]]; then
                    mkdir -p /Geex
                fi
                export geexMount=/Geex
            fi
        else
            if [[ ! -d "/Mount" ]]; then
                mkdir -p /Mount
            fi
            export geexMount=/Mount
        fi
    else
        if [[ ! -d "/mnt" ]]; then
            mkdir -p /mnt
        fi
        export geexMount=/mnt
    fi
    if mountpoint -q $geexMount; then
        export geexLastMountCheckResult="fatal"
    fi
    if [ "$geexLastMountCheckResult" == "fatal" ] || [ "$GEEX_PRETEND_FATAL" == 1 ]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Fatal Error" --msgbox "The installer has encountered a fatal, un-recoverable error:\n - Every single possible mount-point (including ones with 64-symbol long randomly generated name) are already marked as used.\n\nThis is most certainly a problem with your system, filesystem permissions, or the result of someone having tampered with the code of this installer.\n\nPlease FIX THIS YOURSELF before trying again. The installer will now forcefully exit." 34 68 3>&1 1>&2 2>&3)
        dialog --clear
        clear
        exit 1
    fi
    if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
        verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has analized your systems mount-points and determined that:\n\n$geexMount\n\nis the appropriate, free mount-point to use for systems initialization.\n\nThe Installer tested '/mnt', '/Mount', '/Geex', '/UniqueMountPointFromGeex', and '/mnt${randomMountNum}' for available mount-points.\n\nIn worst-case scenarios, the installer would have fell back to mount to '/tmp/geex.emergency.mount-${longRandomString}'." 34 68 3>&1 1>&2 2>&3)
    fi
}
channelPullHook() {
    if [ -f "/tmp/geex.channels.dd" ]; then
        cp /tmp/geex.channels.dd /tmp/channels.scm
        if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
            verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has successfully copied the '/tmp/geex.channels.dd' file to '/tmp/channels.scm', and is now ready to pull the required Guix Channels." 24 40 3>&1 1>&2 2>&3)
        fi
        if [ -d "${geexMount}/etc/guix" ]; then
            cp /tmp/channels.scm ${geexMount}/etc/guix/channels.scm
            if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
                verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has successfully copied the '/tmp/channels.scm' file to '${geexMount}/etc/guix/channels.scm', and is now ready to pull the required Guix Channels." 24 40 3>&1 1>&2 2>&3)
            fi
            export pullFrom="mnt"
        elif [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
            export pullFrom="Mock"
        else
            if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
                verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has not been able to set up the 'channels.scm' file to be copied to/appear in '${geexMount}/etc/guix/channels.scm', and will now continue to pull the channels from '/tmp/channels.scm'." 24 40 3>&1 1>&2 2>&3)
            fi
            export pullFrom="tmp"
        fi
        if [ "$pullFrom" == "mnt" ]; then
            export GEEX_GUIX_CHANNEL_PULL_CHECKFILE=/tmp/geex.guix.channel.pull.check.file.dd
            if [ -f "$GEEX_GUIX_CHANNEL_PULL_CHECKFILE" ]; then
                rm $GEEX_GUIX_CHANNEL_PULL_CHECKFILE
            fi
            guix pull --channels=${geexMount}/etc/guix/channels.scm && touch $GEEX_GUIX_CHANNEL_PULL_CHECKFILE
            if [ -f "$GEEX_GUIX_CHANNEL_PULL_CHECKFILE" ]; then
                export finPull=1
            else
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer has failed to pull in the new and required channels in '${geexMount}/etc/guix/channels.scm'. This error is un-recoverable, and the installer would recommend you to abort the installation process now, and investigate this error. Possible causes are:\n\n - 'guix' Command is not Available\n - Installer Failed to write the Channels File ('${geexMount}/etc/guix/channels.scm')\n - Your Filesystem was not correctly Mounted to '${geexMount}'.\n\nIf you still want to continue, which is NOT RECOMMENDED, select 'Ignore and Continue Anyways', otherwise investigate the error and try again later." 40 120 10 \
                                      abort "Abort" \
                                      ignore "Ignore and Continue Anyways" \
                                      3>&1 1>&2 2>&3) || exit 1
                if [ "$errorMessage" == "abort" ]; then
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                else
                    export finPull=0
                fi
            fi
        elif [ "$pullFrom" == "tmp" ]; then
            export GEEX_GUIX_CHANNEL_PULL_CHECKFILE=/tmp/geex.guix.channel.pull.check.file.dd
            if [ -f "$GEEX_GUIX_CHANNEL_PULL_CHECKFILE" ]; then
                rm $GEEX_GUIX_CHANNEL_PULL_CHECKFILE
            fi
            guix pull --channels=/tmp/channels.scm && touch $GEEX_GUIX_CHANNEL_PULL_CHECKFILE
            if [ -f "$GEEX_GUIX_CHANNEL_PULL_CHECKFILE" ]; then
                export finPull=1
            else
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer has failed to pull in the new and required channels in '/tmp/channels.scm'. This error is un-recoverable, and the installer would recommend you to abort the installation process now, and investigate this error. Possible causes are:\n\n - 'guix' Command is not Available\n - Installer Failed to write the Channels File ('/tmp/channels.scm')\n - Your Filesystem was not correctly Mounted to '${geexMount}'.\n - Your '/tmp' Directory is not Writeable/Read-Only\n\nIf you still want to continue, which is NOT RECOMMENDED, select 'Ignore and Continue Anyways', otherwise investigate the error and try again later." 40 120 10 \
                                      abort "Abort" \
                                      ignore "Ignore and Continue Anyways" \
                                      3>&1 1>&2 2>&3) || exit 1
                if [ "$errorMessage" == "abort" ]; then
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                else
                    export finPull=0
                fi
            fi
        elif [ "$pullFrom" == "Mock" ]; then
            if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
                verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has set your Guix Channel Pull-Method (pullFrom=$pullFrom) to 'Mock', this probably happened because you are running in Debug Mode.\n\nThe Installer will now pretend to have pulled the required Channels correctly and continue with a Mock Installation Process." 24 40 3>&1 1>&2 2>&3)
            fi
            echo "[ Debug ]: Pretending to have pulled channels..."
            export finPull=1
        else
            export finPull=0
        fi
    else
        if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
            verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has encountered one or more errors trying to correctly set-up your 'channels.scm' Guix Channels File on both your '${geexMount}', as well as your '/tmp' directories.\n\nIt will now try to pull the 'channels.scm' file directly from GitHub in a last attempt to recover and continue with the installation process as intended." 24 40 3>&1 1>&2 2>&3)
        fi
        mkdir -p /tmp/geex.git.storage
        git clone https://github.com/librepup/geex.git /tmp/geex.git.storage/geex
        cp /tmp/geex.git.storage/geex/channels.scm /tmp/geex.git.channels.scm
        export GEEX_GUIX_CHANNEL_PULL_CHECKFILE=/tmp/geex.guix.channel.pull.check.file.dd
        if [ -f "$GEEX_GUIX_CHANNEL_PULL_CHECKFILE" ]; then
            rm $GEEX_GUIX_CHANNEL_PULL_CHECKFILE
        fi
        guix pull /tmp/geex.git.channels.scm && touch $GEEX_GUIX_CHANNEL_PULL_CHECKFILE
        if [ -f "$GEEX_GUIX_CHANNEL_PULL_CHECKFILE" ]; then
            export finPull=1
        else
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer has failed to pull in the new and required channels in '/tmp/geex.git.channels.scm'. This error is un-recoverable, and the installer would recommend you to abort the installation process now, and investigate this error. Possible causes are:\n\n - 'git' Command is not Available\n - The Repository URL Changed and is now Outdated (Unlikely)\n - 'guix' Command is not Available\n - Installer Failed to write the Channels File ('/tmp/geex.git.channels.scm')\n - Your Filesystem was not correctly Mounted to '${geexMount}'.\n - Your '/tmp' Directory is not Writeable/Read-Only\n\nIf you still want to continue, which is NOT RECOMMENDED, select 'Ignore and Continue Anyways', otherwise investigate the error and try again later." 40 120 10 \
                                  abort "Abort" \
                                  ignore "Ignore and Continue Anyways" \
                                  3>&1 1>&2 2>&3) || exit 1
            if [ "$errorMessage" == "abort" ]; then
                dialog --clear
                clear
                echo "[ Status ]: Aborting..."
                exit 1
            else
                export finPull=0
            fi
        fi
    fi
    if [ "$finPull" == 0 ]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer failed to pull the appropriate Guix channels, due to one of the following issues:\n1. Connection issue - check your internet connection.\n2. File mismatch - cannot find channels.scm occurrence anywhere.\n3. Guix binary missing - if this is the issue, make sure you have the command 'guix' available.\n\nContinue anyways?" 32 50 10 \
                              abort "Abort" \
                              continue "Continue Anyways" \
                              3>&1 1>&2 2>&3) || exit 1
        if [ "$errorMessage" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        else
            export channelReport="No"
        fi
    elif [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
        if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
            successMessage=$(dialog --backtitle "Geex Installer" --title "Channels" --menu "Debug Mode Detected, pretending as if channels were pulled successfully.\n\nYour Mock Installation will now continue as planned." 32 50 10 \
                            continue "Continue" \
                            abort "Abort" \
                            3>&1 1>&2 2>&3) || exit 1
            if [ "$successMessage" == "abort" ]; then
                dialog --clear
                clear
                echo "[ Status ]: Aborting..."
                exit 1
            fi
        fi
        export channelReport="Mock"
    else
        successMessage=$(dialog --backtitle "Geex Installer" --title "Channels" --menu "Successfully pulled in the latest channels! Installation will now continue as planned." 32 50 10 \
                                continue "Continue" \
                                abort "Abort" \
                                3>&1 1>&2 2>&3) || exit 1
        if [ "$successMessage" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        export channelReport="Yes"
    fi
}
kernelHook() {
    if [[ -n "$GEEX_THE_HURD" ]] || [[ "$GEEX_THE_HURD" == 1 ]]; then
        export hurdBlock="(kernel hurd)\n    (hurd hurd)\n    (server-services %base-services-hurd)\n"
        sed -i "s|GEEX_KERNEL_OPTIONAL|$hurdBlock|g" /tmp/geex.config.${stager}.dd
        sed -i "/GEEX_INITRD_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        sed -i "/GEEX_FIRMWARE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        sed -i "/GEEX_NONFREE_MODULES_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        sed -i "s|ext4|ext2|g" /tmp/geex.config.${stager}.dd
        return 0
    fi
    if lspci | grep -i intel | grep -i "wireless" >/dev/null || lspci | grep -i nvidia | grep -i "VGA"; then
        userWantsNonfreeKernel=$(dialog --backtitle "Geex Installer" --title "Kernel" --menu "Do you want to enable and install the non-free Linux Kernel, instead of the default Linux-Libre Kernel offered by GNU Guix?\n\nInfo: The installer detected an Intel Wireless or Ethernet Device in your machine, you may want to say 'Yes'." 20 50 10 \
                                        yes "Yes (Recommended)" \
                                        no "No" \
                                        3>&1 1>&2 2>&3) || exit 1
    else
        userWantsNonfreeKernel=$(dialog --backtitle "Geex Installer" --title "Kernel" --menu "Do you want to enable and install the non-free Linux Kernel, instead of the default Linux-Libre Kernel offered by GNU Guix?\n\nInfo: The installer did not detect any proprietary Intel Wireless or Ethernet Device in your machine, however, it is still recommended to use the full Linux Kernel." 20 50 10 \
                                        no "No" \
                                        yes "Yes (Recommended)" \
                                        3>&1 1>&2 2>&3) || exit 1
    fi
    if [[ "$userWantsNonfreeKernel" == "yes" ]]; then
        export userWantsNonfreeKernel="yes"
    elif [[ "$userWantsNonfreeKernel" == "no" ]]; then
        export userWantsNonfreeKernel="no"
    else
        export userWantsNonfreeKernel="yes"
    fi
    if [[ -f "/tmp/geex.config.${stager}.dd" ]]; then
        modulesNonfreeBlock="$(echo -e "             (nongnu packages linux)\n             (nongnu system linux-initrd)\n")"
        kernelNonfreeBlock="$(echo -e "(kernel linux)")"
        if [[ -f "/tmp/geex.kernel.block.dd" ]]; then
            rm /tmp/geex.kernel.block.dd
        fi
        if [[ -f "/tmp/geex.kernel.modules.block.dd" ]]; then
            rm /tmp/geex.kernel.modules.block.dd
        fi
        if [[ "$userWantsNonfreeKernel" == "yes" ]]; then
            echo -e "    (kernel linux)" >> /tmp/geex.kernel.block.dd
            sed -i "/GEEX_KERNEL_OPTIONAL/{
                   r /tmp/geex.kernel.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export wroteKernelBlock="Yes (Non-Free)"
            echo -e "             (nongnu packages linux)\n             (nongnu system linux-initrd)" >> /tmp/geex.kernel.modules.block.dd
            sed -i "/GEEX_NONFREE_MODULES_OPTIONAL/{
                   r /tmp/geex.kernel.modules.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export initrd="(initrd microcode-initrd)"
            export firmware="(firmware (append (list intel-microcode linux-firmware) %base-firmware))"
            sed -i "s|GEEX_INITRD_OPTIONAL|$initrd|g" /tmp/geex.config.${stager}.dd
            sed -i "s|GEEX_FIRMWARE_OPTIONAL|$firmware|g" /tmp/geex.config.${stager}.dd
        elif [[ "$userWantsNonfreeKernel" == "no" ]]; then
            sed -i "/GEEX_KERNEL_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_NONFREE_MODULES_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_INITRD_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_FIRMWARE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export wroteKernelBlock="Yes (Free)"
        elif [[ "$userWantsNonfreeKernel" != "yes" ]] && [[ "$userWantsNonfreeKernel" != "no" ]]; then
            echo -e "    (kernel linux)" >> /tmp/geex.kernel.block.dd
            sed -i "/GEEX_KERNEL_OPTIONAL/{
                   r /tmp/geex.kernel.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export wroteKernelBlock="Yes (Fallback Non-Free)"
            echo -e "             (nongnu packages linux)\n             (nongnu system linux-initrd)" >> /tmp/geex.kernel.modules.block.dd
            sed -i "/GEEX_NONFREE_MODULES_OPTIONAL/{
                   r /tmp/geex.kernel.modules.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export initrd="(initrd microcode-initrd)"
            export firmware="(firmware (append (list intel-microcode linux-firmware) %base-firmware))"
            sed -i "s|GEEX_INITRD_OPTIONAL|$initrd|g" /tmp/geex.config.${stager}.dd
            sed -i "s|GEEX_FIRMWARE_OPTIONAL|$firmware|g" /tmp/geex.config.${stager}.dd
        else
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an error while trying to write the Kernel Block into your '/tmp/geex.config.${stager}.dd' configuration file. This error is not supposed to be possible to occur, so it is more than likely that you or someone else has tampered with this installers code before executing it, please make sure you are running the official Geex Installer." 34 75 3>&1 1>&2 2>&3)
            export wroteKernelBlock="No"
        fi
    else
        export wroteKernelBlock="No"
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an error while trying to write the Kernel Block into your '/tmp/geex.config.${stager}.dd' configuration file. This error is not supposed to be possible to occur, so it is more than likely that you or someone else has tampered with this installers code before executing it, please make sure you are running the official Geex Installer." 34 75 3>&1 1>&2 2>&3)
    fi
}
xorgHook() {
    if [[ "$userWantsNonfreeKernel" == "no" ]]; then
        if [[ -f "/tmp/geex.xorg.optional.dd" ]]; then
            rm /tmp/geex.xorg.optional.dd
        fi
        if [[ -n "$wroteXorgBlock" ]]; then
            unset wroteXorgBlock
        fi
        echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)))\n" >> /tmp/geex.xorg.optional.dd
        sed -i "s|GEEX_NVIDIA_MODULES_OPTIONAL||g" /tmp/geex.config.${stager}.dd
        if [[ -f "/tmp/geex.xorg.nvidia.modules.optional.dd" ]]; then
            rm /tmp/geex.xorg.nvidia.modules.optional.dd
        fi
        sed -i "/GEEX_XORG_SERVICE_OPTIONAL/{
               r /tmp/geex.xorg.optional.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteXorgBlock="Yes${wroteXorgBlock}"
        return 0
    fi
    if [[ "$GEEX_THE_HURD" == 1 ]] || [[ -n "$GEEX_THE_HURD" ]]; then
        sed -i "/GEEX_NVIDIA_MODULES_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        sed -i "/GEEX_XORG_SERVICE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        return 0
    fi
    if lspci | grep -i nvidia >/dev/null; then
        userWantsNvidia=$(dialog --backtitle "Geex Installer" --title "Nvidia" --menu "Do you want to enable and install proprietary Nvidia Drivers?" 14 50 10 \
                                 yes "Yes" \
                                 no "No" \
                                 3>&1 1>&2 2>&3) || exit 1
    else
        userWantsNvidia=$(dialog --backtitle "Geex Installer" --title "Nvidia" --menu "Do you want to enable and install proprietary Nvidia Drivers?" 14 50 10 \
                                 no "No" \
                                 yes "Yes" \
                                 3>&1 1>&2 2>&3) || exit 1
    fi
    if [[ "$userWantsNvidia" == "yes" ]]; then
        export userWantsNvidia="yes"
    elif [[ "$userWantsNvidia" == "no" ]]; then
        export userWantsNvidia="no"
    else
        export userWantsNvidia="no"
    fi
    if [ -f "/tmp/geex.xorg.optional.dd" ]; then
        rm /tmp/geex.xorg.optional.dd
    fi
    if [ -f "/tmp/geex.xorg.nvidia.modules.optional.dd" ]; then
        rm /tmp/geex.xorg.nvidia.modules.optional.dd
    fi
    modulesBlockNvidia="$(echo -e "             (nongnu packages nvidia)\n             (nongnu services nvidia)\n             (nonguix transformations)")"
    xorgBlockNvidia="$(echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)\n                                        (modules (cons nvidia-driver\n                                                       %default-xorg-modules))\n                                        (drivers '("nvidia"))))\n")"
    xorgBlockRegular="$(echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)))\n")"
    if [[ "$userWantsNvidia" == "yes" ]]; then
        echo -e "             (nongnu packages nvidia)\n             (nongnu services nvidia)\n             (nonguix transformations)" >> /tmp/geex.xorg.nvidia.modules.optional.dd
        echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)\n                                        (modules (cons nvidia-driver\n                                                       %default-xorg-modules))\n                                        (drivers '(\"nvidia\"))))\n" >> /tmp/geex.xorg.optional.dd
        if [[ -n "$wroteXorgBlock" ]]; then
            unset wroteXorgBlock
        fi
        export wroteXorgBlock=" (Nvidia)"
        if [[ -f "/tmp/geex.config.${stager}.dd" ]]; then
            sed -i "/GEEX_NVIDIA_MODULES_OPTIONAL/{
                   r /tmp/geex.xorg.nvidia.modules.optional.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
        else
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered a fatal Error:\n\nIt cannot find the '/tmp/geex.config.${stager}.dd' file to work with and insert the optional Nvidia Modules block. The Installer will now quit forcefully." 34 75 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Error ]: Aborting..."
            exit 1
        fi
    else
        if [[ -n "$wroteXorgBlock" ]]; then
            unset wroteXorgBlock
        fi
        echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)))\n" >> /tmp/geex.xorg.optional.dd
        sed -i "/GEEX_NVIDIA_MODULES_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        if [[ -f "/tmp/geex.xorg.nvidia.modules.optional.dd" ]]; then
            rm /tmp/geex.xorg.nvidia.modules.optional.dd
        fi
    fi
    if [[ -f "/tmp/geex.xorg.optional.dd" ]]; then
        sed -i "/GEEX_XORG_SERVICE_OPTIONAL/{
               r /tmp/geex.xorg.optional.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteXorgBlock="Yes${wroteXorgBlock}"
    elif [[ ! -f "/tmp/geex.xorg.optional.dd" ]]; then
        export xorgBlockFallback="$(echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)))\n")"
        if [ -f "/tmp/geex.xorg.optional.dd" ]; then
            rm /tmp/geex.xorg.optional.dd
        fi
        echo -e "                   (set-xorg-configuration\n                    (xorg-configuration (keyboard-layout keyboard-layout)))\n" >> /tmp/geex.xorg.optional.dd
        sed -i "/GEEX_XORG_SERVICE_OPTIONAL/{
               r /tmp/geex.xorg.optional.dd
               d
               }" /tmp/geex.config.${stager}.dd
        if [ -f "/tmp/geex.xorg.optional.dd" ]; then
            rm /tmp/geex.xorg.optional.dd
        fi
        export wroteXorgBlock="Yes (Fallback)"
    else
        export wroteXorgBlock="No"
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an error while trying to write the Xorg Block into your '/tmp/geex.config.${stager}.dd' configuration file. This error is not supposed to be possible to occur, so it is more than likely that you or someone else has tampered with this installers code before executing it, please make sure you are running the official Geex Installer." 34 75 3>&1 1>&2 2>&3)
    fi
}
transformationsHook() {
    if [[ "$userWantsNonfreeKernel" == "no" ]]; then
        if [[ -f "/tmp/geex.compose.block.dd" ]]; then
            rm /tmp/geex.compose.block.dd
        fi
        export composeBlock="$(echo -e "%guix-os")"
        echo -e $composeBlock >> /tmp/geex.compose.block.dd
        sed -i "/GEEX_OS_END_CALL_BLOCK/{
               r /tmp/geex.compose.block.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteOSEndBlock="Yes"
        export wroteComposeBlock="No"
        return 0
    fi
    if [[ "$GEEX_THE_HURD" == 1 ]] || [[ -n "$GEEX_THE_HURD" ]]; then
        if [[ -f "/tmp/geex.compose.block.dd" ]]; then
            rm /tmp/geex.compose.block.dd
        fi
        export composeBlock="$(echo -e "%guix-os")"
        echo -e $composeBlock >> /tmp/geex.compose.block.dd
        sed -i "/GEEX_OS_END_CALL_BLOCK/{
               r /tmp/geex.compose.block.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteOSEndBlock="Yes"
        export wroteComposeBlock="No"
        return 0
    fi
    if [[ "$userWantsNvidia" == "yes" ]]; then
        if [[ -f "/tmp/geex.compose.block.dd" ]]; then
            rm /tmp/geex.compose.block.dd
        fi
        export composeBlock="$(echo -e "((compose (nonguix-transformation-nvidia))\n %guix-os)")"
        echo -e "((compose (nonguix-transformation-nvidia))\n %guix-os)" >> /tmp/geex.compose.block.dd
        sed -i "/GEEX_OS_END_CALL_BLOCK/{
               r /tmp/geex.compose.block.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteOSEndBlock="Yes"
        export wroteComposeBlock="Yes"
    elif [[ "$userWantsNvidia" == "no" ]]; then
        if [[ -f "/tmp/geex.compose.block.dd" ]]; then
            rm /tmp/geex.compose.block.dd
        fi
        export composeBlock="$(echo -e "%guix-os")"
        echo -e $composeBlock >> /tmp/geex.compose.block.dd
        sed -i "/GEEX_OS_END_CALL_BLOCK/{
               r /tmp/geex.compose.block.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteOSEndBlock="Yes"
        export wroteComposeBlock="No"
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered a fatal Error:\n\nIt cannot write the compose/os-end block to '/tmp/geex.config.${stager}.dd'. This could be because '/tmp' is read-only, doesn't exist, or someone tinkered with the code of this installer.\n\nPlease verify you can write to '/tmp', and that you are using the official Geex Installer. The installer will now forcefully quit." 34 75 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Aborting..."
        exit 1
    fi
}
desktopEnvironmentsHook() {
    deSelection=$(dialog --checklist "Select Desktop(s):" 15 50 5 \
                         gnome "Gnome" on \
                         naitre "NaitreHUD" on \
                         xmonad "XMonad" off \
                         i3wm "i3wm" off \
                         3>&1 1>&2 2>&3) || exit 1
    # i3wm
    i3PackageBlock="$(echo -e "                             \"i3-wm\"\n                             \"i3-autotiling\"\n                             \"dmenu\"\n                             \"i3status\"")"
    if [ -f "/tmp/geex.wm.i3.packages.dd" ]; then
        rm /tmp/geex.wm.i3.packages.dd
    fi
    echo "$i3PackageBlock" >> /tmp/geex.wm.i3.packages.dd
    # Gnome
    gnomeServiceBlock="$(echo -e "                   (service gnome-desktop-service-type)")"
    if [ -f "/tmp/geex.wm.gnome.service.dd" ]; then
        rm /tmp/geex.wm.gnome.service.dd
    fi
    echo "$gnomeServiceBlock" >> /tmp/geex.wm.gnome.service.dd
    # NaitreHUD
    naitrePackageBlock="$(echo -e "                             \"naitre\"\n                             \"vicinae\"\n                             \"waybar\"\n                             \"dankmaterialshell\"\n                             \"swaybg\"\n                             \"wl-clipboard\"")"
    if [ -f "/tmp/geex.wm.naitre.packages.dd" ]; then
        rm /tmp/geex.wm.naitre.packages.dd
    fi
    echo "$naitrePackageBlock" >> /tmp/geex.wm.naitre.packages.dd
    # XMonad
    xmonadPackageBlock="$(echo -e "                             \"xmonad\"\n                             \"ghc-xmonad-contrib\"\n                             \"xmobad\"")"
    if [ -f "/tmp/geex.wm.xmonad.packages.dd" ]; then
        rm /tmp/geex.wm.xmonad.packages.dd
    fi
    echo "$xmonadPackageBlock" >> /tmp/geex.wm.xmonad.packages.dd
    # Replace if Selected
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        if [[ "$deSelection" == *i3wm* ]]; then
            sed -i "/GEEX_I3_PACKAGE_OPTIONAL/{
                   r /tmp/geex.wm.i3.packages.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedDesktopi3=1
        else
            sed -i "/GEEX_I3_PACKAGE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedDesktopi3=0
        fi
        if [[ "$deSelection" == *gnome* ]]; then
            sed -i "/GEEX_GNOME_SERVICE_OPTIONAL/{
                   r /tmp/geex.wm.gnome.service.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedDesktopGnome=1
        else
            sed -i "/GEEX_GNOME_SERVICE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedDesktopGnome=0
        fi
        if [[ "$deSelection" == *naitre* ]]; then
            sed -i "/GEEX_NAITRE_PACKAGE_OPTIONAL/{
                   r /tmp/geex.wm.naitre.packages.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedDesktopNaitre=1
        else
            sed -i "/GEEX_NAITRE_PACKAGE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedDesktopNaitre=0
        fi
        if [[ "$deSelection" == *xmonad* ]]; then
            sed -i "/GEEX_XMONAD_PACKAGE_OPTIONAL/{
                   r /tmp/geex.wm.xmonad.packages.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedDesktopXmonad=1
        else
            sed -i "/GEEX_XMONAD_PACKAGE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedDesktopXmonad=0
        fi
        export finishedDesktopSetup=1
    else
        export finishedDesktopSetup=0
    fi
    if [ "$finishedDesktopSetup" == 0 ]; then
        export areDesktopsWritten="No"
        export deSelection="None"
    else
        export areDesktopsWritten="Yes"
    fi
}
serviceSetupHook() {
    serviceSelection=$(dialog --checklist "Select Services:" 15 50 5 \
                              hurd "GNU Hurd" off \
                              nix "Nix" off \
                              doas "doas" on \
                              tlp "TLP (Laptop)" off \
                              3>&1 1>&2 2>&3) || exit 1
    read -r -a serviceSelectionArray <<< "$serviceSelection"
    serviceSelectionCount="${#serviceSelectionArray[@]}"
    serviceSelectionSummaryText=$(printf '%s\n' "${serviceSelectionArray[@]}")
    # Nix Service
    nixServiceBlock="$(echo -e "                   (service nix-service-type)\n")"
    nixGroupBlock="$(echo -e "                                           \"nixbld\"")"
    if [ -f "/tmp/geex.service.nix.block.dd" ]; then
        rm /tmp/geex.service.nix.block.dd
    fi
    if [ -f "/tmp/geex.group.nix.block.dd" ]; then
        rm /tmp/geex.group.nix.block.dd
    fi
    echo -e "$nixServiceBlock" >> /tmp/geex.service.nix.block.dd
    echo -e "$nixGroupBlock" >> /tmp/geex.group.nix.block.dd
    # TLP Service
    if [ -f "/tmp/geex.service.tlp.block.dd" ]; then
        rm /tmp/geex.service.tlp.block.dd
    fi
    tlpServiceBlock="$(echo -e "                   (service tlp-service-type\n                            (tlp-configuration (cpu-scaling-governor-on-ac '(\"performace\"))\n                                               (cpu-scaling-governor-on-bat '(\"powersave\"))\n                                               (sched-powersave-on-bat? #t)))")"
    echo "$tlpServiceBlock" >> /tmp/geex.service.tlp.block.dd
    # Doas Service
    doasPackageBlock="$(echo -e "                             \"opendoas\"")"
    doasServiceBlock="$(echo -e "                   (simple-service 'doas-config etc-service-type\n                                   (list \`(\"doas.conf\" ,(plain-file\n                                                         \"doas.conf\"\n                                                         \"permit nopass keepenv root\npermit persist keepenv setenv :wheel\"))))")"
    if [ -f "/tmp/geex.service.doas.block.dd" ]; then
        rm /tmp/geex.service.doas.block.dd
    fi
    if [ -f "/tmp/geex.package.doas.block.dd" ]; then
        rm /tmp/geex.package.doas.block.dd
    fi
    echo "$doasServiceBlock" >> /tmp/geex.service.doas.block.dd
    echo "$doasPackageBlock" >> /tmp/geex.package.doas.block.dd
    # Hurd Service
    hurdServiceBlock="$(echo -e "                   (service hurd-vm-service-type\n                                       (hurd-vm-configuration (memory-size 2048)\n                                                              (secret-directory \"/etc/guix/hurd-secrets\")))")"
    if [ -f "/tmp/geex.service.hurd.block.dd" ]; then
        rm /tmp/geex.service.hurd.block.dd
    fi
    echo -e "$hurdServiceBlock" >> /tmp/geex.service.hurd.block.dd
    # Replace if Selected
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        if [[ "$serviceSelection" == *doas* ]]; then
            sed -i "/GEEX_DOAS_SERVICE_OPTIONAL/{
                   r /tmp/geex.service.doas.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_DOAS_PACKAGE_OPTIONAL/{
                   r /tmp/geex.package.doas.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedServiceDoas=1
        else
            sed -i "/GEEX_DOAS_SERVICE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_DOAS_PACKAGE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedServiceDoas=0
        fi
        if [[ "$serviceSelection" == *tlp* ]]; then
            sed -i "/GEEX_TLP_SERVICE_OPTIONAL/{
                   r /tmp/geex.service.tlp.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedServiceTlp=1
        else
            sed -i "/GEEX_TLP_SERVICE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedServiceTlp=0
        fi
        if [[ "$serviceSelection" == *nix* ]]; then
            sed -i "/GEEX_NIX_SERVICE_OPTIONAL/{
                   r /tmp/geex.service.nix.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_NIX_GROUP_OPTIONAL/{
                   r /tmp/geex.group.nix.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedServiceNix=1
        else
            sed -i "/GEEX_NIX_SERVICE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            sed -i "/GEEX_NIX_GROUP_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedServiceNix=0
        fi
        if [[ "$serviceSelection" == *hurd* ]]; then
            sed -i "/GEEX_HURD_SERVICE_OPTIONAL/{
                   r /tmp/geex.service.hurd.block.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export installedServiceHurd=1
        else
            sed -i "/GEEX_HURD_SERVICE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export installedServiceHurd=0
        fi
        export finishedServiceSetup=1
    else
        export finishedServiceSetup=0
    fi
    if [ "$finishedServiceSetup" == 0 ]; then
        export areServicesWritten="No"
        export serviceSelection="None"
    else
        export areServicesWritten="Yes"
    fi
}
biosHook() {
    if [[ -d /sys/firmware/efi ]]; then
        export detectedBios="(U)EFI"
    else
        export detectedBios="Legacy"
    fi
    detectBiosNotice=$(dialog --backtitle "Geex Installer" --title "BIOS Auto-Detection" --msgbox "The Installer has detected that you are using '$detectedBios' as your BIOS type. You may want to select this option/BIOS type at the next questionnaire/menu selection." 10 50 3>&1 1>&2 2>&3)
    if [ -f "/tmp/geex.detectedbios.dd" ]; then
        rm /tmp/geex.detectedbios.dd
    fi
    if [ "$detectedBios" == "Legacy" ]; then
        bios=$(dialog --backtitle "Geex Installer" --title "BIOS" --menu "Select BIOS Type" 10 60 10 \
                  legacy "Legacy" \
                  uefi "(U)EFI" \
                  3>&1 1>&2 2>&3) || exit 1
        export bios=$bios
    else
        bios=$(dialog --backtitle "Geex Installer" --title "BIOS" --menu "Select BIOS Type" 10 60 10 \
                  uefi "(U)EFI" \
                  legacy "Legacy" \
                  3>&1 1>&2 2>&3) || exit 1
        export bios=$bios
    fi
}
disksHook() {
    if [ "$OLD_DISK_HOOK" == 1 ]; then
        detectedDisksNoticeText="The Installer has detected the following Disks available to your Device:"
        detectedDisks="$(lsblk -o NAME,LABEL,UUID,FSTYPE)"
        echo -e "$detectedDisksNoticeText\n\n$detectedDisks" >> /tmp/geex.detecteddisks.dd
        detectedDisksNotice=$(dialog --backtitle "Geex Installer" --title "Disk Auto-Detection" --textbox "/tmp/geex.detecteddisks.dd" 22 75 3>&1 1>&2 2>&3) || exit 1
        disk=$(dialog --backtitle "Geex Installer" --title "Disk" --inputbox "Enter your Disk Name (e.g. '/dev/sda', '/dev/sdb', '/dev/nvme0n1'):" 8 40 \
                      3>&1 1>&2 2>&3) || exit 1
        if [ "$disk" == "" ]; then
            dialog --clear
            clear
            echo "[ Error ]: No Disk provided, aborting..."
            exit 1
        fi
        if [[ "$disk" == /dev/nvme* ]]; then
            export diskPrefixed="${disk}p"
        else
            export diskPrefixed="$disk"
        fi
        export disk=$disk
    else
        PARTS_WITH_LABELS=$(ls -l /dev/disk/by-label/ | awk '{print $11 " " $9"\\n"}' | sed "s|../../|\n/dev/|g")
        if [[ "$PARTS_WITH_LABELS" == "" ]] || [[ -z "$PARTS_WITH_LABELS" ]]; then
            echo "[ Status ]: No partitions with labels found, skipping notice message..."
        else
            partitionsNotice=$(dialog --backtitle "Geex Installer" --title "Partitions Notice" --msgbox "The Installer has detected the following Partitions with a Label assigned to them, you may want to watch out and make sure you do not overwrite the Disk they are a Part of, if these Partitions are important to you.\n\n$PARTS_WITH_LABELS" 15 50 3>&1 1>&2 2>&3) || exit 1
        fi
        DISK_LIST=$(lsblk -dno NAME,SIZE | awk '{print "/dev/"$1, "("$2")"}')
        SELECTED_DISK=$(dialog --menu "Select Disk" 15 50 10 $DISK_LIST 3>&1 1>&2 2>&3) || exit 1
        if [[ -z "$SELECTED_DISK" ]]; then
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "You have not selected a valid (or any) disk, the installer cannot continue and will now abort the installation process." 32 50 10 \
                                  okay "Okay" \
                                  3>&1 1>&2 2>&3) || exit 1
            if [ "$errorMessage" == "okay" ]; then
                dialog --clear
                clear
                echo "[ Status ]: Aborting..."
                exit 1
            else
                dialog --clear
                clear
                echo "[ Status ]: You have somehow selected a non-existent option in the error message, this is not intended - please verify that the Geex installer's code has not been tampered with.\n[ Status ]: Aborting..."
                exit 1
            fi
        fi
        export disk=$SELECTED_DISK
        if [[ "$disk" == /dev/nvme* ]]; then
            export diskPrefixed="${disk}p"
        else
            export diskPrefixed="$disk"
        fi
    fi
}
disksSetup() {
    echo "Formatting disks ($disk)..."
    if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
        echo "[ Status ]: Debug Mode Detected..."
        export formattedDisksStatus=2
        if [ "$userWantsSwap" == 1 ]; then
            export formattedWithSwap="Mock Yes"
        else
            export formattedWithSwap="Mock No"
        fi
        if [ "$GEEX_THE_HURD" == 1 ]; then
            export formattedHurd=1
        fi
    else
        if [[ "$escalationUtilInfo" == "none" ]]; then
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an Escalation Error: no valid escalation utility was found, and the current user is also not root. The Installer will forceably quit now." 34 68 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Error ]: Escalation Error, Aborting..."
            exit 1
        fi
        if [ "$bios" == "legacy" ]; then
            if [[ "$userWantsSwap" == 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
                runWithEscalationUtil "parted $disk --script \
                     mklabel msdos \
                     mkpart primary linux-swap 1MiB 4096MiB \
                     mkpart primary ext4 4096MiB 100% \
                     set 2 boot on"
                runWithEscalationUtil "mkswap -L guix-swap ${diskPrefixed}1 && swapon ${diskPrefixed}1 &&  mkfs.ext4 -L guix-root ${diskPrefixed}2 && mount ${diskPrefixed}2 ${geexMount}"
                export formattedWithSwap="Yes"
                export formattedDisksStatus=1
            elif [[ "$userWantsSwap" != 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
                runWithEscalationUtil "parted $disk --script \
                  mklabel msdos \
                  mkpart primary ext4 1MiB 100% \
                  set 1 boot on"
                runWithEscalationUtil "mkfs.ext4 -L guix-root ${diskPrefixed}1 && mount ${diskPrefixed}1 ${geexMount}"
                export formattedWithSwap="No"
                export formattedDisksStatus=1
            else
                runWithEscalationUtil "parted $disk --script \
                     mklabel msdos \
                     mkpart primary ext2 1MiB 100% \
                     set 1 boot on"
                runWithEscalationUtil "mkfs.ext2 -L guix-root ${diskPrefixed}1 && mount ${diskPrefixed}1 ${geexMount}"
                export formattedDisksStatus=1
                export formattedWithSwap="No"
                export formattedHurd=1
            fi
        else
            # UEFI Logic
            if [[ "$userWantsSwap" == 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
                runWithEscalationUtil "parted $disk --script \
                     mklabel gpt \
                     mkpart primary linux-swap 1MiB 4096MiB \
                     name 1 guix-swap \
                     mkpart ESP fat32 4096MiB 6144MiB \
                     name 2 guix-efi \
                     set 2 esp on \
                     mkpart primary ext4 6144MiB 100% \
                     name 3 guix-root"
                runWithEscalationUtil "mkswap -L guix-swap ${diskPrefixed}1 && swapon ${diskPrefixed}1 &&  mkfs.fat -F32 -n guix-efi ${diskPrefixed}2 && mkfs.ext4 -L guix-root ${diskPrefixed}3 &&  mount ${diskPrefixed}3 ${geexMount} && mkdir -p ${geexMount}/boot/efi && mount ${diskPrefixed}2 ${geexMount}/boot/efi"
                export formattedWithSwap="Yes"
                export formattedDisksStatus=1
            elif [[ "$userWantsSwap" != 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
              runWithEscalationUtil "parted $disk --script \
                  mklabel gpt \
                  mkpart ESP fat32 1MiB 2048MiB \
                  name 1 guix-efi \
                  set 1 esp on \
                  mkpart primary ext4 2048MiB 100% \
                  name 2 guix-root"
                runWithEscalationUtil "mkfs.fat -F32 -n guix-efi ${diskPrefixed}1 && mkfs.ext4 -L guix-root ${diskPrefixed}2 &&  mount ${diskPrefixed}2 ${geexMount} && mkdir -p ${geexMount}/boot/efi && mount ${diskPrefixed}1 ${geexMount}/boot/efi"
                echo -e "\nFinished (U)EFI Formatting and Mounting\n"
                export formattedDisksStatus=1
                export formattedWithSwap="No"
            else
                runWithEscalationUtil "parted $disk --script \
                     mklabel gpt \
                     mkpart ESP fat32 1MiB 2048MiB \
                     set 1 esp on \
                     name 1 guix-efi \
                     mkpart primary ext2 2048MiB 100% \
                     name 2 guix-root"
                runWithEscalationUtil "mkfs.fat -F32 -n guix-efi ${diskPrefixed}1 && mkfs.ext2 -L guix-root ${diskPrefixed}2 && mount ${diskPrefixed}2 ${geexMount} && mkdir -p ${geexMount}/boot/efi && mount ${diskPrefixed}1 ${geexMount}/boot/efi"
                export formattedDisksStatus=1
                export formattedWithSwap="No"
                export formattedHurd=1
            fi
        fi
        if [[ "$formattedDisksStatus" != 1 ]]; then
            export formattedDisksStatus=0
        fi
    fi
    if [ "$wroteBiosBlock" == 0 ]; then
        export wroteBiosBlock="No"
    else
        export wroteBiosBlock="Yes"
    fi
}
filesystemHook() {
    local swapBlock=""
    if [ "$userWantsSwap" == 1 ]; then
        export swapBlock="    (swap-devices (list (swap-space (target (file-system-label \"guix-swap\")))))"
        if [ -f "/tmp/geex.swap.block.dd" ]; then
            rm /tmp/geex.swap.block.dd
        fi
        echo "$swapBlock" >> /tmp/geex.swap.block.dd
        sed -i "/GEEX_SWAP_OPTIONAL/{
               r /tmp/geex.swap.block.dd
               d
               }" /tmp/geex.config.${stager}.dd
        export wroteSwapBlock="Yes"
    else
        unset swapBlock
        sed -i "/GEEX_SWAP_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        export wroteSwapBlock="No"
    fi
    export rootPartName=$(ls /dev/disk/by-label/ | grep -x -e 'guix-root' -e 'GUIX-ROOT')
    if [[ "$rootPartName" == "" ]]; then
        export rootPartName="guix-root"
    fi
    if [ "$bios" == "uefi" ]; then
        export efiPartName=$(ls /dev/disk/by-label/ | grep -x -e 'guix-efi' -e 'GUIX-EFI')
        if [[ "$efiPartName" == "" ]]; then
            export efiPartName="guix-efi"
        fi
        export filesystemBlock="$(echo -e "    (file-systems (cons* (file-system\n                           (mount-point \"/\")\n                           (device (file-system-label \"$rootPartName\"))\n                           (type \"ext4\"))\n                         (file-system\n                           (mount-point \"/boot/efi\")\n                           (device (file-system-label \"$efiPartName\"))\n                           (type \"vfat\")) %base-file-systems))\n")"
    else
        export filesystemBlock="$(echo -e "    (file-systems (cons* (file-system\n                           (mount-point \"/\")\n                           (device (file-system-label \"$rootPartName\"))\n                           (type \"ext4\")) %base-file-systems))\n")"
    fi
    if [ -f "/tmp/geex.filesystem.block.dd" ]; then
        rm /tmp/geex.filesystem.block.dd
    fi
    if [[ "$GEEX_THE_HURD" == 1 ]] && [[ -n "$GEEX_THE_HURD" ]]; then
        export filesystemBlockType="Hurd"
        if [[ -n "$filesystemBlock" ]]; then
            unset filesystemBlock
        fi
        if [[ "$bios" == "uefi" ]]; then
            export filesystemBlock="$(echo -e "    (file-systems (cons* (file-system\n                           (mount-point \"/\")\n                           (device (file-system-label \"$rootPartName\"))\n                           (type \"ext2\"))\n                         (file-system\n                           (mount-point \"/boot/efi\")\n                           (device (file-system-label \"$efiPartName\"))\n                           (type \"vfat\")) %base-file-systems))\n")"
        else
            export filesystemBlock="$(echo -e "    (file-systems (cons* (file-system\n                           (mount-point \"/\")\n                           (device (file-system-label \"$rootPartName\"))\n                           (type \"ext2\")) %base-file-systems))\n")"
        fi
    else
        export filesystemBlockType="GNU+Linux"
    fi
    echo "$filesystemBlock" >> /tmp/geex.filesystem.block.dd
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        sed -i "/GEEX_FILESYSTEM_OPTIONAL/{
        r /tmp/geex.filesystem.block.dd
        d
        }" /tmp/geex.config.${stager}.dd
        export wroteFilesystemBlock=1
        export verboseFilesystemBlockText=$filesystemBlock
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer failed to write the File-System Block to '/tmp/geex.config.${stager}.dd'. Do you still want to continue?\n\n(!) Warning (!)\nThis may make your system unable to boot, unless you manually write a file-system block into the resulting, final config." 32 50 10 \
                              continue "Continue" \
                              abort "Abort" \
                              3>&1 1>&2 2>&3) || exit 1
        if [ "$errorMessage" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        export wroteFilesystemBlock=0
    fi
    if [ -n "$swapBlock" ] || [ "$userWantsSwap" == 1 ]; then
        if [ -f "/tmp/geex.swap.block.dd" ]; then
            rm /tmp/geex.swap.block.dd
        fi
        echo "$swapBlock" >> /tmp/geex.swap.block.dd
        sed -i "/GEEX_SWAP_OPTIONAL/{
               r /tmp/geex.swap.block.dd
               d
               }" /tmp/gex.config.${stager}.dd
        export wroteSwapBlock="Yes"
    else
        sed -i "/GEEX_SWAP_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        export wroteSwapBlock="No"
    fi
    if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
        if [ "$wroteFilesystemBlock" == 1 ]; then
            verboseWasFilesystemBlockWritten="Yes"
        else
            verboseWasFilesystemBlockWritten="No"
        fi
        if [ "$verboseWasFilesystemBlockWritten" == "Yes" ]; then
            verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has successfully written your filesystems block into your '/tmp/geex.config.${stager}.dd' configuration file. Below is the full written block for verbosity:\n\nFilesystem Block:\n\`\`\`\n$verboseFilesystemBlockText\n\`\`\`" 34 68 3>&1 1>&2 2>&3)
        else
            verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer failed to write your filesystems block into the appropriate '/tmp/geex.config.${stager}.dd' file for unknown reasons.\n\nPlease investigate this!" 34 68 3>&1 1>&2 2>&3)
        fi
    fi
    if [ "$wroteFilesystemBlock" == 0 ]; then
        export isFilesystemWritten="No"
    else
        export isFilesystemWritten="Yes"
    fi
}
biosLegacyEditHook() {
    export diskPrefixed=$diskPrefixed
    export diskChoice=${diskPrefixed}1
    if [ "$userWantsSwap" == 1 ]; then
        export diskChoice=${diskPrefixed}2
    fi
    legacyBlock="$(echo -e "    (bootloader (bootloader-configuration\n              (keyboard-layout keyboard-layout)\n              (bootloader grub-bootloader)\n              (targets '(\"${disk}\"))))\n")"
    legacyBlockText="$(echo -e "(bootloader (bootloader-configuration\n              (keyboard-layout keyboard-layout)\n              (bootloader grub-bootloader)\n              (targets '(\"${disk}\"))))\n")"
    legacyBlockVerify=$(dialog --backtitle "Geex Installer" --title "Verify BIOS Block" --menu "Please verify that the BIOS Block below is correct and can be written:\n\n\`\`\`\n$legacyBlockText\n\`\`\`\n\n\n  " 28 50 10 \
                               continue "Continue" \
                               abort "Abort" \
                               3>&1 1>&2 2>&3) || exit 1
    if [ "$legacyBlockVerify" == "abort" ]; then
        dialog --clear
        clear
        echo "[ Status ]: Aborting..."
        exit 1
    fi
    if [ -f "/tmp/geex.bios.block.dd" ]; then
        rm /tmp/geex.bios.block.dd
    fi
    echo "$legacyBlock" >> /tmp/geex.bios.block.dd
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        sed -i "/GEEX_BIOS_OPTIONAL/{
        r /tmp/geex.bios.block.dd
        d
        }" /tmp/geex.config.${stager}.dd
        if [ "$GEEX_VERBOSE_MODE" == 1 ]; then
            successMessage=$(dialog --backtitle "Geex Installer" --title "Success" --menu "Successfully wrote BIOS hook into '/tmp/geex.config.${stager}.dd'." 32 50 10 \
                                    continue "Continue" \
                                    abort "Abort" \
                                    3>&1 1>&2 2>&3) || exit 1
            if [ "$successMessage" == "abort" ]; then
                dialog --clear
                clear
                echo "[ Status ] Aborting..."
                exit 1
            fi
        fi
        export wroteBiosBlock=1
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error: '/tmp/geex.config.${stager}.dd' was not found, thus the BIOS hook did not finish writing.\n\nContinue anyways?" 32 50 10 \
                              continue "Continue" \
                              abort "Abort" \
                              3>&1 1>&2 2>&3) || exit 1
        if [ "$errorMessage" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        export wroteBiosBlock=0
    fi
}
biosUefiEditHook() {
    export diskPrefixed=$diskPrefixed
    uefiBlock="$(echo -e "    (bootloader (bootloader-configuration\n              (keyboard-layout keyboard-layout)\n              (bootloader grub-efi-bootloader)\n              (targets '(\"/boot/efi\"))))\n")"
    uefiBlockText="$(echo -e "(bootloader (bootloader-configuration\n              (keyboard-layout keyboard-layout)\n              (bootloader grub-efi-bootloader)\n              (targets '(\"/boot/efi\"))))\n")"
    uefiBlockVerify=$(dialog --backtitle "Geex Installer" --title "Verify BIOS Block" --menu "Please verify that the BIOS Block below is correct and can be written:\n\n\`\`\`\n$uefiBlockText\n\`\`\`\n\n\n  " 28 50 10 \
                             continue "Continue" \
                             abort "Abort" \
                             3>&1 1>&2 2>&3) || exit 1
    if [ "$uefiBlockVerify" == "abort" ]; then
        dialog --clear
        clear
        echo "[ Status ]: Aborting..."
        exit 1
    fi
    if [ -f "/tmp/geex.bios.block.dd" ]; then
        rm /tmp/geex.bios.block.dd
    fi
    echo "$uefiBlock" >> /tmp/geex.bios.block.dd
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        sed -i "/GEEX_BIOS_OPTIONAL/{
        r /tmp/geex.bios.block.dd
        d
        }" /tmp/geex.config.${stager}.dd
        if [ "$GEEX_VERBOSE_MODE" == 1 ]; then
            successMessage=$(dialog --backtitle "Geex Installer" --title "Success" --menu "Successfully wrote BIOS hook into '/tmp/geex.config.${stager}.dd'." 32 50 10 \
                                    continue "Continue" \
                                    abort "Abort" \
                                    3>&1 1>&2 2>&3) || exit 1
            if [ "$successMessage" == "abort" ]; then
                dialog --clear
                clear
                echo "[ Status ] Aborting..."
                exit 1
            fi
        fi
        export wroteBiosBlock=1
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error: '/tmp/geex.config.${stager}.dd' was not found, thus the BIOS hook did not finish writing.\n\nContinue anyways?" 32 50 10 \
                              continue "Continue" \
                              abort "Abort" \
                              3>&1 1>&2 2>&3) || exit 1
        if [ "$errorMessage" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        export wroteBiosBlock=0
    fi
}
systemInstallHook() {
    if [[ "$GEEX_HAS_INTERNET" == 3 ]] || [[ "$GEEX_HAS_INTERNET" == 0 ]]; then
        if [[ -n "$GEEX_HAS_INTERNET" ]]; then
            unset GEEX_HAS_INTERNET
        fi
        if [[ "$GEEX_DEBUG_MODE" != 1 ]]; then
            checkInternetHook
        else
            export GEEX_HAS_INTERNET=2
        fi
    fi
    if [[ "$GEEX_HAS_INTERNET" == 2 ]] && [[ "$GEEX_DEBUG_MODE" != 1 ]]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has detected that you are using 'GEEX_SKIP_WIFI' to skip WiFi verification, yet you are trying to call the System Initialization and Installation Hook. This is not supported, unless 'GEEX_DEBUG' is also set. The Installer will forceably quit now." 32 50 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Aborting..."
        exit 1
    fi
    if [[ "$GEEX_HAS_INTERNET" == 3 ]]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has noticed that you still have not set up Networking for an active Internet connection, and will thus forceably quit now.\n\nPlease make sure you have a working Internet Connection before beginning the System Initialization and Installation Phase." 32 50 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Aborting..."
        exit 1
    fi
    if [[ "$GEEX_HAS_INTERNET" != 1 ]] && [[ "$GEEX_HAS_INTERNET" != 2 ]] && [[ "$GEEX_HAS_INTERNET" != 3 ]]; then
        checkInternetHook
        if [[ "$GEEX_HAS_INTERNET" != 1 ]] && [[ "$GEEX_HAS_INTERNET" != 2 ]] && [[ "$GEEX_HAS_INTERNET" != 3 ]]; then
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has not found any way to identify whether you have an active Internet Connection or not (NetNum: $GEEX_HAS_INTERNET). This is not supposed to happen. You have neither a WiFi Connection, nor an Ethernet Connection, skipped the WiFi Setup (with or without Debug Mode), and also have not been set to 'No Internet'. This is an unrecoverable Error, and the installer will now forceably quit." 34 68 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Error ]: Aborting..."
            exit 1
        fi
    fi
    echo "[ Status ]: Beginning formal GNU Guix installation..."
    if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ] || [[ "$GEEX_DEBUG" == 1 ]] || [[ "$GEEX_DEBUG_MODE" == 1 ]]; then
        echo "[ Status ]: Debug Mode Detected, pretending to install system..."
        if [[ "$GEEX_VERBOSE_MODE" == 1 ]] && [[ -n "$GEEX_VERBOSE_MODE" ]]; then
            if [[ -f "/tmp/geex.config.scm" ]]; then
                verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has successfully copied and used your '/tmp/geex.config.scm' file for the system initialization and installation procedure." 34 68 3>&1 1>&2 2>&3)
            fi
        fi
    else
        if ! command -v herd >/dev/null; then
            echo "[ Error ]: Herd Missing, asking how to continue..."
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error: the 'herd' binary is missing. Do you still want to continue?\n\n(!) Warning (!)\nThe system may not install correctly if the cow-store is not initialized correctly, as fetched and built packages and derivations will remain on the live/current operating systems '/gnu/store' - and won't be written to '${geexMount}/gnu/store', continue at your own risk!" 32 50 10 \
                                  abort "Abort" \
                                  continue "Continue Anyways" \
                                  3>&1 1>&2 2>&3) || exit 1
            if [[ "$errorMessage" != "continue" ]] || [[ "$errorMessage" == "abort" ]]; then
                dialog --clear
                clear
                echo "[ Status ]: Aborting..."
                if [ -n "$GEEX_MISSING_HERD_IGNORE" ]; then
                    unset GEEX_MISSING_HERD_IGNORE
                fi
                exit 1
            else
                echo "[ Status ]: Ignoring error..."
                export GEEX_MISSING_HERD_IGNORE=1
            fi
        fi
        if ! command -v herd >/dev/null && [[ "$GEEX_MISSING_HERD_IGNORE" != 1 ]]; then
            echo "[ Error ]: Herd Missing, asking how to continue..."
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer encountered an error: the 'herd' binary is missing, and the variable 'GEEX_MISSING_HERD_IGNORE' is not set (at all, or to '1'). The Installer will now forceably exit." 32 50 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        elif ! command -v herd >/dev/null && [[ "$GEEX_MISSING_HERD_IGNORE" == 1 ]]; then
            echo "[ Status ]: Ignoring missing 'herd' binary, continuing anyways..."
            if [[ ! -f "/tmp/geex.config.${stager}.scm" ]]; then
                cp /tmp/geex.config.${stager}.dd /tmp/geex.config.${stager}.scm
            fi
            if [[ ! -f "/tmp/geex.config.scm" ]]; then
                cp /tmp/geex.config.${stager}.scm /tmp/geex.config.scm
            fi
            mkdir -p ${geexMount}/etc/guix
            cp /tmp/geex.config.scm ${geexMount}/etc/guix/config.scm
            if [ -f "${geexMount}/etc/guix/config.scm" ]; then
                export GEEX_GUIX_SYSTEM_INIT_CHECKFILE=/tmp/geex.guix.system.init.check.file.dd
                export GEEX_GUIX_STYLE_CHECKFILE=/tmp/geex.guix.style.check.file.dd
                if [ -f "$GEEX_GUIX_STYLE_CHECKFILE" ]; then
                    rm $GEEX_GUIX_STYLE_CHECKFILE
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    rm $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                guix style -f ${geexMount}/etc/guix/config.scm && touch $GEEX_GUIX_STYLE_CHECKFILE
                if [[ "$GEEX_FORCE_THE_HURD" != 1 ]] || [[ "$GEEX_THE_HURD_ALLOW" != 1 ]] || [[ "$GEEX_IGNORE_FORCED_DEBUG" != 1 ]]; then
                    export PROCEED_WITH_HURD=0
                else
                    export PROCEED_WITH_HURD=1
                fi
                if [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$GEEX_FORCE_THE_HURD" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                elif [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$PROCEED_WITH_HURD" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                else
                    runWithEscalationUtil guix system init ${geexMount}/etc/guix/config.scm ${geexMount} && touch $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                if [[ ! -f "$GEEX_GUIX_STYLE_CHECKFILE" ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer failed to style the '/tmp/geex.config.${stager}.scm' file. This is either caused by the fact this file does not exist, or a problem with Guix itself (likely to happen if you do not have the 'guix' command available on your system).\n\nThis is not a fatal error, but it could pre-destine the installer to also fail at later stages that invole the 'guix' command, or other file operations.\n\nPlease investigate this error!" 34 75 3>&1 1>&2 2>&3) || exit 1
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    export installationStatus=1
                else
                    fatalSystemErrorMessage="$(echo -e "The Installer has encountered a Fatal Error, it was not able to initialize the Guix System on your '${geexMount}' via the selected configuration file ('${geexMount}/etc/guix/config.scm').\n\nThis error is un-recoverable, and the installer will now quit, unless you force it to continue running.\n\nSelect 'Okay' to abort the installation process, and 'No, Ignore and Keep Going' to continue anyways (not recommended).")"
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Fatal Error" --menu "$fatalSystemErrorMessage" 40 124 10 \
                                          okay "Okay" \
                                          ignore "No, Ignore and Keep Going" \
                                          3>&1 1>&2 2>&3) || exit 1
                    if [ "$errorMessage" == "okay" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    fi
                    export installationStatus=0
                fi
            elif [ -f "/tmp/geex.config.${stager}.scm" ]; then
                export GEEX_GUIX_SYSTEM_INIT_CHECKFILE=/tmp/geex.guix.system.init.check.file.dd
                export GEEX_GUIX_STYLE_CHECKFILE=/tmp/geex.guix.style.check.file.dd
                if [ -f "$GEEX_GUIX_STYLE_CHECKFILE" ]; then
                    rm $GEEX_GUIX_STYLE_CHECKFILE
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    rm $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                guix style -f /tmp/geex.config.${stager}.scm && touch $GEEX_GUIX_STYLE_CHECKFILE
                if [[ "$GEEX_FORCE_THE_HURD" != 1 ]] || [[ "$GEEX_THE_HURD_ALLOW" != 1 ]] || [[ "$GEEX_IGNORE_FORCED_DEBUG" != 1 ]]; then
                    export PROCEED_WITH_HURD=0
                else
                    export PROCEED_WITH_HURD=1
                fi
                if [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$GEEX_THE_HURD_ALLOW" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                elif [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$PROCEED_WITH_HURD" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                else
                    if [[ ! -f "/tmp/geex.config.scm" ]]; then
                        cp /tmp/geex.config.${stager}.scm /tmp/geex.config.scm
                    fi
                    runWithEscalationUtil guix system init /tmp/geex.config.scm ${geexMount} && touch $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                if [[ ! -f "$GEEX_GUIX_STYLE_CHECKFILE" ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer failed to style the '/tmp/geex.config.${stager}.scm' file. This is either caused by the fact this file does not exist, or a problem with Guix itself (likely to happen if you do not have the 'guix' command available on your system).\n\nThis is not a fatal error, but it could pre-destine the installer to also fail at later stages that invole the 'guix' command, or other file operations.\n\nPlease investigate this error!" 34 75 3>&1 1>&2 2>&3) || exit 1
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    export installationStatus=1
                else
                    fatalSystemErrorMessage="$(echo -e "The Installer has encountered a Fatal Error, it was not able to initialize the Guix System on your '${geexMount}' via the selected configuration file ('/tmp/geex.config.${stager}.scm').\n\nThis error is un-recoverable, and the installer will now quit, unless you force it to continue running.\n\nSelect 'Okay' to abort the installation process, and 'No, Ignore and Keep Going' to continue anyways (not recommended).")"
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Fatal Error" --menu "$fatalSystemErrorMessage" 40 124 10 \
                                          okay "Okay" \
                                          ignore "No, Ignore and Keep Going" \
                                          3>&1 1>&2 2>&3) || exit 1
                    if [ "$errorMessage" == "okay" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    fi
                    export installationStatus=0
                fi
            elif [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
                export installationStatus=2
            else
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error: neither the '${geexMount}/etc/guix/config.scm', nor the '/tmp/geex.config.${stager}.scm' files are present. Or, the 'guix' command is not available to your system and thus not available to the installer.\n\nThe Installer must have failed the copying process, or errorer at a different stage. Please investigate.\n\nThe Installer cannot continue meaningfully, still proceed with the broken installation process?" 32 50 10 \
                                      abort "Abort" \
                                      continue "Yes, still Continue" \
                                      3>&1 1>&2 2>&3) || exit 1
                if [ "$errorMessage" == "abort" ]; then
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                fi
                export installationStatus=0
            fi
        else
            herd start cow-store ${geexMount}
            if [[ ! -f "/tmp/geex.config.${stager}.scm" ]]; then
                cp /tmp/geex.config.${stager}.dd /tmp/geex.config.${stager}.scm
            fi
            if [[ ! -f "/tmp/geex.config.scm" ]]; then
                cp /tmp/geex.config.${stager}.scm /tmp/geex.config.scm
            fi
            mkdir -p ${geexMount}/etc/guix
            cp /tmp/geex.config.scm ${geexMount}/etc/guix/config.scm
            if [ -f "${geexMount}/etc/guix/config.scm" ]; then
                export GEEX_GUIX_SYSTEM_INIT_CHECKFILE=/tmp/geex.guix.system.init.check.file.dd
                export GEEX_GUIX_STYLE_CHECKFILE=/tmp/geex.guix.style.check.file.dd
                if [ -f "$GEEX_GUIX_STYLE_CHECKFILE" ]; then
                    rm $GEEX_GUIX_STYLE_CHECKFILE
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    rm $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                if [[ "$GEEX_FORCE_THE_HURD" != 1 ]] || [[ "$GEEX_THE_HURD_ALLOW" != 1 ]] || [[ "$GEEX_IGNORE_FORCED_DEBUG" != 1 ]]; then
                    export PROCEED_WITH_HURD=0
                else
                    export PROCEED_WITH_HURD=1
                fi
                guix style -f ${geexMount}/etc/guix/config.scm && touch $GEEX_GUIX_STYLE_CHECKFILE
                if [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$GEEX_FORCE_THE_HURD" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                elif [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$PROCEED_WITH_HURD" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                else
                    runWithEscalationUtil guix system init ${geexMount}/etc/guix/config.scm ${geexMount} && touch $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                if [[ ! -f "$GEEX_GUIX_STYLE_CHECKFILE" ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer failed to style the '/tmp/geex.config.${stager}.scm' file. This is either caused by the fact this file does not exist, or a problem with Guix itself (likely to happen if you do not have the 'guix' command available on your system).\n\nThis is not a fatal error, but it could pre-destine the installer to also fail at later stages that invole the 'guix' command, or other file operations.\n\nPlease investigate this error!" 34 75 3>&1 1>&2 2>&3) || exit 1
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    export installationStatus=1
                else
                    fatalSystemErrorMessage="$(echo -e "The Installer has encountered a Fatal Error, it was not able to initialize the Guix System on your '${geexMount}' via the selected configuration file ('${geexMount}/etc/guix/config.scm').\n\nThis error is un-recoverable, and the installer will now quit, unless you force it to continue running.\n\nSelect 'Okay' to abort the installation process, and 'No, Ignore and Keep Going' to continue anyways (not recommended).")"
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Fatal Error" --menu "$fatalSystemErrorMessage" 40 124 10 \
                                          okay "Okay" \
                                          ignore "No, Ignore and Keep Going" \
                                          3>&1 1>&2 2>&3) || exit 1
                    if [ "$errorMessage" == "okay" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    fi
                    export installationStatus=0
                fi
            elif [ -f "/tmp/geex.config.${stager}.scm" ]; then
                export GEEX_GUIX_SYSTEM_INIT_CHECKFILE=/tmp/geex.guix.system.init.check.file.dd
                export GEEX_GUIX_STYLE_CHECKFILE=/tmp/geex.guix.style.check.file.dd
                if [ -f "$GEEX_GUIX_STYLE_CHECKFILE" ]; then
                    rm $GEEX_GUIX_STYLE_CHECKFILE
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    rm $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                if [[ ! -f "/tmp/geex.config.${stager}.scm" ]]; then
                    cp /tmp/geex.config.${stager}.dd /tmp/geex.config.${stager}.scm
                fi
                if [[ ! -f "/tmp/geex.config.scm" ]]; then
                    cp /tmp/geex.config.${stager}.scm /tmp/geex.config.scm
                fi
                guix style -f /tmp/geex.config.scm && touch $GEEX_GUIX_STYLE_CHECKFILE
                if [[ "$GEEX_FORCE_THE_HURD" != 1 ]] || [[ "$GEEX_THE_HURD_ALLOW" != 1 ]] || [[ "$GEEX_IGNORE_FORCED_DEBUG" != 1 ]]; then
                    export PROCEED_WITH_HURD=0
                else
                    export PROCEED_WITH_HURD=1
                fi
                if [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$GEEX_THE_HURD_ALLOW" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                elif [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$PROCEED_WITH_HURD" != 1 ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "The Installer refused to initialize the GNU Guix system, because you have enabled 'GEEX_THE_HURD'. It is not intended to install GNU Guix with GNU Hurd 'the hurd' as your kernel. The Hurd does not support most hardware, is 32-bit only, and intended to be run inside a virtual machine.\n\nIf you want to experience GNU Hurd yourself, unset the variable 'GEEX_THE_HURD', restart the installer, and enable the 'GNU Hurd' service in the services selection." 18 48 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                else
                    runWithEscalationUtil guix system init /tmp/geex.config.scm ${geexMount} && touch $GEEX_GUIX_SYSTEM_INIT_CHECKFILE
                fi
                if [[ ! -f "$GEEX_GUIX_STYLE_CHECKFILE" ]]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer failed to style the '/tmp/geex.config.${stager}.scm' file. This is either caused by the fact this file does not exist, or a problem with Guix itself (likely to happen if you do not have the 'guix' command available on your system).\n\nThis is not a fatal error, but it could pre-destine the installer to also fail at later stages that invole the 'guix' command, or other file operations.\n\nPlease investigate this error!" 34 75 3>&1 1>&2 2>&3) || exit 1
                fi
                if [ -f "$GEEX_GUIX_SYSTEM_INIT_CHECKFILE" ]; then
                    export installationStatus=1
                else
                    fatalSystemErrorMessage="$(echo -e "The Installer has encountered a Fatal Error, it was not able to initialize the Guix System on your '${geexMount}' via the selected configuration file ('/tmp/geex.config.${stager}.scm').\n\nThis error is un-recoverable, and the installer will now quit, unless you force it to continue running.\n\nSelect 'Okay' to abort the installation process, and 'No, Ignore and Keep Going' to continue anyways (not recommended).")"
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Fatal Error" --menu "$fatalSystemErrorMessage" 40 124 10 \
                                          okay "Okay" \
                                          ignore "No, Ignore and Keep Going" \
                                          3>&1 1>&2 2>&3) || exit 1
                    if [ "$errorMessage" == "okay" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    fi
                    export installationStatus=0
                fi
            elif [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
                export installationStatus=2
            else
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error: neither the '${geexMount}/etc/guix/config.scm', nor the '/tmp/geex.config.${stager}.scm' files are present. Or, the 'guix' command is not available to your system and thus not available to the installer.\n\nThe Installer must have failed the copying process, or errorer at a different stage. Please investigate.\n\nThe Installer cannot continue meaningfully, still proceed with the broken installation process?" 32 50 10 \
                                      abort "Abort" \
                                      continue "Yes, still Continue" \
                                      3>&1 1>&2 2>&3) || exit 1
                if [ "$errorMessage" == "abort" ]; then
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                fi
                export installationStatus=0
            fi
        fi
    fi
}
hostnameFunction() {
    while true; do
        hostname=$(dialog --backtitle "Geex Installer" --title "Hostname" --inputbox "Enter Hostname:" 8 40 \
                          3>&1 1>&2 2>&3) || exit 1
        if [[ -n "$hostname" ]]; then
            export hostname=$hostname
            if [ -f "/tmp/geex.config.${stager}.dd" ]; then
                sed -i "s/GEEX_HOSTNAME/$hostname/g" /tmp/geex.config.${stager}.dd
            else
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "File '/tmp/geex.config.${stager}.dd' was not found, thus the Installer did NOT set the Hostname to '$hostname'.\n\nThis Error is un-recoverable, and the installer will now forceably quit." 34 68 3>&1 1>&2 2>&3) || exit 1
                dialog --clear
                clear
                echo "[ Error ]: Aborting..."
                exit 1
            fi
            return 0
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "You have either not provided a Hostname, or the provided Hostname was Invalid. Please try again and enter a valid (or any) Hostname." 32 50
    done
}
usernameFunction() {
    while true; do
        username=$(dialog --backtitle "Geex Installer" --title "Username" --inputbox "Enter Username:" 8 40 \
                          3>&1 1>&2 2>&3) || exit 1
        if [[ -n "$username" ]]; then
            export username=$username
            if [ -f "/tmp/geex.config.${stager}.dd" ]; then
                sed -i "s/GEEX_USERNAME/$username/g" /tmp/geex.config.${stager}.dd
            else
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "File '/tmp/geex.config.${stager}.dd' was not found, thus the Installer did NOT set the Username to '$username'.\n\nThis Error is un-recoverable, and the installer will now forceably quit." 34 68 3>&1 1>&2 2>&3) || exit 1
                dialog --clear
                clear
                echo "[ Error ]: Aborting..."
                exit 1
            fi
            return 0
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "You have either not provided a Username, or the provided Username was Invalid. Please try again and enter a valid (or any) Username." 32 50
    done
}
passwordFunction() {
    while true; do
        password=$(dialog --backtitle "Geex Installer" --title "Password" --passwordbox "Enter 'root' Password" 10 50 \
                          3>&1 1>&2 2>&3) || exit 1
        if [[ -n "$password" ]]; then
            passwordConfirm=$(dialog --backtitle "Geex Installer" --title "Confirm Password" --passwordbox "Confirm 'root' Password" 10 50 \
                                     3>&1 1>&2 2>&3) || exit 1
            if [[ -n "$passwordConfirm" ]] && [ "$password" == "$passwordConfirm" ]; then
                export password=$password
                return 0
            fi
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "You have either not specified a password, or your password confirmation mismatches the password you entered first.\n\nPlease try again.." 32 50
    done
}
userPasswordFunction() {
    while true; do
        userPassword=$(dialog --backtitle "Geex Installer" --title "User Password" --passwordbox "Enter '$username' Password" 10 50 \
                              3>&1 1>&2 2>&3) || exit 1
        if [[ -n "$userPassword" ]]; then
            userPasswordConfirm=$(dialog --backtitle "Geex Installer" --title "Confirm User Password" --passwordbox "Confirm '$username' Password" 10 50 \
                                     3>&1 1>&2 2>&3) || exit 1
            if [[ -n "$userPasswordConfirm" ]] && [ "$userPassword" == "$userPasswordConfirm" ]; then
                export userPassword=$userPassword
                return 0
            fi
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "You have either not specified a password, or your password confirmation mismatches the password you entered first.\n\nPlease try again.." 32 50
    done
}
passwordHook() {
    passwordFunction
    uniqueUserPass=$(dialog --backtitle "Geex Installer" --title "User Password" --menu "Do you want to use the same password for the '$username' account?" 12 50 10 \
                            yes "Yes" \
                            no "No" \
                            3>&1 1>&2 2>&3) || exit 1
    if [ "$uniqueUserPass" == "yes" ]; then
        export userPassword=$password
        export reUsedRootPassword=1
    else
        userPasswordFunction
        export reUsedRootPassword=0
    fi
    if [[ -n "$password" ]] && [[ -n "$userPassword" ]]; then
        if [[ -n "$GEEX_DEBUG" ]] || [[ -n "$GEEX_DEBUG_MODE" ]]; then
            export configuredPasswords=2
            if [[ -n "$GEEX_VERBOSE_MODE" ]]; then
                verbosePopup=$(dialog --backtitle "Geex Installer" --title "Password Setup" --msgbox "Debug Mode Detected, pretending to have set passwords. You have entered the following passwords and options:\n\nRoot Password: $password\n$username Password: $userPassword\nRe-Use Root Password: $uniqueUserPass" 24 40 3>&1 1>&2 2>&3)
            fi
        else
            export configuredPasswords=1
            if [[ -n "$GEEX_VERBOSE_MODE" ]]; then
                verbosePopup=$(dialog --backtitle "Geex Installer" --title "Password Setup" --msgbox "Verbose Mode Detected, informing you on the state of the password configuration process. You have entered the following passwords and options:\n\nRoot Password: $password\n\n$username Password: $userPassword\n\nRe-Use Root Password: $uniqueUserPass" 24 40 3>&1 1>&2 2>&3) || exit 1
            fi
        fi
    else
        export configuredPasswords=0
    fi
    if [ "$configuredPasswords" == 1 ]; then
        export areAllPasswordsSet="Yes"
    elif [ "$configuredPasswords" == 2 ]; then
        export areAllPasswordsSet="Mock"
    else
        export areAllPasswordsSet="No"
    fi
    if [ "$reUsedRootPassword" == 1 ]; then
        export wasPasswordReUsed="Yes"
    else
        export wasPasswordReUsed="No"
    fi
}
passwordApplyHook() {
    if [[ -n "$GEEX_DEBUG" ]] || [[ -n "$GEEX_DEBUG_MODE" ]]; then
        echo "[ Status ]: Debug Mode Detected, pretending to set passwords..."
    else
        printf "%s\n%s\n" "$password" "$password" | passwd -R ${geexMount} root
        printf "%s\n%s\n" "$userPassword" "$userPassword" | passwd -R ${geexMount} $username
    fi
    if [[ -n "$GEEX_VERBOSE_MODE" ]]; then
        verbosePopup=$(dialog --backtitle "Geex Installer" --title "Password Setup" --msgbox "Verbose Mode Detected, informing you that the installer has successfully applied your password settings and configuration to your GNU Guix System installation." 24 40 3>&1 1>&2 2>&3) || exit 1
    fi
}
homeHook() {
    homeQuestion=$(dialog --backtitle "Geex Installer" --title "Home Setup" --menu "The Geex Installer offers the option to copy the generic Geex GNU Guix Home Configuration (home.scm) to your newly installed System.\n\nDo you want to copy the Geex GNU Guix Home Configuration to your system?\n\n(You can edit the '${geexMount}/etc/guix/home.scm' before or after rebooting to make changes.)" 32 50 10 \
                          yes "Yes, Copy the Files" \
                          no "No, don't Copy the Files" \
                          3>&1 1>&2 2>&3) || exit 1
    if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
        echo "[ Status ]: Debug Mode Detected, continuing with mock installation..."
        if [ "$homeQuestion" == "yes" ]; then
            export copiedHome=2
            echo "[ Status ]: Mock-copied Guix Home files..."
        else
            export copiedHome=0
            echo "[ Status ]: Mock-denied the Guix Home File copying process..."
        fi
        export systemFinished=1
        export homeGetMethod="Mock"
    else
        if [ "$homeQuestion" == "yes" ]; then
            if [ -f "/tmp/geex.home.scm" ]; then
                cp /tmp/geex.home.scm ${geexMount}/etc/guix/home.scm
                if [ -d "/tmp/geex.files" ] && [ -d "/tmp/geex.container" ]; then
                    cp -r /tmp/geex.files ${geexMount}/etc/guix/files
                    cp -r /tmp/geex.containers ${geexMount}/etc/guix/containers
                    export copiedHome=1
                    export homeGetMethod="local"
                else
                    mkdir -p /tmp/geex.git.storage
                    git clone https://github.com/librepup/geex.git /tmp/geex.git.storage/geex
                    cp -r /tmp/geex.git.storage/geex/files ${geexMount}/etc/guix/files
                    cp -r /tmp/geex.git.storage/geex/containers ${geexMount}/etc/guix/containers
                    export copiedHome=1
                    export homeGetMethod="local+git"
                fi
                if [ "$copiedHome" == 0 ] || [ -z "$copiedHome" ]; then
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer has encountered an error trying to copy the necessary directories needed for the Guix Home Configuration Setup. This is unexpected, as the installer did not expect it to be possible to encounter an error at this specific stage, as every possibility has been accounted for, so this error is either due to someone having tampered with the Geex Installers Code (do NOT do this), or another un-recoverable error.\n\nThe Installer will now pretend as if you have not selected the option to copy Guix Home Configuration Files at all." 32 50 10 \
                                          okay "Okay" \
                                          abort "Abort" \
                                          3>&1 1>&2 2>&3) || exit 1
                    if [ "$errorMessage" == "abort" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    fi
                    export homeGetMethod="broken"
                    export copiedHome=0
                    export systemFinished=1
                fi
            elif [ "$compatibleDirectory" == 1 ]; then
                cp ${compatibleDirectoryPath}/home.scm ${geexMount}/etc/guix/home.scm
                cp -r ${compatibleDirectoryPath}/files ${geexMount}/etc/guix/files
                cp -r ${compatibleDirectoryPath}/systems ${geexMount}/etc/guix/systems
                cp -r ${compatibleDirectoryPath}/containers ${geexMount}/etc/guix/containers
                export copiedHome=1
                export homeGetMethod="local"
            elif command -v git >/dev/null; then
                mkdir -p /tmp/geex.git.storage
                git clone https://github.com/librepup/geex.git /tmp/geex.git.storage/geex
                cp /tmp/geex.git.storage/geex/home.scm ${geexMount}/etc/guix/home.scm
                cp -r /tmp/geex.git.storage/geex/files ${geexMount}/etc/guix/files
                cp -r /tmp/geex.git.storage/geex/containers ${geexMount}/etc/guix/containers
                export copiedHome=1
                export homeGetMethod="git"
            else
                export copiedHome=0
                errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error while trying to copy the Geex GNU Guix Home Configuration File(s):\n  Neither '/tmp/geex.home.scm', nor the 'git' command were present.\n\nSkipping Guix Home configuration hook." 32 50 10 \
                                      okay "Okay" \
                                      3>&1 1>&2 2>&3) || exit 1
                export homeGetMethod="none"
            fi
            export systemFinished=1
        elif [ "$homeQuestion" == "no" ]; then
            export homeGetMethod="none"
            export copiedHome=0
            export systemFinished=1
            notice=$(dialog --backtitle "Geex Installer" --title "Notice" --menu "You have aborted the copying of Geex GNU Guix Home Configuration Files, the Installer will continue on as if the home configuration hook were never called." 32 50 10 \
                            okay "Okay" \
                            3>&1 1>&2 2>&3) || exit 1
        else
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an unrecoverable error trying to set up (or decide whether to set up or not) your Guix Home Configuration Files. The installer will now exit, please verify that you have not made any changes to the installer's code, or changed the content of files midway through the installation process yourself (unless you are asked/prompted to)." 34 75 3>&1 1>&2 2>&3) || exit 1
            if [[ -n "$GEEX_DEBUG" ]] || [[ -n "$GEEX_DEBUG_MODE" ]]; then
                debugNotice=$(dialog --backtitle "Geex Installer" --title "Debug Notice" --msgbox "The Installer has detected that you are running inside Debug Mode, thus it will pretend to continue with a Mock Installation Process, and ignore the present Error.\n\nHowever, the occurrence of this Error is not intended, and this is not supposed to happen at all.\n\n(!) Important (!)\nPLEASE verify the integrity of the Geex Installer's Code, as well as your System, your Internet Connection, and other possible factors of failure." 34 75 3>&1 1>&2 2>&3) || exit 1
                export ignoredHomeErrorDueToDebug=1
                export systemFinished=2
            else
                dialog --clear
                clear
                echo "[ Status ]: Aborting..."
                exit 1
            fi
        fi
    fi
}
timezoneHook() {
    dialog --backtitle "Geex Installer" --title "Loading..." --infobox "Loading Timezone Information, please wait..." 10 50
    export ZONEINFO_DIR="@@GEEX_ZONEINFO@@"
    if [ "$zoneinfoError" == 1 ]; then
        if [ -f "/tmp/geex.timezone.notice.dd" ]; then
            rm /tmp/geex.timezone.notice.dd
        fi
        echo -e "Since the installer has encountered a timezone/zoneinfo error:\n\nGuix is not present, Nix is not present, and the directories '/usr/share/zoneinfo' as well as '/etc/zoneinfo' do not exist.\n\nThe installer will now set your timezone to a fallback timezone automatically set up in case of zoneinfo errors.\nThe default fallback timezone is:\n\nEurope/Berlin\n\nThe installer will now continue with the setup, configuraiton, and installation process as if it had not encountered an error." >> /tmp/geex.timezone.notice.dd
        noticePopup=$(dialog --backtitle "Geex Installer" --title "Notice" --textbox "/tmp/geex.timezone.notice.dd" 22 75 3>&1 1>&2 2>&3)
        export TIMEZONE="Europe/Berlin"
    else
        REGION=$(find "$ZONEINFO_DIR" -maxdepth 1 -type d -printf "%f\n" | \
                     grep -E 'Africa|America|Antarctica|Arctic|Asia|Atlantic|Australia|Europe|Indian|Pacific' | \
                     sort | xargs -I {} echo {} {} | \
                     xargs dialog --menu "Select Region" 15 50 10 3>&1 1>&2 2>&3 >/dev/tty) || exit 1
        export REGION=$REGION
        if [[ -z "$REGION" ]]; then
            export REGION="Europe"
            export fallbackRegion=1
        else
            export fallbackRegion=0
        fi
        ZONE=$(find "$ZONEINFO_DIR/$REGION" -type f -printf "%P\n" | \
                   sort | xargs -I {} echo {} {} | \
                   xargs dialog --menu "Select Timezone in $REGION" 15 50 10 3>&1 1>&2 2>&3 >/dev/tty) || exit 1
        export ZONE=$ZONE
        if [[ -z "$ZONE" ]]; then
            export ZONE="Berlin"
            export fallbackZone=1
        else
            export fallbackZone=0
        fi
        export TIMEZONE="$REGION/$ZONE"
        if [ "$fallbackRegion" == 1 ]; then
            if [ "$fallbackZone" == 1 ]; then
                export fellBackOnTimezone="Region and Zone"
                export fellBackNum=1
            else
                export fellBackOnTimezone="Region"
                export fellbackNum=1
            fi
        else
            export fellBackNum=0
        fi
        if [[ -z "$REGION" ]] || [[ -z "$ZONE" ]] || [[ -z "$TIMEZONE" ]] || [[ "$TIMEZONE" == "" ]] || [[ "$fellBackNum" == 1 ]]; then
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "You have not selected a valid Timezone, the installer will use the default fallback $fellBackOnTimezone, and export this as your chosen timezone.\n\nContinue?" 32 50 10 \
                                  continue "Continue" \
                                  abort "Abort" \
                                  3>&1 1>&2 2>&3) || exit 1
            if [ "$errorMessage" == "abort" ]; then
                dialog --clear
                clear
                echo "[ Status ]: Aborting..."
                exit 1
            fi
            if [[ -z "$REGION" ]]; then
                export REGION="Europe"
            fi
            if [[ -z "$ZONE" ]]; then
                export ZONE="Berlin"
            fi
            export TIMEZONE="$REGION/$ZONE"
        fi
    fi
    export TIMEZONE=$TIMEZONE
    sed -i "s|GEEX_TIMEZONE|$TIMEZONE|g" /tmp/geex.config.${stager}.dd
    if [ -n "$TIMEZONE" ]; then
        if [ "$fellBackNum" == 0 ]; then
            export wroteTimezoneBlock="Yes"
        else
            export wroteTimezoneBlock="Yes (Fallback)"
        fi
    else
        export wroteTimezoneBlock="No"
    fi
    if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
        export verboseTimezoneBlockText=$(cat /tmp/geex.config.${stager}.dd | grep "$TIMEZONE" | sed "s/^[ \t]*//g")
    fi
}
keyboardSelectVariantHook() {
    while true; do
        SELECTED_KEYBOARD_VARIANT=$(xargs -a "$TMP_VARIANTS" dialog --menu "Select Variant for $selectedLayout" 20 70 12 \
                                          3>&1 1>&2 2>&3) || exit 1

        if [ "$SELECTED_KEYBOARD_VARIANT" == "default" ]; then
            export keyboard="\"$selectedLayout\""
            export keyboardInfo="$selectedLayout"
            return 1
        else
            export selectedVariant=$SELECTED_KEYBOARD_VARIANT
            export keyboard="\"$selectedLayout\" \"$selectedVariant\""
            export keyboardInfo="$selectedLayout ($selectedVariant)"
            return 1
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "Please select a valid (or any) keyboard layout variant for your selected layout '$SELECTED_KEYBOARD_LAYOUT', or choose the default variant option (first option in the list) to not choose any special/specific layout." 32 50
    done
}
keyboardSelectLayoutHook() {
    while true; do
        SELECTED_KEYBOARD_LAYOUT=$(dialog --menu "Select Layout" 20 60 12 \
                                          $LAYOUT_LIST \
                     3>&1 1>&2 2>&3) || exit 1
        if [[ -n "$SELECTED_KEYBOARD_LAYOUT" ]]; then
            export selectedLayout=$SELECTED_KEYBOARD_LAYOUT
            export TMP_VARIANTS="/tmp/geex.keyboard.variants.dd"
            if [ -f "/tmp/geex.keyboard.variants.dd" ]; then
                rm /tmp/geex.keyboard.variants.dd
            fi
            echo "default \"Standard layout ($SELECTED_KEYBOARD_LAYOUT)\"" > "$TMP_VARIANTS"
            awk -v lay="$SELECTED_KEYBOARD_LAYOUT" '
                /! variant/ {flag=1; next}
                /^!/ {flag=0}
                flag && $2 ~ lay":" {
                    tag = $1;
                    # Find the first colon and take everything after it as description
                    desc = substr($0, index($0, ":") + 1);
                    # Remove leading spaces
                    gsub(/^[ \t]+/, "", desc);
                    # Print tag then description in quotes
                    printf "%s \"%s\"\n", tag, desc;
                }' "$LST_FILE" >> "$TMP_VARIANTS"
            return 0
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "Please select a valid (or any) keyboard layout from the provided list/installer selection window." 32 50
    done
}
keyboardHook() {
    dialog --backtitle "Geex Installer" --title "Loading..." --infobox "Loading Keyboard Layout and Variant List, please wait..." 10 50
    export XKB_BASE="@@GEEX_XKB@@"
    export LST_FILE="$XKB_BASE/rules/base.lst"
    export LAYOUT_LIST=$(awk '/! layout/ {flag=1; next} /^!/ {flag=0} flag {print $1, $2}' "$LST_FILE")
    export TMP_VARIANTS="/tmp/geex.keyboard.variants.dd"

    if [ "$keyboardLayoutError" == 1 ]; then
        export keyboard="$(echo -e "\"us\"")"
    else
        keyboardSelectLayoutHook
        keyboardSelectVariantHook
    fi

    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        sed -i "s|GEEX_KEYBOARD_LAYOUT|$keyboard|g" /tmp/geex.config.${stager}.dd
        export wroteKeyboardBlock="Yes"
        if grep "(keyboard-layout $keyboard)" /tmp/geex.config.${stager}.dd &>/dev/null; then
            export foundKeyboardBlock="Yes"
        else
            export foundKeyboardBlock="No"
        fi
    else
        echo "No '/tmp/geex.config.${stager}.dd' found..."
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer encountered an error when trying to set your keyboard layout and variant. This may have occurred because the '/tmp/geex.config.${stager}.dd' is absent, unwriteable, or someone has tinkered with the code of this installer.\n\nPlease verify the installers integrity. The Installer will now quit unless specifically instructed not to." 32 60 10 \
                              okay "Okay" \
                              continue "Continue despite Errors" \
                              3>&1 1>&2 2>&3) || exit 1
        if [ "$errorMessage" == "okay" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
    fi
}
livePreviewHook() {
    if command -v kitty >/dev/null; then
        kitty --title "geexLive" --app-id=geexLive --os-window-tag=geexLive watch -n 1 "cat /tmp/geex.config.${stager}.dd" &>/dev/null &
        kitty --title "geexLive" --app-id=geexLive --os-window-tag=geexLive watch -n 1 "cat /tmp/geex.config.${stager}.dd | tail -n 50" &>/dev/null &
    elif command -v alacritty >/dev/null; then
        alacritty --title geexLive --class geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd" &>/dev/null &
        alacritty --title geexLive --class geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd | tail -n 50" &>/dev/null &
    elif command -v xterm >/dev/null; then
        xterm -class geexLive -tn geexLive -name geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd" &>/dev/null &
        xterm -class geexLive -tn geexLive -name geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd | tail -n 50" &>/dev/null &
    elif command -v ghostty >/dev/null; then
        ghostty --class=geexLive --x11-instance-name=geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd" &>/dev/null &
        ghostty --class=geexLive --x11-instance-name=geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd | tail -n 50" &>/dev/null &
    elif command -v st >/dev/null; then
        st -c geexLive -T geexLive -t geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd" &>/dev/null &
        st -c geexLive -T geexLive -t geexLive -e watch -n 1 "cat /tmp/geex.config.${stager}.dd | tail -n 50" &>/dev/null &
    elif command -v xfce4-terminal >/dev/null; then
        xfce4-terminal --role=geexLive --class=geexLive --startup-id=geexLive --command='watch -n 1 "cat /tmp/geex.config.${stager}.dd"' &>/dev/null &
        xfce4-terminal --role=geexLive --class=geexLive --startup-id=geexLive --command='watch -n 1 "cat /tmp/geex.config.${stager}.dd | tail -n 50"' &>/dev/null &
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an error. You have enabled the live preview mode, however the installer could not find a supported terminal emulator installed on your machine.\n\nSupported Terminals are:\n - kitty\n - alacritty\n - xterm\n - ghostty\n - st\n - xfce4-terminal\n\nThe Live Setting will be ignored for now." 34 75 3>&1 1>&2 2>&3) || exit 1
    fi
}
liveKillHook() {
    while findInstance=$(ps aux | grep "[g]eexLive" | awk '{print $2}' | head -n 1) && [ -n "$findInstance" ]; do
        kill "$findInstance"
    done
    if pgrep -f "geexLive" >/dev/null; then
        pkill -9 -f "geexLive"
    fi
}
searchForPackageFunction() {
    local pkgName=$(echo "$1" | tr -d '[:space:]')
    [[ -z "$pkgName" ]] && return 1
    if guix search "$pkgName" | grep -qx "name: $pkgName"; then
        echo "$pkgName"
        return 0
    else
        return 1
    fi
}
packageBundlesHook() {
    bundleSelection=$(dialog --backtitle "Geex Installer" --title "Package Bundles" --checklist "Select Package Bundles:" 15 50 5 \
                             office "Office" off \
                             gaming "Gaming" off \
                             wine "Wine" off \
                             media "Media Players" on \
                             plan9 "Plan9 Tools" off \
                             archivers "Archiving Tools" off \
                             rescue "Rescue Tools" off \
                             fonts "Fonts" on \
                             networking "Networking" on \
                             3>&1 1>&2 2>&3) || exit 1
    read -r -a bundleSelectionArray <<< "$bundleSelection"
    bundleSelectionCount="${#bundleSelectionArray[@]}"
    bundleSelectionSummaryText=$(printf '%s\n' "${bundleSelectionArray[@]}")
    if [ -f "/tmp/geex.bundle.wine.dd" ]; then
        rm /tmp/geex.bundle.wine.dd
    fi
    bundleWineBlock="$(echo -e "                             \"wine\"\n                             \"winetricks\"\n                             \"wine64\"\n                             \"wine64-staging\"")"
    if [ -f "/tmp/geex.bundle.gaming.dd" ]; then
        rm /tmp/geex.bundle.gaming.dd
    fi
    if [[ "$userWantsNvidia" == "yes" ]]; then
        bundleGamingBlock="$(echo -e "                             \"steam-nvidia\"\n                             \"heroic-nvidia\"\n                             \"protonup\"")"
    else
        bundleGamingBlock="$(echo -e "                             \"steam\"\n                             \"heroic\"\n                             \"protonup\"")"
    fi
    if [ -f "/tmp/geex.bundle.media.dd" ]; then
        rm /tmp/geex.bundle.media.dd
    fi
    if [[ "$userWantsNvidia" == "yes" ]]; then
        bundleMediaBlock="$(echo -e "                             \"ffmpeg\"\n                             \"mpv-nvidia\"\n                             \"cmus\"")"
    else
        bundleMediaBlock="$(echo -e "                             \"ffmpeg\"\n                             \"mpv\"\n                             \"cmus\"")"
    fi
    if [ -f "/tmp/geex.bundle.office.dd" ]; then
        rm /tmp/geex.bundle.office.dd
    fi
    bundleOfficeBlock="$(echo -e "                             \"libreoffice\"\n                             \"gnucash\"")"
    if [ -f "/tmp/geex.bundle.plan9.dd" ]; then
        rm /tmp/geex.bundle.plan9.dd
    fi
    bundleNetworkingBlock="$(echo -e "                             \"isc-dhcp\"\n                             \"dhcpcd\"\n                             \"wpa-supplicant\"")"
    if [ -f "/tmp/geex.bundle.networking.dd" ]; then
        rm /tmp/geex.bundle.networking.dd
    fi
    bundlePlan9Block="$(echo -e "                             \"plan9port\"\n                             \"plan9-term\"\n                             \"plan9-rc-shell\"\n                             \"plan9-acme\"\n                             \"plan9-rio-session\"")"
    if [ -f "/tmp/geex.bundle.archivers.dd" ]; then
        rm /tmp/geex.bundle.archivers.dd
    fi
    bundleArchiversBlock="$(echo -e "                             \"unrar-free\"\n                             \"7zip\"\n                             \"unzip\"")"
    if [ -f "/tmp/geex.bundle.rescue.dd" ]; then
        rm /tmp/geex.bundle.rescue.dd
    fi
    bundleRescueBlock="$(echo -e "                             \"cryptsetup\"\n                             \"encfs\"\n                             \"testdisk\"\n                             \"ntfs-3g\"")"
    if [ -f "/tmp/geex.bundle.fonts.dd" ]; then
        rm /tmp/geex.bundle.fonts.dd
    fi
    bundleFontsBlock="$(echo -e "                             \"font-jonafonts\"\n                             \"font-dejavu\"\n                             \"font-google-noto-emoji\"")"
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        if [[ "$bundleSelection" == *office* ]]; then
            bundleCombined="$(echo -e "$bundleOfficeBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *wine* ]]; then
            bundleCombined="$(echo -e "$bundleWineBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *gaming* ]]; then
            bundleCombined="$(echo -e "$bundleGamingBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *media* ]]; then
            bundleCombined="$(echo -e "$bundleMediaBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *plan9* ]]; then
            bundleCombined="$(echo -e "$bundlePlan9Block\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *archivers* ]]; then
            bundleCombined="$(echo -e "$bundleArchiversBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *rescue* ]]; then
            bundleCombined="$(echo -e "$bundleRescueBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *fonts* ]]; then
            bundleCombined="$(echo -e "$bundleFontsBlock\n$bundleCombined\n")"
        fi
        if [[ "$bundleSelection" == *networking* ]]; then
            bundleCombined="$(echo -e "$bundleNetworkingBlock\n$bundleCombined\n")"
        fi
        if [ -f "/tmp/geex.bundle.combined.dd" ]; then
            rm /tmp/geex.bundle.combined.dd
        fi
        echo "$bundleCombined" >> /tmp/geex.bundle.combined.dd
        if [[ "$bundleCombined" != "" ]] || [[ -n "$bundleCombined" ]]; then
            sed -i "/GEEX_BUNDLE_OPTIONAL/{
                   r /tmp/geex.bundle.combined.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export wroteBundles="Yes"
        else
            sed -i "/GEEX_BUNDLE_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export wroteBundles="No"
        fi
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered an un-recoverable Error:\n\nIt could not find the '/tmp/geex.config.${stager}.dd' file to write to. This is not supposed to happen. Please verify the installers code was not tinkered with, and that '/tmp' is writeable.\n\nThe Installer will now forcefully quit." 34 75 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Aborting..."
        exit 1
    fi
}
addCustomPackageHook() {
    userPkgList=$(dialog --backtitle "Geex Installer" --title "Extra Packages" --inputbox "Enter Packages Separated by a Comma\n\nExample:\n\`\`\`\npackage-1, package-2, package-3\n\`\`\`\n" 20 64 3>&1 1>&2 2>&3) || exit 1
    waitNotice=$(dialog --backtitle "Geex Installer" --title "Loading..." --infobox "Querying, Filtering, and De-Duplicating Packages, please wait." 18 70 3>&1 1>&2 2>&3)
    confirmedPkgList=""
    duplicationList=""
    IFS=',' read -ra ADDR <<< "$userPkgList"
    for i in "${ADDR[@]}"; do
        verified=$(searchForPackageFunction "$i")
        if [[ $? -eq 0 ]]; then
            if cat /tmp/geex.config.${stager}.dd | grep -i "$verified" >/dev/null 2>&1; then
                if [ "$duplicationList" == "" ] || [ -z "$duplicationList" ]; then
                    duplicationList="$verified"
                else
                    duplicationList="$duplicationList $verified"
                fi
            else
                if [ "$confirmedPkgList" == "" ] || [ -z "$confirmedPkgList" ]; then
                    confirmedPkgList="$verified"
                else
                    confirmedPkgList="$confirmedPkgList $verified"
                fi
            fi
        fi
    done
    echo "[ Status ]: Final Package List: $confirmedPkgList"
    export finalCustomPkgListWithCommas="$confirmedPkgList"
    export finalCustomPkgListWithoutCommas=$(echo -e "$finalCustomPkgListWithCommas" | sed "s/, / /g" | sed "s/,/ /g")
    export finalCustomPkgListCleanup=$(echo "$finalCustomPkgListWithoutCommas" | tr ' ' '\n' | sort -u | xargs)
    export finalCustomPkgList=$(echo ${finalCustomPkgListCleanup//$'\n'/ })
    export finalDuplicationListWithoutCommas=$(echo -e "$duplicationList" | sed "s/, / /g" | sed "s/,/ /g")
    export finalDuplicationListCleanup=$(echo "$finalDuplicationListWithoutCommas" | tr ' ' '\n' | sort -u | xargs)
    export finalDuplicationList=$(echo ${finalDuplicationListCleanup//$'\n'/ })
    pkgListConfirmationText="$(echo -e "Please confirm that the list below contains all of the custom packages you selected (filtered through a 'guix search' to determine whether each package exists or not):\n\n$finalCustomPkgList\n\nThe Installer removed the following provided Packages due to duplication:\n\n$finalDuplicationList")"
    if [[ "$finalCustomPkgList" == "" ]]; then
        sed -i "/GEEX_EXTRA_PACKAGE_LIST_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        export userWantedCustomPackages="No"
        export wroteCustomPackages="No"
        return 0
    fi
    pkgListConfirmation=$(dialog --backtitle "Geex Installer" --title "Confirm Packages" --yesno "$pkgListConfirmationText" 34 75 3>&1 1>&2 2>&3)
    pkgListConfirmation_RESPONSE_CODE=$?
    if [ "$pkgListConfirmation_RESPONSE_CODE" -eq 0 ]; then
        export pkgListWasConfirmed=1
    else
        export pkgListWasConfirmed=0
    fi
    if [ "$pkgListWasConfirmed" == 1 ]; then
        export extraPackageList="$(echo -e "$finalCustomPkgList")"
        echo -e "[ Status ]: List was Confirmed, full List:\n'$extraPackageList'"
        if [[ "$extraPackageList" != "" ]] || [[ -n "$extraPackageList" ]]; then
            export extraPackageListInsertable="$(echo -e "UWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWUUWU\"$extraPackageList\"" | sed "s/ /\"\n                             \"/g" | sed "s/UWU/ /g")"
            if [ -f "/tmp/geex.extra.packages.insertable.dd" ]; then
                rm /tmp/geex.extra.packages.insertable.dd
            fi
            echo -e "$extraPackageListInsertable" >> /tmp/geex.extra.packages.insertable.dd
            sed -i "/GEEX_EXTRA_PACKAGE_LIST_OPTIONAL/{
                   r /tmp/geex.extra.packages.insertable.dd
                   d
                   }" /tmp/geex.config.${stager}.dd
            export userWantedCustomPackages="Yes"
            export wroteCustomPackages="$finalCustomPkgList"
        else
            sed -i "/GEEX_EXTRA_PACKAGE_LIST_OPTIONAL/d" /tmp/geex.config.${stager}.dd
            export userWantedCustomPackages="No"
            export wroteCustomPackages="No"
        fi
    else
        echo -e "[ Error ]: Error with List Confirmation"
        sed -i "/GEEX_EXTRA_PACKAGE_LIST_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        export userWantedCustomPackages="Unknown"
        export wroteCustomPackages="No"
    fi
    if [ "$extraPackageListInsertable" == "" ] || [ -z "$extraPackageListInsertable" ]; then
        sed -i "/GEEX_EXTRA_PACKAGE_LIST_OPTIONAL/d" /tmp/geex.config.${stager}.dd
        export userWantedCustomPackages="No"
        export wroteCustomPackages="No"
    fi
}
swapQuestion() {
    userSwapQuestion=$(dialog --backtitle "Geex Installer" --title "Swap Partition" --menu "Do you want to set up a 4GiB Swap Partition for your System?" 12 50 10 \
                           yes "Yes" \
                           no "No" \
                           3>&1 1>&2 2>&3) || exit 1
    if [ "$userSwapQuestion" == "yes" ] || [[ "$userSwapQuestion" != "no" ]]; then
        export userWantsSwap=1
    else
        export userWantsSwap=0
    fi
}
verifyDirectoryStatusHook() {
    if [[ -d "files" && -f "channels.scm" && -f "config.scm" && -d "systems" && -f "geex.sh" && -d "containers" ]]; then
        export compatibleDirectory=1
        export compatibleDirectoryPath=$(pwd)
    else
        export compatibleDirectory=0
        if [[ -n "$compatibleDirectoryPath" ]] || [[ "$compatibleDirectoryPath" != "" ]]; then
            unset compatibleDirectoryPath
        fi
    fi
}
openConfigHook() {
    openConfigQuestion=$(dialog --backtitle "Geex Installer" --title "Open Config" --menu "Do you want to open the current configuration file inside a text editor to make any last changes before the system initialization and installation?" 12 50 10 \
                                no "No" \
                                yes "Yes" \
                                3>&1 1>&2 2>&3) || exit 1
    if [[ "$openConfigQuestion" == "no" ]] || [[ "$openConfigQuestion" != "yes" ]]; then
        return 0
    fi
    shopt -s expand_aliases
    alias 9 >/dev/null 2>&1 && unalias 9
    if command -v nixmacs >/dev/null; then
        if [[ -n "$EMACSLOADPATH" ]]; then
            unset EMACSLOADPATH
        fi
        if [[ "$XDG_SESSION_TYPE" == "wayland" ]] || [[ -n "$WAYLAND_DISPLAY" ]]; then
            export openEditor="nixmacs-wayland"
        else
            export openEditor="nixmacs"
        fi
    elif command -v emacs >/dev/null; then
        export openEditor="emacs"
    elif command -v vim >/dev/null; then
        export openEditor="vim"
    elif command -v nvim >/dev/null; then
        export openEditor="nvim"
    elif command -v nano >/dev/null; then
        export openEditor="nano"
    elif command -v vi >/dev/null; then
        export openEditor="vi"
    elif command -v 9 >/dev/null; then
        if command -v whereis >/dev/null && command -v ls >/dev/null; then
            local stage1=$(whereis 9 | awk '{print $2}')
            local stage2=$(ls -la $stage1 | awk '{print $9}')
            local stage3=$(echo $stage2 | sed "s|/bin/9|/plan9|g")
            local stage4=$(ls -l -a $stage3 | grep -i acme)
            if $stage4 >/dev/null; then
                export openEditor="9 acme"
            fi
        fi
    elif command -v helix >/dev/null; then
        export openEditor="hx"
    else
        export openEditor="none"
    fi
    if [[ -n "$GEEX_EDITOR" ]] && [[ "$GEEX_EDITOR" != "" ]]; then
        case "$GEEX_EDITOR" in
            "vim"|"nvim"|"vi"|"nano"|"hx")
                export openEditor="$GEEX_EDITOR"
                ;;
            "acme"|"9 acme"|"plan9"|"plan9port"|"9")
                alias 9 >/dev/null 2>&1 && unalias 9
                export openEditor="9 acme"
                ;;
            *)
                export openEditor="$GEEX_EDITOR"
                ;;
        esac
    fi
    dialog --backtitle "Geex Installer" --title "Waiting..." --infobox "\n\nWaiting for Editor ($openEditor) to close..." 7 50
    case "$openEditor" in
        "$GEEX_EDITOR")
            $openEditor /tmp/geex.config.scm
            ;;
        "vim"|"nvim"|"vi"|"nano"|"9 acme"|"hx"|"9 acme")
            $openEditor /tmp/geex.config.scm
            ;;
        "emacs"|"nixmacs-wayland"|"nixmacs")
            $openEditor -nw /tmp/geex.config.scm
            ;;
        *|"none")
            errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer could not find any Text Editor on your Device, and thus was not able to run the Configuration File Editor Hook.\n\nThe Installer will continue as if you did not select to open the configuration File." 34 75 3>&1 1>&2 2>&3)
            ;;
    esac
}
checkInternetHook() {
    if ping -q -c 1 -W 5 8.8.8.8 >/dev/null 2>&1; then
        export GEEX_HAS_INTERNET=1
        return 0
    else
        export GEEX_HAS_INTERNET=0
        return 1
    fi
}
alreadyConnectedHook() {
    dialog --backtitle "Geex Installer" --title "Checking Internet Connection" --infobox "Checking Internet Connection, please wait..." 7 50
    if ping -q -c 1 -W 5 8.8.8.8 >/dev/null 2>&1 && ping -q -c 1 -W 5 google.com >/dev/null 2>&1; then
        activeWifiConnection=$(nmcli connection show --active | grep "wifi" | awk '{print $1 " " $4}')
        activeEthernetConnection=$(nmcli connection show --active | grep "ethernet" | awk '{print $1 " " $4}')
        if [[ "$activeEthernetConnection" == "" ]] || [[ -z "$activeEthernetConnection" ]]; then
            if [[ "$activeWifiConnection" == "" ]] || [[ -z "$activeWifiConnection" ]]; then
                export connectivityStatusCheck="off"
            else
                export connectivityStatusCheck="WiFi"
            fi
        else
            export connectivityStatusCheck="Ethernet"
        fi
        if [[ "$connectivityStatusCheck" != "off" ]]; then
            if [[ "$connectivityStatusCheck" == "Ethernet" ]]; then
                activeNetwork=$(echo $activeEthernetConnection | awk '{print $1}')
                activeDevice=$(echo $activeEthernetConnection | awk '{print $2}')
            else
                activeNetwork=$(echo $activeWifiConnection | awk '{print $1}')
                activeDevice=$(echo $activeWifiConnection | awk '{print $2}')
            fi
            connectivityNotice=$(dialog --backtitle "Geex Installer" --title "Network Connection" --msgbox "Already Connected via '$connectivityStatusCheck' to Network '$activeNetwork' with Device '$activeDevice'." 8 50 3>&1 1>&2 2>&3)
            export GEEX_HAS_INTERNET=1
        fi
    else
        export GEEX_HAS_INTERNET=0
    fi
}
selectInternetDeviceHook() {
    devices=$(ls /sys/class/net | grep -E '^w')
    if [ -z "$devices" ]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer could not find any WiFi Devices attached to your Machine." 8 60 3>&1 1>&2 2>&3)
        if [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
            export GEEX_WIFI_DEVICE="Mock"
            return 1
        else
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        return 1
    fi
    deviceList=""
    for dev in $devices; do
        deviceList="$deviceList $dev 'Wireless Interface'"
    done
    selectDevice=$(eval "dialog --backtitle \"Geex Installer\" --title \"Select WiFi Device\" --menu \"Select the WiFi Device you wish to use:\" 15 60 5 $deviceList" 3>&1 1>&2 2>&3)
    if [ -n "$selectDevice" ]; then
        export GEEX_WIFI_DEVICE=$selectDevice
        echo "[ Status ]: Pinned WiFi Device to '$selectDevice'..."
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "You either did not select a valid WiFi Device, or did not select any WiFi Device at all." 8 60 3>&1 1>&2 2>&3)
        if [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
            export GEEX_WIFI_DEVICE="Mock"
            return 0
        else
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
    fi
}
internetGetNetworks() {
    networks=$(nmcli -t -f "SSID,SECURITY" device wifi list | grep -v '^--' | sort -u | sed "/^:/d" | sed 's/\([^:]\):$/\1:NONE/')
    export networks=$networks
    if [ -z "$networks" ]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "No WiFi Networks found nearby." 8 60 3>&1 1>&2 2>&3)
        if [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
            export GEEX_HAS_INTERNET=2
            return 1
        else
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        return 1
    fi
    menuItems=""
    while IFS=: read -r ssid security; do
        [ -z "$ssid" ] && continue
        menuItems="$menuItems \"$ssid\" \"$security\""
    done <<EOF
$networks
EOF
    export menuItems=$menuItems
}
internetChooseNetwork() {
    while true; do
        chosenSsid=$(eval "dialog --backtitle \"Geex Installer\" --title \"WiFi Selection\" --menu \"Select Network to Connect to:\" 20 60 10 $menuItems" 3>&1 1>&2 2>&3)
        if [ -n "$chosenSsid" ]; then
            export chosenSsid=$chosenSsid
            securityType=$(echo "$networks" | grep "^$chosenSsid:" | cut -d: -f2)
            if [ -z "$securityType" ] || [ "$securityType" == "--" ]; then
                if [[ "$GEEX_HAS_INTERNET" == 1 ]]; then
                    dialog --infobox "Already Connected to the Internet" 3 50
                elif [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
                    dialog --infobox "Skipping WiFi Setup" 3 50
                else
                    dialog --infobox "Connecting to Open Network: $chosenSsid..." 3 50
                    nmcli device wifi connect "$chosenSsid"
                fi
            else
                wifiPass=$(dialog --backtitle "Geex Installer" --title "Network Password" --passwordbox "Enter Password for $chosenSsid:" 10 50 3>&1 1>&2 2>&3)
                if [ -n "$wifiPass" ]; then
                    dialog --infobox "Attempting to Connect to $chosenSsid..." 3 50
                    nmcli device wifi connect "$chosenSsid" password "$wifiPass"
                fi
            fi
            if checkInternetHook; then
                dialog --msgbox "Successfully Connected to '$chosenSsid'." 8 50
                export GEEX_HAS_INTERNET=1
                return 0
            else
                dialog --msgbox "Failed to Connect to '$chosenSsid', please verify Credentials or choose another Network." 8 60
            fi
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "Please choose a Valid (or any) Network and try again." 32 50
    done
}
manualInternetPasswordHook() {
    while true; do
        manualNetworkPassword=$(dialog --backtitle "Geex Installer" --title "Network Password" --inputbox "Enter Network Password:" 12 44 3>&1 1>&2 2>&3) || exit 1
        passwordNoSpaces=$(echo "$manualNetworkPassword" | sed "s|[[:space:]]||g")
        if [[ -n "$passwordIsEmpty" ]]; then
            unset passwordIsEmpty
        fi
        if [[ "$passwordNoSpaces" == "" ]] || [[ -z "$passwordNoSpaces" ]]; then
            export passwordIsEmpty=1
        elif [[ "$manualNetworkPassword" == "" ]] || [[ -z "$manualNetworkPassword" ]]; then
            export passwordIsEmpty=1
        else
            export passwordIsEmpty=0
        fi
        if [[ "$passwordIsEmpty" != 1 ]]; then
            export passwordIsEmpty=$passwordIsEmpty
            return 0
        fi
        dialog --backtitle "Geex Installer" --title "Error" --msgbox "Please enter a valid (or any) Password for your Network." 8 50
    done
}
manualInternetConfigCreationHook() {
    if [[ "$GEEX_RUNNING_IN" == "guix" ]]; then
        export dhRunner="dhclient -v"
    elif [[ "$GEEX_RUNNING_IN" == "nix" ]]; then
        export dhRunner="dhcpcd"
    elif command -v dhclient >/dev/null 2>&1; then
        export dhRunner="dhclient -v"
    elif command -v dhcpcd >/dev/null 2>&1; then
        export dhRunner="dhcpcd"
    else
        export dhRunner="none"
    fi
    while true; do
        manualNetworkName=$(dialog --backtitle "Geex Installer" --title "Network Name" --inputbox "Enter Network Name:" 12 44 3>&1 1>&2 2>&3) || exit 1
        nameNoSpaces=$(echo "$manualNetworkName" | sed "s|[[:space:]]||g")
        if [[ -n "$nameIsEmpty" ]]; then
            unset nameIsEmpty
        fi
        if [[ "$nameNoSpaces" == "" ]] || [[ -z "$nameNoSpaces" ]]; then
            export nameIsEmpty=1
        elif [[ "$manualNetworkName" == "" ]] || [[ -z "$manualNetworkName" ]]; then
            export nameIsEmpty=1
        else
            export nameIsEmpty=0
        fi
        if [[ "$nameIsEmpty" != 1 ]]; then
            export manualNetworkName=$manualNetworkName
            manualNetworkSecurity=$(dialog --backtitle "Geex Installer" --title "Network Security" --menu "Is Network '$manualNetworkName' Secured with a Password?" 12 50 10 \
                                           yes "Yes" \
                                           no "No" \
                                           3>&1 1>&2 2>&3) || exit 1
            if [[ "$manualNetworkSecurity" == "no" ]] || [[ "$manualNetworkSecurity" != "yes" ]]; then
                if [[ -n "$passwordIsEmpty" ]]; then
                    unset passwordIsEmpty
                fi
                export passwordIsEmpty=0
            fi
            if [[ "$manualNetworkSecurity" == "yes" ]]; then
                manualInternetPasswordHook
                if [[ "$passwordIsEmpty" != 1 ]]; then
                    if [ -f "/tmp/geex.manual.network.config.conf" ]; then
                        rm /tmp/geex.manual.network.config.conf
                    fi
                    echo -e "network={\n  ssid=\"$manualNetworkName\"\n  key_mgmt=WPA-PSK\n  psk=\"$manualNetworkPassword\"\n  priority=1\n}" >> /tmp/geex.manual.network.config.conf
                    if [[ -f "/tmp/geex.manual.network.config.conf" ]]; then
                        wifiConfigFile=$(cat /tmp/geex.manual.network.config.conf)
                    fi
                    manualNetworkStart=$(dialog --cr-wrap --backtitle "Geex Installer" --title "Network Connection" --yesno "Wrote WiFi Configuration:\n\`\`\`\n${wifiConfigFile}\n\`\`\`\n\nConnect to Network now?" 18 75 3>&1 1>&2 2>&3)
                    manualNetworkStart_RESPONSE_CODE=$?
                    if [[ ! "$manualNetworkStart_RESPONSE_CODE" -eq 0 ]]; then
                        noticePopup=$(dialog --backtitle "Geex Installer" --title "Network Setup" --msgbox "If you want to manually connect to the '$manualNetworkName' Network, run the following commands from a separate Terminal, before continuing the installation process.\n\n$escalationUtil rfkill unblock all\n$escalationUtil wpa_supplicant -B -i $GEEX_WIFI_DEVICE -c /tmp/geex.manual.network.config.conf &\n$escalationUtil $dhRunner $GEEX_WIFI_DEVICE" 18 90 3>&1 1>&2 2>&3)
                        export GEEX_HAS_INTERNET=3
                        if [[ -z "$connectivityStatusCheck" ]] || [[ "$connectivityStatusCheck" == "" ]]; then
                            export connectivityStatusCheck="WiFi (Not Started)"
                        fi
                    fi
                    if [ "$manualNetworkStart_RESPONSE_CODE" -eq 0 ]; then
                        if command -v rfkill >/dev/null; then
                            runWithEscalationUtil rfkill unblock all &
                        fi
                        runWithEscalationUtil wpa_supplicant -B -i $GEEX_WIFI_DEVICE -c /tmp/geex.manual.network.config.conf &
                        if [[ "$dhRunner" != "none" ]]; then
                            if command -v wpa_cli >/dev/null; then
                                timeoutThreshhold=30
                                i=0
                                dialog --backtitle "Geex Installer" --title "'wpa_cli' Confirmation" --infobox "Waiting for 'wpa_cli' Status Response, please wait..." 10 50
                                until wpa_cli -i $GEEX_WIFI_DEVICE status >/dev/null 2>&1; do
                                    sleep 1
                                    ((i++))
                                    if (( i >= timeoutThreshhold )); then
                                        errorMessage=$(dialog --backtitle "Geex Installer" --title "Networking Error" --msgbox "The Installers request for a positive Status Message from 'wpa_cli' has timed out after 30 Seconds. This means that the Installer was not able to establish a working Internet Connection, or 'wpa_cli' is faulty (or not present on your System).\n\nThe Installer will now forceably quit." 34 75 3>&1 1>&2 2>&3) || exit 1
                                        dialog --clear
                                        clear
                                        echo "[ Error ]: No reponse from 'wpa_cli', aborting..."
                                        exit 1
                                    fi
                                done
                                if wpa_cli -i $GEEX_WIFI_DEVICE ping 2>/dev/null | grep -q PONG; then
                                    echo "[ Status ]: Confirmed wpa_cli Pong..."
                                else
                                    echo "[ Error ]: Could not confirm wpa_cli Pong..."
                                    wpaCliNoPong=$(dialog --backtitle "Geex Installer" --title "Networking Error" --yesno "The Installer could not get a valid PONG Signal from 'wpa_cli', it is possible that Network Connection Establishment did not work as expected, please verify whether you have an active and working Internet Connection or not.\n\nContinue anyways?" 34 75 3>&1 1>&2 2>&3)
                                    wpaCliNoPong_RESPONSE_CODE=$?
                                    if [ "$wpaCliNoPong_RESPONSE_CODE" -eq 0 ]; then
                                        echo "[ Status ]: Continuing anyways..."
                                    else
                                        dialog --clear
                                        clear
                                        echo "[ Error ]: Could not get valid PONG Signal from 'wpa_cli', aborting..."
                                        exit 1
                                    fi
                                fi
                            fi
                            runWithEscalationUtil $dhRunner $GEEX_WIFI_DEVICE &
                        fi
                    fi
                fi
            else
                if [ -f "/tmp/geex.manual.network.config.conf" ]; then
                    rm /tmp/geex.manual.network.config.conf
                fi
                echo -e "network={\n  ssid=\"$manualNetworkName\"\n  key_mgmt=NONE\n  priority=1\n}" >> /tmp/geex.manual.network.config.conf
                if [[ -f "/tmp/geex.manual.network.config.conf" ]]; then
                    wifiConfigFile=$(cat /tmp/geex.manual.network.config.conf)
                fi
                manualNetworkStart=$(dialog --cr-wrap --backtitle "Geex Installer" --title "Network Connection" --yesno "Wrote WiFi Configuration:\n\`\`\`\n${wifiConfigFile}\n\`\`\`\n\nConnect to Network now?" 18 75 3>&1 1>&2 2>&3)
                manualNetworkStart_RESPONSE_CODE=$?
                if [[ ! "$manualNetworkStart_RESPONSE_CODE" -eq 0 ]]; then
                    noticePopup=$(dialog --backtitle "Geex Installer" --title "Network Setup" --msgbox "If you want to manually connect to the '$manualNetworkName' Network, run the following commands from a separate Terminal, before continuing the installation process.\n\n$escalationUtil rfkill unblock all\n$escalationUtil wpa_supplicant -B -i $GEEX_WIFI_DEVICE -c /tmp/geex.manual.network.config.conf &\n$escalationUtil $dhRunner $GEEX_WIFI_DEVICE" 18 90 3>&1 1>&2 2>&3)
                    export GEEX_HAS_INTERNET=3
                    if [[ -z "$connectivityStatusCheck" ]] || [[ "$connectivityStatusCheck" == "" ]]; then
                        export connectivityStatusCheck="WiFi (Not Started)"
                    fi
                fi
                if [ "$manualNetworkStart_RESPONSE_CODE" -eq 0 ]; then
                    if command -v rfkill >/dev/null; then
                        runWithEscalationUtil rfkill unblock all &
                    fi
                    runWithEscalationUtil wpa_supplicant -B -i $GEEX_WIFI_DEVICE -c /tmp/geex.manual.network.config.conf &
                    if [[ "$dhRunner" != "none" ]]; then
                        if command -v wpa_cli >/dev/null; then
                            timeoutThreshhold=30
                            i=0
                            dialog --backtitle "Geex Installer" --title "'wpa_cli' Confirmation" --infobox "Waiting for 'wpa_cli' Status Response, please wait..." 10 50
                            until wpa_cli -i $GEEX_WIFI_DEVICE status >/dev/null 2>&1; do
                                sleep 1
                                ((i++))
                                if (( i >= timeoutThreshhold )); then
                                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Networking Error" --msgbox "The Installers request for a positive Status Message from 'wpa_cli' has timed out after 30 Seconds. This means that the Installer was not able to establish a working Internet Connection, or 'wpa_cli' is faulty (or not present on your System).\n\nThe Installer will now forceably quit." 34 75 3>&1 1>&2 2>&3) || exit 1
                                    dialog --clear
                                    clear
                                    echo "[ Error ]: No reponse from 'wpa_cli', aborting..."
                                    exit 1
                                fi
                            done
                            if wpa_cli -i $GEEX_WIFI_DEVICE ping 2>/dev/null | grep -q PONG; then
                                echo "[ Status ]: Confirmed wpa_cli Pong..."
                            else
                                echo "[ Error ]: Could not confirm wpa_cli Pong..."
                                wpaCliNoPong=$(dialog --backtitle "Geex Installer" --title "Networking Error" --yesno "The Installer could not get a valid PONG Signal from 'wpa_cli', it is possible that Network Connection Establishment did not work as expected, please verify whether you have an active and working Internet Connection or not.\n\nContinue anyways?" 34 75 3>&1 1>&2 2>&3)
                                wpaCliNoPong_RESPONSE_CODE=$?
                                if [ "$wpaCliNoPong_RESPONSE_CODE" -eq 0 ]; then
                                    echo "[ Status ]: Continuing anyways..."
                                else
                                    dialog --clear
                                    clear
                                    echo "[ Error ]: Could not get valid PONG Signal from 'wpa_cli', aborting..."
                                    exit 1
                                fi
                            fi
                        fi
                        runWithEscalationUtil $dhRunner $GEEX_WIFI_DEVICE &
                    fi
                fi
            fi
        fi
        if [[ "$nameIsEmpty" != 1 ]] && [[ "$GEEX_HAS_INTERNET" != 3 ]] && [[ "$passwordIsEmpty" != 1 ]] && checkInternetHook; then
            export GEEX_HAS_INTERNET=1
            if [[ -n "$MANUAL_NETWORK_LOOP_BREAKOUT" ]] || [[ "$MANUAL_NETWORK_LOOP_BREAKOUT" == 0 ]]; then
                unset MANUAL_NETWORK_LOOP_BREAKOUT
            fi
            export MANUAL_NETWORK_LOOP_BREAKOUT=1
            export connectivityStatusCheck="WiFi (Manual)"
            return 0
        elif [[ "$nameIsEmpty" != 1 ]] && [[ "$passwordIsEmpty" != 1 ]] && [[ "$GEEX_HAS_INTERNET" == 3 ]]; then
            export GEEX_HAS_INTERNET=3
            export MANUAL_NETWORK_LOOP_BREAKOUT=1
        else
            if [[ "$GEEX_HAS_INTERNET" == 1 ]]; then
                unset GEEX_HAS_INTERNET
            fi
            if [[ -n "$MANUAL_NETWORK_LOOP_BREAKOUT" ]] || [[ "$MANUAL_NETWORK_LOOP_BREAKOUT" == 1 ]]; then
                unset MANUAL_NETWORK_LOOP_BREAKOUT
            fi
            export MANUAL_NETWORK_LOOP_BREAKOUT=0
        fi
        if [[ "$MANUAL_NETWORK_LOOP_BREAKOUT" == 1 ]] || [[ "$GEEX_HAS_INTERNET" == 1 ]]; then
            return 0
        fi
        if [[ "$nameIsEmpty" == 1 ]]; then
            dialog --backtitle "Geex Installer" --title "Error" --msgbox "Please enter a valid (or any) Network Name/SSID." 8 50
        else
            dialog --backtitle "Geex Installer" --title "Error" --msgbox "There was a problem establishing a connection to your selected Network, please check your answers again or choose another Network and try again." 8 50
        fi
    done
}
manualInternetHook() {
    if ! command -v wpa_supplicant >/dev/null 2>&1; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer detected that you are missing both 'nmcli' and 'wpa_supplicant', which means no Internet Connection can be established.\n\nThe Installer will now forceably quit." 34 68 3>&1 1>&2 2>&3) || exit 1
        if [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
            export GEEX_HAS_INTERNET=2
            return 1
        else
            dialog --clear
            clear
            echo "[ Error ]: Not Connection Possible, Aborting..."
            exit 1
        fi
    else
        selectInternetDeviceHook
        manualInternetConfigCreationHook
    fi
}
internetConnectionHook() {
    if [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
        export GEEX_HAS_INTERNET=2
        if [[ "$GEEX_VERBOSE_MODE" == 1 ]]; then
            verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "You have set the variable 'GEEX_SKIP_WIFI' to '1', the Installer will skip the Internet Connectivity Check." 34 68 3>&1 1>&2 2>&3)
        fi
        return 1
    fi
    if [[ "$GEEX_MANUAL_NET_SETUP_MODE" != 1 ]] || [[ -z "$GEEX_MANUAL_NET_SETUP_MODE" ]]; then
        alreadyConnectedHook
    fi
    if [[ "$GEEX_HAS_INTERNET" == 1 ]] && [[ "$GEEX_MANUAL_NET_SETUP_MODE" != 1 ]]; then
        return 1
    fi
    if ! command -v nmcli >/dev/null 2>&1 || [[ "$GEEX_MANUAL_NET_SETUP_MODE" == 1 ]]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer detected that you are missing 'nmcli', a required dependency for setting up a WiFi Connection, do you want to continue to manually set up your Networking Connection via 'wpa_supplicant', or quit the installer?" 20 60 10 \
                              manual "Manual" \
                              quit "Quit" \
                              3>&1 1>&2 2>&3) || exit 1
        if [[ "$errorMessage" == "manual" ]]; then
            selectInternetDeviceHook
            manualInternetConfigCreationHook
            return 1
        else
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        if [[ "$GEEX_SKIP_WIFI" == 1 ]]; then
            export GEEX_HAS_INTERNET=2
            return 1
        else
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        return 1
    fi
    internetGetNetworks
    internetChooseNetwork
}

# Bootstrap Hooks
disksHookBootstrapper() {
    PARTS_WITH_LABELS=$(ls -l /dev/disk/by-label/ | awk '{print $11 " " $9"\\n"}' | sed "s|../../|\n/dev/|g")
    if [[ "$PARTS_WITH_LABELS" == "" ]] || [[ -z "$PARTS_WITH_LABELS" ]]; then
        echo "[ Status ]: No partitions with labels found, skipping notice message..."
    else
        partitionsNotice=$(dialog --backtitle "Geex Bootstrapper" --title "Partitions Notice" --msgbox "The Bootstrapper has detected the following Partitions with a Label assigned to them, you may want to watch out and make sure you do not overwrite the Disk they are a Part of, if these Partitions are important to you.\n\n$PARTS_WITH_LABELS" 15 50 3>&1 1>&2 2>&3) || exit 1
    fi
    DISK_LIST=$(lsblk -dno NAME,SIZE | awk '{print "/dev/"$1, "("$2")"}')
    SELECTED_DISK=$(dialog --menu "Select Disk" 15 50 10 $DISK_LIST 3>&1 1>&2 2>&3) || exit 1
    if [[ -z "$SELECTED_DISK" ]]; then
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --menu "You have not selected a valid (or any) disk, the installer cannot continue and will now abort the installation process." 32 50 10 \
                              okay "Okay" \
                              3>&1 1>&2 2>&3) || exit 1
        if [ "$errorMessage" == "okay" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        else
            dialog --clear
            clear
            echo -e "[ Status ]: You have somehow selected a non-existent option in the error message, this is not intended - please verify that the Geex installer's code has not been tampered with.\n[ Status ]: Aborting..."
            exit 1
        fi
    fi
    export disk=$SELECTED_DISK
    if [[ "$disk" == /dev/nvme* ]]; then
        export diskPrefixed="${disk}p"
    else
        export diskPrefixed="$disk"
    fi
}
renderBarHook() {
    local current=$1
    local total=$2
    (( total == 0 )) && return
    percent=$(( current * 100 / total ))
    echo "$percent" | dialog --backtitle "Geex Bootstrapper" --title "Downloading" --gauge "Downloading ${filename}" 7 50
}
imageSelectionHook() {
    selectRelease=$(dialog --backtitle "Geex Bootstrapper" --title "Release Selection" --menu "Select which Release/Version of GNU Guix you want to Bootstrap:" 22 60 10 \
                           official "Official" \
                           nonguix "NonGuix" \
                           3>&1 1>&2 2>&3) || exit 1
    export release=$selectRelease
    case "$release" in
        "official")
            versionList="1.5.0 1.5.0 1.4.0 1.4.0 1.3.0 1.3.0"
            archList="x86_64 x86_64 i686 i686 aarch64 aarch64"
            releaseName="Official"
            downloadName="Guix"
            ;;
        "nonguix")
            releaseName="NonGuix"
            downloadName="NonGuix"
            versionList="1.4.0 1.4.0"
            ;;
    esac
    version=$(echo "$versionList" | xargs dialog --menu "Select Version" 15 50 10 3>&1 1>&2 2>&3 >/dev/tty) || exit 1
    if [[ "$release" == "official" ]]; then
        arch=$(echo "$archList" | xargs dialog --menu "Select Arch" 15 50 10 3>&1 1>&2 2>&3 >/dev/tty) || exit 1
    else
        arch="x86_64"
    fi
    case "$release" in
        "official")
            isoURL="https://ftpmirror.gnu.org/gnu/guix/guix-system-install-${version}.${arch}-linux.iso"
            ;;
        "nonguix")
            isoURL="https://substitutes.nonguix.org/nonguix-system-install-${version}.${arch}-linux.iso"
            ;;
    esac
    summary=$(dialog --backtitle "Geex Bootstrapper" --title "Summary" --yesno "You have selected GNU Guix Release '$releaseName', at Version '$version', with Arch '$arch'.\n\nIs this correct?" 18 75 3>&1 1>&2 2>&3)
    summary_RESPONSE_CODE=$?
    if [ "$summary_RESPONSE_CODE" -eq 0 ]; then
        export filename="${downloadName}-${version}-${arch}.iso"
        targetSize=$(wget --spider --server-response $isoURL 2>&1 | awk '/Content-Length/ {print $2}')
        if [[ -z "$targetSize" ]]; then
            dialog --clear
            clear
            echo "[ Error ]: Could not determine Target Filesize, File possible non-existant, aborting..."
            echo "[ Report ]: isoURL='$isoURL'"
            exit 1
        fi
        outFile="/tmp/geex.bootstrapper.store/${filename}"
        if [[ -f "$outFile" ]]; then
            localSize=$(stat -c %s $outFile)
        fi
        if [[ -f "/tmp/geex.bootstrapper.store/${filename}" ]] && [[ "$targetSize" == "$localSize" ]]; then
            export downloadFinished=1
            export downloadMethod="localmatch"
        fi
        if [[ "$downloadFinished" == 1 ]] && [[ "$downloadMethod" == "localmatch" ]]; then
            success=$(dialog --backtitle "Geex Bootstrapper" --title "Download Finished" --msgbox "The Geex Bootstrapper has successfully downloaded '${filename}', and will now proceed to the USB Setup Hook." 24 40 3>&1 1>&2 2>&3)
            return 0
        fi
        wget $isoURL -O /tmp/geex.bootstrapper.store/${downloadName}-${version}-${arch}.iso &>/dev/null &
        wgetPid=$!
        barWidth=40
        while kill -0 "$wgetPid" 2>/dev/null; do
            if [[ -f "$outFile" ]]; then
                currentSize=$(du -b "$outFile" | awk '{print $1}')
                renderBarHook "$currentSize" "$targetSize"
            else
                echo "0" | dialog --backtitle "Geex Bootstrapper" --title "Downloading" --gauge "Downloading ${filename}" 7 50
            fi
            sleep 3
        done
        wait "$wgetPid"
        status=$?
        currentSize=$(du -b "$outFile" 2>/dev/null | awk '{print $1}')
        renderBarHook "${currentSize:-0}" "$targetSize"
        echo
        if (( status == 0 )); then
            export downloadFinished=1
            echo "[ Status ]: Download Completed."
        else
            export downloadFinished=0
            echo "[ Error ]: Download Failed."
        fi
        if [[ "$downloadFinished" == 1 ]]; then
            success=$(dialog --backtitle "Geex Bootstrapper" --title "Download Finished" --msgbox "The Geex Bootstrapper has successfully downloaded '${filename}', and will now proceed to the USB Setup Hook." 24 40 3>&1 1>&2 2>&3)
        else
            errorMessage=$(dialog --backtitle "Geex Bootstrapper" --title "Download Error" --msgbox "There was an Error trying to Download '${filename}', please select a different Release, Version, or Arch - and try again." 34 68 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Error ]: Download of '${filename}' Failed, Aborting..."
            echo "[ Report ]: Tried URL: '${isoURL}'."
            exit 1
        fi
    fi
}
bootstrapperUnspecifiedExitHook() {
    dialog --clear
    clear
    echo "[ Status ]: Bootstrapper called the Unspecified Exit Hook, Geex will now Quit."
    exit 1
}
diskWriteBootstrapper() {
    confirmation=$(dialog --backtitle "Geex Bootstrapper" --title "Confirmation" --yesno "Do you really want to write '${filename}' to '${disk}'? This is not reversible!" 18 75 3>&1 1>&2 2>&3)
    confirmation_RESPONSE_CODE=$?
    if [ "$confirmation_RESPONSE_CODE" -eq 0 ]; then
        targetFileFullPath="/tmp/geex.bootstrapper.store/${filename}"
        totalSize=$(stat -c %s "${targetFileFullPath}")
        if [[ "$escalationUtil" == "none" ]]; then
            errorMessage=$(dialog --backtitle "Geex Bootstrapper" --title "Error" --msgbox "The Geex Bootstrapper has encountered an Escalation Error: it could not find any Escalation Tool available on your System, and would like to Quit now.\n\nIf you are absolutely sure you want to circumvent this Forceful Exit, then run the Bootstrapper again with the Environment Variable 'BOOTSTRAPPER_FORCE_FORMAT' set to '1'." 34 75 3>&1 1>&2 2>&3) || exit 1
            if [[ "$BOOTSTRAPPER_FORCE_FORMAT" != 1 ]]; then
                dialog --clear
                clear
                echo "[ Error ]: Fatal Escalation Error Encountered, Aborting..."
                exit 1
            fi
        fi
        if [[ "$escalationUtil" == "none" ]] && [[ "$BOOTSTRAPPER_FORCE_FORMAT" == 1 ]]; then
            export BOOTSTRAPPER_DIRECT_INSTRUCTION="ExecuteRegardless"
        else
            export BOOTSTRAPPER_DIRECT_INSTRUCTION="AbortFatally"
        fi
        if [[ "$escalationUtil" != "root" ]] && [[ "$escalationUtil" != "none" ]]; then
            dialog --backtitle "Geex Bootstrapper" --title "Authenticating" --infobox "Please check your Terminal for a Password Prompt." 5 50
            runWithEscalationUtil true > /dev/tty 2>&1 || bootstrapperUnspecifiedExitHook
        fi
        notice=$(dialog --backtitle "Geex Bootstrapper" --title "Escalation Notice" --infobox "The Installer can, for whatever reason, not yet remember Escalation, so please be prepared to enter your Superuser Password one or more times during this process.\n\nPress any Key to Skip this Notice." 18 70 3>&1 1>&2 2>&3)
        read -r skippingNotice
        unset skippingNotice
        if [[ "$escalationUtil" != "root" ]] && [[ "$escalationUtil" != "none" ]]; then
            runWithEscalationUtil "stdbuf -i0 -o0 -e0 dd if='$targetFileFullPath' of='$disk' bs=4M status=progress" 2>&1 | \
                while LC_ALL=C read -d $'\r' -r line; do
                    if [[ "$line" =~ ([0-9]+)\ bytes ]]; then
                        written=${BASH_REMATCH[1]}
                        percent=$(( written * 100 / totalSize ))
                        echo "$percent"
                    fi
                done | dialog \
                           --backtitle "Geex Bootstrapper" \
                           --title "Writing Image" \
                           --gauge "Writing '${filename}' to '${disk}'...\nDo not Remove the USB Device!" \
                           10 60
            export bootstrapperWroteImageToDisk="Yes"
        elif [[ "$escalationUtil" == "root" ]]; then
            dd \
                if="$targetFileFullPath" \
                of="$disk" \
                bs=4M \
                status=progress 2>&1 | \
                while LC_ALL=C read -d $'\r' -r line; do
                    if [[ "$line" =~ ([0-9]+)\ bytes ]]; then
                        written=${BASH_REMATCH[1]}
                        percent=$(( written * 100 / totalSize ))
                        echo "$percent"
                    fi
                done | dialog \
                           --backtitle "Geex Bootstrapper" \
                           --title "Writing Image" \
                           --gauge "Writing '${filename}' to '${disk}'...\nDo not Remove the USB Device!" \
                           10 60
            export bootstrapperWroteImageToDisk="Yes"
        elif [[ "$BOOTSTRAPPER_DIRECT_INSTRUCTION" == "ExecuteRegardless" ]] && [[ "$BOOTSTRAPPER_FORCE_FORMAT" == 1 ]]; then
            dd \
                if="$targetFileFullPath" \
                of="$disk" \
                bs=4M \
                status=progress 2>&1 | \
                while LC_ALL=C read -d $'\r' -r line; do
                    if [[ "$line" =~ ([0-9]+)\ bytes ]]; then
                        written=${BASH_REMATCH[1]}
                        percent=$(( written * 100 / totalSize ))
                        echo "$percent"
                    fi
                done | dialog \
                           --backtitle "Geex Bootstrapper" \
                           --title "Forceably Writing Image" \
                           --gauge "Forceably Writing '${filename}' to '${disk}'...\nDo not Remove the USB Device!" \
                           10 60
            export bootstrapperWroteImageToDisk="Yes (Force)"
        elif [[ "$escalationUtil" != "root" ]] && [[ "$escalationUtil" != "none" ]] && [[ "$escalationUtil" != "su" ]] && [[ "$escalationUtil" != "sudo" ]] && [[ "$escalationUtil" != "doas" ]]; then
            export bootstrapperWroteImageToDisk="No"
            errorMessage=$(dialog --backtitle "Geex Bootstrapper" --title "Fatal Error" --msgbox "The Geex Bootstrapper has Encountered a Fatal Error: no Escalation Utility has been found available on your System, and the Escalation Detection did not report any of its available Options back to the Bootstrapper.\n\nThis means the Escalation Utility Detection has not been called for some Reason. This Error is un-recoverable, and the Bootstrapper will now Quit." 34 75 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Fatal Error ]: Escalation Detection Never Called or Never Responded, Aborting..."
            exit 1
        else
            export bootstrapperWroteImageToDisk="No"
            errorMessage=$(dialog --backtitle "Geex Bootstrapper" --title "Fatal Error" --msgbox "The Geex Bootstrapper has Encountered an Impossible State: no Escalation Utility was found on your System, the Escalation Detection was Never Called or did not Respond, and all previous Error Handling Failed to Catch the User.\n\nThis Error is un-recoverable and the Bootstrapper will now Quit." 34 75 3>&1 1>&2 2>&3) || exit 1
            dialog --clear
            clear
            echo "[ Fatal Error ]: Impossible Case Encountered, please Investigate this! Aborting..."
            exit 1
        fi
    else
        dialog --clear
        clear
        export bootstrapperWroteImageToDisk="No"
    fi
    if [[ "$bootstrapperWroteImageToDisk" == *Yes* ]]; then
        success=$(dialog --backtitle "Geex Bootstrapper" --title "Success" --msgbox "The Geex Bootstrapper Successfully Wrote '${filename}' to Disk '${disk}'." 24 40 3>&1 1>&2 2>&3)
    else
        failure=$(dialog --backtitle "Geex Bootstrapper" --title "Failure" --msgbox "The Geex Bootstrapper Failed to Write '${filename}' to Disk '${disk}', and will Quit now." 24 40 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Unknown Failure Encountered, Aborting..."
        exit 1
    fi
}
diskPretendBootstrapper() {
    targetFileFullPath="/tmp/geex.bootstrapper.store/${filename}"
    totalSize=$(stat -c %s "${targetFileFullPath}")
    if [[ "$escalationUtil" != "none" ]] && [[ "$escalationUtil" != "root" ]]; then
        runWithEscalationUtil true > /dev/tty 2>&1 || bootstrapperUnspecifiedExitHook
    fi
    notice=$(dialog --backtitle "Geex Bootstrapper" --title "Escalation Notice" --infobox "The Installer can, for whatever reason, not yet remember Escalation, so please be prepared to enter your Superuser Password one or more times during this process.\n\nPress any Key to Skip this Notice." 18 70 3>&1 1>&2 2>&3)
    read -r skippingNotice
    unset skippingNotice
    if [[ "$escalationUtil" != "none" ]] && [[ "$escalationUtil" != "root" ]]; then
        count=0
        maxCount=100
        while true; do
            ((count++))
            sleep 0.1
            if (( count == 20 )); then
                runWithEscalationUtil "whoami" >/dev/null 2>&1
            fi
            if (( count >= maxCount )); then
                echo "$count"
                return 0
            fi
            echo "$count"
        done | dialog \
                   --backtitle "Geex Bootstrapper" \
                   --title "Writing Image (Pretend)" \
                   --gauge "Pretending to Write '${filename}' to '${disk}'...\nThe Escalation Tool used is '$escalationUtil'." \
                   10 60
        export bootstrapperWroteImageToDisk="Yes (Pretend)"
    elif [[ "$escalationUtil" == "root" ]]; then
        count=0
        maxCount=100
        while true; do
            ((count++))
            sleep 0.1
            if (( count >= maxCount )); then
                echo "$count"
                return 0
            fi
            echo "$count"
        done | dialog \
                   --backtitle "Geex Bootstrapper" \
                   --title "Writing Image (Pretend)" \
                   --gauge "Pretending to Write '${filename}' to '${disk}'...\nNo Escalation Tool Used, as User is 'root'." \
                   10 60
        export bootstrapperWroteImageToDisk="Yes (Pretend)"
    else
        errorMessage=$(dialog --backtitle "Geex Bootstrapper" --title "Error" --msgbox "The Geex Bootstrapper has Encountered an Escalation Error: it found no Escalation Tool Available on your System, and would usually Quit now - but since this is the Pretend Write Process, it will continue." 34 75 3>&1 1>&2 2>&3) || exit 1
        export bootstrapperWroteImageToDisk="No (Pretend)"
    fi
    if [[ "$bootstrapperWroteImageToDisk" == *Yes* ]]; then
        success=$(dialog --backtitle "Geex Bootstrapper" --title "Success" --msgbox "The Geex Bootstrapper successfully pretended to write '${filename}' to Disk '${disk}'." 24 40 3>&1 1>&2 2>&3)
    else
        failure=$(dialog --backtitle "Geex Bootstrapper" --title "Failure" --msgbox "The Geex Bootstrapper pretended to write '${filename}' to Disk '${disk}' but Failed, and will Quit now." 24 40 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Unknown Failure Encountered, Aborting..."
        exit 1
    fi
}
diskWriteBootstrapperHook() {
    if [[ "$GEEX_BOOTSTRAPPER_WRITE" == 1 ]]; then
        diskWriteBootstrapper
    else
        diskPretendBootstrapper
    fi
}
bootstrapHook() {
    escalationUtilHook
    internetConnectionHook
    welcome=$(dialog --backtitle "Geex Bootstrapper" --title "Welcome" --menu "The Geex Bootstrapper is an Experimental Addition to the Geex Application. It's purpose is to allow you to easily create various kinds of GNU Guix Live USB Devices, which you can then Boot from.\n\nPlease Note that the Bootstrapper is in very Early-Access, and may cause issues. If you find any behaviour to be suspicious, immediately Exit the Geex Bootstrapper." 22 60 10 \
                     understood "Understood" \
                     abort "Abort" \
                     3>&1 1>&2 2>&3) || exit 1
    if [[ "$welcome" != "understood" ]]; then
        dialog --clear
        clear
        echo "[ Status ]: Aborting..."
        exit 1
    fi
    imageSelectionHook
    disksHookBootstrapper
    diskWriteBootstrapperHook
}

# Container Manager Hooks
containerExecuteSystem() {
    warning=$(dialog --backtitle "Geex Container Manager" --title "Warning" --yesno "The Systems Container Execution Hook is incredibly fragile, and incredibly experimental. It currently requires that you have the 'kitty' Terminal Emulator Installed, and operates on multilple windows simultaneously.\n\nAre you sure you want to run the Execution Hook?\n\nSelect 'Yes' to run the experimental Execution Hook, and 'No' to print a Guide on how to manually start your container, then exit." 14 75 3>&1 1>&2 2>&3)
    warning_RESPONSE_CODE=$?
    if [[ ! "$warning_RESPONSE_CODE" -eq 0 ]]; then
        cd "$selectedContainerPath"
        dialog --clear
        clear
        echo "[ Status ]: Building your Container for you..."
        builtContainerPath=$(guix system container -L . "${selectedContainer}" 2>/dev/null | tail -n 1)
        clear
        echo -e "Running your Container:

Run COMMANDS in ORDER of NUMBERS:
  1. 'cd $selectedContainerPath'
  2. 'guix system container -L . ${selectedContainer} 2>/dev/null | tail -n 1'
  2.1. Copy Output of Previous Command ('/gnu/store/<hash>-run-container').
  3. 'sudo /gnu/store/<hash>-run-container'
  3.1. Copy PID of Executed Container (Printed out Near the Top).
  4. 'sudo guix container exec <PID> /run/current-system/profile/bin/bash --login'
  OR
  4. 'sudo guix container exec <PID> /run/current-system/profile/bin/bash --login -c zsh'

Running COMMAND with OUTPUT as CONTAINER:
  guix system container -L . ${selectedContainer} 2>/dev/null | tail -n 1

OUTPUT as CONTAINER:
  $builtContainerPath

"
        exit 1
    fi

    cd "$selectedContainerPath"
    sudo -v || return 1
    buildContainer=$(guix system container -L . "${selectedContainer}" 2>/dev/null | tail -n 1)
    if [[ ! "$buildContainer" =~ ^/gnu/store/ ]]; then
        echo "[ Error ]: Build failed."
        return 1
    fi
    kitty --app-id GuixBoot --class GuixBoot --name GuixBoot --os-window-tag GuixBoot --title "GuixBoot" sh -c "sudo stdbuf -oL -eL $buildContainer" &>/dev/null >/dev/null 2>&1 &
    local kittyPid=$!

    clear
    printf "[ PID ]: Please Enter the Container PID from the newly Opened Terminal Window: "
    read -r containerPid
    clear
    sudo guix container exec $containerPid /run/current-system/profile/bin/bash --login -c zsh
    sudo kill $containerPid
    kill "$kittyPid" 2>/dev/null
    clear
    echo "[ Status ]: Successfully Ended all Container Processes."
    exit 1
}
containerExecuteHome() {
    cd $selectedContainerPath
    trap '' SIGTTOU SIGTTIN
    dialog --backtitle "Geex Container Manager" --title "Loading" --infobox "Loading Container, please be patient..." 10 50
    command guix home container "${selectedContainer}"
    local exit_status=$?
    stty sane
    set -m
    dialog --clear
    clear
    echo "[ Status ]: Container Exited with ${exit_status}."
    echo "[ Status ]: Successfully Executed Container."
    trap - SIGTTOU SIGTTIN
    exit 1
}
checkForContainers() {
    filteredContainerFiles=$(find ${compatibleDirectoryPath}/containers -maxdepth 1 -type f -printf "%f-local %f\n" | grep -i "scm" | sed "s/.scm-/-/g")
    if [[ -d "/tmp/geex.container.store" ]]; then
        containerStoreFiles=$(find /tmp/geex.container.store -maxdepth 1 -type f -printf "%f-store %f\n" | grep -i "scm" | sed "s/.scm-/-/g")
        if [[ "$containerStoreFiles" != "" ]] && [[ -n "$containerStoreFiles" ]]; then
            filteredContainerFiles=$(printf "$filteredContainerFiles $containerStoreFiles")
        fi
    fi
    containerSelection=$(echo $filteredContainerFiles | xargs dialog --backtitle "Container Selection" --title "Geex Container Manager" --menu "Select a Container File:" 24 40 10 3>&1 1>&2 2>&3)
    if [[ "$containerSelection" == *local* ]]; then
        selectedContainerName=$(printf "$containerSelection" | sed "s/-local/.scm/g")
        selectedContainer=$(echo "${compatibleDirectoryPath}/containers/${selectedContainerName}")
        export selectedContainerPath=$(echo "${compatibleDirectoryPath}/containers")
    else
        selectedContainerName=$(printf "$containerSelection" | sed "s/-store/.scm/g")
        selectedContainer=$(echo "/tmp/geex.container.store/${selectedContainerName}")
        export selectedContainerPath=$(echo "/tmp/geex.container.store")
    fi
    summary=$(dialog --backtitle "Geex Container Manager" --title "Summary" --msgbox "You Selected the Container '${selectedContainerName}', located at '${selectedContainer}'." 12 74 3>&1 1>&2 2>&3)
    if cat ${selectedContainer} | grep -i "operating-system"; then
        export containerType="System"
    elif cat ${selectedContainer} | grep -i "home-environment"; then
        export containerType="Home"
    else
        dialog --clear
        clear
        echo "[ Error ]: Unable to Determine Container Type, Aborting..."
        exit 1
    fi
    if [[ "$containerType" == "System" ]]; then
        typeNotice=$(dialog --backtitle "Geex Container Manager" --title "Container Type" --yesno "Container ${selectedContainerName} is of Type 'System'.\n\nAttempt to Run the Container?" 12 75 3>&1 1>&2 2>&3)
        typeNotice_RESPONSE_CODE=$?
        if [[ "$typeNotice_RESPONSE_CODE" -eq 0 ]]; then
            containerExecuteSystem
        else
            dialog --clear
            clear
            echo -e "${selectedContainerName} is a Systems Container:

  SETUP for CONTAINER:
    guix system container -L . ${selectedContainerName}
    doas /gnu/store/<hash>-run-container
    doas guix container exec <PID> /run/current-system/profile/bin/bash --login -c zsh

"
            exit 1
        fi
    else
        typeNotice=$(dialog --backtitle "Geex Container Manager" --title "Container Type" --yesno "Container ${selectedContainerName} is of Type 'Home'.\n\nAttempt to Run the Container?" 18 75 3>&1 1>&2 2>&3)
        typeNotice_RESPONSE_CODE=$?
        if [[ "$typeNotice_RESPONSE_CODE" -eq 0 ]]; then
            containerExecuteHome
        else
            dialog --clear
            clear
            echo -e "${selectedContainerName} is a Home Container:

  SETUP for CONTAINER:
    guix home container ${selectedContainerName}

"
            exit 1
        fi
    fi
}
containerManagerHook() {
    verifyDirectoryStatusHook
    if [[ "$compatibleDirectory" != 1 ]]; then
        dialog --clear
        clear
        echo "[ Error ]: Current Directory is Incompatible, please run the Geex Container Manager from within the Full Cloned Geex Repository, Aborting..."
        exit 1
    fi
    checkForContainers
}
# Extra Hooks (for Future Use)
detectDistro() {
    if [[ -f "/etc/os-release" ]]; then
        optName=$(cat /etc/os-release | grep -i "^NAME=" | sed "s/NAME=//g" | sed "s/\"//g")
        optName=$(echo "${optName,,}")
    elif [[ ! -f "/etc/os-release" ]] && [[ -f "/etc/lsb-release" ]]; then
        optName=$(cat /etc/lsb-release | grep -i "DISTRIB_ID=" | sed "s/DISTRIB_ID=//g" | sed "s/\"//g")
        optName=$(echo "${optName,,}")
    elif [[ ! -f "/etc/os-release" ]] && [[ ! -f "/etc/lsb-release" ]] && command -v lsb_release 2>&1; then
        optName=$(lsb_release -si | sed "s/\"//g")
        optName=$(echo "${optName,,}")
    elif command -v apt-get >/dev/null; then
        optName="debian"
    elif command -v pacman >/dev/null; then
        optName="arch"
    elif command -v emerge >/dev/null; then
        optName="gentoo"
    elif command -v apk >/dev/null; then
        optName="alpine"
    elif command -v dnf >/dev/null; then
        optName="fedora"
    elif [[ -f "/etc/debian_version" ]]; then
        optName="debian"
    elif [[ -f "/etc/redhat-release" ]]; then
        optName="fedora"
    else
        optName="unknown"
    fi
    case "$optName" in
        *fedora*|*rhel*|*redhat*|*red-hat*|*red_hat*)
            optName="fedora"
            ;;
        *alpine*)
            optName="alpine"
            ;;
        *guix*)
            optName="guix"
            ;;
        *nix*)
            optName="nixos"
            ;;
        *ubuntu*)
            optName="ubuntu"
            ;;
        *debian*|*pop*|*mint*|*elementary*)
            optName="debian"
            ;;
        *arch*|*cachy*|*parabola*|*artix*|*arco*|*hyperbola*|*endeavour*|*garuda*|*manjaro*)
            optName="arch"
            ;;
        *suse*)
            optName="opensuse"
            ;;
        *gentoo*|*pentoo*|*calculate*)
            optName="gentoo"
            ;;
        *kiss*)
            optName="kiss"
            ;;
        *deriv*)
            optName="derive"
            ;;
        *bsd*)
            optName="bsd"
            ;;
        *void*)
            optName="void"
            ;;
        *)
            [[ "$optName" == "unknown" ]] && optName="unknown"
            ;;
    esac
    export distro=$optName
    export distribution=$optName
}
agnosticSetup() {
    welcome=$(dialog --backtitle "Geex Installer" --title "Welcome" --msgbox "Welcome to the Geex Installers Distro Agnostic Setup. This Mode attempts to bring the declarative System Configuration of GNU Guix (and by extend NixOS) to any Regular, Supported, Distribution.\n\nThis Mode is very Experimental, Proceed at your own Risk!\n\nThe Automatic Distribution Detection System has pinned your Distribution to be: '$distro', if this is Wrong, please Exit the Installer now!" 22 60 3>&1 1>&2 2>&3) || exit 1
    export GEEX_IN_AGNOSTIC_MODE=1
}
# Update Hooks
updateSelfHook() {
    ask=$(dialog --backtitle "Geex Updater" --title "Really Update" --yesno "Do you really want to Update the Geex Installer? This will replace your current copy with the latest remotely fetched copy." 18 75 3>&1 1>&2 2>&3)
    ask_RESPONSE_CODE=$?
    if [[ "$ask_RESPONSE_CODE" -eq 0 ]]; then
        if [[ -f "/tmp/geex.outdated.scriptfile.dd" ]]; then
            rm /tmp/geex.outdated.scriptfile.dd
        fi
        cp "$scriptFile" /tmp/geex.outdated.scriptfile.dd
        mv /tmp/geex.self.update.checkfile.dd "$scriptFile"
        chmod a+x "$scriptFile"
        dialog --clear
        clear
        echo "[ Status ]: Update Complete, the Installer will now Exit so you can Re-Start it with the Latest Version."
        echo "[ Info ]: Your Outdated Version was Backed Up to '/tmp/geex.outdated.scriptfile.dd'."
        exit 1
    else
        echo "[ Status ]: Aborting Update..."
    fi
}
getRemoteFile() {
    wget -O /tmp/geex.self.update.checkfile.dd "https://raw.githubusercontent.com/librepup/geex/refs/heads/main/geex.sh"
    if [[ -f "/tmp/geex.self.update.checkfile.dd" ]]; then
        export gotFile=1
    else
        export gotFile=0
    fi
}
checkGeexVersion() {
    export scriptDirectory=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
    export scriptName=$(basename -- "${BASH_SOURCE[0]}")
    export scriptFile="$scriptDirectory/$scriptName"
    export scriptVersion=$(cat $scriptFile | head -n 2 | grep -i "Version" | awk '{print $3}')
    if [[ -f "/tmp/geex.self.update.checkfile.dd" ]]; then
        rm /tmp/geex.self.update.checkfile.dd
    fi
    getRemoteFile
    if [[ "$gotFile" == 1 ]]; then
        export remoteVersion=$(cat /tmp/geex.self.update.checkfile.dd | head -n 2 | grep -i "Version" | awk '{print $3}')
    else
        dialog --clear
        clear
        echo "[ Error ]: Could not fetch Remote File."
        exit 1
    fi
    if (($remoteVersion > $scriptVersion)); then
        export outdated=1
    else
        export outdated=0
    fi
    if [[ "$outdated" == 0 ]]; then
        notice=$(dialog --backtitle "Geex Updater" --title "Update" --msgbox "Your Geex Installer is already Updated to the Latest Version ($scriptVersion) (Remote: $remoteVersion)!" 24 40 3>&1 1>&2 2>&3) || exit 1
    else
        updateSelfHook
    fi
}

# Installer Hooks
installerHook() {
    if [[ "$GEEX_UPDATE" == 1 ]]; then
        checkGeexVersion
    fi
    detectDistro
    if [[ ($distro != "unknown" && $distro != "nixos" && $distro != "guix" && $GEEX_MOVER_MODE != 1 && $GEEX_THE_HURD != 1 && $GEEX_THE_HURD_ALLOW != 1 && $GEEX_HURD_RENDER_WARNING != 1) || ($GEEX_FORCE_AGNOSTIC == 1 && $GEEX_THE_HURD != 1 && $GEEX_THE_HURD_ALLOW != 1 && $GEEX_HURD_RENDER_WARNING != 1 && $GEEX_MOVER_MODE != 1) ]]; then
        if [ -n "$(LC_ALL=C type -t agnosticSetup)" ] && [ "$(LC_ALL=C type -t agnosticSetup)" = function ]; then
            agnosticQuestion=$(dialog --backtitle "Geex Installer" --title "Distro Agnosticism" --yesno "The Geex Installer has Detected that you are on an Unsupported, but Detected, Distribution. Do you want to start the Distro Agnostic Installation Hook?" 14 75 3>&1 1>&2 2>&3)
            agnosticQuestion_RESPONSE_CODE=$?
            if [ "$agnosticQuestion_RESPONSE_CODE" -eq 0 ]; then
                agnosticSetup
            else
                export GEEX_IN_AGNOSTIC_MODE=0
                echo "[ Status ]: Skipping Distro Agnostic Setup..."
            fi
        fi
    else
        export GEEX_IN_AGNOSTIC_MODE=0
    fi
    if [[ "$GEEX_HURD_RENDER_WARNING" == 1 ]]; then
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --infobox "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'.\n\n3..." 18 70 3>&1 1>&2 2>&3)
        sleep 1
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --infobox "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'.\n\n3... 2..." 18 70 3>&1 1>&2 2>&3)
        sleep 1
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --infobox "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'.\n\n3... 2... 1..." 18 70 3>&1 1>&2 2>&3)
        sleep 1
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --menu "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'." 18 70 10 \
                            okay "Okay" \
                            okay "Okay" \
                            force "Force" \
                            okay "Okay" \
                            3>&1 1>&2 2>&3) || exit 1
        if [[ "$hurdNotice" != "force" ]] || [[ "$hurdNotice" == "okay" ]]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
    elif [[ "$GEEX_IGNORE_FORCED_DEBUG" == 1 ]] && [[ "$GEEX_HURD_RENDER_WARNING" == 2 ]] && [[ "$GEEX_THE_HURD" == 1 ]] && [[ "$GEEX_THE_HURD_ALLOW" == 1 ]] && [[ "$GEEX_FORCE_THE_HURD" == 1 ]]; then
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --infobox "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'.\n\n3..." 18 70 3>&1 1>&2 2>&3)
        sleep 1
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --infobox "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'.\n\n3... 2..." 18 70 3>&1 1>&2 2>&3)
        sleep 1
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --infobox "You have forceably enabled GNU Hurd Support for the Geex Installer. This is not intended, and that option merely exists for testing purposes. You SHOULD NOT and DO NOT want to use GNU Hurd as your main Systems Kernel.\n\nIf you are absolutely sure you know what you are doing, select 'Force', otherwise, please select 'Okay'.\n\n3... 2... 1..." 18 70 3>&1 1>&2 2>&3)
        sleep 1
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Immense Danger Detected" --msgbox "WARNING: Extreme Danger Detected :WARNING\n\nThe Installer detected that, on top of forcing GNU Hurd support, you also forcefully disable the automatic Debug Mode safety mechanism, we urge you to quit the installer and do not try to install a GNU Guix System with GNU Hurd as your Kernel.\n\nDue to the situation, you will have to write a confirmation text if you REALLY want to go through with this." 18 70 3>&1 1>&2 2>&3)
        hurdNotice=$(dialog --backtitle "Geex Installer" --title "Hurd Notice" --inputbox "If you are absolutely sure you want to go through with this, please type out the following case-and-space-sensitive sentence in the input field below. Any deviation from the presented text will result in the immediate forceful exit of the installer.\n\n'Yes, I really want to install GNU Hurd.'" 18 70 3>&1 1>&2 2>&3) || exit 1
        if [[ "$hurdNotice" != "Yes, I really want to install GNU Hurd." ]]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
    else
        echo "[ Status ]: No forced Hurd Support detected, skipping..."
    fi
    escalationUtilHook
    verifyDirectoryStatusHook
    if [ -n "$GEEX_VERBOSE_MODE" ]; then
        verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has detected that you are running in Verbose Mode!\n\nYou will now see more popups and status messages as is the default, for the sake of debugging." 24 40 3>&1 1>&2 2>&3)
    fi
    checkMountPointHook
    if [ "$GEEX_MOVER_MODE" == 1 ]; then
        echo "[ Mover ]: Running Mover inside Installer Hook"
        moverFunction
        exit 1
    fi
    if [ "$GEEX_LIVE_MODE" == 1 ]; then
        liveMessage="$(echo -e "The Installer has detected that you are running in Live Mode, thus, once you have made a system choice, two live windows will open automatically, one display the entire configuration file while the installer is working on it (in full length), and one displaying the same file, but exclusive to the last 50 lines.\n\nIf you want to disable this, do not use the 'l', 'live', or '--live' flags. If you did not use those flags and Live Mode was still activated, make sure to unset the GEEX_LIVE_MODE variable before running the Geex Installer again.\n\nDo you want to continue using Live Mode, then select the 'Yes' option. If you did not want to or intend to use Live Mode, please select the 'No' option.")"
        livePopup=$(dialog --backtitle "Geex Installer" --title "Live Notice" --yesno "$liveMessage" 24 60 \
                           3>&1 1>&2 2>&3)
        LIVE_ANSWER_RESPONSE_CODE=$?
        if [ $LIVE_ANSWER_RESPONSE_CODE -eq 0 ]; then
            export liveAnswer="yes"
        else
            export liveAnswer="no"
        fi
        if [ "$liveAnswer" == "no" ]; then
            unset GEEX_LIVE_MODE
            export GEEX_LIVE_OVERRIDE=1
        else
            if [ -z "$GEEX_LIVE_MODE" ]; then
                export GEEX_LIVE_MODE=1
            fi
        fi
    fi
    if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
        if [ -n "$GEEX_FORCED_DEBUG" ] || [ "$GEEX_FORCED_DEBUG" == 1 ]; then
            debugNotice=$(dialog --backtitle "Geex Installer" --title "Debug Notice" --msgbox "The Installer has detected that you are running in Debug Mode!\n\nThis Debug Mode has been FORCEFULLY activated since 'herd' was not detected on your device, thus assuming you are not running on a system that intends to actually install GNU Guix.\n\nCommands will now not actually install anything, copy anything, make changes to your disks, or initialize the GNU Guix System.\n\nIf you don't want this to happen, run the Geex Installer with the 'GEEX_IGNORE_FORCED_DEBUG' environment variable set to '1'." 24 40 3>&1 1>&2 2>&3)
        else
            debugNotice=$(dialog --backtitle "Geex Installer" --title "Debug Notice" --msgbox "The Installer has detected that you are running in Debug Mode!\n\nCommands will now not actually install anything, copy anything, make changes to your disks, or initialize the GNU Guix System." 24 40 3>&1 1>&2 2>&3)
        fi
    fi
    if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
        if [ -f "/tmp/geex.channels.dd" ]; then
            export verboseWroteChannelsFile="Yes"
        else
            export verboseWroteChannelsFile="No"
        fi
        if [ -f "/tmp/geex.config.custom.template.dd" ]; then
            export verboseWroteCustomTemplateFile="Yes"
        else
            export verboseWroteCustomTemplateFile="No"
        fi
        if [[ "$escalationUtil" == "none" ]] && [[ "$USER" == "root" ]]; then
            export verboseEscalationUtil="None (Root User Detected)"
        elif [[ "$escalationUtil" == "none" ]] && [[ "$USER" != "root" ]]; then
             export verboseEscalationUtil="None (Error)"
        else
            export verboseEscalationUtil="$escalationUtil"
        fi
        if [[ "$compatibleDirectory" == 1 ]]; then
            export verboseCompatibleDirectory="Yes (Path: ${compatibleDirectoryPath})"
        else
            export verboseCompatibleDirectory="No"
        fi
        verboseDefaultFileWriteStatusText="$(echo -e "Verbose Mode Detected, informing you of all default systems template files that were created, as well as checking for any leftover files from possible previous installation procedures.\n\n(1) General\nEscalation Util?: $verboseEscalationUtil\nCompatible Directory?: $verboseCompatibleDirectory\nFound Leftovers?: $verboseFoundLeftoverFiles\n\n(2) Installer Wrote Files\nChannels?: $verboseWroteChannelsFile\nCustom Template?: $verboseWroteCustomTemplateFile")"
        verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "$verboseDefaultFileWriteStatusText" 34 68 3>&1 1>&2 2>&3)
    fi
    welcome=$(dialog --backtitle "Geex Installer" --title "Welcome" --menu "Welcome to the (still experimental) Geex Installer, this Installer will help you to install the Geex Configuration Files onto real Hardware, or install a custom version of Guix, with your very own Configuration Files, to a system of your choice.\n\nThis Installer is pre-alpha code, so please follow instructions carefully when given, and verify everything worked after the installation finishes.\n\nTo begin, click 'I agree'." 22 60 10 \
                                agree "I agree" \
                                abort "Abort" \
                                3>&1 1>&2 2>&3) || exit 1
    if [[ "$welcome" != "agree" ]]; then
        dialog --clear
        clear
        echo "[ Status ]: Aborting..."
        exit 1
    fi
    if [ -f "/tmp/geex.keyboardstatus.dd" ]; then
        rm /tmp/geex.keyboardstatus.dd
    fi
    if [ -f "/tmp/geex.summary.dd" ]; then
        rm /tmp/geex.summary.dd
    fi
    if [ -f "/tmp/geex.detecteddisks.dd" ]; then
        rm /tmp/geex.detecteddisks.dd
    fi
    if [ -f "/tmp/geex.detectedbios.dd" ]; then
        rm /tmp/geex.detectedbios.dd
    fi
    export systemchoice="custom"
    export stager="custom"
    if [ -f "/tmp/geex.config.${stager}.dd" ]; then
        rm /tmp/geex.config.${stager}.dd
    fi
    if [ -f "/tmp/geex.config.${stager}.template.dd" ]; then
        cp /tmp/geex.config.${stager}.template.dd /tmp/geex.config.${stager}.dd
    else
        errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered a fatal Error:\n\nThe '/tmp/geex.config.${stager}.template.dd' file was not found. This means the installer failed to generate/write the file at initialization, which is only possible if the '/tmp' filesystem is read-only or otherwise inaccessible. Please fix this before trying again!" 34 75 3>&1 1>&2 2>&3) || exit 1
        dialog --clear
        clear
        echo "[ Error ]: Aborting..."
        exit 1
    fi
    if [ -n "$GEEX_THE_HURD_ALLOW" ] || [ "$GEEX_THE_HURD_ALLOW" == 1 ]; then
        hurdNoticeImportant=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "THIS IS YOUR LAST WARNING - TURN BACK NOW!\n\nYou have enable the environment variable 'GEEX_THE_HURD_ALLOW', this option ONLY exists, because I, the creator of this installer, want to give users FULL CONTROL, even if it is AGAINST THEIR OWN BEST INTEREST!\n\nDO NOT INSTALL GNU HURD AS YOUR MAIN SYSTEMS KERNEL!" 32 50 3>&1 1>&2 2>&3)
    fi
    if [ "$GEEX_LIVE_MODE" == 1 ] && [ -z "$GEEX_LIVE_OVERRIDE" ]; then
        livePreviewHook
    fi
    internetConnectionHook
    usernameFunction
    hostnameFunction
    if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
        verboseUsernameBlock=$(cat /tmp/geex.config.${stager}.dd | grep "name" | grep "$username" | sed "s/^[ \t]*//g")
        verboseHostnameBlock=$(cat /tmp/geex.config.${stager}.dd | grep "$hostname" | sed "s/^[ \t]*//g")
        verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has successfully set and wrote the following variables based on your input/selection:\n\n - Username: $username\n - Hostname: $hostname\n\nUsername Block:\n\`\`\`\n$verboseUsernameBlock\n\`\`\`\n\nHostname Block:\n\`\`\`\n$verboseHostnameBlock\n\`\`\`" 34 68 3>&1 1>&2 2>&3)
    fi
    timezoneHook
    keyboardHook
    if [ -n "$GEEX_VERBOSE_MODE" ] || [ "$GEEX_VERBOSE_MODE" == 1 ]; then
        verboseKeyboardBlockText=$(cat /tmp/geex.config.${stager}.dd | grep "$keyboard" | sed "s/^[ \t]*//g")
        verboseNotice=$(dialog --backtitle "Geex Installer" --title "Verbose Notice" --msgbox "The Installer has written and set up the following options and code-blocks for your Region, Timezone, Keyboard Layout and Layout Variant:\n\nSettings:\n - Keyboard: $keyboardInfo\n - Region/Timezone: $TIMEZONE\n - Wrote Keyboard Block?: $wroteKeyboardBlock\n - Wrote Timezone Block?: $wroteTimezoneBlock\n\nTimezone Block:\n\`\`\`\n$verboseTimezoneBlockText\n\`\`\`\n\nKeyboard Block:\n\`\`\`\n$verboseKeyboardBlockText\n\`\`\`" 34 68 3>&1 1>&2 2>&3)
    fi
    if [ -f "/tmp/geex.timezone.success.dd" ]; then
        rm /tmp/geex.timezone.success.dd
    fi
    if [[ -z "$TIMEZONE" ]]; then
        noticePopup=$(dialog --backtitle "Geex Installer" --title "Timezone Notice" --menu "There was an unrecoverable error setting your timezone. This means that either the timezoneHook function was never called, or a different error prohibited the installer from interacting with your system, its own temporary files, and your general environment.\n\nIf you have made any manual code changes to this installer, that could be the cause of an error like this, please verify the installers code integrity and make sure you have NOT changed anything about the timezoneHook or installerHook.\n\nStill continue? (This means the timezone variable in your Guix Configuration will remain as the 'GEEX_TIMEZONE' placeholder - change this manually if you still want to continue!)" 12 40 5 \
                             continue "Continue" \
                             abort "Abort" \
                             3>&1 1>&2 2>&3) || exit 1
        if [ "$noticePopup" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
    fi
    kernelHook
    xorgHook
    transformationsHook
    if [[ "$GEEX_THE_HURD" == 1 ]] && [[ -n "$GEEX_THE_HURD" ]]; then
        export userWantsSwap=0
    else
        swapQuestion
    fi
    disksHook
    biosHook
    if [ "$bios" == "legacy" ]; then
        biosLegacyEditHook
    else
        biosUefiEditHook
    fi
    disksSetup
    filesystemHook
    serviceSetupHook
    desktopEnvironmentsHook
    passwordHook
    if [ "$bios" == "legacy" ]; then
        if [[ "$userWantsSwap" == 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
            export diskPrefixedPartNameTextblock="${diskPrefixed}2 (guix-root)"
        elif [[ "$userWantsSwap" != 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
            export diskPrefixedPartNameTextblock="${diskPrefixed}1 (guix-root)"
        else
            export diskPrefixedPartNameTextblock="${diskPrefixed}1 (guix-root) (hurd)"
        fi
    else
        if [[ "$userWantsSwap" == 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
            export diskPrefixedPartNameTextblock="${diskPrefixed}2 (guix-efi), ${diskPrefixed}3 (guix-root)"
        elif [[ "$userWantsSwap" != 1 ]] && [[ "$GEEX_THE_HURD" != 1 ]]; then
            export diskPrefixedPartNameTextblock="${diskPrefixed}1 (guix-efi), ${diskPrefixed}2 (guix-root)"
        else
            export diskPrefixedPartNameTextblock="${diskPrefixed}1 (guix-efi), ${diskPrefixed}2 (guix-root) (hurd)"
        fi
    fi
    packageBundlesHook
    addCustomPackageHook
    if [[ "$GEEX_HAS_INTERNET" == 1 ]]; then
        export hasInternetConnection="Yes"
    elif [[ "$GEEX_HAS_INTERNET" == 2 ]]; then
        export hasInternetConnection="Mock"
    elif [[ "$GEEX_HAS_INTERNET" == 3 ]]; then
        export hasInternetConnection="Configured"
    else
        export hasInternetConnection="No"
    fi
    if [[ "$connectivityStatusCheck" == "" ]] || [[ -z "$connectivityStatusCheck" ]]; then
        export connectivityStatusSummary="Unknown"
    elif [[ "$connectivityStatusCheck" == "off" ]]; then
        export connectivityStatusSummary="Disabled"
    else
        export connectivityStatusSummary=$connectivityStatusCheck
    fi
    summaryTextContents="$(echo -e "(1) Information\nUsername: $username\nHostname: $hostname\nTimezone: $TIMEZONE\nPasswords Set?: $areAllPasswordsSet (Re-Used?: $wasPasswordReUsed)\nDisk: $disk (Parts: $diskPrefixedPartNameTextblock)\nSwap: $formattedWithSwap\nBIOS: $bios (Detected: $detectedBios)\nKeyboard: $keyboardInfo\nInternet: $hasInternetConnection (Via: $connectivityStatusSummary)\nExtra Packages: $userWantedCustomPackages\n\n(2) Multiple-Choice\nServices: $serviceSelection\nDesktops: $deSelection\nBundles: $bundleSelection\nExtra Packages: $wroteCustomPackages\n\n(3) The Installer Wrote\nSwap Block?: $wroteSwapBlock\nBIOS Block?: $wroteBiosBlock\nFilesystem Block?: $isFilesystemWritten (Type: $filesystemBlockType)\nServices Block?: $areServicesWritten\nDesktop Block?: $areDesktopsWritten\nKeyboard Block?: $wroteKeyboardBlock\nTimezone Block?: $wroteTimezoneBlock\nBundles Block?: $wroteBundles\nXorg Block?: $wroteXorgBlock\nClosing OS Block?: $wroteOSEndBlock (Compose?: $wroteComposeBlock)")"
    if [ -f "/tmp/geex.summary.dd" ]; then
        rm /tmp/geex.summary.dd
    fi
    echo "$summaryTextContents" >> /tmp/geex.summary.dd
    summary=$(dialog --backtitle "Geex Installer" --title "Summary" --msgbox "$summaryTextContents" 34 75 3>&1 1>&2 2>&3)
    if [[ -f "/tmp/geex.config.scm" ]]; then
        rm /tmp/geex.config.scm
    fi
    if [[ -f "/tmp/geex.config.${stager}.dd" ]]; then
        cp /tmp/geex.config.${stager}.dd /tmp/geex.config.scm
        if command -v guix >/dev/null; then
            guix style -f /tmp/geex.config.scm
        fi
    fi
    configDisplay=$(dialog --backtitle "Geex Installer" --title "Written Configuration (/tmp/geex.config.scm)" --textbox "/tmp/geex.config.scm" 34 75 3>&1 1>&2 2>&3) || exit 1
    if [ "$GEEX_LIVE_MODE" == 1 ]; then
        liveNotice=$(dialog --backtitle "Geex Installer" --title "Live Notice" --msgbox "You have been using Live Mode for the duration of this installation process. However, the install process is nearing its end, the Live Preview Windows will now be selectively killed, and the installation procedure will continue." 34 75 3>&1 1>&2 2>&3) || exit 1
        liveKillHook
    fi
    openConfigHook
    confirmation=$(dialog --backtitle "Geex Installer" --title "Confirmation" --menu "Have you confirmed whether or not all the information provided is correct? If so, would you like to begin the installation now?" 12 60 10 \
                          yes "Yes, begin Installation" \
                          no "No, Abort" \
                          3>&1 1>&2 2>&3) || exit 1
    if [ "$confirmation" == "yes" ]; then
        channelPullHook
        systemInstallHook
    else
        dialog --clear
        clear
        echo "[ Status ]: Aborting..."
        exit 1
    fi
    if [ "$installationStatus" == 1 ]; then
        success=$(dialog --backtitle "Geex Installer" --title "Success" --menu "The Installer successfully installed your GNU Guix System to '${geexMount}', attached to Disk '$disk'.\n\nPlease verify whether the installation process actually worked. The Installer will now continue to the Password Application Phase, and then ask you for your Guix Home preferences." 32 50 10 \
                         continue "Continue" \
                         abort "Abort" \
                         3>&1 1>&2 2>&3) || exit 1
        if [ "$success" == "abort" ]; then
            dialog --clear
            clear
            echo "[ Status ]: Aborting..."
            exit 1
        fi
        if [ "$configuredPasswords" == 1 ]; then
            passwordApplyHook
        fi
        homeHook
    else
        if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
            noticePopup=$(dialog --backtitle "Geex Installer" --title "Debug Notice" --menu "Debug Mode has been detected by the Installer. Your installation has been running in Debug Mode.\n\nThe Installer will now continue with a Mock installation success hook." 16 50 10 \
                                 okay "Okay" \
                                 abort "Abort" \
                                 3>&1 1>&2 2>&3) || exit 1
            if [ "$noticePopup" == "okay" ]; then
                if [ "$configuredPasswords" == 1 ]; then
                    passwordApplyHook
                fi
                homeHook
                if [ "$formattedDisksStatus" == 1 ]; then
                    export formattedDisksStatus="Yes"
                elif [ "$formattedDisksStatus" == 2 ]; then
                    export formattedDisksStatus="Mock"
                else
                    export formattedDisksStatus="No"
                fi
                if [ "$copiedHome" == 1 ]; then
                    export homeStatus="Yes"
                elif [ "$copiedHome" == 2 ]; then
                    export homeStatus="Mock"
                else
                    export homeStatus="No"
                fi
                if [ "$finishedDesktopSetup" == 0 ]; then
                    desktopsExist="No"
                else
                    desktopsExist="Yes"
                fi
                if [ "$formattedHurd" == 1 ]; then
                    hurdNotice=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "Warning: Your disks have been formatted to be compatible with GNU Hurd 'the hurd'. This is not intended, not recommended, and most likely not even supported by your device. It is very likely that, if you GENUINELY set your system up to use GNU Hurd as its Kernel, your system WILL NOT BOOT!\n\nPlease consider this a Warning, exit the installer by pressing 'Ctrl+c' (or selecting 'Abort'/'No' at the next possible opportunity), and try again without variables like 'GEEX_THE_HURD' set." 32 0 3>&1 1>&2 2>&3)
                    export formattedDisksStatus="Hurd"
                fi
                export finishedMessage="$(echo -e "
(1) Information
Username: $username
Hostname: $hostname
Timezone: $TIMEZONE
Keyboard: $keyboardInfo
Passwords Set?: $areAllPasswordsSet (Re-Used?: $wasPasswordReUsed)
Disk: $disk (Parts: $diskPrefixedPartNameTextblock)
BIOS: $bios (Detected: $detectedBios)
Swap: $formattedWithSwap (Part: ${diskPrefixed}1)

(2) Multiple-Choice
Services: $serviceSelection
Desktops: $deSelection
Bundles: $bundleSelection

(3) Wrote Blocks
Swap Block?: $wroteSwapBlock
BIOS Block?: $wroteBiosBlock
Filesystem Block?: $isFilesystemWritten
Service Block?: $areServicesWritten
Desktop Block?: $areDesktopsWritten
Keyboard Block?: $wroteKeyboardBlock
Timezone Block?: $wroteTimezoneBlock
Bundles Block?: $wroteBundles
Xorg Block?: $wroteXorgBlock
Extra Packages Block?: $wroteCustomPackages
Closing OS Block?: $wroteOSEndBlock (Compose?: $wroteComposeBlock)

(4) Other
Internet Connection?: $hasInternetConnection (Via: $connectivityStatusSummary)
Pulled Channels?: $channelReport
Copied Home?: $homeStatus
Home-Get Method?: $homeGetMethod
Formatted Disks?: $formattedDisksStatus
Mount-Point?: $geexMount

Complete Installation?
")"
                if [ "$systemFinished" == 1 ]; then
                    finishedNotice=$(dialog --backtitle "Geex Installer" --title "Finalization" --yesno "$finishedMessage" 36 100 \
                                            3>&1 1>&2 2>&3)
                    FINISHED_NOTICE_RESPONSE_CODE=$?

                    if [ $FINISHED_NOTICE_RESPONSE_CODE -eq 0 ]; then
                        export finishedNoticeAnswer="yes"
                    else
                        export finishedNoticeAnswer="no"
                    fi
                    if [ "$finishedNoticeAnswer" == "no" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    else
                        dialog --clear
                        clear
                        if [[ ! -f "/tmp/config.scm" ]]; then
                            if [ -f "/tmp/geex.config.scm" ]; then
                                cp /tmp/geex.config.scm /tmp/config.scm
                            else
                                cp /tmp/geex.config.${stager}.dd /tmp/config.scm
                            fi
                            if command -v guix >/dev/null; then
                                guix style -f /tmp/config.scm
                                echo -e "\n[ Style Notice ]: Styled '/tmp/config.scm'"
                            fi
                        else
                            rm /tmp/config.scm
                            if [ -f "/tmp/geex.config.scm" ]; then
                                cp /tmp/geex.config.scm /tmp/config.scm
                            else
                                cp /tmp/geex.config.${stager}.dd /tmp/config.scm
                            fi
                            if command -v guix >/dev/null; then
                                guix style -f /tmp/config.scm
                                echo -e "\n[ Style Notice ]: Styled '/tmp/config.scm'"
                            fi
                        fi
                        if [[ "$copiedHome" == 1 ]]; then
                            export geexHomeStatusBlock="
[ Geex Home ]
The Geex Installer copied a set of Guix Home Configuration File(s) to your target System ($disk). The Home Configuration was copied to '${geexMount}/etc/guix/home.scm', and the directories '${geexMount}/etc/guix/files' and '${geexMount}/etc/guix/containers' were also copied/set up.

It fetched these Files via ${homeGetMethod}. (local: from local files, git: cloned from remote source, mock/none: not copied or just pretended to)

To build the Guix Home Configuration once you have booted into/entered your new GNU Guix System, run:
  'guix home reconfigure /etc/guix/home.scm'
"
                        elif [[ "$copiedHome" == 2 ]]; then
                            export geexHomeStatusBlock="
[ Geex Home ]
The Geex Installer pretended to copy a set of Guix Home Configuration File(s) to your target System ($disk). The Home Configuration was copied to '${geexMount}/etc/guix/home.scm', and the directories '${geexMount}/etc/guix/files' and '${geexMount}/etc/guix/containers' were also copied/set up.

It fetched these Files via ${homeGetMethod}. (local: from local files, git: cloned from remote source, mock/none: not copied or just pretended to)

To build the Guix Home Configuration once you have booted into/entered your new GNU Guix System, run:
  'guix home reconfigure /etc/guix/home.scm'
"
                        else
                            export geexHomeStatusBlock="
[ Geex Home ]
The Installer did not copy any Home Files.
"
                        fi
                        printf "
[ Closing Report ]
Success! The Geex Installer has successfully installed GNU Guix to your '$disk' Drive, mounted at '$geexMount', and copied your custom configuration to '${geexMount}/etc/guix/config.scm'.

${geexHomeStatusBlock}

[ System Management ]
To rebuild your GNU Guix System, run the command:
  '$escalationUtil guix system reconfigure /etc/guix/config.scm'
from within your new GNU Guix System.

To describe your current system, run 'guix system describe', and to see what channels you have set up, run 'guix describe'.

If you want to pull new channels/update your channels according to a '~/.config/guix/channels.scm' or '/etc/guix/channels.scm' file, run: 'guix pull', or alternatively 'guix pull --channels=/etc/guix/channels.scm'.

[ Goodbye ]
Thank you for using the Geex Installer!

"
                    fi
                elif [ "$systemFinished" == 2 ]; then
                    if [ "$formattedHurd" == 1 ]; then
                        hurdNotice=$(dialog --backtitle "Geex Installer" --title "GNU Hurd" --msgbox "Warning: Your disks have been formatted to be compatible with GNU Hurd 'the hurd'. This is not intended, not recommended, and most likely not even supported by your device. It is very likely that, if you GENUINELY set your system up to use GNU Hurd as its Kernel, your system WILL NOT BOOT!\n\nPlease consider this a Warning, exit the installer by pressing 'Ctrl+c' (or selecting 'Abort'/'No' at the next possible opportunity), and try again without variables like 'GEEX_THE_HURD' set." 32 0 3>&1 1>&2 2>&3)
                        export formattedDisksStatus="Hurd"
                    fi
                    finishedNotice=$(dialog --backtitle "Geex Installer" --title "Finalization" --yesno "$finishedMessage" 36 100 \
                                            3>&1 1>&2 2>&3)
                    FINISHED_NOTICE_RESPONSE_CODE=$?
                    if [ $FINISHED_NOTICE_RESPONSE_CODE -eq 0 ]; then
                        export finishedNoticeAnswer="yes"
                    else
                        export finishedNoticeAnswer="no"
                    fi
                    if [ "$finishedNoticeAnswer" == "no" ]; then
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    else
                        dialog --clear
                        clear
                        if [[ "$copiedHome" == 1 ]]; then
                            export geexHomeStatusBlock="
[ Geex Home ]
The Geex Installer copied a set of Guix Home Configuration File(s) to your target System ($disk). The Home Configuration was copied to '${geexMount}/etc/guix/home.scm', and the directories '${geexMount}/etc/guix/files' and '${geexMount}/etc/guix/containers' were also copied/set up.

It fetched these Files via ${homeGetMethod}. (local: from local files, git: cloned from remote source, mock/none: not copied or just pretended to)

To build the Guix Home Configuration once you have booted into/entered your new GNU Guix System, run:
  'guix home reconfigure /etc/guix/home.scm'
"
                        elif [[ "$copiedHome" == 2 ]]; then
                            export geexHomeStatusBlock="
[ Geex Home ]
The Geex Installer pretended to copy a set of Guix Home Configuration File(s) to your target System ($disk). The Home Configuration was copied to '${geexMount}/etc/guix/home.scm', and the directories '${geexMount}/etc/guix/files' and '${geexMount}/etc/guix/containers' were also copied/set up.

It fetched these Files via ${homeGetMethod}. (local: from local files, git: cloned from remote source, mock/none: not copied or just pretended to)

To build the Guix Home Configuration once you have booted into/entered your new GNU Guix System, run:
  'guix home reconfigure /etc/guix/home.scm'
"
                        else
                            export geexHomeStatusBlock="
[ Geex Home ]
The Installer did not copy any Home Files.
"
                        fi
                        printf "
[ Closing Report ]
Success! The Geex Installer has successfully installed GNU Guix to your '$disk' Drive, mounted at '$geexMount', and copied your custom configuration to '${geexMount}/etc/guix/config.scm'.

${geexHomeStatusBlock}

[ System Management ]
To rebuild your GNU Guix System, run the command:
  '$escalationUtil guix system reconfigure /etc/guix/config.scm'
from within your new GNU Guix System.

To describe your current system, run 'guix system describe', and to see what channels you have set up, run 'guix describe'.

If you want to pull new channels/update your channels according to a '~/.config/guix/channels.scm' or '/etc/guix/channels.scm' file, run: 'guix pull', or alternatively 'guix pull --channels=/etc/guix/channels.scm'.

[ Goodbye ]
Thank you for using the Geex Installer!

"
                        if [[ ! -f "/tmp/config.scm" ]]; then
                            if [ -f "/tmp/geex.config.scm" ]; then
                                cp /tmp/geex.config.scm /tmp/config.scm
                            else
                                cp /tmp/geex.config.${stager}.dd /tmp/config.scm
                            fi
                            if command -v guix; then
                                guix style -f /tmp/config.scm
                                echo -e "\n[ Style Notice ]: Styled '/tmp/config.scm'"
                            fi
                            echo -e "[ Notice ]: Copied your '/tmp/geex.config.${stager}.dd' to '/tmp/config.scm'."
                        else
                            rm /tmp/config.scm
                            if [ -f "/tmp/geex.config.scm" ]; then
                                cp /tmp/geex.config.scm /tmp/config.scm
                            else
                                cp /tmp/geex.config.${stager}.dd /tmp/config.scm
                            fi
                            if command -v guix; then
                                guix style -f /tmp/config.scm
                                echo -e "\n[ Style Notice ]: Styled '/tmp/config.scm'"
                            fi
                            echo -e "\n\n[ Notice ]: Copied your '/tmp/geex.config.${stager}.dd' to '/tmp/config.scm'.\n\n"
                        fi
                        if [[ ! -n "$GEEX_DEBUG_MODE" ]] || [[ "$GEEX_DEBUG_MODE" != 1 ]]; then
                            if mountpoint -q ${geexMount}; then
                                umount ${geexMount}
                                if [[ "$geexMount" != "/mnt" ]] && [[ "$geexMount" != "/Mount" ]]; then
                                    rm -rf $geexMount
                                fi
                            fi
                        fi
                    fi
                elif [ "$systemFinished" == 0 ]; then
                    if [[ -n "$GEEX_DEBUG" ]] || [[ -n "$GEEX_DEBUG_MODE" ]]; then
                        debugNotice=$(dialog --backtitle "Geex Installer" --title "Debug Notice" --msgbox "The Installer has detected Debug Mode, all though it will no longer force-exit/quit the installation procedure, you have STILL encountered an error that is not supposed to ever occur under any circumstances, please verify this!\n\nActions to take:\n - Fetch the Official Geex Installer from 'https://github.com/librepup/geex'\n - Verify your '/tmp' Directory is Writeable\n\nContinuing due to Debug Setting." 34 75 3>&1 1>&2 2>&3) || exit 1
                    else
                        errorMessage=$(dialog  --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has encountered one (or more) errors due to which the 'systemFinished' state could not be reached.\n\nPlease debug this error yourself. Possible causes are:\n - Faulty Internet Connection\n - Entire (Selected) Disk is Read-Only\n - The Geex Installer's Code was Tampered With\n - Files or Processes were Modified (or Killed) while the Geex Installer was not Done with them.\n\nThe Installer will now quit, but may have still been working on and with your disk(s), filesystem, written configuration files, copied or fetched remote sources, and more.\n\nPLEASE verify this yourself!" 34 75 3>&1 1>&2 2>&3) || exit 1
                        dialog --clear
                        clear
                        echo "[ Status ]: Aborting..."
                        exit 1
                    fi
                else
                    errorMessage=$(dialog --backtitle "Geex Installer" --title "Error" --msgbox "The Installer has experienced and unknown (and supposed to be impossible to reach) state due to an unknown error. This is pretty much only possible if you or someone else has tampered with the Geex Installer's Code beforehand, and you are not running an original copy of this Installer.\n\nPlease verify you are running the official Geex Installer from the following repository:\n - https://github.com/librepup/geex\n\nThe Installer cannot continue. If you are not inside Debug Mode, the installer may still have formatted and worked with your disk(s), as well as written files, and possibly taken other actions as well.\n\nPlease check this yourself." 34 75 3>&1 1>&2 2>&3) || exit 1
                    dialog --clear
                    clear
                    echo "[ Status ]: Aborting..."
                    exit 1
                fi
            fi
            exit 1
        fi
        error=$(dialog --backtitle "Geex Installer" --title "Error" --menu "The Installer has encountered one or more errors during the system installation phase. Your system was NOT installed. Disks may still have been partitioned, formatted, and mounted - CHECK THIS!\n\nThe installer will now quit." 32 50 10 \
                       okay "Okay" \
                       3>&1 1>&2 2>&3) || exit 1

        if [ -n "$GEEX_DEBUG" ] || [ -n "$GEEX_DEBUG_MODE" ]; then
            if [ "$error" == "okay" ]; then
                echo "[ Status ]: Debug Mode Detected, ignoring Exit Call..."
            fi
        else
            if [ "$error" == "okay" ]; then
                dialog --clear
                clear
                echo "[ Status ]: Quitting..."
                exit 1
            fi
        fi
        exit 1
    fi
}

for arg in "$@"; do
    case "$arg" in
        b|-b|--b|bootstrap|-bootstrap|--bootstrap)
            bootstrapHook
            exit 1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        v|-v|--v|verbose|-verbose|--verbose)
            export GEEX_VERBOSE_MODE=1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        u|-u|--u|update|-update|--update)
            export GEEX_UPDATE=1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        d|-d|--d|debug|-debug|--debug)
            export GEEX_DEBUG=1
            export GEEX_DEBUG_MODE=1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        container|-container|--container|con|-con|--con)
            containerManagerHook
            exit 1
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        iwantgnuhurd|-iwantgnuhurd|--iwantgnuhurd)
            export GEEX_THE_HURD=1
            export GEEX_THE_HURD_ALLOW=1
            export GEEX_FORCE_THE_HURD=1
            if [[ "$GEEX_IGNORE_FORCED_DEBUG" != 1 ]] || [[ -z "$GEEX_IGNORE_FORCED_DEBUG" ]]; then
                export GEEX_DEBUG_MODE=1
                export GEEX_DEBUG=1
                export GEEX_FORCED_DEBUG=1
                export GEEX_HURD_RENDER_WARNING=1
            elif [[ "$GEEX_IGNORE_FORCED_DEBUG" == 1 ]] && [[ -n "$GEEX_IGNORE_FORCED_DEBUG" ]]; then
                export GEEX_HURD_RENDER_WARNING=2
            else
                export GEEX_DEBUG_MODE=1
                export GEEX_DEBUG=1
                export GEEX_FORCED_DEBUG=1
                export GEEX_HURD_RENDER_WARNING=1
            fi
            installerHook
            ;;
    esac
done

for arg in "$@"; do
    case "$arg" in
        i|-i|--i|install|-install|--install)
            installerHook
            ;;
    esac
done

if [ "$GEEX_MOVER_MODE" == 1 ]; then
    installerHook
fi

# Unset Exported Variables
unset missingCommandCount
unset IN_GUIX_SHELL
unset randFunc
unset randFuncString
unset randEtcName
unset randCfgName
unset backedUpCfg
unset backedUpEtc
unset backupState
unset copyState
unset userName
unset homeDirectory
unset manualHomeDirectory
unset manualUserName
unset userGuess
