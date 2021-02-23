#!/bin/bash

function usage() {
    cat << HEREDOC

    arguments:
    -e          environment - possible options: allegra, launchpad, mary_qa, mainnet, staging, testnet, shelley_qa
    -t          tag

    optional arguments:
    -h        show this help message and exit

Example:

./start_node.sh -e shelley_qa -t 1.25.0

USE UNDERSCORES IN environment NAMES !!!
HEREDOC
}

while getopts ":h:e:t:" o; do
    case "${o}" in
        h)
            usage
            ;;
        e)
            environment=${OPTARG}
            ;;
        t)
            tag=${OPTARG}
            ;;
        *)
            echo "NO SUCH ARGUMENT: ${OPTARG}"
            usage
            ;;
    esac
done
if [ $? != 0 ] || [ $# == 0 ] ; then
    echo "ERROR: Error in command line arguments." >&2 ; usage; exit 1 ;
fi
shift $((OPTIND-1))

IOHK_ROOT_REPO="input-output-hk"
NODE_REPO="${IOHK_ROOT_REPO}/cardano-node"
NODE_LOGFILEPATH="node_logfile.log"

get_latest_release() {
    curl --silent "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

echo "We are here: ${PWD}, script name is $0"
echo ""
echo "Creating cardano-node directory and entering it ..."

mkdir cardano-node
cd cardano-node

REPO_LATEST_TAG=$(get_latest_release ${NODE_REPO})
NODE_LATEST_TAG=${tag:-"${REPO_LATEST_TAG}"}


echo ""
echo "Downloading latest version of cardano-node tag: $NODE_LATEST_TAG"

wget -q "https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-linux/latest-finished/download/1/cardano-node-$NODE_LATEST_TAG-linux.tar.gz"

echo ""
echo "Unpacking and removing archive ..."

tar -xf "cardano-node-$NODE_LATEST_TAG-linux.tar.gz"
rm "cardano-node-$NODE_LATEST_TAG-linux.tar.gz"

NODE_CONFIGS_URL=$(curl -Ls -o /dev/null -w %{url_effective} https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest-finished/download/1/index.html | sed 's|\(.*\)/.*|\1|')

echo ""
echo "Downloading node configuration files from $NODE_CONFIGS_URL for environments specified in script ..."
echo ""

# Get latest configs for environment(s) you need:
# List of all current environments: "allegra" "launchpad" "mainnet" "mary_qa" "shelley_qa" "staging" "testnet"

for _environment in ${environment}
do
	mkdir ${_environment}
	cd ${_environment}
	echo "${PWD}"
	wget -q  $NODE_CONFIGS_URL/${_environment}-config.json
	wget -q  $NODE_CONFIGS_URL/${_environment}-byron-genesis.json
	wget -q  $NODE_CONFIGS_URL/${_environment}-shelley-genesis.json
	wget -q  $NODE_CONFIGS_URL/${_environment}-topology.json
	wget -q  $NODE_CONFIGS_URL/${_environment}-db-sync-config.json
	echo ""
	cd ..
done

echo ""
echo "Node configuration files located in ${PWD}:"
echo ""

ls -1

echo ""
echo "Node version: "
echo ""
./cardano-node --version

echo ""
echo "CLI version: "
echo ""
./cardano-cli --version


echo ""
echo ""
echo "Starting node."

./cardano-node run --topology ${environment}/${environment}-topology.json --database-path ${environment}/db --socket-path ${environment}/node.socket --config ${environment}/${environment}-config.json >> $NODE_LOGFILEPATH &