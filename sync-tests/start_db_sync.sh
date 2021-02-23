#!/bin/bash

db_sync_logfile="logs/db_sync_logfile.log"
db_sync_summary_logfile="logs/db_sync_summary.log"

function usage() {
    cat << HEREDOC

    arguments:
    -e          environment (network) - possible options: mainnet, shelley_qa

    optional arguments:
    -h          show this help message and exit

	Example:

	./start_db_sync.sh -e shelley_qa

	USE UNDERSCORES IN NETWORK NAMES !!!
HEREDOC
}

while getopts ":h:e:" o; do
    case "${o}" in
        h)
            usage
            ;;
        e)
            environment=${OPTARG}
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


function get_network_param_value() {

	if [ "$environment" = "mainnet"  ]
	    then
	        echo "--mainnet"

	elif [ "$environment" = "testnet" ]
		then
	    	echo "--testnet-magic 1097911063"

	elif [ "$environment" = "staging" ]
		then
	    	echo "--testnet-magic 633343913"

	elif [ "$environment" = "shelley_qa" ]
		then
	    	echo "--testnet-magic 3"

		else:
	    	echo ""
		fi
}

function get_block_from_tip() {

	 ./../cardano-node/cardano-cli query tip $(get_network_param_value) |

	while read -r line
	do
		if [[ $line == *"block"* ]]; then
	    	IFS=' ' read -ra ADDR <<< "$line"
	      	echo "${ADDR[1]}" | sed 's/.$//'
		fi
	done
}

function get_latest_db_synced_slot() {

	local log_filepath=$1
	IN=$(tail -n 1 $log_filepath)
	preformated_string=$(echo "$IN" | sed 's/^.*slot/slot/') # this will return: "slot 19999, block 20000, hash 683be7324c47df71e2a234639a26d7747f1501addbba778636e66f3a18a46db7"

	IFS=' ' read -ra ADDR <<< "$preformated_string"
	for i in "${!ADDR[@]}"; do
		if [[ "${ADDR[$i]}" == *"slot"* ]]; then # we get the index $i for slot keyword - we know that slot number has ${i+1) position
	    	slot_number=$(echo "${ADDR[$((i+1))]}"| sed 's/.$//') # use sed to remove comma at the end of slot number
	       	echo $slot_number
	    fi
	done
}

function calculate_latest_node_slot_for_environment() {

	if [ "$environment" = "mainnet"  ]
	then
	        byron_start_time_in_seconds=1506203091   # 2017-09-23 21:44:51
	        shelley_start_time_in_seconds=1596059091 # 2020-07-29 21:44:51
	        allegra_start_time_in_seconds=1608155091 # 2020-12-16 21:44:51

	elif [ "$environment" = "testnet" ]
	then
	        byron_start_time_in_seconds=1563999616    # 2019-07-24 20:20:16
	        shelley_start_time_in_seconds=1595967616  # 2020-07-28 20:20:16
	        allegra_start_time_in_seconds=1608063616  # 2020-12-15 20:20:16

	elif [ "$environment" = "staging" ]
	then
	        byron_start_time_in_seconds=1506450213    # 2017-09-26 18:23:33
	        shelley_start_time_in_seconds=1596306213  # 2020-08-01 18:23:33
	        allegra_start_time_in_seconds=1608402213  # 2020-12-19 18:23:33

	elif [ "$environment" = "shelley_qa" ]
	then
	        byron_start_time_in_seconds=1597669200    # 2020-08-17 13:00:00
	        shelley_start_time_in_seconds=1597683600  # 2020-08-17 17:00:00
	        allegra_start_time_in_seconds=1607367600  # 2020-12-07 19:00:00
	else:
	    echo ""
	fi

	slots_in_epoch=432000
	current_time=$(date +'%s')
	current_slot=$(( (shelley_start_time_in_seconds - byron_start_time_in_seconds)/20  + current_time - shelley_start_time_in_seconds - slots_in_epoch/3))
	echo $current_slot
}


export CARDANO_NODE_SOCKET_PATH=/home/runner/work/database-sync-tests/database-sync-tests/cardano-node/${environment}/node.socket
network_param=$(get_network_param_value)

######################################## GET CONFIGS FOR ALL NETWORKS ########################################

cd ..
cd config

echo ""
echo "Creating config files for each network in ${PWD}:"

mv shelley-qa-testnet.yaml shelley_qa-config.yaml
mv pgpass-shelley-qa-testnet pgpass-shelley_qa

sed -i "s/NodeConfigFile.*/NodeConfigFile: ..\/..\/cardano-node\/mainnet\/mainnet-config.json/g" mainnet-config.yaml
sed -i "s/NodeConfigFile.*/NodeConfigFile: ..\/..\/cardano-node\/shelley_qa\/shelley_qa-config.json/g" shelley_qa-config.yaml

MODIFIED_NETWORK_NAME=""
for _network in "mary_qa" "staging" "testnet"
do
	cp pgpass-mainnet "pgpass-${_network}"
    sed -i "s/mainnet/${_network}/g" "pgpass-${_network}"

	cp mainnet-config.yaml "${_network}-config.yaml"
	MODIFIED_NETWORK_NAME=$(echo "${_network}" | sed 's/_/-/') # NetworkName entry in db-sync config file requires hyphen instead of underscore
	sed -i "s/NetworkName.*/NetworkName: ${MODIFIED_NETWORK_NAME}/g" "${_network}-config.yaml"
	sed -i "s/NodeConfigFile.*/NodeConfigFile: ..\/..\/cardano-node\/${_network}\/${_network}-config.json/g" "${_network}-config.yaml"
done

cd ..

#################################### END OF: GET CONFIGS FOR ALL NETWORKS ####################################

mkdir logs

db_sync_start_time=$(echo "$(date +'%d/%m/%Y %H:%M:%S')")  # format: 17/02/2021 23:42:12

chmod 600 config/pgpass-${environment}
PGPASSFILE=config/pgpass-${environment} scripts/postgresql-setup.sh --createdb

PGPASSFILE=config/pgpass-${environment} cabal run cardano-db-sync-extended -- \
--config config/${environment}-config.yaml \
--socket-path ../cardano-node/${environment}/node.socket \
--state-dir ledger-state/${environment} \
--schema-dir schema/ >> $db_sync_logfile &

sleep 60

latest_node_slot=$(calculate_latest_node_slot_for_environment)
echo "latest_node_slot: $latest_node_slot"
latest_db_synced_slot=$(get_latest_db_synced_slot $db_sync_logfile)

echo "BEFORE: db_sync_logfile"
cat $db_sync_logfile
echo "AFTER: db_sync_logfile"

re='^[0-9]+$'
while ! [[ $latest_db_synced_slot =~ $re ]] ; do
   echo "Not a slot number, waiting for proper log line that contains slot number..."
   sleep 20
   latest_db_synced_slot=$(get_latest_db_synced_slot $db_sync_logfile)
done

tmp_latest_db_synced_slot=$latest_db_synced_slot


while [ $latest_db_synced_slot -lt $latest_node_slot ]
do
	sleep 20
	latest_db_synced_slot=$(get_latest_db_synced_slot $db_sync_logfile)

	if ! [[ $latest_db_synced_slot =~ $re ]] ; then
		latest_db_synced_slot=$tmp_latest_db_synced_slot
    	continue
	fi
	echo "latest_db_synced_slot: $latest_db_synced_slot"
done

db_sync_end_time=$(echo "$(date +'%d/%m/%Y %H:%M:%S')")

echo "db_sync_start_time: $db_sync_start_time" >> $db_sync_summary_logfile
echo "db_sync_end_time: $db_sync_end_time" >> $db_sync_summary_logfile
echo "latest_db_synced_slot: $latest_db_synced_slot" >> $db_sync_summary_logfile
