Devcon50 preparation on The Moon
--------------------------------

Workshop requirements: linux, browser with [metamask extension](https://metamask.io/), and [installed ipfs](https://docs.ipfs.io/guides/guides/install/).

## Step by step instruction

1. Open statistics dapp in browser with metamask extension - [devcon5.robonomics.network](http://devcon5.robonomics.network)

2. Download lunar worker binary - [devcon50-linux-x86_64.tar.gz
](https://github.com/airalab/devcon5/releases/download/v1-rc1/devcon50-linux-x86_64.tar.gz)

3. Run lunar worker with the next options:
./devcon50 (--construction | --life | --brewery) YOUR_METAMASK_ADDRESS

4. Open Robonomics dapp to communicate with your lunar workers: [devcon5 dapp](https://dapp.robonomics.network/#/lighthouse/devcon50.lighthouse.5.robonomics.eth).

5. Go to [statistics dapp](http://devcon5.robonomics.network) from step 1 and copy "The program's model" associated with selected for your lunar worker type of work (--construction | --life | --brewery).

6. Paste selected program's model in section "Send message to the Robonomics.network" and push "Broadcast signal to the network". In section "Messages from the Robonomics.network" you will find your signal and after 1 - 3 sec you will see signal from your lunar worker.

7. Go back to [statistics dapp](http://devcon5.robonomics.network). Wait some time and in section "Users rating" you will +1 for  your metamask address. Common progress for selected type of work will be updated too.

8. Now, you can repeat steps 6 - 7 with 1 instance of your lunar worker or you can run in new terminal tab more lunar worker with the same type of work or with other. Run 2 or 3 lunar workers and start sending signal to them.  

Any questions send to [Robonomics eng telegram channel](https://aira.life/chat) or just raise your hand to ask our engineers to help you.

> More is coming...
