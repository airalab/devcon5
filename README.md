Devcon50 preparation on The Moon
--------------------------------

Workshop requirements: Linux, browser with [the MetaMask extension](https://metamask.io/), and [installed IPFS](https://docs.ipfs.io/guides/guides/install/).

## Step by step instructions

1. Open the statistics dapp in a browser with the MetaMask extension - [devcon5.robonomics.network](http://devcon5.robonomics.network)

![statistics dapp](https://github.com/airalab/devcon5/raw/master/pic/step-1.png)

2. Download the lunar worker binary - [devcon50-linux-x86_64.tar.gz
](https://github.com/airalab/devcon5/releases/download/v1-rc1/devcon50-linux-x86_64.tar.gz)

3. Run the lunar worker with the next options:
`./devcon50 (--construction | --life | --brewery) YOUR_METAMASK_ADDRESS`

4. Open the Robonomics dapp to communicate with your lunar workers: [devcon5 dapp](https://dapp.robonomics.network/#/lighthouse/devcon50.lighthouse.5.robonomics.eth).

5. Go to [the statistics dapp](http://devcon5.robonomics.network) from step 1 and copy "The program's model" associated with the selected type of work for your lunar worker (--construction | --life | --brewery).

6. Paste the selected program's model into section "Send message to the Robonomics.network" and push "Broadcast signal to the network" button. In section "Messages from the Robonomics.network" you will find your signal and after 1 - 3 sec you will see the signal from your lunar worker.

7. Go back to [the statistics dapp](http://devcon5.robonomics.network). Wait some time and in the section "Users rating" you will see +1 for your MetaMask address. Common progress for selected type of work will be updated too.

8. Now, you can repeat steps 6 - 7 with 1 instance of your lunar worker or you can run more lunar workers with the same or different type of work in a new terminal tab. Run 2 or 3 lunar workers and start sending signals to them.

Any questions send to [Robonomics eng telegram channel](https://aira.life/chat) or just raise your hand to ask our engineers to help you.

> More is coming...
