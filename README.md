Devcon50 preparation on The Moon
--------------------------------

Workshop requirements: Linux, browser with [the MetaMask extension](https://metamask.io/), and [installed IPFS](https://docs.ipfs.io/guides/guides/install/).

## Step by step instructions

1. Open the statistics dapp in a browser with the MetaMask extension - [The Moon base statistics](http://devcon5.robonomics.network)

![statistics dapp](https://github.com/airalab/devcon5/raw/master/pic/step-1.png)

2. Download the lunar worker binary - [lunar_worker-linux-x86_64.tar.gz
](https://github.com/airalab/devcon5/releases/download/v1-rc2/lunar_worker-linux-x86_64.tar.gz)

![Robonomics on The Moon :: RC2](https://github.com/airalab/devcon5/raw/master/pic/step-2.png)

3. Run the lunar worker with one of the next options and find your robot ID in terminal log:
`./lunar_worker (--construction | --life | --brewery)`

![Example of launched the lunar worker](https://github.com/airalab/devcon5/raw/master/pic/step-3.png)

4. Open the Robonomics dapp to communicate with your lunar worker: [devcon5 dapp](https://dapp.robonomics.network/#/lighthouse/devcon50.lighthouse.5.robonomics.eth).

![Robonomics dapp section lighthouse devcon50](https://github.com/airalab/devcon5/raw/master/pic/step-4.png)

5. Go to [The Moon base statistics](http://devcon5.robonomics.network) from step 1 and copy "The program's model" associated with the selected type of work for your lunar worker (--construction | --life | --brewery).

!["The program's model" associated with the selected type of work](https://github.com/airalab/devcon5/raw/master/pic/step-5.png)

6. Paste the selected program's model and your robot id into section "Send message to the Robonomics.network" and push "Broadcast signal to the network" button. In section "Messages from the Robonomics.network" you will find your signal and after 1 - 3 sec you will see the signal from your lunar worker.

![In section "Messages from the Robonomics.network" you will find your signal](https://github.com/airalab/devcon5/raw/master/pic/step-6.png)

And in terminal you will find next messages from lunar worker:

![signal from your lunar worker](https://github.com/airalab/devcon5/raw/master/pic/step-6-1.png)

7. Go back to [The Moon base statistics](http://devcon5.robonomics.network). Wait some time and in the section "Users rating" you will see +1 for your MetaMask address. Common progress for selected type of work will be updated too.

![signal from your lunar worker](https://github.com/airalab/devcon5/raw/master/pic/step-7.png)

8. Now, you can repeat steps 6 - 7 with 1 instance of your lunar worker or you can run more lunar workers with the same or different type of work in a new terminal tab. Run 2 or 3 lunar workers and start sending signals to them.

Any questions send to [Robonomics eng telegram channel](https://aira.life/chat) or just raise your hand to ask our engineers to help you.
