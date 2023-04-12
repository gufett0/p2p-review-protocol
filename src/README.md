# source code description

The following is a brief walk-through of the main functions and data types of the code. 

## On-chain

The on-chain part is comprised of both a validator (ReviewContract.hs) and a policy minting (ReviewPolicy.hs) script. The former will validate spending transactions occurring during the reviewing process and the latter will validate the minting of a single NFT to the author during the closing transaction.

In the Types.hs module, we define the datum as a custom data type `PaperDatum`, containing key state information, and the redeemer `PaperRedeemer` to call for validation of single blockchain actions (the ReviewContract.hs module has commented datum and redeemer cases in lines 132-247). Among other useful data types, `PaperDecision` defines the four possible options (`Accept|Minor|Major|Reject`) the reviewer can choose to update their verdict on every last paper submission.  

We also define the data type `Paper` whose fields account for the script parameters: the author public key hash, the amount of Ada at stake, the compensation for each reviewer, the least number of reviews required, the time inverval before the next deadline, and the asset class of the tracking tokens. Given the need to create a new UTXO every time the state of the paper review changes, the tracking tokens will govern the whole process in two ways:
- Identify the UTXO carrying the ongoing (`Submitted` or `Reviewed` PaperStatus type) review datum (token value: 2)
- Identify the UTXO carrying the closed (`Closed` PaperStatus type) review datum (token value: 1)
- Identify the UTXO carrying the peer-reviewed (`d_peerReviewed`=True) final datum (token value: >2)

<i>E.g., with 3 participating reviewers, the author will have pre-minted 6 tokens and locked 3 pairs in 3 different utxos. once all reviews get closed, new corresponding utxos will have 1 token each, as the other token will go back to the author. finally, the author will be able to consume them into a final script utxo bundled with the original, no-longer spendable, 6 tokens</i> 

The function `mkPaperValidator` represents the core on-chain validator and it first checks through `validate_inputs` whether there is one input with the token pair (see `oneScriptInputTwoTokens`) or more inputs containing just one token, except for those inputs coming from the author address (see `allScriptInputsOneToken`). So, only if either of these return True, inlinable functions `mkReviewing` and `mkPeerReviewed` will be called to check for several conditions.

In particular, `mkPeerReviewed` (lines 70-125) will enforce the final datum to have fields `d_peerReviewed` equal to true and `d_allRevDecisions` equal to a list of tuples containing each reviewer's pubkey hash and corresponding final decision (see utils function `getRevPkhAndDecision`). The inner function `checkNFTname` will ensure that the pending tx also mint an Nft with its name derived from the bytestring link to the manuscript file. 

Finally, in the ReviewPolicy.hs module, we define the parametrized minting policy (`nftMintingPolicy`) for a unique NFT that can validate only if at least one script input is being validated in the same tx (see `scriptInput`). 



## Off-chain

The off-chain code is mainly comprised of the TxConstruction.hs module. Here we first define four endpoints for each required blockchain action:
- <b>paperSubmission</b>: 

the author submits a tx with initial datum and value locked to the script utxo

- <b>reviewerAction</b>: 

`findPaperOutput` takes in the parameter Paper, the script address and reviewer's pubkey hash to identify the utxo carrying the right datum in the contract monad. After that, `checkDeadline` and `checkStatus` helper functions will guide the required action by means of (Either String ()) as a return type of the contract monad:



 2) if the datum's field d_status is not Closed then d_status, then `checkDeadline` takes in the d_nextDeadline to return a left or right type. 

## Testing
