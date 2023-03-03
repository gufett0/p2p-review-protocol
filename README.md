# mesposito_CDP

## Introduction
`peer-to-peer review` is a proof of concept exercise that shows how a decentralized, blockchain-based system could be used to establish the quality and credibility of academic papers. By allowing a large number of reviewers (i.e. identified peers) to participate in a transparent way, the system aims at incentivizing a thorough and honest feedback without the need of centralized authorities or intermediaries (e.g journals, peer-review platforms, conference organizers). For more information on promoting transparency, inclusivity, and accountability in the peer review process, you can learn more about open peer review.

## Diagram

```mermaid
stateDiagram-v2
direction LR
state Script_UTXO1 {
state Empty <<fork>>
state Final <<choice>>
[*] --> Submitted  : <i>Created UTXO\n (2 NFTs + Datum)</i>  
--
Submitted --> Reviewed : <b>Updated At</b> 
Reviewed --> Submitted : <b>Revision </b> 
Submitted --> Final : <b>Claim Author</b> 
Reviewed --> Closed : <b>Closed At</b> 
Reviewed --> Final : <b>Claim Reviewer</b> 
Closed --> Final : <b>Claim Reviewer</b> 
Final --> [*] : <i>Locked UTXO\n (1 NFT + Datum)</i> 
Final --> Empty : <i>Consumed UTXO\n (2 NFTs back to author)</i> 
--
state Endpoints {
note right of Final : Funds are \n redistributed \n accordingly
note right of Submitted : Author has (re)submitted paper
note right of Reviewed : Reviewer has requested\n minor/major revision
note right of Closed : Author has ended\n review process
}
}
```

## Cardano Professional Developer 
Find Certificate at this link