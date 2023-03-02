# mesposito_CDP

## Introduction
`peer-to-peer review` is a proof of concept exercise that shows how a decentralized, blockchain-based system could be used to establish the quality and credibility of academic papers. By allowing a large number of reviewers (i.e. identified peers) to participate in a transparent way, the system aims at incentivizing a thorough and honest feedback without the need of centralized authorities or intermediaries (e.g journals, peer-review platforms, conference organizers). For more information on promoting transparency, inclusivity, and accountability in the peer review process, you can learn more about open peer review.

## Diagram

```mermaid
stateDiagram-v2
[*] --> Submitted
Submitted --> Consumed_UTXO : Claim Author
Submitted --> Reviewed : Updated At
Reviewed --> Closed : Closed At
Closed --> Locked_UTXO : Claim Reviewer
Locked_UTXO --> [*]
note right of Submitted : Author has (re)submitted paper
note left of Reviewed : Reviewer has requested\n minor/major revision
note left of Locked_UTXO : Final datum + 1 NFT
note right of Consumed_UTXO : 2 NFTs back to the author
note right of Closed : Author has ended\n review process
Reviewed --> Consumed_UTXO : Claim Reviewer
Reviewed --> Submitted : Revision   
```

## Cardano Professional Developer 
Find Certificate at this link
