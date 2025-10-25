# 🎨 NFT-based Royalty-Splitting for Collaborators

A smart contract solution for managing collaborative creative works and automating royalty distributions.

## 🚀 Features

- Mint NFTs representing co-owned creative works
- Define and manage collaborator shares
- Automatic royalty distribution
- Update ownership and contribution ratios
- Transfer NFT ownership

## 📝 Contract Functions

### Core Operations

- `mint-collaborative-nft`: Create a new collaborative NFT
- `add-collaborator`: Add a collaborator with their share percentage
- `distribute-royalty`: Distribute payments according to shares
- `update-share`: Modify a collaborator's share percentage
- `transfer-nft`: Transfer NFT ownership

### Read-Only Functions

- `get-nft-metadata`: Retrieve NFT details
- `get-collaborator-share`: Get a collaborator's share percentage

## 🛠️ Usage

1. Deploy the contract
2. Mint a collaborative NFT
3. Add collaborators with their shares
4. Use distribute-royalty for payments

## ⚡ Quick Start

```clarity
;; Mint new collaborative NFT
(contract-call? .nft-royalty mint-collaborative-nft "Project Name" "Description")

;; Add collaborator
(contract-call? .nft-royalty add-collaborator u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u30)
```

## 🔒 Security

- Only NFT creator can add/modify collaborators
- Total shares must equal 100%
- Built-in ownership verification
```
