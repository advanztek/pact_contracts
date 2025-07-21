(namespace "free")
(define-keyset "free.module-name-admin" (read-keyset "k"))

(module module-name GOVERNANCE
    @doc "Reusable NFT creator for managing a single collection with multiple tokens"

    (use marmalade-ng.ledger)
    (use marmalade-ng.policy-collection)
    (use marmalade-ng.std-policies)
    (use free.util-lists [chain])

    ; --------------------------------------------------------------------------
    ; Constants
    ; --------------------------------------------------------------------------
    (defconst COLLECTION_KEY "main-collection")
    (defconst TOKEN_COUNT "token-count")
    (defconst ADMIN_KEYSET "free.module-name-admin")
    (defconst COLLECTION_INITIALIZED "collection-initialized")

    ; --------------------------------------------------------------------------
    ; Schema
    ; --------------------------------------------------------------------------
    (defschema collection-schema
        @doc "Stores the single collection data"
        id:string
        collection-name:string
        collection-guard:guard
        creator:string
        minted-tokens:[string]
        non-minted-tokens:[string]
        reserved-tokens:[string]
        minted-total:integer
        total-supply:integer
        royalty:decimal
        initialized:bool
    )
    
    (defschema token-schema
        @doc "Stores token data"
        id:string
        supply:decimal
        token-uri:string
        is-minted:bool
        owner:string
        created-at:time
    )

    (defschema counter-schema
        @doc "Simple counter schema"
        count:integer
    )

    ; --------------------------------------------------------------------------
    ; Tables
    ; --------------------------------------------------------------------------
    (deftable collection-table:{collection-schema})
    (deftable tokens-table:{token-schema})
    (deftable counters-table:{counter-schema})

    ; --------------------------------------------------------------------------
    ; Capabilities
    ; --------------------------------------------------------------------------
    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard ADMIN_KEYSET))
    )

    (defcap PRIVATE ()
        true
    )

    (defcap ACCOUNT_GUARD (account:string) 
        @doc "Verifies account meets format and belongs to caller"
        (enforce-guard 
            (at "guard" (coin.details account))
        )
    )

    ; --------------------------------------------------------------------------
    ; Init Functions
    ; --------------------------------------------------------------------------
    (defun init-contract()
        @doc "Initialize the contract with default values"
        (with-capability (GOVERNANCE)
            (insert counters-table TOKEN_COUNT { "count": 0 })
        )
    )

    (defun initialize-collection (col-name:string creator:string royalty:decimal)
        @doc "Initialize the single collection for this contract instance"
        (with-capability (GOVERNANCE)
            (let*
                (
                    (collection-guard (read-keyset 'ks))
                    (collection-id (create-collection-id col-name collection-guard))
                    (is-initialized (try false (at 'initialized (read collection-table COLLECTION_KEY))))
                )
                (enforce (= is-initialized false) "Collection already initialized!")
                (enforce (> (length col-name) 0) "Collection name cannot be empty")
                (enforce (>= royalty 0.0) "Royalty cannot be negative")
                (enforce (<= royalty 1.0) "Royalty cannot exceed 100%")
                
                ; Create the collection in the policy system
                (create-collection collection-id col-name 0 creator collection-guard)
                
                ; Store collection data
                (insert collection-table COLLECTION_KEY {
                    'id: collection-id,
                    'collection-name: col-name,
                    'collection-guard: collection-guard,
                    'creator: creator,
                    'minted-tokens: [],
                    'non-minted-tokens: [],
                    'reserved-tokens: [],
                    'minted-total: 0,
                    'total-supply: 0,
                    'royalty: royalty,
                    'initialized: true
                })
                
                (format "Collection '{}' initialized successfully with ID: {}" [col-name collection-id])
            )
        )
    )

    ; --------------------------------------------------------------------------
    ; Token Creation and Minting Functions
    ; --------------------------------------------------------------------------
    (defun create-and-mint-tokens (receivers:[string] uris:[string])
        @doc "Create and mint tokens to multiple receivers"
        (with-capability (GOVERNANCE)
            (with-capability (PRIVATE)
                (let* 
                    (
                        (collection-data (get-collection))
                        (collection-id (at 'id collection-data))
                        (policies (list-to-policies (read-msg 'policies)))
                        (tmp-guard (read-keyset 'ks-tmp))
                        (token-ids (map (create-token-id tmp-guard) uris))
                        (receivers-count (length receivers))
                        (uris-count (length uris))
                        (new-minted-total (+ (at 'minted-total collection-data) uris-count))
                        (current-token-count (get-token-count))
                        (new-token-count (+ current-token-count uris-count))
                        (old-minted-list (at 'minted-tokens collection-data))
                        (new-minted-list (chain [old-minted-list token-ids]))
                        ; Create combined data for processing
                        (token-data (zip (lambda (id uri) {"id": id, "uri": uri}) token-ids uris))
                        (combined-data (zip (lambda (data receiver) 
                                            {"id": (at "id" data), 
                                            "uri": (at "uri" data), 
                                            "receiver": receiver}) 
                                        token-data receivers))
                    )
                    
                    (enforce (= receivers-count uris-count) "Receivers and URIs count must match")
                    (enforce (> uris-count 0) "Must provide at least one URI")
                    
                    ; Create tokens in the policy system
                    (zip (lambda (id uri) (create-token id 1 uri policies tmp-guard)) token-ids uris)
                    
                    ; Mint tokens to receivers
                    (zip (lambda (id receiver) 
                        (mint id receiver (at "guard" (coin.details receiver)) 1.0)) 
                        token-ids receivers)
                    
                    ; Store token data using the combined data
                    (map (lambda (item) 
                        (insert tokens-table (at "id" item) {
                            'id: (at "id" item),
                            'supply: 1.0,
                            'token-uri: (at "uri" item),
                            'is-minted: true,
                            'owner: (at "receiver" item),
                            'created-at: (at "block-time" (chain-data))
                        })) combined-data)
                    
                    ; Update collection data
                    (update collection-table COLLECTION_KEY {
                        "minted-total": new-minted-total,
                        "total-supply": new-minted-total,
                        "minted-tokens": new-minted-list
                    })
                    
                    ; Update token count
                    (update counters-table TOKEN_COUNT {"count": new-token-count})
                    
                    ; Return created token IDs
                    token-ids
                )
            )
        )
    )

    (defun create-and-mint-to-single-account (receiver:string uris:[string])
        @doc "Create and mint multiple tokens to a single receiver"
        (with-capability (GOVERNANCE)
            (with-capability (PRIVATE)
                (let* 
                    (
                        (collection-data (get-collection))
                        (collection-id (at 'id collection-data))
                        (policies (list-to-policies (read-msg 'policies)))
                        (tmp-guard (read-keyset 'ks-tmp))
                        (receiver-guard (read-keyset 'ks-receiver))
                        (token-ids (map (create-token-id tmp-guard) uris))
                        (uris-count (length uris))
                        (new-minted-total (+ (at 'minted-total collection-data) uris-count))
                        (current-token-count (get-token-count))
                        (new-token-count (+ current-token-count uris-count))
                        (old-non-minted-list (at 'non-minted-tokens collection-data))
                        (new-non-minted-list (chain [old-non-minted-list token-ids]))
                    )
                    
                    (enforce (> uris-count 0) "Must provide at least one URI")
                    
                    ; Create tokens in the policy system
                    (zip (lambda (id uri) (create-token id 1 uri policies tmp-guard)) token-ids uris)
                    
                    ; Mint tokens to receiver
                    (map (lambda (id) (mint id receiver receiver-guard 1.0)) token-ids)
                    
                    ; Store token data
                    (zip (lambda (id uri) 
                        (insert tokens-table id {
                            'id: id,
                            'supply: 1.0,
                            'token-uri: uri,
                            'is-minted: false,
                            'owner: receiver,
                            'created-at: (at "block-time" (chain-data))
                        })) token-ids uris)
                    
                    ; Update collection data
                    (update collection-table COLLECTION_KEY {
                        "total-supply": new-minted-total,
                        "non-minted-tokens": new-non-minted-list
                    })
                    
                    ; Update token count
                    (update counters-table TOKEN_COUNT {"count": new-token-count})
                    
                    ; Return created token IDs
                    token-ids
                )
            )
        )
    )

    ; --------------------------------------------------------------------------
    ; Query Functions
    ; --------------------------------------------------------------------------
    (defun get-collection()
        @doc "Returns the collection information"
        (read collection-table COLLECTION_KEY)
    )

    (defun get-collection-info()
        @doc "Returns basic collection information"
        (let ((col-data (get-collection)))
            {
                "name": (at 'collection-name col-data),
                "id": (at 'id col-data),
                "creator": (at 'creator col-data),
                "minted-total": (at 'minted-total col-data),
                "total-supply": (at 'total-supply col-data),
                "royalty": (at 'royalty col-data)
            }
        )
    )

    (defun get-token (token-id:string)
        @doc "Returns token information"
        (read tokens-table token-id)
    )

    (defun get-all-tokens()
        @doc "Returns all tokens"
        (select tokens-table (where "id" (!= "")))
    )

    (defun get-minted-tokens()
        @doc "Returns all minted tokens"
        (select tokens-table (where "is-minted" (= true)))
    )

    (defun get-non-minted-tokens()
        @doc "Returns all non-minted tokens"
        (select tokens-table (where "is-minted" (= false)))
    )

    (defun get-tokens-by-owner (owner:string)
        @doc "Returns all tokens owned by a specific account"
        (select tokens-table (where "owner" (= owner)))
    )

    (defun get-token-count:integer ()
        @doc "Gets the total token count"
        (at "count" (read counters-table TOKEN_COUNT ['count]))
    )

    (defun is-collection-initialized:bool ()
        @doc "Check if collection has been initialized"
        (try false (at 'initialized (read collection-table COLLECTION_KEY)))
    )

    ; --------------------------------------------------------------------------
    ; Utility Functions
    ; --------------------------------------------------------------------------
    (defun get-collection-stats()
        @doc "Returns collection statistics"
        (let ((col-data (get-collection)))
            {
                "collection-name": (at 'collection-name col-data),
                "total-tokens": (get-token-count),
                "minted-total": (at 'minted-total col-data),
                "total-supply": (at 'total-supply col-data),
                "royalty-percentage": (at 'royalty col-data)
            }
        )
    )
)

(create-table collection-table)
(create-table tokens-table)  
(create-table counters-table)
(init-contract)