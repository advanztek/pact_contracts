(namespace 'free)
(define-keyset "free.blackjack-admin-keyset" (read-keyset "ks"))

(module blackjack GOVERNANCE
    @doc "A Pact module for Blackjack game with dynamic fees."

    ; Constants
    (defconst GAMEMODES:list ["pvc", "pvp", "tournament"])
    (defconst BANK_ACCOUNT_NAME "blackjack-bank")
    (defconst PVP_REWARD_PERC 0.6)
    (defconst TOURNAMENT_REWARD_PERC 0.7)

    ; Schemas
    (defschema player-stats
        @doc "Schema to track player stats."
        games-played:integer
        wins:integer
    )

    (defschema leaderboard-entry
        @doc "Leaderboard entry schema."
        player:string
        wins:integer
    )

    (defschema tournament
        @doc "Schema to track tournament data."
        id:string
        pool:decimal
        players:[string]
        maxPlayers:integer
        startSignup:time
        endSignup:time
        startDate:time
        endDate:time
    )

    (defschema fees-schema
        @doc "Schema to store fees for different game modes."
        fee:decimal
    )

    (defschema game-record
        @doc "Schema to record game results."
        player:string
        wins:integer
    )

    (defschema account-guard-schema
        @doc "Schema to store account guards."
        guard:guard
    )

    ; Tables
    (deftable player-table:{player-stats})
    (deftable leaderboard:{leaderboard-entry})
    (deftable tournament-table:{tournament})
    (deftable fees-table:{fees-schema})
    (deftable game-records:{game-record})
    (deftable account-guards:{account-guard-schema})

    ; --------------------------------------------------------------------------
    ; Capabilities
    ; --------------------------------------------------------------------------
    (defcap DEFAULT_ACCESS ()
        true
    )

    (defcap GOVERNANCE ()
        (enforce-guard (keyset-ref-guard "free.blackjack-admin-keyset"))
    )

    (defcap ACCOUNT_GUARD (account:string)
        @doc "Verifies account meets format and belongs to caller"
        (with-default-read account-guards account 
            { 'guard: (at "guard" (coin.details account)) }
            { 'guard := guard }
            (enforce-guard guard)
        )
    )

    (defcap BANK_TRANSFER ()
        @doc "Capability for bank transfers"
        true
    )

    ; --------------------------------------------------------------------------
    ; Bank Guard Functions (MUST BE DEFINED FIRST)
    ; --------------------------------------------------------------------------
    (defun require-BJ_BANK ()
      @doc "The function used when building the user guard for managed accounts"
      (require-capability (BANK_TRANSFER))
    )

    (defun create-BJ_BANK-guard ()
      @doc "Creates the user guard"
      (create-user-guard (require-BJ_BANK))
    )

    (defun get-BJ_BANK-account ()
      @doc "Gets the bank account principal"
      (create-principal (create-BJ_BANK-guard))
    )

    ; --------------------------------------------------------------------------
    ; Initialization
    ; --------------------------------------------------------------------------
    (defun init ()
      @doc "Initialize the contract with default fees and create bank account"
      (with-capability (GOVERNANCE)
        ; Create the bank account first
        (coin.create-account (get-BJ_BANK-account) (create-BJ_BANK-guard))
        
        ; Set default fees
        (set-game-fee "pvc" 1.0)
        (set-game-fee "pvp" 2.0)
        (set-game-fee "tournament" 3.0)
      )
    )

    ; --------------------------------------------------------------------------
    ; Admin Functions
    ; --------------------------------------------------------------------------
    (defun set-game-fee(mode:string price:decimal)
        @doc "Allows the admin to set game mode fees."
        (with-capability (GOVERNANCE)
            (enforce (contains mode GAMEMODES) (format "Invalid game mode '{}'. Available modes: {}" [mode GAMEMODES]))
            (write fees-table mode { 'fee: price })
        )
    )

    (defun create-tournament (id:string pool:decimal maxPlayers:integer startSignup:time endSignup:time startDate:time endDate:time)
        @doc "Allows the admin to create a tournament."
        (with-capability (GOVERNANCE)
            (insert tournament-table id {
                'id: id,
                'pool: pool,
                'players: [],
                'maxPlayers: maxPlayers,
                'startSignup: startSignup,
                'endSignup: endSignup,
                'startDate: startDate,
                'endDate: endDate
            })
        )
    )

    ; --------------------------------------------------------------------------
    ; Guard Management
    ; --------------------------------------------------------------------------
    (defun store-account-guard (account:string)
        @doc "Store account guard for future use"
        (with-capability (ACCOUNT_GUARD account)
            (write account-guards account { 'guard: (at "guard" (coin.details account)) })
        )
    )

    ; --------------------------------------------------------------------------
    ; Game Functions
    ; --------------------------------------------------------------------------
    (defun register-for-tournament (tournamentId:string account:string)
      @doc "Pay to register for a tournament."
      (with-capability (ACCOUNT_GUARD account)
        (with-read tournament-table tournamentId
            { 'startSignup:=ss, 'endSignup:=es, 'maxPlayers:=mp, 'players:=players, 'pool:=pool }
            (let ((current-time (curr-time))
                  (current-player-count (length players)))
                (enforce (and (>= current-time ss) (<= current-time es)) "Signup period is closed.")
                (enforce (< current-player-count mp) "Maximum player count reached.")
                (enforce (not (contains account players)) "Player already registered.")

                ; Deduct fee and transfer to bank
                (coin.transfer account (get-BJ_BANK-account) (get-fee "tournament"))

                ; Register player
                (update tournament-table tournamentId {
                    'players: (+ players [account])
                })
            )
        )
      )
    )

    (defun join-game (account:string mode:string tournamentId:string)
      @doc "Join a game or tournament by paying the required fee."
      (with-capability (ACCOUNT_GUARD account)
        ;; Validate the game mode
        (enforce (contains mode GAMEMODES)
                (format "Invalid game mode '{}'. Available modes: {}" [mode GAMEMODES]))

        ;; Handle tournament mode
        (if (= mode "tournament")
            (with-read tournament-table tournamentId
                { 'startDate:=sd, 'endDate:=ed, 'players:=players }
              ;; Check if the tournament is active
              (enforce (and (>= (curr-time) sd) (<= (curr-time) ed))
                      "Tournament is not active.")
              ;; Check if the player is registered for the tournament
              (enforce (contains account players)
                      "Player is not registered for the tournament."))

            ;; Handle non-tournament modes: Deduct fee
            (coin.transfer account (get-BJ_BANK-account) (get-fee mode))
        )

        ;; Update player statistics
        (with-default-read player-table account { 'games-played: 0, 'wins: 0 }
            {'games-played:=games-played, 'wins:=wins}
          (write player-table account { 'games-played: (+ 1 games-played), 'wins: wins }))
      )
    )

    (defun record-win (account:string mode:string tournamentId:string)
      @doc "Records a win for a player and distributes winnings for pvp and tournaments."
      (with-capability (GOVERNANCE)
        ;; Validate the game mode
        (enforce (contains mode GAMEMODES)
          (format "Invalid game mode '{}'. Available modes: {}" [mode GAMEMODES]))

        ;; Update player statistics
        (with-default-read player-table account { 'games-played: 0, 'wins: 0 }
          {'wins:=wins}
          (update player-table account { 'wins: (+ 1 wins) })
        )

        ;; Handle rewards based on game mode
        (with-capability (BANK_TRANSFER)
          (cond
            ((= mode "tournament")
              (with-read tournament-table tournamentId { 'pool:=pool }
                (let ((payout (* TOURNAMENT_REWARD_PERC pool)))
                  (install-capability (coin.TRANSFER (get-BJ_BANK-account) account payout))
                  (coin.transfer (get-BJ_BANK-account) account payout))))
            
            ((= mode "pvp")
              (let ((payout (* PVP_REWARD_PERC (* 2.0 (get-fee "pvp")))))
                (install-capability (coin.TRANSFER (get-BJ_BANK-account) account payout))
                (coin.transfer (get-BJ_BANK-account) account payout)))
            
            ;; PVC mode - no rewards
            "No reward for PVC mode"
          )
        )
      )
    )

    ; --------------------------------------------------------------------------
    ; Get Information
    ; --------------------------------------------------------------------------
    (defun get-game-modes ()
        @doc "Returns the game modes."
        GAMEMODES
    )

    (defun get-fees ()
        @doc "Returns all game fees."
        (select fees-table (where "fee" (!= 0.0)))
    )

    (defun get-player-stats (account:string)
        @doc "Returns the stats for a player."
        (with-default-read player-table account 
            { 'games-played: 0, 'wins: 0 }
            { 'games-played:=gp, 'wins:=w }
            { 'games-played: gp, 'wins: w }
        )
    )

    (defun get-leaderboard ()
        @doc "Returns the leaderboard sorted by wins (descending)."
        (reverse (sort ['wins] (select player-table (where "wins" (> 0)))))
    )

    (defun get-tournament (id:string)
        @doc "Returns the details of a specific tournament."
        (read tournament-table id)
    )

    (defun get-fee (mode:string)
        @doc "Returns the fee for a specific game mode."
        (at "fee" (read fees-table mode))
    )

    (defun curr-time:time ()
      @doc "Returns current chain's block-time"
      (at 'block-time (chain-data))
    )
)

(create-table player-table)
(create-table leaderboard)
(create-table tournament-table)
(create-table fees-table)
(create-table game-records)
(create-table account-guards)
(init)