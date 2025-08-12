(namespace 'free)
(define-keyset "free.blackjack-admin-keyset" (read-keyset "ks"))
(define-keyset "free.blackjack-win-recorder-keyset" (read-keyset "wr-ks"))

(module blackjack GOVERNANCE
    @doc "A Pact module for Blackjack game with dynamic fees and game session management."

    ; Constants
    (defconst PVP_REWARD_PERC 0.6)
    (defconst PVC_REWARD_PERC 0.7)
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

    (defschema game-session
        @doc "Schema to track individual game sessions."
        game-id:string
        game-type:string  ; "pvc", "pvp", or "tournament"
        players:[string]
        status:string     ; "active", "completed", "cancelled"
        winner:string
        tournament-id:string  ; only for tournament games
        created-at:time
        completed-at:time
    )

    ; Tables
    (deftable player-table:{player-stats})
    (deftable leaderboard:{leaderboard-entry})
    (deftable tournament-table:{tournament})
    (deftable pvc-fees-table:{fees-schema})
    (deftable pvp-fees-table:{fees-schema})
    (deftable tournament-fees-table:{fees-schema})
    (deftable game-records:{game-record})
    (deftable account-guards:{account-guard-schema})
    (deftable game-sessions:{game-session})

    ; --------------------------------------------------------------------------
    ; Capabilities
    ; --------------------------------------------------------------------------
    (defcap PRIVATE ()
        true
    )

    (defcap GOVERNANCE ()
        @doc "Only for critical contract initialization and emergency upgrades"
        (enforce-guard (keyset-ref-guard "free.blackjack-admin-keyset"))
    )

    (defcap ADMIN ()
        @doc "For daily admin operations like fee adjustments and tournament creation"
        (enforce-guard (keyset-ref-guard "free.blackjack-admin-keyset"))
    )

    (defcap WIN_RECORDER ()
        (enforce-guard (keyset-ref-guard "free.blackjack-win-recorder-keyset"))
    )

    (defcap ACCOUNT_GUARD (account:string)
        @doc "Verifies account meets format and belongs to caller"
        (with-read account-guards account 
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
    ; Utility Functions
    ; --------------------------------------------------------------------------
    (defun generate-game-id (game-type:string account:string)
        @doc "Generate a unique game ID using hash for better management"
        (hash (format "{}-{}-{}" [game-type account (curr-time)]))
    )

    (defun curr-time:time ()
      @doc "Returns current chain's block-time"
      (at 'block-time (chain-data))
    )

    ; --------------------------------------------------------------------------
    ; Events
    ; --------------------------------------------------------------------------
    (defun emit-game-created (game-id:string game-type:string player:string)
        @doc "Emit event when game is created"
        (emit-event (GAME_CREATED game-id game-type player))
    )

    (defun emit-game-joined (game-id:string player:string)
        @doc "Emit event when player joins game"
        (emit-event (GAME_JOINED game-id player))
    )

    (defun emit-game-completed (game-id:string winner:string)
        @doc "Emit event when game is completed"
        (emit-event (GAME_COMPLETED game-id winner))
    )

    (defcap GAME_CREATED (game-id:string game-type:string player:string)
        @doc "Event emitted when a game is created"
        @event
        true
    )

    (defcap GAME_JOINED (game-id:string player:string)
        @doc "Event emitted when a player joins a game"
        @event
        true
    )

    (defcap GAME_COMPLETED (game-id:string winner:string)
        @doc "Event emitted when a game is completed"
        @event
        true
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
        (set-pvc-fee 1.0)
        (set-pvp-fee 2.0)
        (set-tournament-fee 3.0)
      )
    )

    ; --------------------------------------------------------------------------
    ; Admin Functions
    ; --------------------------------------------------------------------------
    (defun set-pvc-fee(price:decimal)
        @doc "Allows the admin to set PVC game mode fee."
        (with-capability (ADMIN)
            (enforce (> price 0.0) "Price must be positive")
            (write pvc-fees-table "pvc" { 'fee: price })
        )
    )

    (defun set-pvp-fee(price:decimal)
        @doc "Allows the admin to set PVP game mode fee."
        (with-capability (ADMIN)
            (enforce (> price 0.0) "Price must be positive")
            (write pvp-fees-table "pvp" { 'fee: price })
        )
    )

    (defun set-tournament-fee(price:decimal)
        @doc "Allows the admin to set tournament game mode fee."
        (with-capability (ADMIN)
            (enforce (> price 0.0) "Price must be positive")
            (write tournament-fees-table "tournament" { 'fee: price })
        )
    )

    (defun create-tournament (id:string pool:decimal maxPlayers:integer startSignup:time endSignup:time startDate:time endDate:time)
        @doc "Allows the admin to create a tournament."
        (with-capability (ADMIN)
            (enforce (!= id "") "Tournament ID cannot be empty")
            (enforce (> pool 0.0) "Pool must be positive")
            (enforce (> maxPlayers 0) "Max players must be positive")
            (enforce (< startSignup endSignup) "Signup end must be after signup start")
            (enforce (< startDate endDate) "Tournament end must be after tournament start")
            (enforce (<= endSignup startDate) "Tournament must start after signup ends")
            
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
    (defun store-account-guard (account:string new-guard:guard)
      @doc "Store account guard for future use - validates current guard first"
      (require-capability (PRIVATE))
      
      (with-default-read account-guards account
          { 'guard: new-guard }  ;; 
          { 'guard := current-guard }
          
          (if (!= current-guard new-guard)
              (enforce-guard current-guard)
              true
          )
          
          (write account-guards account { 'guard: new-guard })
          (format "Guard stored successfully for account: {}" [account])
      )
    )

    ; --------------------------------------------------------------------------
    ; PVC Game Functions
    ; --------------------------------------------------------------------------
    (defun join-pvc-game (account:string guard:guard)
      @doc "Join a PVC game by paying the required fee. Returns the game ID."
      (with-capability (PRIVATE)
      (store-account-guard account guard)
      (with-capability (ACCOUNT_GUARD account)
        (let ((game-id (generate-game-id "pvc" account)))
          ; Deduct fee
          (coin.transfer account (get-BJ_BANK-account) (get-pvc-fee))

          ; Create game session
          (insert game-sessions game-id {
            'game-id: game-id,
            'game-type: "pvc",
            'players: [account],
            'status: "active",
            'winner: "",
            'tournament-id: "",
            'created-at: (curr-time),
            'completed-at: (time "1970-01-01T00:00:00Z")
          })

          ; Update player statistics
          (with-default-read player-table account { 'games-played: 0, 'wins: 0 }
              {'games-played:=games-played, 'wins:=wins}
            (write player-table account { 'games-played: (+ 1 games-played), 'wins: wins }))
          
          ; Emit game created event
          (emit-game-created game-id "pvc" account)
          
          ; Return the game ID
          game-id
        )
      ))
    )

    (defun record-pvc-win (game-id:string winner:string)
      @doc "Records a PVC win for a specific game."
      (with-capability (WIN_RECORDER)
        ; Verify game exists and is active
        (with-read game-sessions game-id
          { 'game-type:=gt, 'players:=players, 'status:=status }
          (enforce (= gt "pvc") "Game is not a PVC game")
          (enforce (= status "active") "Game is not active")
          (enforce (contains winner players) "Winner must be a participant in this game")

          ; Update game session
          (update game-sessions game-id {
            'status: "completed",
            'winner: winner,
            'completed-at: (curr-time)
          })

          ; Update player statistics
          (with-default-read player-table winner { 'games-played: 0, 'wins: 0 }
            {'wins:=wins}
            (update player-table winner { 'wins: (+ 1 wins) })
          )

          ; Distribute PVC winnings - Install capability within the contract
          (with-capability (BANK_TRANSFER)
            (let ((payout (* PVC_REWARD_PERC (get-pvc-fee))))
              (install-capability (coin.TRANSFER (get-BJ_BANK-account) winner payout))
              (coin.transfer (get-BJ_BANK-account) winner payout)
            )
          )

          ; Emit game completed event
          (emit-game-completed game-id winner)
        )
      )
    )

    ; --------------------------------------------------------------------------
    ; PVP Game Functions
    ; --------------------------------------------------------------------------
    (defun join-pvp-game (account:string guard:guard)
      @doc "Join or create a PVP game by paying the required fee. Returns the game ID."
      (with-capability (PRIVATE)
      (store-account-guard account guard)
      (with-capability (ACCOUNT_GUARD account)
        (let ((game-id (generate-game-id "pvp" account)))
          ; Deduct fee
          (coin.transfer account (get-BJ_BANK-account) (get-pvp-fee))

          ; Create game session (in real implementation, you might want to match with existing games)
          (insert game-sessions game-id {
            'game-id: game-id,
            'game-type: "pvp",
            'players: [account],
            'status: "active",
            'winner: "",
            'tournament-id: "",
            'created-at: (curr-time),
            'completed-at: (time "1970-01-01T00:00:00Z")
          })

          ; Update player statistics
          (with-default-read player-table account { 'games-played: 0, 'wins: 0 }
              {'games-played:=games-played, 'wins:=wins}
            (write player-table account { 'games-played: (+ 1 games-played), 'wins: wins }))
          
          ; Emit game created event
          (emit-game-created game-id "pvp" account)
          
          ; Return the game ID
          game-id
        )
      ))
    )

    (defun add-player-to-pvp-game (game-id:string account:string guard:guard)
        @doc "Add a second player to an existing PVP game."
        (with-capability (PRIVATE)
        (store-account-guard account guard)
        (with-capability (ACCOUNT_GUARD account)
        ; Verify game exists and can accept another player
        (with-read game-sessions game-id
            { 'game-type:=gt, 'players:=players, 'status:=status }
            (enforce (= gt "pvp") "Game is not a PVP game")
            (enforce (= status "active") "Game is not active")
            (enforce (< (length players) 2) "Game is already full")
            (enforce (not (contains account players)) "Player already in this game")

            ; Deduct fee
            (coin.transfer account (get-BJ_BANK-account) (get-pvp-fee))

            ; Add player to game
            (update game-sessions game-id {
            'players: (+ players [account])
            })

            ; Update player statistics
            (with-default-read player-table account { 'games-played: 0, 'wins: 0 }
                {'games-played:=games-played, 'wins:=wins}
            (write player-table account { 'games-played: (+ 1 games-played), 'wins: wins }))

            ; Emit game joined event
            (emit-game-joined game-id account)
        )))
    )

    (defun record-pvp-win (game-id:string winner:string)
      @doc "Records a PVP win for a specific game and distributes winnings."
      (with-capability (WIN_RECORDER)
        ; Verify game exists and is active
        (with-read game-sessions game-id
          { 'game-type:=gt, 'players:=players, 'status:=status }
          (enforce (= gt "pvp") "Game is not a PVP game")
          (enforce (= status "active") "Game is not active")
          (enforce (contains winner players) "Winner must be a participant in this game")

          ; Update game session
          (update game-sessions game-id {
            'status: "completed",
            'winner: winner,
            'completed-at: (curr-time)
          })

          ; Update player statistics
          (with-default-read player-table winner { 'games-played: 0, 'wins: 0 }
            {'wins:=wins}
            (update player-table winner { 'wins: (+ 1 wins) })
          )

          ; Distribute PVP winnings - Install capability within the contract
          (with-capability (BANK_TRANSFER)
            (let ((payout (* PVP_REWARD_PERC (* 2.0 (get-pvp-fee)))))
              (install-capability (coin.TRANSFER (get-BJ_BANK-account) winner payout))
              (coin.transfer (get-BJ_BANK-account) winner payout)
            )
          )

          ; Emit game completed event
          (emit-game-completed game-id winner)
        )
      )
    )

    ; --------------------------------------------------------------------------
    ; Tournament Functions
    ; --------------------------------------------------------------------------
    (defun register-for-tournament (tournamentId:string account:string guard:guard)
      @doc "Pay to register for a tournament."
      (with-capability (PRIVATE)
      (store-account-guard account guard)
      (with-capability (ACCOUNT_GUARD account)
        (with-read tournament-table tournamentId
            { 'startSignup:=ss, 'endSignup:=es, 'maxPlayers:=mp, 'players:=players }
            (let ((current-time (curr-time))
                  (current-player-count (length players)))
                (enforce (and (>= current-time ss) (<= current-time es)) "Signup period is closed.")
                (enforce (< current-player-count mp) "Maximum player count reached.")
                (enforce (not (contains account players)) "Player already registered.")

                ; Deduct fee and transfer to bank
                (coin.transfer account (get-BJ_BANK-account) (get-tournament-fee))

                ; Register player
                (update tournament-table tournamentId {
                    'players: (+ players [account])
                })
            )
        )
      ))
    )

    (defun join-tournament-game (account:string tournamentId:string)
      @doc "Join a tournament game (must be registered and tournament active). Returns the game ID."
      (with-capability (ACCOUNT_GUARD account)
        (with-read tournament-table tournamentId
            { 'startDate:=sd, 'endDate:=ed, 'players:=players }
          ; Check if the tournament is active
          (enforce (and (>= (curr-time) sd) (<= (curr-time) ed))
                  "Tournament is not active.")
          ; Check if the player is registered for the tournament
          (enforce (contains account players)
                  "Player is not registered for the tournament.")

          (let ((game-id (generate-game-id "tournament" account)))
            ; Create tournament game session
            (insert game-sessions game-id {
              'game-id: game-id,
              'game-type: "tournament",
              'players: [account],
              'status: "active",
              'winner: "",
              'tournament-id: tournamentId,
              'created-at: (curr-time),
              'completed-at: (time "1970-01-01T00:00:00Z")
            })

            ; Update player statistics
            (with-default-read player-table account { 'games-played: 0, 'wins: 0 }
                {'games-played:=games-played, 'wins:=wins}
              (write player-table account { 'games-played: (+ 1 games-played), 'wins: wins }))
            
            ; Emit game created event
            (emit-game-created game-id "tournament" account)
            
            ; Return the game ID
            game-id
          )
        )
      )
    )

    (defun record-tournament-win (game-id:string winner:string)
      @doc "Records a tournament win for a specific game and distributes winnings."
      (with-capability (WIN_RECORDER)
        ; Verify game exists and is active
        (with-read game-sessions game-id
          { 'game-type:=gt, 'players:=players, 'status:=status, 'tournament-id:=tid }
          (enforce (= gt "tournament") "Game is not a tournament game")
          (enforce (= status "active") "Game is not active")
          (enforce (contains winner players) "Winner must be a participant in this game")

          ; Update game session
          (update game-sessions game-id {
            'status: "completed",
            'winner: winner,
            'completed-at: (curr-time)
          })

          ; Update player statistics
          (with-default-read player-table winner { 'games-played: 0, 'wins: 0 }
            {'wins:=wins}
            (update player-table winner { 'wins: (+ 1 wins) })
          )

          ; Distribute tournament winnings - Install capability within the contract
          (with-capability (BANK_TRANSFER)
            (with-read tournament-table tid { 'pool:=pool }
              (let ((payout (* TOURNAMENT_REWARD_PERC pool)))
                (install-capability (coin.TRANSFER (get-BJ_BANK-account) winner payout))
                (coin.transfer (get-BJ_BANK-account) winner payout)
              )
            )
          )

          ; Emit game completed event
          (emit-game-completed game-id winner)
        )
      )
    )

    ; --------------------------------------------------------------------------
    ; Get Information
    ; --------------------------------------------------------------------------
    (defun get-pvc-fee ()
        @doc "Returns the PVC game fee."
        (at "fee" (read pvc-fees-table "pvc"))
    )

    (defun get-pvp-fee ()
        @doc "Returns the PVP game fee."
        (at "fee" (read pvp-fees-table "pvp"))
    )

    (defun get-tournament-fee ()
        @doc "Returns the tournament game fee."
        (at "fee" (read tournament-fees-table "tournament"))
    )

    (defun get-all-fees ()
        @doc "Returns all game fees."
        {
          "pvc": (get-pvc-fee),
          "pvp": (get-pvp-fee),
          "tournament": (get-tournament-fee)
        }
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

    (defun get-game-session (game-id:string)
        @doc "Returns the details of a specific game session."
        (read game-sessions game-id)
    )

    (defun get-player-games (account:string)
        @doc "Returns all game sessions for a specific player."
        (select game-sessions (where "players" (contains account)))
    )

    (defun get-active-games ()
        @doc "Returns all active game sessions."
        (select game-sessions (where "status" (= "active")))
    )

    (defun get-completed-games ()
        @doc "Returns all completed game sessions."
        (select game-sessions (where "status" (= "completed")))
    )
)

(create-table player-table)
(create-table leaderboard)
(create-table tournament-table)
(create-table pvc-fees-table)
(create-table pvp-fees-table)
(create-table tournament-fees-table)
(create-table game-records)
(create-table account-guards)
(create-table game-sessions)
(init)