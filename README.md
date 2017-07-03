## Installation

* [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/#installupgrade)
* `stack build yesod-bin cabal-install --install-ghc`
* Execute the file via command line `./Item.hs` or `stack runghc ./Item.hs`. Note that the first run will
take a while, as Stack will build the project.
* Navigate to http://localhost:3000/


## RESTful

http://localhost:3000/api/v1.0/items?_accept=application/json
