
Todo
====

  * Implement a trie store of entityIDs
    * Note that the trie leaves should store the type of the entity.
      And thus, the entity server should also store (at least) a list of
      types. 
    * Perhaps another server should store the relational information.
      So between these two services is a complete schema. 
    * Although, beyond just wanting to structure this as a microservice
      architecture, I'm having a hard time justifying why this needs to
      be in a seperate server. For now, I can probably keep these
      in the same place, and seperate them if I need to.
  * Implement collision handling of data entity IDs
  * Look at the hs_microservice code, and see what needs to be scrapped/modified.