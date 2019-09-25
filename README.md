
Note: For the structure of resources and resource locations, we can use 
      urls. This allows for both http:// and file:// locations, so seems
      ideal for my use case.

hs-entity-server
================

In this project, I am building the entity microservice server for bedelibry. The purpose of this server is to:

  1. Generate unique compact timestamps.
       GET http://server/timestamp
  2. Adding a new entity.
       POST http://server/entities
       data: ...
  3. Retrieving data about (data) entities
       GET http://server/entities/<PRIMARY_ID>/<FIELD_NAME>
       >>> Makes a request to the SQL server to fetch the field
           of the entity in question.
  4. Make queries to the server.
       GET http://server/request?args=...
       data: ...
     1. "Is a data entity referenced in the server?"
     2. "Is an entity referenced in the server?"
     -- Note: This is useful for making assertions which make new entities in bli-prolog.
  5. Adding a new alias to an entity.
       PATCH http://server/entities/<PRIMARY_ID>
       data: id: this_is_a_new_identifier
       
       response:
           The server should respond with an error if the name already exists, along with
           which primaryId that alias belongs to.
           otherwise, return a success code, and update the entity with the new alias.

In addition, to interface with the possible relations that a server can ask about, we can also make the following requests:

  6. Get all schema data stored by the server
      GET http://server/schema
  7. Get a specific sub-schema
      GET http://server/schema/<name_of_subschema>
  8. Add a new relation to the schema stored by the server (stored in a misc. schema)
      PATCH http://server/schema
      data: ...
        side effect: Creates a new table of the appropriate name in the configured
                     sql database used to store the relational facts.
      
  9. Add a new relation to a specific sub-schema of the server.
      PATCH http://server/schema/<name_of_subschema>
        (data + side effects similar to the above)
  10. Make a query which can be handled by the SQL backend.
       GET http://server/sql_query
       data: ???
    * For instance, using select statements, we can make queries of the form
       pred(X,a,b,c), pred(a,X,b,c), pred(X,a,Y,c), etc... and we can integrate these
       into our search procedure.

Note: There are two types of entities:

  1. A data entity, which is a static resource that is not liable to change.
  2. An object entity, like a person, for instance, which *is* liable to change.

The first has a primary_id which is determined by its content, whereas the second is
determined entirely by it's primary_id. However, either kind of entity can have
a set of other ids (aliases) assosciated with them.

Collision handling:
-------------------

One important feature of the entity microservice is collision handling of identifiers.
In the off-chance of an MD5 collision, the entity server should prepend alphanumeric
digits in the order 1, 2, ..., 9, a, b, c, ..., z, A, B, C, ..., Z to the id of an entity
until collisions no longer occur. 

Approach for generating primaryIds for data entities
----------------------------------------------------

In our architecture, data entities are stored in an SQL database. And thus, they have a name,
as well as a number of different fields, where each field has a name. The name of each of the fields
is either auto-generated by the database, or a meaningful name. For instance:

table quotes_by_wittgenstein_i_like:

| primaryId | content
----------------------------------------------------------------------------- |
 "adsfalkf" | "This is a quote by wittgenstein that I found very intriguing." |
-----------------------------------------------------------------------------
 ...

Note: For the sake of later flexibility in design, we also store the primaryId of 
      data entities in the relevant SQL tables. This should all be stripped in the
      process that I describe here.

Each row of such a database should be parsed into JSON (or really, probably some form of BSON)
representation, for instance:

{
 "table": "quotes_by_wittgenstein_that_i_like",
 "content": "This is a quote by wittgenstein that I found very intriguing."
}

and this representation is then hashed. For example, if we run:

./hexToInt '{"table": "quotes_by_wittgenstein_that_i_like","content":"This is a quote by wittgenstein that I found very intriguing."}'
>>> 6EGbxaa45JFBnnBxtO4sUV

Whereas, if we change the table name, for example, we get a different result:
./hexToInt '{"table": "quotes","content":"This is a quote by wittgenstein that I found very intriguing."}'
>>> 6qMVBCw0EWZQGuK4fchvsF

So, in other words, a quote by Wittgenstein *as a generic quotation*, is different from a quote by Wittgenstein
as a *quotation by Wittgenstein*.

Note: (this is just an interesting design idea): Associated with each table is a set of Prolog rules. For example:

    % quote_by_wittgenstein_that_i_like.meta.pl
    
    quote(PID) :- 
       quote_by_wittgenstein_that_i_like(PID, X).
    author(PID, wittgenstein) :-
       quote_by_wittgenstein_that_i_like(PID, X).
    content(PID, X) :-
       quote_by_wittgenstein_that_i_like(PID, X).
    likes(nate, PID) :-
       quote_by_wittgenstein_that_i_like(PID, X).

However, this raises some difficulties. Namely, if we also want to have a general database of 
quotations, then our first rule doesn't make sense, because the automatically generated PID
of the "quote" version will be different from the PID of the "quote_by_wittgenstein_that_i_like"
version. 

To deal with this, we might have two different types of IDs: A "raw" ID, which only deals with the pure content
of a table (i.e. doesn't take into account table name), and a "categorical" ID, which does take this into account.
However, I'm not sure how good of an idea this is. 

Note: One potential use for the rule file would be to add values to different tables on insert 
      to the specified table. Is such a thing possible with native SQL? -- Yes, these are called
      *triggers* in the database world.

Note: One important property to keep in mind when structuring our knowledge representation model is 
      that of *completeness*. Namely, if a query can be answered in our system, it will be.
      (compare, e.x. breadth first search and depth first search strategies). 

      It would be useful in benchmarking our application to allow for different *stratigies* that we can
      enable and disable at will. For instance, we could choose either a
          STORE_META_IN_TABLES
      strategy for our database fact processing, or a 
          PROCESS_META_PL
      strategy. So, e.x. we have:
         data DB_TRIGGER_STRATEGY = STORE_META_IN_TABLES 
                                  | PROCESS_META_PL
      each of these strategies should ensure completeness, but there may be differences in
      efficency.
      
Note: To make the query language more like natural language syntax (and closer to the idea of the lambek calculus),
we could add postfix operators to the language, e.x.

quote(X) that(likes(i, X)).
        ~~~~>
that(quote(X), likes(i, X))

(although, at this moment, I'm not entirely sure what problem this would solve.)

Multi-user features
-------------------

Note that it may be possible to have multiple users of bedelibry who can communicate with eachother. 

For instance, a user can ask if another user has data about some primaryID using the following syntax:

(Note: This syntax is depreciated by most modern browers. But we would still use the underlying
authentication architecture here)

GET https://<userEntityID>:<passwordToExternalServer>@external.server/entities/<primaryID>

and if the user of this external bedelibry server has configured their entity server to allow
outside access for this particular request by this particular user, and if they have configured
a password, then this is a valid password, the request will go through. Otherwise, an authorization
failure is returned.

API Documentation
-----------------

See [API doc](api.md).

Todo
----

See [todo list](todo.md).