type query = {author: option(Js.null(author))}
and author = {
  firstName: option(Js.null(string)),
  lastName: option(Js.null(string)),
  age: option(int),
  posts: option(list(Js.null(post))),
}
and post = {
  title: option(string),
  author: option(Js.null(author)),
};

type gqlType =
  | Query
  | Author
  | Post;

let rec decode: (gqlType, Js.Json.t) => node =
  (t, json) =>
    switch (t) {
    | Query =>
      Json.Decode.{
        author:
          json
          |> optionalNullableField("author", json =>
               decode(Author, json) |> fromNode
             ),
      }
      |> toNode
    | Author =>
      Json.Decode.{
        firstName:
          json |> optionalNullableField("firstName", Json.Decode.string),
        lastName:
          json |> optionalNullableField("lastName", Json.Decode.string),
        age: json |> optionalField("age", Json.Decode.int),
        posts:
          json
          |> optionalNullableField(
               "posts",
               list(json => decode(Post, json) |> fromNode),
             ),
      }
      |> toNode
    | Post =>
      Json.Decode.{
        title: json |> optionalField("title", Json.Decode.string),
        author:
          json
          |> optionalNullableField("author", json =>
               decode(Author, json) |> fromNode
             ),
      }
      |> toNode
    };

let rec encodeNode = (gqlType, record: node) =>
  switch (gqlType) {
  | Query =>
    let r: query = record |> fromNode;
    Json.Encode(
      [("author", optionalNullable(encodeNode(Author), r.author))]
      |> addTypeName("Query")
      |> object_,
    );
  | Author =>
    let r: author = record |> fromNode;
    Json.Encode(
      [
        ("firstName", nullable(Json.Encode.string, r.firstName)),
        ("lastName", nullable(Json.Encode.string, r.lastName)),
        ("age", nullable(Json.Encode.int, r.age)),
        ("posts", optionalNodeList(encodeNode(Post), r.posts)),
      ]
      |> addTypeName("Author")
      |> object_,
    );
  | Post =>
    let r: post = record |> fromNode;
    Json.Encode(
      [
        ("title", nullable(Json.Encode.string, r.title)),
        ("author", optionalNullable(encodeNode(Author), r.author)),
      ]
      |> addTypeName("Post")
      |> object_,
    );
  };
