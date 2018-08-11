open Jest;

open GQLTypes;

describe("Encode/Decode", () => {
  open Expect;
  test("Post with null field", () => {
    let post: post = {
      id: "abc123",
      title: Some("My Post"),
      author: Some(Js.null),
      status: Some(`Published),
    };
    let json = post |> toNode |> encodeNode(Post);
    let decodedPost: post = json |> decodeType(Post);
    expect(Js.Option.getExn(decodedPost.title)) |> toBe("My Post");
  });
  test("Author with Posts", () => {
    let author: author = {
      id: "abc123",
      firstName: Some(Js.Null.return("Tom")),
      lastName: Some(Js.Null.return("Clancy")),
      age: Some(99),
      posts: None,
    };
    let post: post = {
      id: "abc123",
      title: Some("My Post"),
      author: Some(Js.Null.return(author)),
      status: Some(`Draft),
    };
    let authorWithPost = {...author, posts: Some([post])};
    let json = authorWithPost |> toNode |> encodeNode(Author);
    let decodedAuthor: author = json |> decodeType(Author);
    let hasPost =
      decodedAuthor.posts
      |> Js.Option.getExn
      |> List.exists(p => p.title |> Js.Option.getExn === "My Post");
    expect(hasPost) |> toBe(true);
  });
});
