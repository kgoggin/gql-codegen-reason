type query = {author: option(Js.null(author))}
and author = {
  firstName: option(Js.null(string)),
  lastName: option(Js.null(string)),
  age: option(int),
  posts: option(list(post)),
}
and post = {
  title: option(string),
  author: option(Js.null(author)),
};
