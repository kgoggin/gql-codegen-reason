const {buildSchema} = require('graphql');

const sdlSchema = `
	type Post {
		title: String!
		author: Author
	}
  type Author {
    firstName: String
    lastName: String
		age: Int!
		posts: [Post]
  }
  type Query {
    author(id: Int!): Author
  }
`;

module.exports = buildSchema(sdlSchema);
