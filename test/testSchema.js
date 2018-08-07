const {buildSchema} = require('graphql');

const sdlSchema = `
	enum PostStatus {
		DRAFT
		PUBLISHED
	}
	type Post {
		title: String!
		author: Author
		status: PostStatus!
	}
  type Author {
    firstName: String
    lastName: String
		age: Int!
		posts: [Post!]!
  }
  type Query {
    author(id: Int!): Author
  }
`;

module.exports = buildSchema(sdlSchema);
