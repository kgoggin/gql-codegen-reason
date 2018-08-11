const {buildSchema} = require('graphql');

const sdlSchema = `
	enum PostStatus {
		DRAFT
		PUBLISHED
	}
	type Post {
		id: ID!
		title: String!
		author: Author
		status: PostStatus!
	}
  type Author {
		id: ID!
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
