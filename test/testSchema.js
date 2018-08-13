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

	input AuthorSearch {
		id: ID
		firstName: String
	}

  type Query {
    author(search: AuthorSearch!): Author
  }
`;

module.exports = buildSchema(sdlSchema);
