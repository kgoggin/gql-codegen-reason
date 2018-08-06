const processor = (context, documents, opt) => {
  return [
    {
      filename: 'test.json',
      content: JSON.stringify(context),
    },
  ];
};

module.exports = processor;
