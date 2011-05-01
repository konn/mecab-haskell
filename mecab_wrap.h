#include <mecab.h>

const mecab_node_t *mecab(char *input);

mecab_node_t *next_node(const mecab_node_t *node);
mecab_node_t *prev_node(const mecab_node_t *node);
mecab_node_t *enext_node(const mecab_node_t *node);
mecab_node_t *bnext_node(const mecab_node_t *node);
const char *surface(const mecab_node_t *node);
const char *feature(const mecab_node_t *node);
unsigned int length(const mecab_node_t *node);
unsigned int rlength(const mecab_node_t *node);
unsigned int id(const mecab_node_t *node);
unsigned short rcAttr(const mecab_node_t *node);
unsigned short lcAttr(const mecab_node_t *node);
unsigned short posid(const mecab_node_t *node);
unsigned char char_type(const mecab_node_t *node);
unsigned char stat(const mecab_node_t *node);
unsigned char isbest(const mecab_node_t *node);
float alpha(const mecab_node_t *node);
float beta(const mecab_node_t *node);
float prob(const mecab_node_t *node);
short wcost(const mecab_node_t *node);
long cost(const mecab_node_t *node);
