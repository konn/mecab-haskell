#include <mecab.h>
#include "mecab_wrap.h"

const mecab_node_t *mecab(char *input)
{
  mecab_t *mecab;
  mecab_node_t *node;
  const mecab_node_t *result;
  char *argv[] = {"-l1"};

  mecab = mecab_new(1, argv);
  result = mecab_sparse_tonode(mecab, input);
  mecab_destroy(mecab);
  return result;
}

mecab_node_t *next_node(const mecab_node_t *node)
{
  return node->next;
}

mecab_node_t *prev_node(const mecab_node_t *node)
{
  return node->prev;
}

mecab_node_t *enext_node(const mecab_node_t *node)
{
  return node->enext;
}

mecab_node_t *bnext_node(const mecab_node_t *node)
{
  return node->bnext;
}

const char *surface(const mecab_node_t *node)
{
  return node->surface;
}

const char *feature(const mecab_node_t *node)
{
  return node->feature;
}

unsigned int length(const mecab_node_t *node)
{
  return node->length;
}

unsigned int rlength(const mecab_node_t *node)
{
  return node->rlength;
}

unsigned int id(const mecab_node_t *node)
{
  return node->id;
}

unsigned short rcAttr(const mecab_node_t *node)
{
  return node->rcAttr;
}

unsigned short lcAttr(const mecab_node_t *node)
{
  return node->lcAttr;
}

unsigned short posid(const mecab_node_t *node)
{
  return node->posid;
}

unsigned char char_type(const mecab_node_t *node)
{
  return node->char_type;
}

unsigned char stat(const mecab_node_t *node)
{
  return node->stat;
}

unsigned char isbest(const mecab_node_t *node)
{
  return node->isbest;
}

float alpha(const mecab_node_t *node)
{
  return node->alpha;
}

float beta(const mecab_node_t *node)
{
  return node->beta;
}

float prob(const mecab_node_t *node)
{
  return node->prob;
}

short wcost(const mecab_node_t *node)
{
  return node->wcost;
}

long cost(const mecab_node_t *node)
{
  return node->cost;
}
