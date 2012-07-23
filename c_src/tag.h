#include <stdint.h>

#define TABLE_SIZE 64
#define SUCCESS                  0
#define ERROR_TAG_HAS_WRONG_TYPE 1
#define ERROR_TAG_NOT_FOUND      2
#define ERROR_GROUP_WRONG_INDEX  3

enum TagType
{
   TagValue = 1,
   TagGroup = 2
};

typedef struct TagTable_* TagTable;

typedef struct Tag_
{
   uint32_t num;
   enum TagType type;
   union
   {
      struct
      {
         unsigned char* data;
         uint32_t len;
      } value;
      struct
      {
         uint32_t size;
         TagTable* grpTbl;
      } groups;
   };
   struct Tag_* next;
} Tag;

TagTable new_table();
void free_table(TagTable tbl);

int set_tag(TagTable tbl, uint32_t tagNum, unsigned char const* data, uint32_t len);
int get_tag(TagTable tbl, uint32_t tagNum, Tag** tag);
int rm_tag(TagTable tbl, uint32_t tagNum);

int add_group(TagTable tbl, uint32_t tagNum, TagTable* grpTbl);
int get_grp(TagTable tbl, uint32_t tagNum, uint32_t grpIdx, TagTable* grpTbl);
int rm_grp(TagTable tbl, uint32_t tagNum, uint32_t grpIdx);