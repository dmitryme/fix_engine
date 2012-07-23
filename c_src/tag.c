#include "tag.h"

#include <stdlib.h>
#include <string.h>

struct TagTable_
{
   Tag* tags[TABLE_SIZE];
};

TagTable new_table()
{
   return (TagTable)calloc(1, sizeof(struct TagTable_));
}

Tag* free_tag(Tag* tag)
{
   Tag* next = tag->next;
   if (tag->type == TagValue)
   {
      free(tag->value.data);
      free(tag);
   }
   else if (tag->type == TagGroup)
   {
      free_table(*tag->groups.grpTbl);
      free(tag->groups.grpTbl);
      free(tag);
   }
   return next;
}

void free_table(TagTable tbl)
{
   for(int i = 0; i < TABLE_SIZE; ++i)
   {
      Tag* tag = tbl->tags[i];
      while(tag)
      {
         tag = free_tag(tag);
      }
   }
   free(tbl);
}

int set_tag(TagTable tbl, uint32_t tagNum, unsigned char const* data, uint32_t len)
{
   Tag* tag = NULL;
   int ret = get_tag(tbl, tagNum, &tag);
   if (ret && ret != ERROR_TAG_NOT_FOUND)
   {
      return ret;
   }
   if (!tag)
   {
      int idx = tagNum % TABLE_SIZE;
      tag = malloc(sizeof(Tag));
      tag->type = TagValue;
      tag->next = tbl->tags[idx];
      tag->num = tagNum;
      tbl->tags[idx] = tag;
   }
   if (tag->value.data)
   {
      free(tag->value.data);
   }
   tag->value.data = (unsigned char*)malloc(len);
   tag->value.len = len;
   memcpy(tag->value.data, data, len);
   return SUCCESS;
}

int get_tag(TagTable tbl, uint32_t tagNum, Tag** tag)
{
   uint32_t const idx = tagNum % TABLE_SIZE;
   Tag* it = tbl->tags[idx];
   while(it)
   {
      if (it->num == tagNum)
      {
         if (it->type != TagValue)
         {
            return ERROR_TAG_HAS_WRONG_TYPE;
         }
         if (tag)
         {
            *tag = it;
         }
         return SUCCESS;
      }
      else
      {
         it = it->next;
      }
   }
   return ERROR_TAG_NOT_FOUND;
}

int rm_tag(TagTable tbl, uint32_t tagNum)
{
   uint32_t const idx = tagNum % TABLE_SIZE;
   Tag* tag = tbl->tags[idx];
   Tag* prev = tag;
   while(tag)
   {
      if (tag->num == tagNum)
      {
         if (prev == tag)
         {
            free_tag(tag);
            tbl->tags[idx] = NULL;
         }
         else
         {
            prev->next = free_tag(tag);
         }
         return SUCCESS;
      }
      if (prev == tag)
      {
         tag = tag->next;
      }
      else
      {
         prev = tag;
         tag = tag->next;
      }
   }
   return ERROR_TAG_NOT_FOUND;
}

int add_group(TagTable tbl, uint32_t tagNum, TagTable* grpTbl)
{
   Tag* tag = NULL;
   get_tag(tbl, tagNum, &tag);
   if (tag && tag->type != TagGroup)
   {
      return ERROR_TAG_HAS_WRONG_TYPE;
   }
   if (!tag)
   {
      uint32_t const idx = tagNum % TABLE_SIZE;
      tag = calloc(1, sizeof(Tag));
      tag->type = TagGroup;
      tag->num = tagNum;
      tag->next = tbl->tags[idx];
      tbl->tags[idx] = tag;
   }
   TagTable* groups = tag->groups.grpTbl;
   tag->groups.grpTbl = (TagTable*)calloc(tag->groups.size + 1, sizeof(TagTable));
   memcpy(tag->groups.grpTbl, groups, tag->groups.size);
   tag->groups.grpTbl[tag->groups.size] = new_table();
   *grpTbl = tag->groups.grpTbl[tag->groups.size];
   tag->groups.size += 1;
   return SUCCESS;
}

int get_grp(TagTable tbl, uint32_t tagNum, uint32_t grpIdx, TagTable* grpTbl)
{
   int const idx = tagNum % TABLE_SIZE;
   Tag* it = tbl->tags[idx];
   while(it)
   {
      if (it->num == tagNum)
      {
         if (it->type != TagGroup)
         {
            return ERROR_TAG_HAS_WRONG_TYPE;
         }
         if (grpIdx >= it->groups.size)
         {
            return ERROR_GROUP_WRONG_INDEX;
         }
         *grpTbl = it->groups.grpTbl[grpIdx];
         return SUCCESS;
      }
      else
      {
         it = it->next;
      }
   }
   return ERROR_TAG_NOT_FOUND;
}

int rm_grp(TagTable tbl, uint32_t tagNum, uint32_t grpIdx)
{
   Tag* tag = NULL;
   int res = get_tag(tbl, tagNum, &tag);
   if (res)
   {
      return res;
   }
   if (tag->type != TagGroup)
   {
      return ERROR_TAG_HAS_WRONG_TYPE;
   }
   free_table(tag->groups.grpTbl[grpIdx]);
   tag->groups.size =-1;
   if (tag->groups.size == grpIdx)
   {
      tag->groups.grpTbl[tag->groups.size] = NULL;
   }
   else
   {
      memcpy(&tag->groups.grpTbl[grpIdx], tag->groups.grpTbl[grpIdx + 1], tag->groups.size - grpIdx);
      tag->groups.grpTbl[tag->groups.size] = NULL;
   }
   return SUCCESS;
}