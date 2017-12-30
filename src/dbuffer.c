#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "dbuffer.h"

void dbuffer_init_with_capacity(dbuffer* db, ureg capacity)
{
	assert(capacity >= 8);  // we do this so we don't have to bounds check in
							// dbuffer_make_small_space
	db->start = malloc(capacity);
	db->end = db->start + capacity;
	db->head = db->start;
}

void dbuffer_init(dbuffer* db)
{
	dbuffer_init_with_capacity(db, sizeof(ureg) * 4);
}
bool dbuffer_is_emtpy(dbuffer* db)
{
	return (db->start == db->head);
}
void dbuffer_free(dbuffer* db)
{
	free(db->start);
}
ureg dbuffer_get_size(dbuffer* db)
{
	return db->head - db->start;
}

ureg dbuffer_get_capacity(dbuffer* db)
{
	return db->end - db->start;
}

void dbuffer_set_capacity(dbuffer* db, ureg capacity)
{
	// set bigger capacity can be used to set smaller capacities, but it doesn't
	// do a bounds check.
	dbuffer_set_bigger_capacity(db, capacity);
	if (db->head > db->end) db->head = db->end;
}

void dbuffer_set_bigger_capacity(dbuffer* db, ureg capacity)
{
	uint8_t* temp = realloc(db->start, capacity);
	assert(temp);
	db->head = temp + (db->head - db->start);
	db->end = temp + capacity;
	db->start = temp;
	//printf("growing capacity to %llu\n", dbuffer_get_capacity(db));
}

void dbuffer_grow(dbuffer* db)
{
	// there are currently no optimizations I can think of here so we use
	// default
	dbuffer_set_bigger_capacity(db, (db->end - db->start) * 2);
}


int dbuffer_has_space(dbuffer* db, ureg required_space)
{
	return (db->head + required_space < db->end);
}

void dbuffer_make_space(dbuffer* db, ureg space)
{
	if (db->head + space > db->end)
	{
		ureg capacity = dbuffer_get_capacity(db);
		if (capacity > space)
		{
			capacity = capacity * 2;
		}
		else
		{
			capacity = dbuffer_get_size(db) + space;
		}
		dbuffer_set_bigger_capacity(db, capacity);
	}
}

void dbuffer_make_small_space(dbuffer* db, ureg space)
{
	if (db->head + space > db->end) dbuffer_grow(db);
}
void* dbuffer_claim_space(dbuffer* db, ureg space)
{
	dbuffer_make_space(db, space);
	void* r = db->head;
	db->head += space;
	return r;
}
void* dbuffer_claim_small_space(dbuffer* db, ureg space)
{
	dbuffer_make_small_space(db, space);
	void* r = db->head;
	db->head += space;
	return r;
}
void dbuffer_pop_back(dbuffer* db, ureg size)
{
	db->head -= size;
}

void dbuffer_remove_at(dbuffer* db, void* pos, ureg size)
{
	memmove(pos, (u8*)pos + size, db->head - (u8*)pos - size);
	db->head -= size;
}
void dbuffer_insert_at(dbuffer* db, const void* data, void* pos, ureg size)
{
	//this is practically a reimplementation of make_space which is unfortunate,
	// but we need to know if we need to reallocate before we do
	if (db->head + size > db->end)
	{
		ureg offs = (u8*)pos - db->start;
		ureg capacity = dbuffer_get_capacity(db);
		if(size<capacity)
		{
			capacity += size;
		}
		else
		{
			capacity += capacity;
		}
		dbuffer_set_bigger_capacity(db,capacity);
		pos = db->start + offs;
	}
	memmove((u8*)pos + size, pos, db->head - (u8*)pos);
	memcpy(pos, data, size);
	db->head += size;
}


void dbuffer_swap(dbuffer* db, void* posa, void* posb, ureg size)
{
	u8* a = posa;
	u8* b = posb;
	if (size < db->end - db->head)
	{
		// if we have a big enough temp buffer after head anyway, use it
		memcpy(db->head, b, size);
		memcpy(b, a, size);
		memcpy(a, db->head, size);
	}
	else
	{
	// otherwise do manual swapping
		// if both pos's are aligned on a sizeof(ureg) boundary
		if (!(((ureg)a) & (sizeof(ureg) - 1)) &&
			!(((ureg)b) & sizeof(ureg) - 1))
		{
			void* aend = a + size - size % sizeof(ureg);
			ureg temp;
			while (a != aend)
			{
				temp = *(ureg*)a;
				*(ureg*)a = *(ureg*)b;
				*(ureg*)b = temp;
				a += sizeof(ureg);
				b += sizeof(ureg);
			}
		}
		void* aend = a + size;
		uint8_t temp;
		while (a != aend)
		{
			temp = *a;
			*a = *b;
			*b = temp;
			a++;
			b++;
		}
	}
}

void dbuffer_push_back_memory(dbuffer* db, const void* data, ureg size)
{
	dbuffer_make_space(db, size);
	memcpy(db->head, data, size);
	db->head += size;
}

void dbuffer_get(dbuffer* db, void* target, void* pos, ureg size)
{
	memcpy(target, pos, size);
}
void dbuffer_get_from_idx(dbuffer* db, void* target, ureg pos, ureg size)
{
	memcpy(target, db->start + pos, size);
}
