// Copyright Epic Games, Inc. All Rights Reserved.
// This source file is licensed solely to users who have
// accepted a valid Unreal Engine license agreement 
// (see e.g., https://www.unrealengine.com/eula), and use
// of this source file is governed by such agreement.


#define NUM_INLINE_CHILDREN	3

struct Children // empty base class
{
};

#pragma pack(push)
#pragma pack(4) // because of the Children* pointer, which otherwise wants 8-byte alignment and pads SuffixNode

struct SuffixNode
{
	int32 pos;
	int32 parent;
	int32 follow;
	int32 depth;

	union
	{
		int32 nodes[NUM_INLINE_CHILDREN]; // inline child node indices
		Children *children;
	};
	uint8 numChildrenMinusOne;
	uint8 iselect[NUM_INLINE_CHILDREN]; // inline selects
};

#pragma pack(pop)

// capacities for ChildrenSparseN
#define CAPACITY1	8
#define CAPACITY2	16

template <int t_num>
struct ChildrenSparseN : public Children
{
	uint8 select[t_num];
	int32 nodes[ t_num];
};

template <int t_num>
void ChildrenSparse_Sort( ChildrenSparseN<t_num> * children, int count )
{
	RR_ASSERT( count <= t_num );
	// simple/slow N^2 insertion sort for now
	for(int i=0;i<count;i++)
	{
		for(int j=i+1;j<count;j++)
		{
			if ( children->select[j] < children->select[i] )
			{
				swap(children->select[i],children->select[j]);
				swap(children->nodes[i],children->nodes[j]);
			}
		}
	}
}

template <int t_num>
int32 const * ChildrenSparse_Find_Simple( ChildrenSparseN<t_num> const * children, int count, uint8 select )
{
	RR_ASSERT( count <= t_num );
	for(int i=0;i<count;i++)
	{
		if ( children->select[i] == select )
		{
			return &(children->nodes[i]);
		}
	}
	return NULL;
}

#ifdef __RADSSE2__

template <int t_num>
RADFORCEINLINE void ChildrenSparse_PreInit( ChildrenSparseN<t_num> * node )
{
	// Zero-clear to make sure all select values are defined
	if ( t_num == 16 )
		_mm_storeu_si128((__m128i *)node->select, _mm_setzero_si128());
	else
		_mm_storel_epi64((__m128i *)node->select, _mm_setzero_si128());
}

template <int t_num>
RADFORCEINLINE int32 const * ChildrenSparse_Find( ChildrenSparseN<t_num> const * children, int count, uint8 select )
{
	RR_ASSERT( count <= t_num );
	RR_COMPILER_ASSERT( t_num == 8 || t_num == 16 );

	__m128i x = (t_num == 16) ? _mm_loadu_si128((__m128i const*)children->select) : _mm_loadl_epi64((__m128i const*)children->select);
    __m128i rep = _mm_set1_epi8(select);

	int32 const * ret = NULL;
    U32 m = _mm_movemask_epi8(_mm_cmpeq_epi8(x, rep));
    if ( m )
    {
		int i = rrCtz32(m);
		if ( i < count )
			ret = &(children->nodes[i]);
	}

	// verify :
	//RR_DURING_ASSERT( int32 const * pc = ChildrenSparse_Find_Simple(children,count,select ) );
	//RR_ASSERT( ret == pc );

	return ret;
}

#elif defined(__RAD64__)

template <int t_num>
RADFORCEINLINE void ChildrenSparse_PreInit( ChildrenSparseN<t_num> * node )
{
	// Zero-clear to make sure all select values are defined
	memset(node->select, 0, t_num);
}

RADFORCEINLINE int32 const * ChildrenSparse_Find8(
	const uint8 * children_select,
	const int32 * children_nodes,
	int count,
	uint8 select)
{
	U64 rep = 0x0101010101010101ULL * select;
	U64 word = RR_GET64_LE_UNALIGNED(children_select);
	U64 xorw = word ^ rep;
	U64 test = ((xorw) - 0x0101010101010101ULL) & ~(xorw);
	test &= 0x8080808080808080ULL;

	if ( test ) // hit here means "xor" contains a zero byte
	{
		// test has a 0x80 in the place where the zero in word^rep is.
		int i = rrCtzBytes64(test);
		if ( i >= count ) return NULL;
		return &(children_nodes[i]);
	}
	else
	{
		return NULL;
	}
}

template <int t_num>
int32 const * ChildrenSparse_Find( ChildrenSparseN<t_num> const * children, int count, uint8 select )
{
	RR_ASSERT( count <= t_num );
	RR_COMPILER_ASSERT( t_num == 8 || t_num == 16 );

	RR_DURING_ASSERT( int32 const * pc = ChildrenSparse_Find_Simple(children,count,select ) );
	int32 const * ret;

	if ( t_num == 8 )
	{
		ret = ChildrenSparse_Find8(children->select,children->nodes,count,select);
	}
	else
	{
		int32 const * p1 = ChildrenSparse_Find8(children->select,children->nodes,8,select);
		if ( p1 )
			ret = p1;
		else
			ret = ChildrenSparse_Find8(children->select+8,children->nodes+8,count-8,select);
	}

	RR_ASSERT( ret == pc );
	return ret;
}

#else

template <int t_num>
RADFORCEINLINE void ChildrenSparse_PreInit( ChildrenSparseN<t_num> * node )
{
	// nothing to do
}

template <int t_num>
int32 const * ChildrenSparse_Find( ChildrenSparseN<t_num> const * children, int count, uint8 select )
{
	int32 const * pc = ChildrenSparse_Find_Simple(children,count,select );
	return pc;
}

#endif

template <int t_num>
static RADFORCEINLINE int32 * ChildrenSparse_Find( ChildrenSparseN<t_num> * children, int count, uint8 select )
{
	int32 const * pc = ChildrenSparse_Find( const_cast<ChildrenSparseN<t_num> const *>(children),count,select );
	return const_cast<int32 *>( pc );
}

typedef ChildrenSparseN<CAPACITY1> ChildrenSparse1;
typedef ChildrenSparseN<CAPACITY2> ChildrenSparse2;

#define CAPACITYSI 48

struct ChildrenSparseI : public Children // Sparse-indirect nodes
{
	uint8 sel2ind[256]; // index into nodes array
	int32 nodes[CAPACITYSI];
	ChildrenSparseI() { RR_ZERO(sel2ind); }
};

static void ChildrenSparseI_Sort( ChildrenSparseI * children, int count )
{
	RR_ASSERT( count <= CAPACITYSI );
	int32 old_nodes[CAPACITYSI];

	// we just have to "defragment" the sel->node mapping
	for(int i=0;i<count;i++)
		old_nodes[i] = children->nodes[i];

	int out_count = 0;
	for (int i=0;i<256;i++)
	{
		int ind = children->sel2ind[i];
		if ( ind != 0 )
		{
			// assign next available ind to it
			children->sel2ind[i] = static_cast<uint8>(out_count + 1);
			children->nodes[out_count] = old_nodes[ind - 1];
			out_count++;
		}
	}
	RR_ASSERT( out_count == count );
}

struct ChildrenFull256 : public Children
{
	int32	nodes[256];
};

// NOTE: this code might be possibly to unify with SuffixTrieObject below
// but for now, keep them separate and stick to purely mechanical changes for
// the big refactor
class SuffixTrie2MatchFinder : public IncrementalMatchFinder
{
	const U8 * m_ubuf;
	int m_size;

	rrPool<ChildrenSparse1,ST_POOL_BYTES_PER_CHUNK>	ChildrenSparse1_pool;
	rrPool<ChildrenSparse2,ST_POOL_BYTES_PER_CHUNK>	ChildrenSparse2_pool;
	rrPool<ChildrenSparseI,ST_POOL_BYTES_PER_CHUNK> ChildrenSparseI_pool;
	rrPool<ChildrenFull256,ST_POOL_BYTES_PER_CHUNK>	ChildrenFull256_pool;

	enum
	{
		CHUNK_SHIFT = 14,
		CHUNK_SIZE = 1<<CHUNK_SHIFT,
		CHUNK_MASK = CHUNK_SIZE-1
	};

	vector<int>	vfirstbytes;
	vector<SuffixNode *> node_chunks; // SuffixNodes are allocated in big chunks

	int lastml;
	int follownode;
	int node_to_update_follow;
	int maxml_offs;
	int maxml_pos;
	int pos;
	int nextnode; // next node index to allocate
	int num_firstbytes;
	int firstbytes_mask;

	bool has_lrm;
	bool run_lrm_as_job;
	LRMScannerWindowed scanner;
	void * jobifyUserPtr;
	vector<UnpackedMatchPair> lrm_matches;

	void LRMJob(int chunkStart, int chunkEnd);
	static void OODLE_CALLBACK LRMJobShim(void *job_data);

	void AllocNodeChunk(int node);

public:
	SuffixTrie2MatchFinder(const U8 * ubuf, int size, int startRecordingPos, LRMSet * lrms, OodleLZ_Jobify jobify, void * jobify_userPtr, int firstbytes);
	~SuffixTrie2MatchFinder();

	int ProcessChunk(int chunkSize, UnpackedMatchPair * matches, int maxPairs); // returns number of bytes processed (0 if we're at end)
};

struct LRMJobDesc
{
	SuffixTrie2MatchFinder * mf;
	int chunkStart, chunkEnd;
};

void SuffixTrie2MatchFinder::LRMJob(int chunkStart, int chunkEnd)
{
	int chunkSize = chunkEnd - chunkStart;

#ifdef OODLE_BUILDING_DATA
	THREADPROFILESCOPE("ST2_LRMJob");
#endif

	const U8 * ubuf = m_ubuf;
	int size = m_size;

	lrm_matches.reserve(chunkSize);
	lrm_matches.clear();
	for (int i = chunkStart; i < chunkEnd; i++)
	{
		UnpackedMatchPair mp;
		SINTa lrm_offset;

		mp.length = LRMScannerWindowed_FindMatchAndRoll(&scanner, ubuf + i, ubuf + size, &lrm_offset);
		mp.offset = S32_checkA(lrm_offset);
		lrm_matches.push_back(mp);
	}
}

void OODLE_CALLBACK SuffixTrie2MatchFinder::LRMJobShim(void *job_data)
{
	LRMJobDesc *job = static_cast<LRMJobDesc*>(job_data);
	job->mf->LRMJob(job->chunkStart, job->chunkEnd);
}

void SuffixTrie2MatchFinder::AllocNodeChunk(int node)
{
	int chunk_id = node >> CHUNK_SHIFT;
	RR_ASSERT(chunk_id < node_chunks.size32());
	RR_ASSERT(node_chunks[chunk_id] == NULL);

	node_chunks[chunk_id] = OODLE_MALLOC_ARRAY_CACHEALIGNED(SuffixNode, CHUNK_SIZE);
}

SuffixTrie2MatchFinder::SuffixTrie2MatchFinder(const U8 * ubuf, int size, int startRecordingPos, LRMSet * lrms, OodleLZ_Jobify jobify, void * jobify_userPtr, int firstbytes)
{
	// pos == 0 not possible cuz I use 0 for nulls
	//	so shift indexing so pos == 1 is the start :
	ubuf--;
	size++;
	startRecordingPos++;

	m_ubuf = ubuf;
	m_size = size;

	// firstbytes == 2 jump-in requires 256k
	// firstbytes == 3 jump-in requires 64 MB
	RR_ASSERT( firstbytes == 2 || firstbytes == 3 );
	num_firstbytes = firstbytes;
	int firstbytes_shift = ( firstbytes == 2 ) ? 8 : 0;
	int firstbytes_count = 1 << (24 - firstbytes_shift);
	firstbytes_mask = firstbytes_count - 1;
	vfirstbytes.resize(firstbytes_count,0);

	node_chunks.resize((size + CHUNK_MASK) >> CHUNK_SHIFT, NULL);

	lastml = 0;
	follownode = 0;
	node_to_update_follow = 0;
	maxml_offs = 0;
	maxml_pos = 0;
	pos = 1; // 1-based (see above)

	nextnode = 1; // we reserve node 0
	AllocNodeChunk(nextnode); // first alloc we need to do explicitly

	has_lrm = lrms != NULL;
	run_lrm_as_job = (jobify != OodleLZ_Jobify_Disable);
	LRMScannerWindowed_Init(&scanner,lrms,ubuf+startRecordingPos,ubuf+size,RR_S32_MAX);
	jobifyUserPtr = jobify_userPtr;

	// Process bytes until startRecordingPos, but don't report anything
	ProcessChunk(startRecordingPos - 1 /*need byte count not 1-based index*/, NULL, 0);
}

static inline int32 read_firstbytes(const uint8 * ptr)
{
	U32 bytes = RR_GET16_LE_UNALIGNED(ptr);
	return bytes | (ptr[2] << 16);
}

int SuffixTrie2MatchFinder::ProcessChunk(int chunkSize, UnpackedMatchPair * matches, int maxPairs)
{
	// If non-NULL matches, you need to ask for at least two match pairs per pos.
	// (Two simplifies this code slightly and there's no good reason not to require it.)
	RR_ASSERT( matches == NULL || maxPairs >= 2 );

	const U8 * ubuf = m_ubuf;
	int size = m_size;

	int * pfirstbytes = vfirstbytes.data();
	SuffixNode ** pnode_chunks = node_chunks.data();

	#define NODE(n) (pnode_chunks[(n) >> CHUNK_SHIFT][(n) & CHUNK_MASK])

	int chunkStart = pos;
	int chunkEnd = RR_MIN(pos + chunkSize, size-3/* max num_firstbytes*/-1); // last position we can find valid-length matches at
	int realChunkEnd = RR_MIN(pos + chunkSize, size); // last position we're asked to report matches for

	#define ST2_PREFETCH_AHEAD	6
	int chunkPrefetchEnd = chunkEnd - ST2_PREFETCH_AHEAD;
	if ( num_firstbytes != 3 ) // only prefetch when num_firstbytes == 3
		chunkPrefetchEnd = chunkStart;

	if ( chunkStart >= chunkEnd )
	{
		// do nothing
		
		// could also just do this clamp above and allow the normal code path to run
		//chunkEnd = RR_MAX(chunkEnd,chunkStart);
		// but I think it's more clear to just catch these degenerate blocks
		//	all the code in the other branch should be a NOP in this case

		//SuffixTrie2 crash bug 03-19-2021 :
		// on buffers of size 262145
		// we'll have chunkStart = 262145
		//	realChunkEnd = 262156
		// chunkEnd = 262142 is actually before chunkStart
		// avoid messy cases for that and just bail out here
	}
	else
	{

	// if we hit a max-length match, just keep on going until the end
	if (maxml_offs)
	{
		if (matches)
		{
			UnpackedMatchPair * matchPairs = matches;
			for(pos = chunkStart; pos < chunkEnd; pos++)
			{
				// NOTE: this code implicitly uses maxPairs >= 2
				matchPairs[0].length = size - pos;
				matchPairs[0].offset = maxml_offs;
				matchPairs[1].length = 0;

				matchPairs += maxPairs;
			}
		}
		
		//SuffixTrie2 crash bug 03-19-2021 :
		//  -> this was it
		//	if chunkEnd < chunkStart do NOT change pos!
		//pos = chunkEnd;
	}
	else
	{
		// Launch LRM job
		LRMJobDesc desc;
		OODLE_NS_PRE OodleJob lrm_job;
		bool run_lrm_job = matches && chunkEnd > chunkStart && has_lrm;

		if (run_lrm_job)
		{
			desc.mf = this;
			desc.chunkStart = chunkStart;
			desc.chunkEnd = chunkEnd;
			lrm_job.run(LRMJobShim, &desc, jobifyUserPtr, run_lrm_as_job);
		}

		for(pos = chunkStart; pos < chunkEnd; pos++)
		{
			const uint8 * ptr = ubuf+pos;

			#if 1 // prefetch ahead
			if ( pos < chunkPrefetchEnd )
			{
				// prefetch pfirstbytes itself for ST2_PREFETCH_AHEAD bytes ahead
				uint32 firstbytes = read_firstbytes(ubuf + pos + ST2_PREFETCH_AHEAD) & firstbytes_mask;
				RR_PREFETCHRW_32B(&pfirstbytes[firstbytes]);

				// prefetch the next node at half the distance (so we had time
				// for the pfirstbytes prefetch to complete)
				firstbytes = read_firstbytes(ubuf + pos + ST2_PREFETCH_AHEAD/2) & firstbytes_mask;
				int cur = RR_MAX(pfirstbytes[firstbytes],0);
				RR_PREFETCHRW_32B(&NODE(cur));
			}
			#endif

			int maxml = size-pos;
			ASSERT( maxml >= num_firstbytes );

			int cur,curml;
			// even if you don't have a follow, you may have some guaranteedml
			//	and if you do have a follow, guaranteedml may be > the follow's depth
			int guaranteedml = MAX(0,lastml-1);
			// I'm not sure I need both of these; just deepest_node might be okay, but whatever
			int deepest_node_with_follow = 0;
			int deepest_node_without_follow = 0;
			int parent = 0;

			ASSERT( guaranteedml <= maxml );

			// I want to update the ->follow pointer in node_to_update_follow
			// do so as I walk deeper; I want to find a node whose depth is < node_to_update_follow->depth
			//	but is greater than the previous follow's depth (
			int node_to_update_follow_prev_depth = INT_MAX;
			if ( node_to_update_follow != 0 )
			{
				if ( NODE(node_to_update_follow).follow == 0 )
					node_to_update_follow_prev_depth = 0;
				else
					node_to_update_follow_prev_depth = NODE(NODE(node_to_update_follow).follow).depth;
			}

			#define DO_UPDATE_FOLLOW(cur,curml) if ( curml > node_to_update_follow_prev_depth ) { \
				ASSERT( node_to_update_follow != 0 ); \
				if ( curml >= (int)NODE(node_to_update_follow).depth ) { \
					node_to_update_follow_prev_depth = INT_MAX; \
				} else { \
					node_to_update_follow_prev_depth = curml; \
					NODE(node_to_update_follow).follow = cur; \
				} \
			}

			if ( follownode > 0 ) // start from follow
			{
				ASSERT( guaranteedml >= num_firstbytes );
				cur = follownode;
				curml = NODE(cur).depth;
				ASSERT( guaranteedml >= curml );
				HEAVY_ASSERT( memcmp(ptr,NODE(cur).ptr,curml) == 0 ); // not guaranteedml match
			}
			else // no follow, need to use pfirstbytes
			{
				uint32 firstbytes = read_firstbytes(ptr) & firstbytes_mask;

				cur = pfirstbytes[firstbytes];
				if ( cur == 0 )
				{
					// no nodes; add me as a leaf :
					pfirstbytes[firstbytes] = -pos;

					lastml = 0;
					follownode = 0;

					if ( matches )
					{
						UnpackedMatchPair * matchPairs = matches + (pos - chunkStart)*maxPairs;
						matchPairs[0].length = 0; // no matches found
					}

					continue; // step to next pos
				}

				curml = num_firstbytes;
				//int maxml = MIN( (size-pos) , Suffix5_MAX_ML );

				// table points directly at a leaf :
				if ( cur < 0 )
				{
					pfirstbytes[firstbytes] = nextnode;
					ASSERT( parent == 0 );
					goto promote_leaf_to_node;
				}
				else // node
				{
					// check child depth and see if I can reach it
					SuffixNode * pchild = & NODE(cur);
					int32 child_depth = pchild->depth;
					curml = MAX(curml,guaranteedml);
					if ( curml >= child_depth )
					{
						curml = child_depth;
						HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
					}
					else
					{
						HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );

						#if DO_GETMATCHLEN_CHILD_DESCENT

						int curmaxml = RR_MIN(child_depth,maxml); //, rrPtrDiff32(ubufend - pchild->ptr) );
						const U8 * ptrend = ptr + curmaxml;
						int moreml = getmatchlen_mml1(ptr + curml,pchild->ptr + curml,ptrend);
						curml += moreml;
						RR_ASSERT( curml <= curmaxml );

						if ( curml == maxml )
							goto hit_maxml;

						if ( curml < child_depth )
						{
							// didn't make it
							// make new node with two children at depth curml
							pfirstbytes[firstbytes] = nextnode;
							ASSERT( parent == 0 );
							goto make_branch_node;
						}

						#else

						const U8 * pchild_ptr = ubuf+pchild->pos;

						while( curml < child_depth )
						{
							if ( curml == maxml )
							{
								ASSERT( parent == 0 );
								goto hit_maxml;
							}
							else if ( ptr[curml] == pchild_ptr[curml] )
							{
								curml++;
							}
							else
							{
								// didn't make it
								// make new node with two children at depth curml
								pfirstbytes[firstbytes] = nextnode;
								ASSERT( parent == 0 );
								goto make_branch_node;
							}
						}

						#endif
					}
					// made it, go to child
					// 'cur' is child already
				}
			}

			for(;;)
			{
				search_cur_node:

				DO_UPDATE_FOLLOW(cur,curml);

				if ( curml >= maxml )
					goto hit_maxml;

				// I'm looking for this char to select a child :
				uint8 select = ptr[curml];

				ASSERT( cur > 0 ); // a node index
				SuffixNode * n = & NODE(cur);
				ASSERT( curml == (int)n->depth );
				HEAVY_ASSERT( memcmp(ptr,n->ptr,curml) == 0 );
				//int numChildren = n->numChildren;

				if ( n->follow )
					deepest_node_with_follow = cur;
				else
					deepest_node_without_follow = cur;

				int numChildren = n->numChildrenMinusOne+1;
				ASSERT( numChildren >= 2 );

				int32 * pchildIndex;

				if ( numChildren <= NUM_INLINE_CHILDREN )
				{
					SuffixNode & children = *n;

					for(int i=0;i<(numChildren);i++)
					{
						HEAVY_ASSERT( children.nodes[i] < 0 || (int)NODE(children.nodes[i]).depth > curml );

						if ( children.iselect[i] == select )
						{
							pchildIndex = &(children.nodes[i]);
							goto found_pchild;
						}
					}

					// did not find child ; add it
					if ( numChildren < NUM_INLINE_CHILDREN )
					{
						children.iselect[numChildren] = select;
						children.nodes[numChildren] = -pos;

						n->numChildrenMinusOne++;
						ASSERT( n->numChildrenMinusOne+1 <= NUM_INLINE_CHILDREN );

						goto search_done;
					}
					else
					{
						// convert old children to ChildrenSparse1 :
						ChildrenSparse1 * newChildren = ChildrenSparse1_pool.Alloc();
						ChildrenSparse_PreInit(newChildren);

						for(int i=0;i<NUM_INLINE_CHILDREN;i++)
						{
							newChildren->select[i] = children.iselect[i];
							newChildren->nodes[i]  = children.nodes[i];
						}
						// and add the new one :
						newChildren->select[NUM_INLINE_CHILDREN] = select;
						newChildren->nodes[ NUM_INLINE_CHILDREN] = -pos;
						n->numChildrenMinusOne++;
						n->children = newChildren;

						goto search_done;
					}
				}
				else if ( numChildren <= CAPACITY1 )
				{
					ChildrenSparse1 * children = static_cast<ChildrenSparse1 *>(n->children);
					pchildIndex = ChildrenSparse_Find(children,numChildren,select);
					if ( pchildIndex )
					{
						goto found_pchild;
					}
					// not found, add it

					if ( numChildren == CAPACITY1 )
					{
						// switch caps :

						// convert old children to ChildrenSparse2 :
						ChildrenSparse2 * newChildren = ChildrenSparse2_pool.Alloc();
						ChildrenSparse_PreInit(newChildren);

						for(int i=0;i<CAPACITY1;i++)
						{
							newChildren->select[i] = children->select[i];
							newChildren->nodes[i]  = children->nodes[i];
						}
						// and add the new one :
						newChildren->select[CAPACITY1] = select;
						newChildren->nodes[ CAPACITY1] = -pos;
						n->numChildrenMinusOne++;
						n->children = newChildren;

						ChildrenSparse1_pool.Free( children );

						goto search_done;
					}
					else
					{
						// tack it on :
						children->select[numChildren] = select;
						children->nodes[numChildren] = -pos;
						n->numChildrenMinusOne++;
						goto search_done;
					}
				}
				else if ( numChildren <= CAPACITY2 )
				{
					ChildrenSparse2 * children = static_cast<ChildrenSparse2 *>(n->children);
					pchildIndex = ChildrenSparse_Find(children,numChildren,select);
					if ( pchildIndex )
					{
						goto found_pchild;
					}
					// not found, add it

					if ( numChildren == CAPACITY2 )
					{
						// switch to SI

						ChildrenSparseI * pSI = ChildrenSparseI_pool.Alloc();

						// select is set
						int node = -pos; // add new leaf first
						for(int i=0;;i++)
						{
							// add it :
							pSI->sel2ind[select] = static_cast<uint8>(i+1);
							pSI->nodes[i] = node;
							if ( i == CAPACITY2 )
								break;
							// get vals for next iteration :
							select = children->select[i];
							node = children->nodes[i];
						}
						n->numChildrenMinusOne++;
						n->children = pSI;

						ChildrenSparse2_pool.Free( children );
						goto search_done;
					}
					else
					{
						// tack it on :
						children->select[numChildren] = select;
						children->nodes[numChildren] = -pos;
						n->numChildrenMinusOne++;
						goto search_done;
					}
				}
				else if ( numChildren <= CAPACITYSI )
				{
					// Sparse-Indirect
					ChildrenSparseI * pSI = static_cast<ChildrenSparseI *>(n->children);
					uint8 idx = pSI->sel2ind[select];
					if ( idx == 0 )
					{
						if ( numChildren == CAPACITYSI )
						{
							// upgrade
							// switch to full 256 :

							// convert old children to full :
							ChildrenFull256 * newChildren = ChildrenFull256_pool.Alloc();
							memset( newChildren->nodes, 0, sizeof(newChildren->nodes) );

							for (int i=0;i<256;i++)
								if (int ind = pSI->sel2ind[i])
									newChildren->nodes[i] = pSI->nodes[ind - 1];

							// and add the new one
							newChildren->nodes[select] = -pos;
							n->numChildrenMinusOne++;
							n->children = newChildren;

							ChildrenSparseI_pool.Free(pSI);
							goto search_done;
						}
						else
						{
							// put me in
							idx = static_cast<uint8>(numChildren);
							pSI->sel2ind[select] = idx + 1;
							pSI->nodes[idx] = -pos;
							n->numChildrenMinusOne++;
							goto search_done;
						}
					}

					pchildIndex = &(pSI->nodes[idx - 1]);
					goto found_pchild;
				}
				else
				{
					ChildrenFull256 * children = static_cast<ChildrenFull256 *>(n->children);

					pchildIndex = &( children->nodes[select] );

					if ( *pchildIndex == 0 )
					{
						*pchildIndex = -pos;
						n->numChildrenMinusOne++;
						goto search_done;
					}
					else
					{
						goto found_pchild;
					}
				}

				if ( 0 )
				{
					found_pchild:

					int child = *pchildIndex;

					if ( child < 0 )
					{
						// previous leaf found, change to a node
						*pchildIndex = nextnode;
						parent = cur;
						cur = child;
						curml++; // they match the one branch at least
						goto promote_leaf_to_node;
					}
					else
					{
						// node found
						curml++;
						// check child depth and see if I can reach it
						SuffixNode * pchild = & NODE(child);
						int child_depth = pchild->depth;
						RR_ASSERT( child_depth > (int)n->depth );
						curml = MAX(curml,guaranteedml);

						if ( curml >= child_depth )
						{
							curml = child_depth;
							HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
						}
						else
						{
							HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );

							#if DO_GETMATCHLEN_CHILD_DESCENT

							int curmaxml = RR_MIN(child_depth,maxml);
							const U8 * ptrend = ptr + curmaxml;
							int moreml = getmatchlen_mml1(ptr + curml,pchild->ptr + curml,ptrend);
							curml += moreml;
							RR_ASSERT( curml <= curmaxml );

							if ( curml == maxml )
							{
								cur = child;
								goto hit_maxml;
							}

							if ( curml < child_depth )
							{
								// didn't make it
								// make new node with two children at depth curml
								*pchildIndex = nextnode;
								parent = cur;
								cur = child;
								goto make_branch_node;
							}

							#else

							const U8 * pchild_ptr = ubuf+pchild->pos;

							while( curml < child_depth )
							{
								if ( curml == maxml )
								{
									cur = child;
									goto hit_maxml;
								}
								else if ( ptr[curml] == pchild_ptr[curml] )
								{
									curml++;
								}
								else
								{
									// didn't make it
									// make new node with two children at depth curml
									*pchildIndex = nextnode;
									parent = cur;
									cur = child;
									goto make_branch_node;
								}
							}

							#endif
						}
						// made it, go to child
						cur = child;
						goto search_cur_node;
					}
				}
			}

			const uint8 * match_ptr;

			search_done:
			// curml is the longest match
			// and cur is the deepest node
			//ASSERT( curml == (int)NODE(cur).depth );
			ASSERT( curml == (int)NODE(cur).depth || ( curml == maxml && curml >= (int)NODE(cur).depth ) );
			ASSERT( curml >= guaranteedml );
			if ( deepest_node_without_follow > 0 )
				node_to_update_follow = deepest_node_without_follow;
			else
				node_to_update_follow = deepest_node_with_follow;
			match_ptr = ubuf + NODE(cur).pos;
			parent = cur;

			if ( 0 )
			{
				if ( 0 )
				{
					promote_leaf_to_node:

					// cur was set to the leaf I found
					// adding pos
					int other_leaf = cur;
					ASSERT( other_leaf < 0 );
					const uint8 * other_leaf_ptr = ubuf - other_leaf;
					HEAVY_ASSERT( memcmp(ptr,other_leaf_ptr,curml) == 0 );

					match_ptr = other_leaf_ptr;

					// step up to guaranteedml if we haven't got there yet :
					curml = MAX(curml,guaranteedml);
					HEAVY_ASSERT( memcmp(ptr,other_leaf_ptr,curml) == 0 );

					// make one node with depth of shared portion
					//	and point it at the two leaves

					// while they match, add 1 child nodes ;
					// @@ in degenerate cases this can be the whole file
					// -> should use a faster matcher here
					for(;;)
					{
						if ( curml == maxml )
							goto hit_maxml;

						if ( ptr[curml] != other_leaf_ptr[curml] )
							break;

						curml++;
					}
				}

				if ( 0 )
				{
					make_branch_node:

					// cur is the child I was trying to reach and didn't make it
					// curml is how much I matched
					// new node has already been pointed at

					int deep_node = cur;
					ASSERT( deep_node > 0 );
					const uint8 * deep_node_ptr = ubuf+NODE(deep_node).pos;
					HEAVY_ASSERT( memcmp(ptr,deep_node_ptr,curml) == 0 );
					ASSERT( curml < maxml );
					ASSERT( curml == maxml || ptr[curml] != deep_node_ptr[curml] );
					ASSERT( curml < (int)NODE(deep_node).depth );
					ASSERT( curml >= guaranteedml );

					// parent is the new node :
					ASSERT( NODE(cur).parent == parent );
					NODE(cur).parent = nextnode;

					match_ptr = deep_node_ptr;
				}

				int new_node = nextnode++;
				if ((new_node & CHUNK_MASK) == 0) // if we crossed over into a new node chunk, allocate!
					AllocNodeChunk(new_node);

				if ( parent != 0 )
				{
					RR_ASSERT( (int)NODE(parent).depth < curml );
				}

				ASSERT( curml < maxml );

				// okay now they don't match :
				SuffixNode * n = &NODE(new_node);

				n->depth = curml;
				n->follow = 0;
				n->pos = pos;
				n->parent = parent;

				RR_ASSERT( cur < 0 || (int)NODE(cur).depth > curml );

				// make children for 2 branches :
				uint8 s1 = ptr[curml];
				int32 n1 = -pos;
				uint8 s2 = match_ptr[curml];
				int32 n2 = cur;

				// doing this when s1 = s2 at curml = maxml is silly but benign
				ASSERT( s1 != s2 || curml == maxml );

				n->numChildrenMinusOne = 1;

				RR_COMPILER_ASSERT( NUM_INLINE_CHILDREN >= 2 );
				n->iselect[0] = s1;
				n->nodes  [0] = n1;
				n->iselect[1] = s2;
				n->nodes  [1] = n2;

				DO_UPDATE_FOLLOW(new_node,curml);

				node_to_update_follow = new_node;
			}

			//-----------------------------
			// all matches wind up here

			if ( deepest_node_with_follow != 0 )
			{
				follownode = NODE(deepest_node_with_follow).follow;
				ASSERT( follownode == 0 || NODE(follownode).depth < NODE(deepest_node_with_follow).depth );
			}
			else
			{
				follownode = 0;
			}

			ASSERT( curml >= num_firstbytes );
			ASSERT( match_ptr < ptr );

			lastml = curml;

			// put the match pairs :
			if ( matches )
			{
				UnpackedMatchPair * matchPairs = matches + (pos - chunkStart)*maxPairs;
				int curoff = rrPtrDiff32(ptr - match_ptr);
				int numPairs = 1;

				matchPairs[0].length = curml;
				matchPairs[0].offset = curoff;

				// get shorter matches :
				// also update parent->ptr so we store the most recent location of this substring
				#ifdef LIMIT_PARENT_STEPS
				int parent_steps = 0;
				#endif
				while( parent != 0 )
				{
					SuffixNode * n = &NODE(parent);

					if ( numPairs < maxPairs )
					{
						int offset = pos - n->pos;

						// only record it if offset went down
						//  (offset == previous is typical due to ptr bubble up)
						if ( offset < matchPairs[numPairs-1].offset )
						{
							int length = n->depth;
							RR_ASSERT( length >= maxml || ptr[length] != (ptr - (SINTa)offset)[length] );

							//RR_ASSERT( length >= 3 );
							if ( length <= 2 )
								break;

							// the only time the lengths are equal is at end-of-block
							//	due to hitting maxlength
							//	otherwise parents are strictly shorter
							RR_ASSERT( length <= matchPairs[numPairs-1].length );

							matchPairs[numPairs].length = length;
							matchPairs[numPairs].offset = offset;
							numPairs++;
						}
					}

					#ifdef LIMIT_PARENT_STEPS
					if ( ++parent_steps > LIMIT_PARENT_STEPS )
						break;
					#endif

					HEAVY_ASSERT( memcmp(ptr,ubuf+n->pos,n->depth) == 0 );

					// store latest :
					n->pos = pos;

					// step back :
					parent = n->parent;
				}

				RR_ASSERT( numPairs <= maxPairs );
				if (numPairs < maxPairs)
					matchPairs[numPairs].length = 0;
			}
			else
			{
				// not recording, but do update parent->ptr

				#ifdef LIMIT_PARENT_STEPS
				int parent_steps = 0;
				#endif
				while( parent != 0 )
				{
					SuffixNode * n = &NODE(parent);

					#ifdef LIMIT_PARENT_STEPS
					if ( ++parent_steps > LIMIT_PARENT_STEPS )
						break;
					#endif

					HEAVY_ASSERT( memcmp(ptr,ubuf+n->pos,n->depth) == 0 );

					// store latest :
					n->pos = pos;

					// step back :
					parent = n->parent;
				}
			}

			if ( 0 )
			{
				hit_maxml:

				/*

				THE END !

				Once we hit maxml, we are done
				we have matches to the end of the buffer
				using the same offset we'll hit maxml every time

				@@ todo : walk to parent and see if it provides the same match with lower offset

				*/

				if ( cur < 0 )
					match_ptr = ubuf - cur;
				else
					match_ptr = ubuf + NODE(cur).pos;

				HEAVY_ASSERT( memcmp(ptr,match_ptr,maxml) == 0 );

				// remember we're in a max-length match
				maxml_offs = rrPtrDiff32( ptr - match_ptr );
				maxml_pos = pos;
				RR_ASSERT( maxml_offs > 0 && maxml_offs <= pos );
				RR_ASSERT( maxml == size - pos );

				if ( matches )
				{
					UnpackedMatchPair * matchPairs = matches + (pos - chunkStart)*maxPairs;
					// NOTE: this code implicitly uses maxPairs >= 2
					// keep reporting that match until the end of the chunk
					for(; pos < chunkEnd; pos++)
					{
						matchPairs[0].length = size - pos;
						matchPairs[0].offset = maxml_offs;
						matchPairs[1].length = 0; // only one match

						matchPairs += maxPairs;
					}
				}

				pos = chunkEnd;
				break;
			}
		}

		RR_ASSERT( pos == chunkEnd );

		// Merge LRM results
		if (run_lrm_job)
		{
			lrm_job.wait(jobifyUserPtr);

			// merge LRM results into master match pair list
			int merge_end = chunkEnd;

			// if we hit maxml, stop merging at that point
			// this is purely to guarantee identical behavior to before
			if (maxml_offs)
				merge_end = maxml_pos;

			const UnpackedMatchPair * lrm_match = &lrm_matches[0];
			UnpackedMatchPair * out_matches = matches;
			for (int merge_pos = chunkStart; merge_pos < merge_end; merge_pos++, lrm_match++, out_matches += maxPairs)
			{
				// If the LRM has a (strictly) longer match, use it, otherwise ignore it entirely.
				// The assumption here is that the contents of the LRM are always at higher offsets than
				// anything in our trie, so if the match isn't longer than the longest match we found,
				// it's strictly inferior to all matches we're gonna get from the trie.
				//
				// NOTE this is not strictly true when LRM overlaps the match backup range!
				if ( lrm_match->length > out_matches[0].length )
				{
					// probably worth special-casing particular values of maxPairs here
					memmove(&out_matches[1], &out_matches[0], (maxPairs - 1) * sizeof(UnpackedMatchPair));
					out_matches[0] = *lrm_match;
				}
			}
		}
	}

	} // degenerate chunk

	#undef NODE

	// generate empty pairs for the ending bit, if any
	if ( pos != realChunkEnd )
	{
		RR_ASSERT( pos < realChunkEnd );

		if ( matches )
		{
			UnpackedMatchPair * matchPairs = matches + (pos - chunkStart)*maxPairs;
			for(int i = pos; i<realChunkEnd; i++)
			{
				matchPairs[0].length = 0;

				matchPairs += maxPairs;
			}
		}

		pos = realChunkEnd;
	}

	return realChunkEnd - chunkStart;
}

SuffixTrie2MatchFinder::~SuffixTrie2MatchFinder()
{
	// pools destruct themselves

	// print out stats about peak memory usage :
	#if DO_LOG_MEMORY_USE
	{

	rrprintf("--------- SuffixTrie -------------\n");
	rrprintf("sizeof(SuffixNode) : %d\n",(int)sizeof(SuffixNode));

	S64 total = 0;

	#define LOG(var) rrprintf("%s : %s\n",RR_STRINGIZE(var),strcomma(var)); total += var;

	S64 size_vfirstbytes = vfirstbytes.size_bytes();

	S64 size_NodeChunks = 0;
	for (UINTa i = 0; i < node_chunks.size(); i++)
	{
		if (node_chunks[i])
			size_NodeChunks += CHUNK_SIZE * sizeof(SuffixNode);
	}
	S64 size_ChildrenSparse1 = ChildrenSparse1_pool.CountBytesAllocated();
	S64 size_ChildrenSparse2 = ChildrenSparse2_pool.CountBytesAllocated();
	S64 size_ChildrenSparseI = ChildrenSparseI_pool.CountBytesAllocated();

	LOG(size_vfirstbytes);
	LOG(size_NodeChunks);
	LOG(size_ChildrenSparse1);
	LOG(size_ChildrenSparse2);
	LOG(size_ChildrenSparseI);

	S64 size_ChildrenFull256 = ChildrenFull256_pool.CountBytesAllocated();

	LOG(size_ChildrenFull256);

	rrprintf("total bytes : %s\n",strcomma(total));

	#undef LOG

	rrprintf("---------------------------------------------------\n");

	//OodleLeakTrack_LogAll();

	}
	#endif

	for (UINTa i = 0; i < node_chunks.size(); i++)
	{
		if (node_chunks[i])
			OODLE_FREE_ARRAY(node_chunks[i], CHUNK_SIZE);
	}
}

//===========================================================

struct SuffixTrieObject
{
	const U8 * m_ubuf;
	int m_ubufsize;

	vector<int>	vfirstbytes;

	rrPool<ChildrenSparse1,ST_POOL_BYTES_PER_CHUNK>	ChildrenSparse1_pool;
	rrPool<ChildrenSparse2,ST_POOL_BYTES_PER_CHUNK>	ChildrenSparse2_pool;
	rrPool<ChildrenSparseI,ST_POOL_BYTES_PER_CHUNK> ChildrenSparseI_pool;
	rrPool<ChildrenFull256,ST_POOL_BYTES_PER_CHUNK>	ChildrenFull256_pool;

	enum
	{
		CHUNK_SHIFT = 16,
		CHUNK_SIZE = 1<<CHUNK_SHIFT,
		CHUNK_MASK = CHUNK_SIZE-1
	};

	vector<SuffixNode *> node_chunks; // SuffixNodes are allocated in big chunks

	#define NODE(n) (pnode_chunks[(n) >> CHUNK_SHIFT][(n) & CHUNK_MASK])

	// SuffixIndex - data I want in the SuffixNode
	//	but keeping it on the side so that I don't change that data type for other usages
	struct SuffixIndex
	{
		int32 low;   // low suffix index of leaves under me
		int32 count; // # of leaves under me
	};
	vector<SuffixIndex> vsuffixindices; // indexed by node
	vector<int> vleaf_to_suffix; // indexed by -leaf

	SuffixTrieObject()
	{
	}

	~SuffixTrieObject()
	{
		for (UINTa i = 0; i < node_chunks.size(); i++)
		{
			if (node_chunks[i])
				OODLE_FREE_ARRAY(node_chunks[i], CHUNK_SIZE);
		}
	}

void AllocNodeChunk(int node)
{
	int chunk_id = node >> CHUNK_SHIFT;
	RR_ASSERT(chunk_id < node_chunks.size32());
	RR_ASSERT(node_chunks[chunk_id] == NULL);

	node_chunks[chunk_id] = OODLE_MALLOC_ARRAY_CACHEALIGNED(SuffixNode, CHUNK_SIZE);
}

void Build(const U8 * ubuf,int size,SuffixTrieSortData * psuffixsortdata = NULL)
{
	//RR_ASSERT( toStride >= 2*sizeof32(int)*maxPairs );

	// pos == 0 not possible cuz I use 0 for nulls
	//	so shift indexing so pos == 1 is the start :
	ubuf--;
	size++;

	m_ubuf = ubuf;
	m_ubufsize = size;

	// SUFFIX2_NUM_FIRST_BYTES == 2 jump-in requires 256k
	// SUFFIX2_NUM_FIRST_BYTES == 3 jump-in requires 64 MB
	RR_COMPILER_ASSERT( SUFFIX2_NUM_FIRST_BYTES == 2 || SUFFIX2_NUM_FIRST_BYTES == 3 );
	int firstbytes_count = 256*256;
	#if SUFFIX2_NUM_FIRST_BYTES == 3
	firstbytes_count *= 256;
	#endif
	vfirstbytes.resize(firstbytes_count,0);
	int * pfirstbytes = vfirstbytes.data();

	// nodes :

	node_chunks.resize((size + CHUNK_MASK) >> CHUNK_SHIFT, NULL);
	int nextnode = 1; // we reserve node 0 for nulls
	AllocNodeChunk(nextnode);
	SuffixNode ** pnode_chunks = node_chunks.data();

	int lastml = 0;
	int follownode = 0;
	int node_to_update_follow = 0;

	int pos;
	for(pos=1;pos<size-SUFFIX2_NUM_FIRST_BYTES-1;pos++)
	{
		const uint8 * ptr = ubuf+pos;
		//int maxml = size-pos;
		// edit 04-26-12 without full thought :
		//	pull back maxml by 1 so that we don't read off the end of the buffer
		// I should be able to have full maxml and fix this deeper down
		//	but this is the easier/safer fix for now
		int maxml = size-pos -1;
		ASSERT( maxml >= SUFFIX2_NUM_FIRST_BYTES );

		// NOTE : 07-10-14 :
		// near the end of the array
		// the suffix may have already occurred in its entirety
		// once that happens, no more nodes are added
		// this is because I don't do the "proper" thing
		//	and consider EOF to be a unique valued character

		int cur,curml;
		// even if you don't have a follow, you may have some guaranteedml
		//	and if you do have a follow, guaranteedml may be > the follow's depth
		int guaranteedml = MAX(0,lastml-1);
		// I'm not sure I need both of these; just deepest_node might be okay, but whatever
		int deepest_node_with_follow = 0;
		int deepest_node_without_follow = 0;
		int parent = 0;

		ASSERT( guaranteedml <= maxml );

		// I want to update the ->follow pointer in node_to_update_follow
		// do so as I walk deeper; I want to find a node whose depth is < node_to_update_follow->depth
		//	but is greater than the previous follow's depth (
		int node_to_update_follow_prev_depth = INT_MAX;
		if ( node_to_update_follow != 0 )
		{
			if ( NODE(node_to_update_follow).follow == 0 )
				node_to_update_follow_prev_depth = 0;
			else
				node_to_update_follow_prev_depth = NODE(NODE(node_to_update_follow).follow).depth;
		}

		if ( follownode > 0 ) // start from follow
		{
			ASSERT( guaranteedml >= SUFFIX2_NUM_FIRST_BYTES );
			cur = follownode;
			curml = NODE(cur).depth;
			ASSERT( guaranteedml >= curml );
			HEAVY_ASSERT( memcmp(ptr,NODE(cur).ptr,curml) == 0 ); // not guaranteedml match
		}
		else // no follow, need to use pfirstbytes
		{
			#if SUFFIX2_NUM_FIRST_BYTES == 2
			uint32 firstbytes = (ptr[0]<<8) | ptr[1];
			#else
			uint32 firstbytes = (ptr[0]<<16) | (ptr[1]<<8) | ptr[2];
			#endif

			cur = pfirstbytes[firstbytes];
			if ( cur == 0 )
			{
				// no nodes; add me as a leaf :
				pfirstbytes[firstbytes] = -pos;

				lastml = 0;
				follownode = 0;

				continue; // step to next pos
			}

			curml = SUFFIX2_NUM_FIRST_BYTES;
			//int maxml = MIN( (size-pos) , Suffix5_MAX_ML );

			// table points directly at a leaf :
			if ( cur < 0 )
			{
				pfirstbytes[firstbytes] = nextnode;
				ASSERT( parent == 0 );
				goto promote_leaf_to_node;
			}
			else // node
			{
				// check child depth and see if I can reach it
				SuffixNode * pchild = & NODE(cur);
				int32 child_depth = pchild->depth;
				curml = MAX(curml,guaranteedml);
				//HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
				if ( curml >= child_depth )
				{
					curml = child_depth;
					HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
				}
				else
				{
					HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );

					#if DO_GETMATCHLEN_CHILD_DESCENT

					int curmaxml = RR_MIN(child_depth,maxml);
					const U8 * ptrend = ptr + curmaxml;
					int moreml = getmatchlen_mml1(ptr + curml,pchild->ptr + curml,ptrend);
					curml += moreml;
					RR_ASSERT( curml <= curmaxml );

					if ( curml < child_depth )
					{
						// didn't make it
						// make new node with two children at depth curml
						pfirstbytes[firstbytes] = nextnode;
						ASSERT( parent == 0 );
						goto make_branch_node;
					}

					#else

					const U8 * pchild_ptr = ubuf + pchild->pos;
					while( curml < child_depth )
					{
						if ( curml < maxml && ptr[curml] == pchild_ptr[curml] )
						{
							curml++;
						}
						else
						{
							// didn't make it
							// make new node with two children at depth curml
							pfirstbytes[firstbytes] = nextnode;
							ASSERT( parent == 0 );
							goto make_branch_node;
						}
					}

					#endif
				}
				// made it, go to child
				// 'cur' is child already
			}
		}

		for(;;)
		{
			search_cur_node:

			DO_UPDATE_FOLLOW(cur,curml);

			if ( curml >= maxml )
				goto search_done;

			// I'm looking for this char to select a child :
			uint8 select = ptr[curml];

			ASSERT( cur > 0 ); // a node index
			SuffixNode * n = & NODE(cur);
			ASSERT( curml == (int)n->depth );
			HEAVY_ASSERT( memcmp(ptr,n->ptr,curml) == 0 );
			//int numChildren = n->numChildren;

			if ( n->follow )
				deepest_node_with_follow = cur;
			else
				deepest_node_without_follow = cur;

			int numChildren = n->numChildrenMinusOne+1;
			ASSERT( numChildren >= 2 );

			int32 * pchildIndex;

			if ( numChildren <= NUM_INLINE_CHILDREN )
			{
				SuffixNode & children = *n;

				for(int i=0;i<(numChildren);i++)
				{
					if ( children.iselect[i] == select )
					{
						pchildIndex = &(children.nodes[i]);
						goto found_pchild;
					}
				}

				// did not find child ; add it
				if ( numChildren < NUM_INLINE_CHILDREN )
				{
					children.iselect[numChildren] = select;
					children.nodes[numChildren] = -pos;

					n->numChildrenMinusOne++;
					ASSERT( n->numChildrenMinusOne+1 <= NUM_INLINE_CHILDREN );

					goto search_done;
				}
				else
				{
					// convert old children to ChildrenSparse1 :
					ChildrenSparse1 * newChildren = ChildrenSparse1_pool.Alloc();
					ChildrenSparse_PreInit(newChildren);

					for(int i=0;i<NUM_INLINE_CHILDREN;i++)
					{
						newChildren->select[i] = children.iselect[i];
						newChildren->nodes[i]  = children.nodes[i];
					}
					// and add the new one :
					newChildren->select[NUM_INLINE_CHILDREN] = select;
					newChildren->nodes[ NUM_INLINE_CHILDREN] = -pos;
					n->numChildrenMinusOne++;
					n->children = newChildren;

					goto search_done;
				}
			}
			else if ( numChildren <= CAPACITY1 )
			{
				ChildrenSparse1 * children = static_cast<ChildrenSparse1 *>(n->children);
				pchildIndex = ChildrenSparse_Find(children,numChildren,select);
				if ( pchildIndex )
				{
					goto found_pchild;
				}
				// not found, add it

				if ( numChildren == CAPACITY1 )
				{
					// switch caps :

					// convert old children to ChildrenSparse1 :
					ChildrenSparse2 * newChildren = ChildrenSparse2_pool.Alloc();
					ChildrenSparse_PreInit(newChildren);

					for(int i=0;i<CAPACITY1;i++)
					{
						newChildren->select[i] = children->select[i];
						newChildren->nodes[i]  = children->nodes[i];
					}
					// and add the new one :
					newChildren->select[CAPACITY1] = select;
					newChildren->nodes[ CAPACITY1] = -pos;
					n->numChildrenMinusOne++;
					n->children = newChildren;

					ChildrenSparse1_pool.Free( children );

					goto search_done;
				}
				else
				{
					// tack it on :
					children->select[numChildren] = select;
					children->nodes[numChildren] = -pos;
					n->numChildrenMinusOne++;
					goto search_done;
				}
			}
			else if ( numChildren <= CAPACITY2 )
			{
				ChildrenSparse2 * children = static_cast<ChildrenSparse2 *>(n->children);
				pchildIndex = ChildrenSparse_Find(children,numChildren,select);
				if ( pchildIndex )
				{
					goto found_pchild;
				}
				// not found, add it

				if ( numChildren == CAPACITY2 )
				{
					// switch to SI

					ChildrenSparseI * pSI = ChildrenSparseI_pool.Alloc();

					// select is set
					int node = -pos; // add new leaf first
					for(int i=0;;i++)
					{
						// add it :
						pSI->sel2ind[select] = static_cast<uint8>(i+1);
						pSI->nodes[i] = node;
						if ( i == CAPACITY2 )
							break;
						// get vals for next iteration :
						select = children->select[i];
						node = children->nodes[i];
					}
					n->numChildrenMinusOne++;
					n->children = pSI;

					ChildrenSparse2_pool.Free( children );
					goto search_done;
				}
				else
				{
					// tack it on :
					children->select[numChildren] = select;
					children->nodes[numChildren] = -pos;
					n->numChildrenMinusOne++;
					goto search_done;
				}
			}
			else if ( numChildren <= CAPACITYSI )
			{
				// Sparse-Indirect
				ChildrenSparseI * pSI = static_cast<ChildrenSparseI *>(n->children);
				uint8 idx = pSI->sel2ind[select];
				if ( idx == 0 )
				{
					if ( numChildren == CAPACITYSI )
					{
						// upgrade
						// switch to full 256 :

						// convert old children to full :
						ChildrenFull256 * newChildren = ChildrenFull256_pool.Alloc();
						memset( newChildren->nodes, 0, sizeof(newChildren->nodes) );

						for (int i=0;i<256;i++)
							if (int ind = pSI->sel2ind[i])
								newChildren->nodes[i] = pSI->nodes[ind - 1];

						// and add the new one
						newChildren->nodes[select] = -pos;
						n->numChildrenMinusOne++;
						n->children = newChildren;

						ChildrenSparseI_pool.Free(pSI);
						goto search_done;
					}
					else
					{
						// put me in
						idx = static_cast<uint8>(numChildren);
						pSI->sel2ind[select] = idx + 1;
						pSI->nodes[idx] = -pos;
						n->numChildrenMinusOne++;
						goto search_done;
					}
				}

				pchildIndex = &(pSI->nodes[idx - 1]);
				goto found_pchild;
			}
			else
			{
				ChildrenFull256 * children = static_cast<ChildrenFull256 *>(n->children);

				pchildIndex = &( children->nodes[select] );

				if ( *pchildIndex == 0 )
				{
					*pchildIndex = -pos;
					n->numChildrenMinusOne++;
					goto search_done;
				}
				else
				{
					goto found_pchild;
				}
			}

			if ( 0 )
			{
				found_pchild:

				int child = *pchildIndex;

				if ( child < 0 )
				{
					// previous leaf found, change to a node
					*pchildIndex = nextnode;
					parent = cur;
					cur = child;
					curml++; // they match the one branch at least
					goto promote_leaf_to_node;
				}
				else
				{
					// node found
					curml++;
					// check child depth and see if I can reach it
					SuffixNode * pchild = & NODE(child);
					int child_depth = pchild->depth;
					curml = MAX(curml,guaranteedml);
					//HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
					if ( curml >= child_depth )
					{
						curml = child_depth;
						HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
					}
					else
					{
						HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );

						#if DO_GETMATCHLEN_CHILD_DESCENT

						int curmaxml = RR_MIN(child_depth,maxml);
						const U8 * ptrend = ptr + curmaxml;
						int moreml = getmatchlen_mml1(ptr + curml,pchild->ptr + curml,ptrend);
						curml += moreml;
						RR_ASSERT( curml <= curmaxml );

						if ( curml < child_depth )
						{
							// didn't make it
							// make new node with two children at depth curml
							*pchildIndex = nextnode;
							parent = cur;
							cur = child;
							goto make_branch_node;
						}

						#else

						const U8 * pchild_ptr = ubuf + pchild->pos;
						while( curml < child_depth )
						{
							if ( curml < maxml && ptr[curml] == pchild_ptr[curml] )
							{
								curml++;
							}
							else
							{
								// didn't make it
								// make new node with two children at depth curml
								*pchildIndex = nextnode;
								parent = cur;
								cur = child;
								goto make_branch_node;
							}
						}

						#endif
					}
					// made it, go to child
					cur = child;
					goto search_cur_node;
				}
			}
		}

		const uint8 * match_ptr;

		search_done:
		// curml is the longest match
		// and cur is the deepest node
		ASSERT( curml == (int)NODE(cur).depth );
 		ASSERT( curml >= guaranteedml );
		if ( deepest_node_without_follow > 0 )
			node_to_update_follow = deepest_node_without_follow;
		else
			node_to_update_follow = deepest_node_with_follow;
		match_ptr = ubuf+NODE(cur).pos;
		parent = cur;

		if ( 0 )
		{
			if ( 0 )
			{
				promote_leaf_to_node:

				// cur was set to the leaf I found
				// adding pos
				int other_leaf = cur;
				ASSERT( other_leaf < 0 );
				const uint8 * other_leaf_ptr = ubuf - other_leaf;
				HEAVY_ASSERT( memcmp(ptr,other_leaf_ptr,curml) == 0 );

				match_ptr = other_leaf_ptr;

				// step up to guaranteedml if we haven't got there yet :
				curml = MAX(curml,guaranteedml);
				HEAVY_ASSERT( memcmp(ptr,other_leaf_ptr,curml) == 0 );

				// make one node with depth of shared portion
				//	and point it at the two leaves

				// while they match, add 1 child nodes
				while( curml < maxml && ptr[curml] == other_leaf_ptr[curml] )
				{
					curml++;
				}
			}

			if ( 0 )
			{
				make_branch_node:

				// cur is the child I was trying to reach and didn't make it
				// curml is how much I matched
				// new node has already been pointed at

				int deep_node = cur;
				ASSERT( deep_node > 0 );
				const uint8 * deep_node_ptr = ubuf+NODE(deep_node).pos;
				HEAVY_ASSERT( memcmp(ptr,deep_node_ptr,curml) == 0 );
				ASSERT( curml == maxml || ptr[curml] != deep_node_ptr[curml] );
				ASSERT( curml < (int)NODE(deep_node).depth );
				ASSERT( curml >= guaranteedml );

				// parent is the new node :
				ASSERT( NODE(cur).parent == parent );
				NODE(cur).parent = nextnode;

				match_ptr = deep_node_ptr;
			}

			int new_node = nextnode++;
			if ((new_node & CHUNK_MASK) == 0) // if we crossed over into a new node chunk, allocate!
				AllocNodeChunk(new_node);

			// okay now they don't match :
			SuffixNode * n = &NODE(new_node);

			n->depth = curml;
			n->follow = 0;
			n->pos = pos;
			n->parent = parent;

			// make children for 2 branches :
			uint8 s1 = ptr[curml];
			int32 n1 = -pos;
			uint8 s2 = match_ptr[curml];
			int32 n2 = cur;

			// doing this when s1 = s2 at curml = maxml is silly but benign
			ASSERT( s1 != s2 || curml == maxml );

			n->numChildrenMinusOne = 1;

			RR_COMPILER_ASSERT( NUM_INLINE_CHILDREN >= 2 );
			n->iselect[0] = s1;
			n->nodes  [0] = n1;
			n->iselect[1] = s2;
			n->nodes  [1] = n2;

			DO_UPDATE_FOLLOW(new_node,curml);

			node_to_update_follow = new_node;
		}

		//-----------------------------
		// all matches wind up here

		if ( deepest_node_with_follow != 0 )
		{
			follownode = NODE(deepest_node_with_follow).follow;
			ASSERT( follownode == 0 || NODE(follownode).depth < NODE(deepest_node_with_follow).depth );
		}
		else
		{
			follownode = 0;
		}

		ASSERT( curml >= SUFFIX2_NUM_FIRST_BYTES );
		ASSERT( match_ptr < ptr );

		lastml = curml;
	}

	CountChildren(psuffixsortdata);

	return;
}

static RADINLINE int GetChildCount(const SuffixIndex * psuffixindices, int node)
{
	if ( node == 0 ) return 0;
	if ( node < 0 ) return 1;
	int ret = psuffixindices[node].count;
	//ASSERT( ret >= 3 );
	ASSERT( ret >= 2 ); // only counting leaves
	return ret;
}

static RADINLINE int ProcessChildCount(int * pnode, int * pcount, const SuffixIndex * psuffixindices, int node)
{
	if ( node == 0 ) return 0;
	if ( node < 0 ) return 1;

	int ret = psuffixindices[node].count;
	//ASSERT( ret >= 3 );
	ASSERT( ret >= 2 ); // only counting leaves

	if ( ret > *pcount )
	{
		*pcount = ret;
		*pnode = node;
	}

	return ret;
}

#if 0
// @@@@ HACK is_wrt_legal_word here
static bool is_wrt_legal_word(const U8 * ptr,int len)
{
	// ensure the word decodes to a full byte-aligned output word
	//	don't take partial nibbles
	//	this is just for decoder optimization

	if ( len < 2 ) return false;

	if ( ptr[-1] == g_wrt_hack_escape ) return false;
	if ( ptr[len-1] == g_wrt_hack_escape ) return false;

	return true;
}
#endif

/*********

CountChildren :

two passes

first passes pushes all kids on a stack
to get a depth-first listing of nodes

next passes pops nodes from the stack
fills out child count from the kids
the kids have valid child counts because they were done already


***********/

void CountChildren(SuffixTrieSortData * psuffixsortdata)
{
	vsuffixindices.resize(m_ubufsize);
	vsuffixindices.memset_zero();

	SuffixIndex * psuffixindices = vsuffixindices.data();

	int firstbytes_count = vfirstbytes.size32();
	int const * const pfirstbytes = vfirstbytes.data();

	SuffixNode const * const * const pnode_chunks = node_chunks.data();

	vector<int> stack;
	stack.reserve(m_ubufsize/16);

	int total = 0;

	/*
	vector<int> debug_leaf_check;
	debug_leaf_check.resize(m_ubufsize);
	debug_leaf_check.memset_zero();
	#define CHECK_LEAF(n) do { if ( (n) < 0 ) { RR_ASSERT( debug_leaf_check[-(n)] == 0 ); debug_leaf_check[-(n)] = 1; } } while(0)
	/*/
	#define CHECK_LEAF(n) ;
	/**/

	for LOOP(fb,firstbytes_count)
	{
		int head = pfirstbytes[fb];
		if ( head <= 0 ) // null or leaf
		{
			CHECK_LEAF(head);
			total += head ? 1 : 0;
			continue;
		}

		ASSERT( stack.empty() );
		stack.push_back( head );

		while( ! stack.empty() )
		{
			int nn = stack.back();
			ASSERT( nn != 0 );
			stack.pop_back();

			// if n > 0 , repush self negative, then push all kids
			// if n < 0 , count all kids

			bool pushing = nn > 0;
			nn = RR_ABS(nn);

			ASSERT( nn < m_ubufsize );

			SuffixNode const * const n = & NODE(nn);

			if ( pushing )
			{
				// push self negative :
				stack.push_back( - nn );

				// visit all kids :

				int numChildren = n->numChildrenMinusOne+1;
				ASSERT( numChildren >= 2 );

				if ( numChildren <= NUM_INLINE_CHILDREN )
				{
					SuffixNode const & children = *n;

					// sort :
					SuffixNode & mchildren = const_cast<SuffixNode &>(children);
					if ( numChildren == 2 )
					{
						if ( children.iselect[1] < children.iselect[0] )
						{
							swap(mchildren.iselect[0],mchildren.iselect[1]);
							swap(mchildren.nodes[0],mchildren.nodes[1]);
						}
					}

					for(int i=0;i<(numChildren);i++)
					{
						int32 ci = children.nodes[i];
						if ( ci > 0 )
							stack.push_back(ci);
						else
							CHECK_LEAF(ci);
					}
				}
				else if ( numChildren <= CAPACITY1 )
				{
					ChildrenSparse1 const * children = static_cast<ChildrenSparse1 const *>(n->children);
					ChildrenSparse1 * mchildren = const_cast<ChildrenSparse1 *>(children);
					ChildrenSparse_Sort(mchildren,numChildren);

					for LOOP(i,numChildren)
					{
						int ci = children->nodes[i];
						if ( ci > 0 )
							stack.push_back(ci);
						else
							CHECK_LEAF(ci);
					}
				}
				else if ( numChildren <= CAPACITY2 )
				{
					ChildrenSparse2 const * children = static_cast<ChildrenSparse2 const *>(n->children);
					ChildrenSparse2 * mchildren = const_cast<ChildrenSparse2 *>(children);
					ChildrenSparse_Sort(mchildren,numChildren);

					for LOOP(i,numChildren)
					{
						int ci = children->nodes[i];
						if ( ci > 0 )
							stack.push_back(ci);
						else
							CHECK_LEAF(ci);
					}
				}
				else if ( numChildren <= CAPACITYSI )
				{
					ChildrenSparseI const * pSI = static_cast<ChildrenSparseI const *>(n->children);
					for LOOP(i,numChildren)
					{
						int ci = pSI->nodes[i];
						if ( ci > 0 )
							stack.push_back(ci);
						else
							CHECK_LEAF(ci);
					}
				}
				else
				{
					ChildrenFull256 const * children = static_cast<ChildrenFull256 const *>(n->children);

					for LOOP(i,256)
					{
						int ci = children->nodes[i];
						if ( ci > 0 )
							stack.push_back(ci);
						else
							CHECK_LEAF(ci);
					}
				}
			}
			else // summing phase
			{
				ASSERT( psuffixindices[nn].count == 0 );

				//int childCount = 1; // count self
				int childCount = 0; // do NOT count self

				// "best_" is the child with highest count
				int best_node = nn;
				int best_count = 0;

				// visit all kids :

				int numChildren = n->numChildrenMinusOne+1;
				ASSERT( numChildren >= 2 );

				if ( numChildren <= NUM_INLINE_CHILDREN )
				{
					SuffixNode const & children = *n;

					for(int i=0;i<(numChildren);i++)
					{
						int32 ci = children.nodes[i];
						childCount += ProcessChildCount(&best_node,&best_count,psuffixindices,ci);
					}
				}
				else if ( numChildren <= CAPACITY1 )
				{
					ChildrenSparse1 const * children = static_cast<ChildrenSparse1 const *>(n->children);
					for LOOP(i,numChildren)
					{
						int ci = children->nodes[i];
						childCount += ProcessChildCount(&best_node,&best_count,psuffixindices,ci);
					}
				}
				else if ( numChildren <= CAPACITY2 )
				{
					ChildrenSparse2 const * children = static_cast<ChildrenSparse2 const *>(n->children);
					for LOOP(i,numChildren)
					{
						int ci = children->nodes[i];
						childCount += ProcessChildCount(&best_node,&best_count,psuffixindices,ci);
					}
				}
				else if ( numChildren <= CAPACITYSI )
				{
					ChildrenSparseI const * pSI = static_cast<ChildrenSparseI const *>(n->children);
					for LOOP(i,numChildren)
					{
						int ci = pSI->nodes[i];
						childCount += ProcessChildCount(&best_node,&best_count,psuffixindices,ci);
					}
				}
				else
				{
					ChildrenFull256 const * children = static_cast<ChildrenFull256 const *>(n->children);

					for LOOP(i,256)
					{
						int ci = children->nodes[i];
						childCount += ProcessChildCount(&best_node,&best_count,psuffixindices,ci);
					}
				}

				psuffixindices[nn].count = childCount;

				#if 1
				// take pointer of child with highest count
				// replace my node->ptr with the pointer with highest child count
				// -> if you don't do this, then n->ptr is just the *first* occurance of the substring
				// the prefix of the substring up to depth is the same, so for that purpose any of the kids works
				// we're trying to change it so that the following part is the most likely
				if ( best_count > 0 )
				{
					S32 best_pos = NODE(best_node).pos;
					HEAVY_ASSERT( memcmp(m_ubuf+n->pos,m_ubuf+best_pos,n->depth) == 0 );

					const_cast<SuffixNode *>(n)->pos = best_pos;
					// MIN is good for OOSROLZ : 172.8
					//const_cast<Suffix2Node *>(n)->ptr = RR_MIN(n->ptr,best_ptr);
				}
				#endif

				#if 0 // for test_wrt
				// rate node for WRT experiment
				if ( psuffixindices[nn].count > 2 ) // min count 3
				{
					int len = RR_MIN(n->depth,8);
					int rating = psuffixindices[nn].count * len;
					//int rating = psuffixindices[nn].count * (len - 1);
					if ( rating > best_rating &&
						is_wrt_legal_word(n->ptr,n->depth) )
					{
						best_rating = rating;
						best_rating_node = n;
					}
				}
				#endif
			}
		}

		total += psuffixindices[head].count;
	}

	// there may be nodes missing at the end
	// when the tail suffix has already occurred (eg. if end is 0000)
	//	(debug: verified total = ubufsize when that doesn't occur)
	RR_ASSERT( total <= m_ubufsize -SUFFIX2_NUM_FIRST_BYTES-2 );

	#if 0
	if ( psuffixsortdata && best_rating_node != NULL )
	{
		psuffixsortdata->best_word_len = best_rating_node->depth;
		psuffixsortdata->best_word_ptr = best_rating_node->ptr;
	}
	#endif

	//=================================================================================

	// now make the cumulative counts (.low in node)
	//	and optionally fill psuffixsortdata
	//		(psuffixsortdata lists the suffix nodes in alpha order)

	if ( psuffixsortdata )
	{
		vleaf_to_suffix.resize(m_ubufsize+1);
		psuffixsortdata->vsort.resize(m_ubufsize+1);
	}

	int suffix_count = 0;

	for LOOP(fb,firstbytes_count)
	{
		int head = pfirstbytes[fb];
		if ( head <= 0 ) // null or leaf
		{
			if ( head < 0 )
			{
				// leaf
				if ( psuffixsortdata )
				{
					psuffixsortdata->vsort[suffix_count] = -head;
					vleaf_to_suffix[-head] = suffix_count;
				}
				suffix_count++;
				RR_ASSERT( suffix_count <= total );
			}

			continue;
		}

		ASSERT( stack.empty() );
		stack.push_back( head );

		while( ! stack.empty() )
		{
			int nn = stack.back();
			ASSERT( nn != 0 );
			stack.pop_back();

			if ( nn < 0 )
			{
				// leaf
				if ( psuffixsortdata )
				{
					psuffixsortdata->vsort[suffix_count] = -nn;
					vleaf_to_suffix[-nn] = suffix_count;
				}
				suffix_count++;
				RR_ASSERT( suffix_count <= total );
			}
			else
			{
				ASSERT( nn < m_ubufsize );

				SuffixNode const * const n = & NODE(nn);

				psuffixindices[nn].low = suffix_count;

				// visit all kids :
				// push in reverse order cuz stack

				/**

				@@ alternative :
				because I already have child counts
				I could walk forward on kids
				handle leaves immediately (no push)
				push nodes with their root index
				increment suffix_count by child count as you go

				**/

				int numChildren = n->numChildrenMinusOne+1;
				ASSERT( numChildren >= 2 );

				if ( numChildren <= NUM_INLINE_CHILDREN )
				{
					SuffixNode const & children = *n;

					for LOOPBACK(i,numChildren)
					{
						int32 ci = children.nodes[i];
						RR_ASSERT( ci != 0 );
						stack.push_back(ci);
					}
				}
				else if ( numChildren <= CAPACITY1 )
				{
					ChildrenSparse1 const * children = static_cast<ChildrenSparse1 const *>(n->children);

					for LOOPBACK(i,numChildren)
					{
						int ci = children->nodes[i];
						RR_ASSERT( ci != 0 );
						stack.push_back(ci);
					}
				}
				else if ( numChildren <= CAPACITY2 )
				{
					ChildrenSparse2 const * children = static_cast<ChildrenSparse2 const *>(n->children);

					ChildrenSparse2 * mchildren = const_cast<ChildrenSparse2 *>(children);
					ChildrenSparse_Sort(mchildren,numChildren);

					for LOOPBACK(i,numChildren)
					{
						int ci = children->nodes[i];
						RR_ASSERT( ci != 0 );
						stack.push_back(ci);
					}
				}
				else if ( numChildren <= CAPACITYSI )
				{
					ChildrenSparseI const * children = static_cast<ChildrenSparseI const *>(n->children);

					ChildrenSparseI * mchildren = const_cast<ChildrenSparseI *>(children);
					ChildrenSparseI_Sort(mchildren,numChildren);

					for LOOPBACK(i,numChildren)
					{
						int ci = children->nodes[i];
						RR_ASSERT( ci != 0 );
						stack.push_back(ci);
					}
				}
				else
				{
					ChildrenFull256 const * children = static_cast<ChildrenFull256 const *>(n->children);

					for LOOPBACK(i,256)
					{
						int ci = children->nodes[i];
						if ( ci )
							stack.push_back(ci);
					}
				}
			}
		}
	}

	RR_ASSERT( suffix_count == total );

	if ( psuffixsortdata )
	{
		psuffixsortdata->vsort.resize(suffix_count);
		psuffixsortdata->count = suffix_count;

		// make the o0 suffix counts :

		psuffixsortdata->o0_cumcounts.resize(257);
		psuffixsortdata->o0_excludes.resize(256);

		for(int c=0;c<256;c++)
		{
			#if SUFFIX2_NUM_FIRST_BYTES == 2
			int fb_begin = c<<8;
			int fb_end = (c+1)<<8;
			#else
			int fb_begin = c<<16;
			int fb_end = (c+1)<<16;
			#endif

			psuffixsortdata->o0_excludes[c].init();

			int first_suffix = -1;

			for(int fb= fb_begin; fb<fb_end; fb++)
			{
				int head = pfirstbytes[fb];
				if ( head == 0 )
					continue;

				if ( first_suffix < 0 )
				{
					if ( head < 0 ) // leaf
					{
						first_suffix = vleaf_to_suffix[-head];
					}
					else
					{
						first_suffix = psuffixindices[head].low;
					}
				}

				// set o0_excludes flag for all fb entries that aren't null

				// this part assumes 2-byte fb :
				#if SUFFIX2_NUM_FIRST_BYTES == 2
				int follow_char = fb&0xFF;
				#else
				int follow_char = (fb>>8)&0xFF;
				#endif
				psuffixsortdata->o0_excludes[c].set( follow_char );
			}

			psuffixsortdata->o0_cumcounts[c] = first_suffix;
		}

		psuffixsortdata->o0_cumcounts[256] = suffix_count;

		// fill empties from their successor :
		for LOOPBACK(c,256)
		{
			if ( psuffixsortdata->o0_cumcounts[c] == -1 )
			{
				psuffixsortdata->o0_cumcounts[c] = psuffixsortdata->o0_cumcounts[c+1];
			}
		}

		RR_ASSERT( psuffixsortdata->o0_cumcounts[0] == 0 );
	}
}

struct LookupResult
{
	int node,parent;
	const U8 * ptr;
	int length;
};

bool Lookup(const U8 * ptr,int maxml, LookupResult * pResult) const
{
	const U8 * ubuf = m_ubuf;
	const U8 * ubufend = m_ubuf + m_ubufsize;

	// SUFFIX2_NUM_FIRST_BYTES == 2 jump-in requires 256k
	// SUFFIX2_NUM_FIRST_BYTES == 3 jump-in requires 64 MB
	RR_COMPILER_ASSERT( SUFFIX2_NUM_FIRST_BYTES == 2 || SUFFIX2_NUM_FIRST_BYTES == 3 );
	const int * pfirstbytes = vfirstbytes.data();

	SuffixNode const * const * const pnode_chunks = node_chunks.data();

	ASSERT( maxml >= SUFFIX2_NUM_FIRST_BYTES );


		int cur,curml;
		int parent = 0;

		// no follow, need to use pfirstbytes
		{
			#if SUFFIX2_NUM_FIRST_BYTES == 2
			uint32 firstbytes = (ptr[0]<<8) | ptr[1];
			#else
			uint32 firstbytes = (ptr[0]<<16) | (ptr[1]<<8) | ptr[2];
			#endif

			cur = pfirstbytes[firstbytes];
			if ( cur == 0 )
			{
				// no nodes; add me as a leaf :
				return false;
			}

			curml = SUFFIX2_NUM_FIRST_BYTES;
			//int maxml = MIN( (size-pos) , Suffix5_MAX_ML );

			// table points directly at a leaf :
			if ( cur < 0 )
			{
				ASSERT( parent == 0 );
				goto promote_leaf_to_node;
			}
			else // node
			{
				// check child depth and see if I can reach it
				SuffixNode const * pchild = & NODE(cur);
				HEAVY_ASSERT( memcmp(ptr,pchild->ptr,curml) == 0 );
				int32 child_depth = pchild->depth;

				#if DO_GETMATCHLEN_CHILD_DESCENT

				int curmaxml = RR_MIN(child_depth,maxml);
				const U8 * ptrend = ptr + curmaxml;
				int moreml = getmatchlen_mml1(ptr + curml,pchild->ptr + curml,ptrend);
				curml += moreml;
				RR_ASSERT( curml <= curmaxml );

				if ( curml < child_depth )
				{
					// didn't make it
					ASSERT( parent == 0 );
					goto make_branch_node;
				}

				#else

				const U8 * pchild_ptr = ubuf + pchild->pos;
				while( curml < child_depth )
				{
					if ( curml < maxml && pchild_ptr+curml < ubufend && ptr[curml] == pchild_ptr[curml] )
					{
						curml++;
					}
					else
					{
						// didn't make it
						ASSERT( parent == 0 );
						goto make_branch_node;
					}
				}

				#endif

				// made it, go to child
				// 'cur' is child already
			}
		}

		for(;;)
		{
			search_cur_node:

			if ( curml >= maxml )
				goto search_done;

			// I'm looking for this char to select a child :
			uint8 select = ptr[curml];

			ASSERT( cur > 0 ); // a node index
			SuffixNode const * n = & NODE(cur);
			ASSERT( curml == (int)n->depth );
			HEAVY_ASSERT( memcmp(ptr,n->ptr,curml) == 0 );
			//int numChildren = n->numChildren;

			int numChildren = n->numChildrenMinusOne+1;
			ASSERT( numChildren >= 2 );

			int32 const * pchildIndex;

			if ( numChildren <= NUM_INLINE_CHILDREN )
			{
				SuffixNode const & children = *n;

				for(int i=0;i<(numChildren);i++)
				{
					if ( children.iselect[i] == select )
					{
						pchildIndex = &(children.nodes[i]);
						goto found_pchild;
					}
				}

				// did not find child
				goto search_done;
			}
			else if ( numChildren <= CAPACITY1 )
			{
				ChildrenSparse1 const * children = static_cast<ChildrenSparse1 const *>(n->children);
				pchildIndex = ChildrenSparse_Find(children,numChildren,select);
				if ( pchildIndex )
				{
					goto found_pchild;
				}
				// not found, add it

				goto search_done;
			}
			else if ( numChildren <= CAPACITY2 )
			{
				ChildrenSparse2 const * children = static_cast<ChildrenSparse2 const *>(n->children);
				pchildIndex = ChildrenSparse_Find(children,numChildren,select);
				if ( pchildIndex )
				{
					goto found_pchild;
				}
				// not found, add it
				goto search_done;
			}
			else if ( numChildren <= CAPACITYSI )
			{
				// Sparse-Indirect
				ChildrenSparseI const * pSI = static_cast<ChildrenSparseI const *>(n->children);
				uint8 idx = pSI->sel2ind[select];
				if ( !idx )
					goto search_done;

				pchildIndex = &(pSI->nodes[idx - 1]);
				RR_ASSERT( *pchildIndex != 0 );
				goto found_pchild;
			}
			else
			{
				ChildrenFull256 const * children = static_cast<ChildrenFull256 const *>(n->children);

				pchildIndex = &( children->nodes[select] );

				if ( *pchildIndex == 0 )
				{
					goto search_done;
				}
				else
				{
					goto found_pchild;
				}
			}

			if ( 0 )
			{
				found_pchild:

				int child = *pchildIndex;

				if ( child < 0 )
				{
					// previous leaf found, change to a node
					//*pchildIndex = pos;
					parent = cur;
					cur = child;
					curml++; // they match the one branch at least
					goto promote_leaf_to_node;
				}
				else
				{
					// node found
					curml++;
					// check child depth and see if I can reach it
					SuffixNode const * pchild = & NODE(child);
					int child_depth = pchild->depth;

					RR_ASSERT( child_depth <= rrPtrDiff32(ubufend - (ubuf+pchild->pos)) );

					#if DO_GETMATCHLEN_CHILD_DESCENT

					int curmaxml = RR_MIN(child_depth,maxml); //, rrPtrDiff32(ubufend - pchild->ptr) );
					const U8 * ptrend = ptr + curmaxml;
					int moreml = getmatchlen_mml1(ptr + curml,pchild->ptr + curml,ptrend);
					curml += moreml;
					RR_ASSERT( curml <= curmaxml );

					if ( curml < child_depth )
					{
						// didn't make it
						parent = cur;
						cur = child;
						goto make_branch_node;
					}

					#else

					const U8 * pchild_ptr = ubuf + pchild->pos;
					while( curml < child_depth )
					{
						if ( curml < maxml && pchild_ptr+curml < ubufend && ptr[curml] == pchild_ptr[curml] )
						{
							curml++;
						}
						else
						{
							// didn't make it
							parent = cur;
							cur = child;
							goto make_branch_node;
						}
					}

					#endif

					// made it, go to child
					cur = child;
					goto search_cur_node;
				}
			}
		}

		const uint8 * match_ptr;

		search_done:
		// curml is the longest match
		// and cur is the deepest node
		ASSERT( cur > 0 );
		ASSERT( curml == (int)NODE(cur).depth );
		match_ptr = ubuf + NODE(cur).pos;
		//parent = cur;
		parent = NODE(cur).parent;

		if ( 0 )
		{
			if ( 0 )
			{
				promote_leaf_to_node:

				// cur was set to the leaf I found
				// adding pos
				ASSERT( cur < 0 );
				int other_leaf = cur;
				ASSERT( other_leaf < 0 );
				const uint8 * other_leaf_ptr = ubuf - other_leaf;

				match_ptr = other_leaf_ptr;

				// make one node with depth of shared portion
				//	and point it at the two leaves

				// while they match, add 1 child nodes
				while( curml < maxml && match_ptr+curml < ubufend && ptr[curml] == match_ptr[curml] )
				{
					curml++;
				}

				ASSERT( parent != cur );
			}

			if ( 0 )
			{
				make_branch_node:

				// cur is the child I was trying to reach and didn't make it
				// curml is how much I matched
				// new node has already been pointed at

				ASSERT( cur > 0 );
				int deep_node = cur;
				ASSERT( deep_node > 0 );
				const uint8 * deep_node_ptr = ubuf + NODE(deep_node).pos;
				ASSERT( curml == maxml || ptr[curml] != deep_node_ptr[curml] );
				ASSERT( curml < (int)NODE(deep_node).depth );

				match_ptr = deep_node_ptr;

				ASSERT( parent != cur );
			}

			// match_ptr
			// curml
			// parent
		}

		//-----------------------------
		// all matches wind up here

		ASSERT( memcmp(ptr,match_ptr,curml) == 0 );

		ASSERT( curml >= SUFFIX2_NUM_FIRST_BYTES );

		// curml should be max :
		ASSERT( curml == maxml || match_ptr+curml >= ubufend || ptr[curml] != match_ptr[curml] );

		pResult->node = cur;
		pResult->parent = parent;
		pResult->length = curml;
		pResult->ptr = match_ptr;

	return true;
}

void LookupMatches(const U8 * ptr,int maxml, vector<SuffixTrieMatch> * pMatches) const
{
	LookupResult lr;
	if ( ! Lookup(ptr,maxml,&lr) )
		return;

	// put the match pairs :

	const U8 * ubuf = m_ubuf;
	int curml = lr.length;
	const U8 * match_ptr = lr.ptr;
	int curnode = lr.node;
	int parent = lr.parent;

	pMatches->push_back();
	{
	SuffixTrieMatch & ump = pMatches->back();
	ump.length = curml;
	ump.ptr = match_ptr;
	ump.count = GetChildCount(vsuffixindices.data(),curnode);
	}

	SuffixNode const * const * const pnode_chunks = node_chunks.data();

	// get shorter matches :
	while( parent != 0 )
	{
		SuffixNode const * n = &NODE(parent);

		match_ptr = ubuf+n->pos;
		curml = n->depth;

		// extend ml from depth :
		/*
		while( curml < maxml && match_ptr+curml < ubufend && ptr[curml] == match_ptr[curml] )
		{
			curml++;
		}
		*/

		pMatches->push_back();
		SuffixTrieMatch & ump = pMatches->back();
		ump.length = curml;
		ump.ptr = match_ptr;
		ump.count = GetChildCount(vsuffixindices.data(),parent);
		RR_ASSERT( ump.count > pMatches->at( pMatches->size() - 2 ).count );

		// step back :
		parent = n->parent;
	}

	// shortest should be the head :
	ASSERT( pMatches->back().length >= SUFFIX2_NUM_FIRST_BYTES );

	// remove count that is in the longer match :
	for(int m=pMatches->size32()-1; m>=1;m--)
	{
		SuffixTrieMatch & cur = pMatches->at(m);
		SuffixTrieMatch & longer = pMatches->at(m-1);
		ASSERT( cur.length < longer.length );
		ASSERT( cur.count > longer.count );
		cur.count -= longer.count;
	}

	return;
}

bool LookupSuffix(const U8 * ptr,int maxml, SuffixTrieMatch * pMatch,BitFlags256 * pFollowExcludes) const
{
	LookupResult lr;
	if ( ! Lookup(ptr,maxml,&lr) )
		return false;

	SuffixNode const * const * const pnode_chunks = node_chunks.data();

	pMatch->ptr = lr.ptr;
	pMatch->length = lr.length;

	int nn = lr.node;
	if ( nn > 0 )
	{
		// node

		const SuffixIndex & SI = vsuffixindices[nn];
		pMatch->count = SI.count;
		pMatch->suffix_index = SI.low;

		if ( pFollowExcludes )
		{
			pFollowExcludes->init();

			if ( lr.ptr+lr.length < m_ubuf+m_ubufsize )
			{
				SuffixNode const * const n = &NODE(nn);
				RR_ASSERT( n->depth >= lr.length );

				if ( n->depth == lr.length )
				{
					// node is a branch right at our level :

					// set flags for all node children :

					int numChildren = n->numChildrenMinusOne+1;
					ASSERT( numChildren >= 2 );

					if ( numChildren <= NUM_INLINE_CHILDREN )
					{
						SuffixNode const & children = *n;

						for LOOPBACK(i,numChildren)
						{
							int s = children.iselect[i];
							pFollowExcludes->set(s);
						}
					}
					else if ( numChildren <= CAPACITY1 )
					{
						ChildrenSparse1 const * children = static_cast<ChildrenSparse1 const *>(n->children);

						for LOOPBACK(i,numChildren)
						{
							int s = children->select[i];
							pFollowExcludes->set(s);
						}
					}
					else if ( numChildren <= CAPACITY2 )
					{
						ChildrenSparse2 const * children = static_cast<ChildrenSparse2 const *>(n->children);

						for LOOPBACK(i,numChildren)
						{
							int s = children->select[i];
							pFollowExcludes->set(s);
						}
					}
					else if ( numChildren <= CAPACITYSI )
					{
						ChildrenSparseI const * children = static_cast<ChildrenSparseI const *>(n->children);

						for LOOPBACK(i,256)
						{
							int ci = children->sel2ind[i];
							if ( ci != 0 )
							{
								pFollowExcludes->set(i);
							}
						}
					}
					else
					{
						ChildrenFull256 const * children = static_cast<ChildrenFull256 const *>(n->children);

						for LOOPBACK(i,256)
						{
							int ci = children->nodes[i];
							if ( ci != 0 )
							{
								pFollowExcludes->set(i);
							}
						}
					}

					RR_DURING_ASSERT( int exclude = lr.ptr[lr.length] );
					RR_ASSERT( pFollowExcludes->test( exclude ) );
				}
				else
				{
					int p = n->parent;
					if ( p > 0 )
					{
						RR_ASSERT( NODE(p).depth < lr.length );
					}

					// single next char on the leaf string is set :
					int exclude = lr.ptr[lr.length];
					pFollowExcludes->set( exclude );
				}
			}
		}
	}
	else
	{
		// leaf
		pMatch->count = 1;
		pMatch->suffix_index = vleaf_to_suffix[-nn];

		if ( pFollowExcludes )
		{
			pFollowExcludes->init();

			if ( lr.ptr+lr.length < m_ubuf+m_ubufsize )
			{
				// single next char on the leaf string is set :
				int exclude = lr.ptr[lr.length];
				pFollowExcludes->set( exclude );
			}
		}
	}

	#ifdef RR_DO_ASSERTS
	if ( lr.parent != 0 )
	{
		// check that parent encloses child
		const SuffixIndex & parent_SI = vsuffixindices[lr.parent];
		RR_ASSERT( parent_SI.low <= pMatch->suffix_index );
		RR_ASSERT( parent_SI.low + parent_SI.count >= pMatch->suffix_index + pMatch->count );
	}
	#endif

	return true;
}

#undef NODE

};

//===========================================================

#undef CAPACITY1
#undef CAPACITY2
#undef CAPACITYGG
#undef NUM_INLINE_CHILDREN
#undef DO_UPDATE_FOLLOW
