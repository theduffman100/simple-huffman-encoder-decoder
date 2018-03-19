/*
 * Very basic Huffman compressor/decompressor
 * Developped by the Duff Man
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Structure defining the status of a bit stream (a stream accepting
 * bit sequences).
 * file is the source or destination stream
 * curChar is the current byte buffer being written/decoded
 * nBit is the current bit position being written/decoded
 */
typedef struct
{
	FILE *file;
	int curChar;
	int nBit;
} BitStream;

/*
 * Initialize a bit stream with a file stream
 */
void initBits (BitStream *str,FILE *file)
{
	str->file = file;
	str->curChar = 0;
	str->nBit = 0;
}

/*
 * flush the last byte to the bit stream (use for ouput only)
 */
void closeBits(BitStream *str)
{
	if (str->nBit>0)
	{
		putc (str->curChar,str->file);
		str->curChar = 0;
	}
}

/*
 * put some bits into the bit stream
 * val is the value
 * nBit is the number of bits (from the lower order) to put
 * str is the stream status structure
 */
void putBits (long val, int nBit, BitStream *str)
{
	int toSend;
	long valSend;
	while (nBit>0)
	{
		if (str->nBit==8)
		{
			putc (str->curChar,str->file);
			str->curChar = 0;
			str->nBit = 0;
		}
		toSend = 8 - str->nBit;
		if (toSend>nBit)
			toSend = nBit;
		valSend = (val >> (nBit - toSend)) & ((1 << nBit)-1);
		str->curChar |= (int)valSend << (8 - str->nBit - toSend);
		str->nBit += toSend;
		nBit -= toSend;
	}
}

/*
 * Read a number of bits from the bit stream
 * str is the input bit stream status
 * nBit is the number of bits to read from the lower order
 */
long getBits (BitStream *str,int nBit)
{
	long ret = 0;
	int toGet;
	while (nBit>0)
	{
		toGet = str->nBit;
		if (toGet==0)
		{
			str->curChar = getc(str->file);
			if (str->curChar==EOF)
				return -1;
			toGet = str->nBit = 8;
		}
		if (nBit<toGet) toGet = nBit;
		ret <<= toGet;
		ret |= (str->curChar >> (str->nBit-toGet)) & ((1 << toGet)-1);
		nBit -= toGet;
		str->nBit -= toGet;
	}
	return ret;
}

/*
 * A Simple Heap (Priority queue) implementation
 * This structure storing a partially ordered set of integers
 * Every item has a priority value and the set is arranged so
 * that any item's priority is larger than its children items priority
 * (The children are located at 2x and 2x+1 relative to its index)
 * The putPrio and getPrio ensure that this rule is always respected
 * when changes are performed. Every operation has a log-n order.
 */
typedef struct
{
	int top;
	int *data;
	int *priority;
} PriorityQueue;

/*
 * Allocate space for a priority queue
 */
void initPrio(PriorityQueue *prio, int nbr)
{
	prio->top = 1;
	prio->data = (int*)malloc((nbr+2)*sizeof(int));
	prio->priority = (int*)malloc((nbr+2)*sizeof(int));
}

/*
 * free space for a priority queue
 */
void deletePrio (PriorityQueue *prio)
{
	free(prio->priority);
	free(prio->data);
}

/*
 * Get an item from a priority queue and ensure that the priority
 * rule is respected
 */
int getPrio (PriorityQueue *prio)
{
	int ret = prio->data[1];
	int i2,i3;
	prio->data[0] = prio->data[1];
	prio->priority[0] = prio->priority[1];
	if (prio->top<=1) return -1;
	prio->top--;
	for (i2=1;;i2=i3)
	{
		i3 = i2 << 1;
		if (i3>=prio->top) break;
		if (prio->priority[i3]<prio->priority[i3+1])
			i3++;
		if (prio->priority[i3]<=prio->priority[prio->top])
			break;
		prio->data[i2] = prio->data[i3];
		prio->priority[i2] = prio->priority[i3];
	}
	prio->data[i2] = prio->data[prio->top];
	prio->priority[i2] = prio->priority[prio->top];
	return ret;
}

/*
 * Put an item into a priority queue and ensure that the priority
 * rule is respected
 */
void putPrio (int data,int priority,PriorityQueue *prio)
{
	int i2,i3;
	for (i2=prio->top++;i2>1;i2=i3)
	{
		i3 = i2 >> 1;
		if (prio->priority[i3]>priority)
			break;
		prio->priority[i2] = prio->priority[i3];
		prio->data[i2] = prio->data[i3];
	}
	prio->priority[i2] = priority;
	prio->data[i2] = data;
}

/*
 * Generate prefix codes from code lengths in bits
 * A set of prefix codes contains bit strings for wich no item
 * is identical to a substring at the beginning of another item.
 * This allows us to know when a code is complete even if we do not
 * know the position of the end of the code in the stream.
 */
void generatePrefix (long table [], int length [], int nbr)
{
	int i,nbrPre,codePre = 0;
	PriorityQueue prio;
	initPrio(&prio,nbr);
	/*
	 * Put the codes with priorities in the heap.
	 * The priority is the length but we also take into account
	 * the index because we want the result to be the same
	 * if we change the code.
	 */
	for (i=0;i<nbr;i++)
		if (length[i]>0)
			putPrio(i,length[i]*nbr+i,&prio);
	i = getPrio(&prio);
	nbrPre = length[i];
	table[i] = codePre;
	/*
	 * Set codes for the indexes, starting from the longest
	 */
	while (prio.top>1)
	{
		i = getPrio(&prio);
		codePre = ((codePre>>(nbrPre-length[i])))+1;
		nbrPre = length[i];
		table[i] = codePre;
	}
	deletePrio (&prio);
}

/*
 * Generate optimal code length for the given frequencies
 * we used an algorithm similar to Huffman's, using
 * a priority queue for efficiency
 */
void generateLength (int length [],int freq [], int nbr)
{
	int i,c,f;
	int *parent = (int*)malloc(2*nbr*sizeof(int));
	PriorityQueue prio;
	initPrio(&prio,2*nbr);
	/*
	 * Store all the indexes and frequencies in a priority queue.
	 */
	for (i=0;i<nbr;i++)
		putPrio (i,-freq[i],&prio);
	/*
	 * Combine repeatedlythe two least frequent index to make a
	 * composite node
	 */
	for (;prio.top>2;i++)
	{
		c = getPrio (&prio);
		f = prio.priority[0];
		parent[c] = i;
		c = getPrio (&prio);
		f += prio.priority[0];
		parent[c] = i;

		/* f-1 -> Lower priority for composite nodes to distribute length
		 * equally among equally frequent indexes
		 */
		putPrio (i,f-1,&prio);
	}
	--i;
	/*
	 * Now go backwards and store code lengths in parent
	 * Note that it is OK to discard the old parent because
	 * the parent index is always larger than the child
	 */
	parent[i--] = 0;
	for (;i>=0;i--)
		parent[i] = parent[parent[i]]+1;
	for (i=0;i<nbr;i++) length[i] = parent[i];
	free (parent);
}

/*
 * This is a structure used to write to a compressed stream in a
 * sequential manner. This is useful if you don't want to take care of
 * the buffering process.
 */
typedef struct
{
	FILE *file;
	char *buffer;
	int bufSize,curIndex;
} HOutStream;

struct HDecodeNode
{
	int n0, n1;
};

/*
 * This is a structure used to access the data from a compressed stream
 * in a sequential manner. This is useful if you don't want to create
 * temporary files for uncompressed data: You use the stream as if it
 * were a file.
 */
typedef struct
{
	BitStream bs;
	struct HDecodeNode decode[256];
	int nbr;
} HInStream;

/*
 * This function writes a buffer to a compressed file
 */
void writeHBuffer (FILE *file, char *buffer,int nbr)
{
	int freq[256];
	int length[256];
	long code[256];
	BitStream bs;
	int i;
	for (i=0;i<256;i++) freq[i] = 0;
	for (i=0;i<nbr;i++)
		freq[(unsigned char)buffer[i]]++;
	generateLength(length,freq,256);
	generatePrefix(code,length,256);
	initBits(&bs,file);
	putBits(nbr,16,&bs);
	for (i=0;i<256;i++)
		putBits(length[i],4,&bs);
	for (i=0;i<nbr;i++)
		putBits(code[(unsigned char)buffer[i]],length[(unsigned char)buffer[i]],&bs);
	closeBits(&bs);
}

/*
 * Create a buffer for an output file
 */
void initHufOutput (HOutStream *str,FILE *file,int size)
{
	str->file = file;
	str->bufSize = size;
	str->buffer = (char*)malloc(size);
	str->curIndex = 0;
}

/*
 * Flush the pending data and the end flag, and flush the buffer space
 */
void closeHufOutput (HOutStream *str)
{
	if (str->curIndex>0)
		writeHBuffer(str->file,str->buffer,str->curIndex);
	/*
	 * Two zero bytes mean an empty block (end of blocks)
	 */
	fputc(0,str->file);
	fputc(0,str->file);
	free (str->buffer);
}

/*
 * Add a character to the stream and flush if the buffer is full
 */
void writeHufOutput(int c, HOutStream *str)
{
	if (str->curIndex>=str->bufSize)
	{
		writeHBuffer(str->file,str->buffer,str->curIndex);
		str->curIndex = 0;
	}
	str->buffer[str->curIndex++] = c;
}

/*
 * Build a tree-structured lookup table for Huffman codes.
 * The tree structure is linked by indexes. An index smaller than nbr
 * refers to another node in the tree. An index larger than nbr refers
 * to a character (substract 256).
 */
void buildDecodeTable (struct HDecodeNode decode[], long code[],int length [],int nbr)
{
	int i,j, top=1,cur;
	decode[0].n0 = decode[0].n1 = -1;
	for (i=0;i<nbr;i++)
	{
		cur = 0;
		for (j=length[i]-1;j>0;j--)
		{
			if ((code[i]>>j)&1)
			{
				if (decode[cur].n1<0)
				{
					decode[top].n0 = decode[top].n1 = -1;
					decode[cur].n1 = top++;
				}
				cur = decode[cur].n1;
			}
			else
			{
				if (decode[cur].n0<0)
				{
					decode[top].n0 = decode[top].n1 = -1;
					decode[cur].n0 = top++;
				}
				cur = decode[cur].n0;
			}
		}
		if (code[i] & 1)
			decode[cur].n1 = nbr+i;
		else decode[cur].n0 = nbr+i;
	}
}

/*
 * Initialize the input from an Huffman stream
 */
void initHufInput (HInStream *str, FILE *in)
{
	initBits(&str->bs,in);
	str->nbr = 0;
}

/*
 * Dummy close function for stream
 */
void closeHufInput(HInStream *str)
{
}

/*
 * Get a character from a Huffman stream
 */
int getHufInput(HInStream *str)
{
	int i;
	int length[256];
	long code[256];
	if (str->nbr==-1) return EOF;
	if (str->nbr==0)
	{
		str->bs.nBit = 0;
		str->nbr = (int)getBits(&str->bs,16);
		if (str->nbr<=0)
		{
			str->nbr = -1;
			return EOF;
		}
		else
		{
			for (i=0;i<256;i++)
				length[i] = (int)getBits(&str->bs,4);
			generatePrefix(code,length,256);
			buildDecodeTable(str->decode,code,length,256);
		}
	}
	i = 0;
	while (i<256)
	{
		if (getBits(&str->bs,1))
			i = str-> decode[i].n1;
		else i = str->decode[i].n0;
	}
	str->nbr--;
	return(i-256);
}

/*
 * Interpretation of the command line in the main function. Running
 * the program with no argument will show an help message.
 * The compressed file format is as follows:
 * - First 2 characters are \/
 * - Next is a list of files. For each file, we have the length of
 *   the name in a byte, the name and a list of compressed blocks.
 *   Each compressed block start with a size (zero for the last one),
 *   a 128 byte header containing the code lengths and the compressed
 *   data.
 * - The end of files can be a zero-length filename or an EOF
 *
 * Be aware that the files are extracted using their original names
 * as stored in the file. Also, as this is not a commercial product,
 * I did not put additionnal protection to avoid erasing a file
 * by mistake. The first file in the command line will be erased
 * if the -x option is not detected!
 */
int main (int argc, char *argv[])
{
	int compress = 1,f,i,firstFile = 0,nbr;
	FILE *file, *cFile;
	char fn[257];
	HInStream hin;
	HOutStream hout;

	/*
	 * Process switches and find first file name
	 */
	for (f=1;f<argc;f++)
	{
		if (argv[f][0]!='-')
		{
			if (firstFile==0) firstFile = f;
		}
		else if (argv[f][1]=='x'||argv[f][1]=='X') compress = 0;
		else fprintf(stderr,"Argument %s ignored\n",argv[f]);
	}
	if (firstFile==0)
	{
		fprintf(stderr,"Small Huffman compressor/decompressor\n");
		fprintf(stderr,"Usage: comp [-x] <compress file> [<files to compress>]\n");
		return -1;
	}

	/*
	 * Execute the command
	 */
	if (!compress)
	{
		cFile = fopen(argv[firstFile],"rb");
		if (!cFile || getc(cFile)!='\\' || getc(cFile)!='/')
		{
			fprintf(stderr,"File %s is incorrect\n",argv[firstFile]);
			return -1;
		}
		for (;;)
		{
			nbr = getc(cFile);
			if (nbr<=0) break;
			for (i=0;i<nbr;i++)
				fn[i] = getc(cFile);
			fn[i] = 0;
			initHufInput(&hin,cFile);
			file = fopen(fn,"wb");
			if (!file) fprintf(stderr,"Error extracting %s\n",fn);
			for (;;)
			{
				i = getHufInput(&hin);
				if (i==EOF) break;
				if (file) putc(i,file);
			}
			if (file) fclose(file);
			closeHufInput(&hin);
		}
		fclose(cFile);
	}
	else
	{
		cFile = fopen(argv[firstFile],"wb");
		if (!cFile)
		{
			fprintf(stderr,"Can't create file %s\n",argv[firstFile]);
			return -1;
		}
		putc('\\',cFile);
		putc('/',cFile);
		for (f=firstFile+1;f<argc;f++)
		{
			file = fopen(argv[f],"rb");
			if (!file)
			{
				fprintf(stderr,"Can't open file %s\n",argv[f]);
				continue;
			}
			putc(strlen(argv[f]),cFile);
			for (i=0;argv[f][i];i++) putc(argv[f][i],cFile);
			initHufOutput(&hout,cFile,4096);
			for (;;)
			{
				i = getc(file);
				if (i==EOF) break;
				writeHufOutput (i,&hout);
			}
			closeHufOutput(&hout);
			fclose(file);
		}
		fclose(cFile);
	}
	return 0;
}
