%{
/*==============================================================*/
/*                                                              */
/* "cue.y"                                                      */
/*                                                              */
/* Automatic Valency Frame Extractor for English Using YACC     */
/*                                                              */
/* (C) Copyright 1997 by Jochen L. Leidner <leidner@acm.org>    */
/*                                                              */
/* All rights reserved.                                         */
/*                                                              */
/* 1996-12-25   LE   Experimental implementation                */
/* 1997-10-16   LE   Adapted for Malaga output format           */
/*                                                              */
/* Ref.: Brent, Michael R.: From Grammar to Lexicon (1993):     */
/*       "Unsupervised Learning of Lexical Syntax." Computa-    */
/*       tional Linguistics 19(2), 244-262.                     */
/*                                                              */
/*==============================================================*/

%}

%start VerbComplex

%token NONE  SUBJ   OBJ  SUBJ_OBJ
%token DET   TENSED CC   PUNC
%token VBASE VING   CAP  THAT
%token TO    PREP   PRP  NOUN

%{

/*--- Definitions ----------------------------------------------*/

/* maximal length of lines in files: */
#define MAXLINE   8192

/* maximal length of file names: */
#define FNAMEMAX  8192

/* minimal verb candidate length: */
#define MINLENGTH 4

#ifndef __STDC__
#  error your compiler is not ANSI/ISO C compliant!
#endif

#define significant(x) (x) /* version without stats filtering   */

/*--- Includes -------------------------------------------------*/

#include "y.tab.h"        /* use yacc -d to generate this file! */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>


/*--- Types ----------------------------------------------------*/

typedef enum
{
  NO, YES
} yn_t;

typedef struct         /* cue lexicon entry         */
{
  char *surface;       /* cue's word-form string    */
  int tag;             /* cue's tag                 */
} cue_t;

typedef struct
{
  char *surface;       /* word-form string          */
  char *base;          /* word-form string          */
  int alternation;     /* 0/-ing alternation?       */
  int num;             /* occurrence counter        */
  int NP;              /* number of V [+NP]         */
  int cl;              /* number of V [+cl]         */
  int inf;             /* number of V [+inf-cl]     */
  int NPcl;            /* number of V [+NP]         */
  int NPinf;           /* number of V [+NP +inf-cl] */
  int NPNP;            /* number of V [+NP +NP]     */
} verb_t;

typedef struct node_s
{
  verb_t verb;         /* content structure         */
  struct node_s *next; /* link                      */
} node_t;


/*--- Constants ------------------------------------------------*/

#ifdef DEBUG
  static const int __debug = 1;
  const char *const __version =
    "@(#)$Header: cue.y 1997-12-16 leidner $";
#else
  static const int __debug = 0;
#endif

static const char *const tagset[] =
{
   "NONE",       "SUBJ",         "OBJ",          "SUBJ_OBJ",
   "DET",        "TENSED",       "CC",           "PUNC",
   "VBASE",      "VING",         "CAP",          "THAT",
   "TO",         "PREP",         "PRP",          "NOUN",
   NULL
};

static const cue_t cue[] =
{
   { "I", SUBJ },            { "he", SUBJ },           { "she", SUBJ },
   { "we", SUBJ },           { "they", SUBJ },         { "me", OBJ },
   { "him", OBJ },           { "us", OBJ },            { "them", OBJ },
   { "you", SUBJ_OBJ },      { "it", SUBJ_OBJ },       { "yours", SUBJ_OBJ },
   { "hers", SUBJ_OBJ },     { "ours", SUBJ_OBJ },     { "theirs", SUBJ_OBJ },
   { "a", DET },             { "an", DET },            { "the", DET },
   { "her", DET },           { "his", DET },           { "its", DET },
   { "my", DET },            { "our", DET },           { "their", DET },
   { "your", DET },          { "this", DET },
   { "whose", DET },         { "has", TENSED },        { "hasn't", TENSED },
   { "have", TENSED },       { "haven't", TENSED },    { "had", TENSED },
   { "hadn't", TENSED },     { "am", TENSED },         { "aren't", TENSED },
   { "is", TENSED },         { "isn't", TENSED },      { "are", TENSED },
   { "aren't", TENSED },     { "was", TENSED },        { "wasn't", TENSED },
   { "were", TENSED },       { "weren't", TENSED },    { "do", TENSED },
   { "don't", TENSED },      { "does", TENSED },       { "doesn't", TENSED },
   { "did", TENSED },        { "didn't", TENSED },     { "can", TENSED },
   { "can't", TENSED },      { "could", TENSED },      { "couldn't", TENSED },
   { "may", TENSED },        { "might", TENSED },      { "must", TENSED },
   { "mustn't", TENSED },    { "will", TENSED },       { "won't", TENSED },
   { "would", TENSED },      { "wouldn't", TENSED },   { "when", CC },
   { "before", CC },         { "after", CC },          { "as", CC },
   { "while", CC },          { "if", CC },             { ".", PUNC },
   { "?", PUNC },            { "!", PUNC },            { ",", PUNC },
   { ";", PUNC },            { ":", PUNC },            { "that", THAT },
   { "That", THAT },         { "to", TO },             { "To", TO },
   { "of", PREP },           { "on", PREP },           { "under", PREP },
   { "above", PREP },        { "below", PREP },        { "over", PREP },
   { "after", PREP },        { "beneath", PREP },      { "at", PREP },
   { "in", PREP },           { "round", PREP },        { "with", PREP },
   { "about", PREP },        { "from", PREP },         { "behind", PREP },
   { "across", PREP },       { "along", PREP },        { "near", PREP },
   { "about", PREP },
  { NULL, 0 }
};


/*--- Variables ------------------------------------------------*/

static node_t *store[256];      /* hashtable                    */

static char line[MAXLINE];      /* line read                    */

static FILE *inputFile;         /* raw text file                */
static FILE *tempFile;          /* tagged file                  */

static char    __word[MAXLINE]; /* current word-form            */
static char    __tag[MAXLINE];  /* current tag                  */
static node_t *__np;            /* current node pointer         */


/*--- Prototypes -----------------------------------------------*/

static int gettag(char *s);
static void yylexerr(void);
static int getcue(char *s);
static void add(char *s);
static void enter(char *s);
static void traverse(void (* apply)(node_t *));
static node_t *find(char *s);
static node_t *makeNode(char *surface);
static char *strsave(char *s);
static void kill(node_t *np);
static void collect(void);
static node_t *alternation(char *word);
static void base(void);
static void tag(void);
static void emitNode(node_t *np);

int yylex(void);
void yyerror(char *msg);
int main(int argc, char *argv[]);


/*--------------------------------------------------------------*/
/*--- The YACC Grammar -----------------------------------------*/
/*--------------------------------------------------------------*/

%}

%%

VerbComplex     :       Verb Slots
                ;

Slots           :       V_NP
                |       V_cl
                |       V_inf
                |       V_NP_cl
                |       V_NP_inf
                |       V_NP_NP
                ;

V_NP            :       NounPhrase StopMark
                        {
			  #ifdef DEBUG
                            fprintf(stderr, "\n[DEBUG: +NP ]\n");
			  #endif

                          if (__np)
                            __np->verb.NP++;
                          else
                            yylexerr();
                        }
                ;

NounPhrase      :       OBJ
                |       SUBJ_OBJ
                |       CAP
                ;

NounPhraseComp  :       NounPhrase ManyCapitals
                ;

ManyCapitals    :       CAP ManyCapitals
                |       CAP
                ;

StopMark        :       PUNC
                |       CC
                ;

V_cl            :       that_cl
                |       SUBJ
                |       SubjObjTensed
                        {
			  #ifdef DEBUG
                            fprintf(stderr, "\n[DEBUG: +cl ]\n");
			  #endif

			  if (__np)
                            __np->verb.cl++;
                          else
                            yylexerr();
                        }
                ;

that_cl         :       THAT DetNounPhrase
                ;

DetNounPhrase   :       DET
                |       SUBJ
                |       SUBJ_OBJ
                |       ManyCapitals
                ;

SubjObjTensed   :       SUBJ_OBJ TENSED
                ;

V_inf           :       TO Verb
                        {
			  #ifdef DEBUG
                            fprintf(stderr, "\n[DEBUG: +inf-cl ]\n");
			  #endif

			  if (__np)
                            __np->verb.inf++;
                          else
                            yylexerr();
                        }
                ;

Verb            :       VBASE
                |       VING
                        { /* do not add side effects here (recursion!) */ }
                ;

V_NP_cl         :       NounPhraseComp V_cl
                        {
			  #ifdef DEBUG
                            fprintf(stderr, "\n[DEBUG: +NP +cl ]\n");
			  #endif

			  if (__np)
                            __np->verb.NPcl++;
                          else
                            yylexerr();
                        }
                ;

V_NP_inf        :       NounPhraseComp V_inf
                        {
			  #ifdef DEBUG
                            fprintf(stderr, "\n[DEBUG: +NP +inf-cl ]\n");
			  #endif

			  if (__np)
                            __np->verb.NPinf++;
                          else
                            yylexerr();
                        }
                ;

V_NP_NP         :       NounPhraseComp V_NP
                        {
			  #ifdef DEBUG
                            fprintf(stderr, "\n[DEBUG: +NP +NP ]\n");
			  #endif

			  if (__np)
                            __np->verb.NPNP++;
                          else
                            yylexerr();
                        }
                ;
%%

/*--------------------------------------------------------------*/
/*--- C Functions ----------------------------------------------*/
/*--------------------------------------------------------------*/

/*--------------------------------------------------------------*/
/* gettag()                                                     */
/*                                                              */
/* map a tag's name string to a tag                             */
/*--------------------------------------------------------------*/

static int gettag(char *s)
{
   if (strcmp(s, "NONE") == 0)     return NONE;
   if (strcmp(s, "SUBJ") == 0)     return SUBJ;
   if (strcmp(s, "OBJ") == 0)      return OBJ;
   if (strcmp(s, "SUBJ_OBJ") == 0) return SUBJ_OBJ;
   if (strcmp(s, "DET") == 0)      return DET;
   if (strcmp(s, "TENSED") == 0)   return TENSED;
   if (strcmp(s, "CC") == 0)       return CC;
   if (strcmp(s, "PUNC") == 0)     return PUNC;
   if (strcmp(s, "VBASE") == 0)    return VBASE;
   if (strcmp(s, "VING") == 0)     return VING;
   if (strcmp(s, "CAP") == 0)      return CAP;
   if (strcmp(s, "THAT") == 0)     return THAT;
   if (strcmp(s, "TO") == 0)       return TO;
   if (strcmp(s, "PREP") == 0)     return PREP;
   if (strcmp(s, "PRP") == 0)      return PRP;
   if (strcmp(s, "NOUN") == 0)     return NOUN;
   
   fprintf(stderr, "gettag(): unknown tag\n");
   
   return NONE;
}

/*--------------------------------------------------------------*/
/* yylexerr()                                                   */
/*                                                              */
/* should never be called                                       */
/*--------------------------------------------------------------*/

static void yylexerr(void)
{
   fprintf(stderr, "yylex(): internal error\n");
}

/*--------------------------------------------------------------*/
/* yylex()                                                      */
/*                                                              */
/* scanner                                                      */
/*--------------------------------------------------------------*/

int yylex(void)
{
   char *cp;
   int c, t;
   
   cp = __word;
   
  while ( isspace((c = fgetc(tempFile))) )
     ;
  
  do
  {
     *cp++ = c;
     
     if (feof(tempFile))
	return EOF;
     
  }
  while( (c = fgetc(tempFile)) != '_' );
  /* in input files no '_' is allowed within words !! */
  
  *cp = '\0';
  
  #ifdef DEBUG
    fprintf(stderr, "\t[DEBUG:__word=\"%s\"] ", __word);
  #endif
  
  cp = __tag;
  
  while( !isspace(c = fgetc(tempFile)) )
  {
     *cp++ = c;
     
     if (feof(tempFile))
	return EOF;
  }
  
  *cp = '\0';
  
  #ifdef DEBUG
    fprintf(stderr, "\t[DEBUG:__tag=\"%s\"]\n", __tag);
  #endif

  t = gettag(__tag);

  if (t == VING)
  {
     __np = find(__word);
     
     if(__np)
	__np->verb.num++;
     else
	yylexerr();
     
  }
  else
  {
     if (t == VBASE)
     {
	__np = alternation(__word);
	
	if(__np)
	   __np->verb.num++;
	else
	   yylexerr();
     }
  }
  
  return t;
}

/*--------------------------------------------------------------*/
/* yyerror()                                                    */
/*                                                              */
/* called if no cues matched; find next parsable island         */
/*--------------------------------------------------------------*/

void yyerror(char *msg)
{
  #ifdef DEBUG
    fprintf(stderr, "[DEBUG:yyerror(): '%s'] ", msg);
  #endif

  /* swim to the next parsable `island'
     which is the beginning of the next physical input line
     which is a verb due to the tagging output format (!): */
    
  (void)fgets(line, MAXLINE - 1, tempFile);
}

/*--------------------------------------------------------------*/
/* getcue()                                                     */
/*                                                              */
/* return the tag of a cue string                               */
/*--------------------------------------------------------------*/

static int getcue(char *s)
{
   cue_t *cp = (cue_t *)cue;

   while ( cp->surface != NULL )
   {
      if ( strcmp(cp->surface, s) == 0 )
	 return cp->tag;
      
      cp++;
   }
   
   return NONE;
}

/*--------------------------------------------------------------*/
/* add()                                                        */
/*                                                              */
/* store a string into the dynamic hashtable                    */
/*--------------------------------------------------------------*/

static void add(char *s)
{
   int index = *s;
   node_t *temp = store[index];
   
   store[index] = makeNode(s);
   store[index]->next = temp;
}

/*--------------------------------------------------------------*/
/* enter()                                                      */
/*                                                              */
/* enter a string into the hashtable if it's not already there  */
/*--------------------------------------------------------------*/

static void enter(char *s)
{
   node_t *np = find(s);
   
   if ( np == NULL )
      add(s);
}

/*--------------------------------------------------------------*/
/* traverse()                                                   */
/*                                                              */
/* walk through the hashtable and apply a function to every     */
/* node                                                         */
/*--------------------------------------------------------------*/

static void traverse(void (* apply)(node_t *))
{
   int index;

   for (index = 'a'; index <= 'z'; index++)
   {
      node_t *np = store[index];
      
      while ( np )
      {
	 if ( np != NULL )
	    apply(np);
	 
	  np = np->next;
      }
   }
}

/*--------------------------------------------------------------*/
/* find()                                                       */
/*                                                              */
/* return pointer to node that has a given string as its key    */
/* or NULL if it is not found                                   */
/*--------------------------------------------------------------*/

static node_t *find(char *s)
{
   int index = *s;
   
   node_t *np = store[index];
   
  while ( np )
  {
     if ( strcmp(np->verb.surface, s) == 0 )
	return np;
     
     np = np->next;
  }
  
  return NULL;
}

/*--------------------------------------------------------------*/
/* makeNode()                                                   */
/*                                                              */
/* allocate memory for one node and initialize it with a given  */
/* string                                                       */
/*--------------------------------------------------------------*/

static node_t *makeNode(char *surface)
{
   node_t *np = calloc(1, sizeof(node_t));
   
   if ( np == NULL )
   {
      fprintf(stderr, "\nmakeNode(): out of memory error\n");
      exit(20);
   }
   
   np->verb.surface = strsave(surface); /* CONTENT(node) */
   np->verb.alternation = NO;
   np->next = NULL;                     /* LINK(node)    */
   
   return np;
}

/*--------------------------------------------------------------*/
/* strsave()                                                    */
/*                                                              */
/* allocate memory for a given string and store a copy of it    */
/*--------------------------------------------------------------*/

static char *strsave(char *s)
{
   char *mem = calloc(strlen(s)+1, sizeof(char));
   
   if ( mem != NULL )
      return strcpy(mem, s);
   else
   {
      fprintf(stderr, "\nstrsave(): out of memory error\n");
      exit(20);
   }
}

/*--------------------------------------------------------------*/
/* kill()                                                       */
/*                                                              */
/* remove a given node from the hashtable                       */
/*--------------------------------------------------------------*/

static void kill(node_t *np)
{
   if ( (np != NULL) && (np->verb.alternation == 0) )
   {
      if ( np->verb.surface != NULL )
	 np->verb.surface[0] = '\0';
   }
}

/*--------------------------------------------------------------*/
/* printNode()                                           DEBUG  */
/*                                                              */
/* print the content of a node for debugging purposes           */
/*--------------------------------------------------------------*/

#ifdef DEBUG

static void printNode(node_t *np)
{
   if (np)
   {
      fprintf( stderr, "\n[DEBUG: surface=%s\n\tbase=%s\n\tnum=%d"
               "\n\tNP=%d\n\tcl=%d\n\tinf=%d\n\tNPcl=%d\n"
               "\tNPinf=%d\n\tNPNP=%d ]\n",
	       np->verb.surface,
	       np->verb.base,
	       np->verb.num,
	       np->verb.NP,
	       np->verb.cl,
	       np->verb.inf,
	       np->verb.NPcl,
	       np->verb.NPinf,
	       np->verb.NPNP );
   }
}

#endif

/*--------------------------------------------------------------*/
/* collect()                                                    */
/*                                                              */
/* identify verb candidates in the input file using 0/-ing      */
/* alternation heuristic                                        */
/*--------------------------------------------------------------*/

static void collect(void)
{
  while ( !feof(inputFile) ) {
    char *word;
       
    (void)fgets(line, MAXLINE - 1, inputFile);
       
    word = strtok(line, " \n\t");
    do {
        #ifdef DEBUG_ALL
          fprintf(stderr, "[DEBUG:word=\"%s\"]\n", word == 0 ? "" : word);
        #endif

	if (word == NULL) continue;
	if (*word == '\0' || *word == '\n') continue;

	if ( (strlen(word) >= MINLENGTH) &&
	     (strncmp("ing", word + strlen(word) - 3, 3) == 0) ) {
	  enter(word);
	}
    }
    while( (word = strtok(NULL, " \n\t")) != NULL );
  }
}

/*--------------------------------------------------------------*/
/* alternation()                                                */
/*                                                              */
/* take a word's surface string and check whether its "-ing"    */
/* form occurs in the hashtable, allowing for a preceeding "e"  */
/* or a preceeding sequence of two equal consonants             */
/*--------------------------------------------------------------*/

static node_t *alternation(char *word)
{
   static char finalCons[2] = " ";
   static char testbuf1[MAXLINE];
   static char testbuf2[MAXLINE];
   static char testbuf3[MAXLINE];
   char *cp;
   node_t *np;
   
   /* if (1) find(word          +ing) or
         (2) find(word-e        +ing) or
         (3) find(word-CONS+CONS+ing) => return node ptr */

   /* (1) -ing */
   strcpy(testbuf1, word);
   strcat(testbuf1, "ing");

   if ((np = find(testbuf1)) != NULL)
      return np;
   
   /* (2) -e-ing */
   strcpy(testbuf2, word);
   cp = &testbuf2[strlen(word)-1];
   if ( *cp == 'e' )
   {
      *cp = '\0';
      strcat(testbuf2, "ing");
      
      if ((np = find(testbuf2)) != NULL)
	return np;
   }
   
   /* (3) CONS-CONS-ing */
   strcpy(testbuf3, word);
   *finalCons = word[strlen(word)-1];
   strcat(testbuf3, finalCons);
   strcat(testbuf3, "ing");
   
   if ((np = find(testbuf3)) != NULL)
      return np;
   
   return NULL;
}

/*--------------------------------------------------------------*/
/* base()                                                       */
/*                                                              */
/* for all word-forms in the corpus: if a word-form exhibits    */
/* 0/-ing alternation, mark its entry                           */
/*--------------------------------------------------------------*/

static void base(void)
{
  while ( !feof(inputFile) )
    {
       char *word;
       
       (void)fgets(line, MAXLINE - 1, inputFile);
       
       word = strtok(line, " \n\t");
       
       while ( word != NULL )
       {
	  node_t *np = alternation(word);
	  
	  if (np != NULL )
	  {
             #ifdef DEBUG
	       fprintf(stderr, "[DEBUG:'%s'->'%s']\n",
		       word, np->verb.surface );
             #endif

	     np->verb.alternation = YES;
	     np->verb.base = strsave(word);
	  }

	  word = strtok(NULL, " \n\t");
       }
    }
}

/*--------------------------------------------------------------*/
/* tag()                                                        */
/*                                                              */
/* use context, a *very* tiny lexicon, and spelling to perform  */
/* a crude tagging of the input                                 */
/*--------------------------------------------------------------*/

static void tag(void)
{
   /* check context (does it occur after "the" or a preposition
     != "to"? If so, do not tag it as a verb */

   int prev = NONE, current;
   
   while ( !feof(inputFile) )
   {
      char *word;

      (void)fgets(line, MAXLINE - 1, inputFile);
      
      if ( ferror(inputFile) )
      {
	 fprintf(stderr, "tag(): read error\n");
	 return;
      }
      
      if ( (word = strtok(line, " \n\t")) != NULL )
      {
	 do
	 {
	    int cuetag;
	    
            #ifdef DEBUG_ALL
	      fprintf(stderr, "[DEBUG:word=\"%s\"]\n", word);
            #endif

	    cuetag = getcue(word);

	    if (cuetag != NONE)
	       current = cuetag;
	    else
	    {
	       if ( find(word) != NULL )
	       {
		  if ( prev == PREP || prev == DET )
		     current = PRP;
		  else
		  {
		     current = VING;
		     fprintf(tempFile, "\n");
		  }
	       }
	       else
	       {
		  if ( alternation(word) != NULL)
		  {
		     if ( prev == PREP || prev == DET )
			current = NOUN;
		     else
		     {
			current = VBASE;
			fprintf(tempFile, "\n");
		     }
		  }
		  else
		  {
		     if ( isupper(*word) )
			current = CAP;
		     else
			current = NONE;
			}
	       }
	    }
	    
	    fprintf(tempFile, "%s_%s ", word, tagset[current - NONE]);
	    
	    if ( ferror(tempFile) )
	    {
	       fprintf(stderr, "tag(): write error\n");
	       return;
	    }
	    
	    prev = current;
	 }
	 while( (word = strtok(NULL, " \n\t")) != NULL );
      } /* if strtok */
    } /* while not EOF */
}

/*--------------------------------------------------------------*/
/* emitPattern()                                                */
/*                                                              */
/* output one single valency pattern                            */
/*--------------------------------------------------------------*/

void emitPattern(char *pattern, int freqAbs)
{
   printf("%s", pattern);
}

/*--------------------------------------------------------------*/
/* emitNode()                                                   */
/*                                                              */
/* output valency lexicon node in Malaga format                 */
/*--------------------------------------------------------------*/

static void emitNode(node_t *np)
{
   if (np && strlen(np->verb.surface) >= MINLENGTH)
   {
      int denom       = 0;
      int itemPrinted = 0;
      int sig_NP      = significant(np->verb.NP);
      int sig_cl      = significant(np->verb.cl);
      int sig_inf     = significant(np->verb.inf);
      int sig_NPcl    = significant(np->verb.NPcl);
      int sig_NPinf   = significant(np->verb.NPinf);
      int sig_NPNP    = significant(np->verb.NPNP);

      if ( sig_NP || sig_cl || sig_inf || sig_NPcl || sig_NPinf || sig_NPNP )
      {
	  #ifdef DEBUG
	    fprintf( stderr,
		     "[DEBUG:sig_NP=%d sig_cl=%d sig_inf=%d "
		     "sig_NPcl=%d sig_NPinf=%d sig_NPNP=%d]\n",
		     sig_NP, sig_cl, sig_inf, sig_NPcl, sig_NPinf, sig_NPNP );
	  #endif

	  printf( "[ Lemma: \"%s\", Valencies: <", np->verb.base );
	  
	  if ( sig_NP )
	  {
	     if (itemPrinted)
	     {
		printf(", ");
	     }
	     emitPattern("<NounPhrase_Obj>", (denom += np->verb.NP));
	     itemPrinted++;
	  }
	  
	  if ( sig_cl )
	  {
	     if (itemPrinted)
	     {
		printf(", ");
	     }
	     emitPattern("<Clause>", (denom += np->verb.cl));
	     itemPrinted++;
	  }
	  
	  if ( sig_inf )
	  {
	     if (itemPrinted)
	     {
		printf(", ");
	     }
	     emitPattern( "<ToInfClause>",
			  (denom += np->verb.inf) );
	     itemPrinted++;
	  }
	  
	  if ( sig_NPcl )
	  {
	     if (itemPrinted)
	     {
		printf(", ");
	     }
	     emitPattern( "<NounPhrase_Obj, Clause>",
			  (denom += np->verb.NPcl) );
	     itemPrinted++;
	  }
	  
	  if ( sig_NPinf )
	  {
	     if (itemPrinted)
	     {
		printf(", ");
		itemPrinted = 0;
	     }
	     emitPattern( "<NounPhrase_Obj, InfClause>",
			  (denom += np->verb.NPinf) );
	  }
	  
	  if ( sig_NPNP )
	  {
	     if (itemPrinted)
	     {
		printf(", ");
	     }
	     emitPattern( "<NounPhrase_Obj, NounPhrase_Obj>",
			  (denom += np->verb.NPNP) );
	  }
	  
	  printf( "> ];\n");
      }
   }
}

/*--------------------------------------------------------------*/
/* main()                                                       */
/*                                                              */
/* 1. extract verb candidates                                   */
/* 2. tag the input text                                        */
/* 3. use LALR(1) shallow island parsing with surface cues to   */
/*    count the frequency of several valency frame patterns     */
/*--------------------------------------------------------------*/

int main(int argc, char *argv[])
{
   static char inputFileName[FNAMEMAX];
   static char tempFileName[FNAMEMAX];
   
   fprintf( stderr, 
	    "\nValency pattern extractor "
	    "(C)opyright 1997 by Jochen L. Leidner\n\n" );
   
   if ( argc == 1 )
   {
      strcpy(inputFileName, "corpus");
      strcpy(tempFileName, "corpus.t");
   }
   else if ( argc == 2 )
   {
      strcpy(inputFileName, argv[1]);
      strcpy(tempFileName, argv[1]);
      strcat(tempFileName, ".t");
   }
   else
   {
      fprintf(stderr, "\n\nusage: cue [ <corpusFile> ]\n\n");
      return 1;
   }
   
  #ifdef DEBUG
     fprintf(stderr, "[DEBUGing mode on.]\n");
  #endif

  /*--- tag the input text-----------------------------------------------*/

  if ( (inputFile = fopen(inputFileName, "r" )) /* open input file */
       == NULL )
  {
     fprintf(stderr, "\nmain(): can't open input file\n");
     exit(10);
  }
  
  if ( (tempFile = fopen(tempFileName, "w" )) /* open temporary file */
       == NULL)
  {
     fprintf(stderr, "\nmain(): can't open temporary file\n");
     exit(11);
  }

  /* first pass: collect -ING forms: */

  fprintf(stderr, "tagging \"%s\"", inputFileName);

  collect();

  fputc('.', stderr);

  /* second pass: go through the corpus and check for each
     word-form whether it is the base-form of a stored -ING form;
     if so, mark it in memory as a potential verb */

  (void)rewind(inputFile);

  base();

  #ifdef DEBUG_ALL
     fprintf(stderr, "[DEBUG:base() finished]\n");
  #endif

  /* for all stored -ING-strings: remove non-marked entries */

  traverse(kill);

  #ifdef DEBUG_ALL
     fprintf(stderr, "[DEBUG:traverse(kill) finished]\n");
  #endif

  fputc('.', stderr);

  /* fourth pass: go through corpus and tag it with the
     help of store and context */

  rewind(inputFile);

  tag();

  fprintf(tempFile, "\n");
  fclose(inputFile);
  fclose(tempFile);

  fprintf(stderr, ".\nshallow parsing in progress... ");
  #ifdef DEBUG
    fprintf(stderr, "[DEBUG:tagging finished]\n");
  #endif

  /*---extract valency patterns by means of shallow parsing--------------*/

  /* re-open temporary file for reading: */

  if ( (tempFile = fopen(tempFileName, "r" ))
       == NULL )
    {
      fprintf(stderr, "\nmain(): can't re-open temporary file\n");
      exit(20);
    }

  /* ignore first line (because there is no verb): */

  (void)fgets(line, MAXLINE - 1, tempFile);

  if (ferror(tempFile))
    return 21;

  /* process the whole file using the YACC grammar: */

  while ( !feof(tempFile) )
  {
     (void)yyparse();
  }

  #ifdef DEBUG
     traverse(printNode);
  #endif

  fclose(tempFile);
  traverse(emitNode); /* write the result: */

  return EXIT_SUCCESS;
}

/*==============================================================*/
/* end of file "cue.y"                                          */
/*==============================================================*/
