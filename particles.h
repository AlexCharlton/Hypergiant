/* particles.h

   Hyperscene particle system extension
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct pool HPGpool;

struct pool{
    unsigned int blockSize;
    unsigned int nBlocks;
    void **freeBlock;
    char name[32];
};

typedef struct {
    HPGpool particlePool;
    void *particleBuffer;
    unsigned int *indexData;
    size_t nParticles, maxParticles, particleSize, iterateIndex;
} HPGemitter;

/* Hyperscene */
typedef struct pipeline HPSpipeline;

typedef struct node HPSnode;

typedef struct HPSextension {
    void (*init)(void **);
    void (*preRender)(void *);
    void (*postRender)(void *);
    void (*visibleNode)(void *, HPSnode *node);
    void (*updateNode)(void *, HPSnode *node);
    void (*delete)(void *);
} HPSextension;

typedef struct camera HPScamera;

int hpsFurtherFromCamera(const HPScamera *camera, const float *a, const float *b);

HPSnode *hpsAddNode(HPSnode *parent, void *data,
                    HPSpipeline *pipeline,
                    void (*deleteFunc)(void *));

void* hpsNodeData(HPSnode *node);

void hpsSetNodeExtension(HPSnode *node, HPSextension *extension);

HPScamera *hpsCurrentCamera();


/*** Pools ***/
void hpgInitPool(HPGpool *pool, void *data, size_t blockSize, size_t nBlocks, char name[32]){
    int i;
    char *poolStart = (char *) data;
    pool->blockSize = blockSize;
    pool->nBlocks = nBlocks;
    pool->freeBlock = (void**) data;
    strcpy(pool->name, name);
    for(i = 0; i < nBlocks - 1; i++){
	void **next = (void **) &poolStart[i * blockSize];
	*next = &poolStart[(i+1) * blockSize];
    }
    void **last = (void **) &poolStart[(nBlocks - 1) * blockSize];
    *last = NULL;
}

void *hpgAllocateFrom(HPGpool *pool){
#ifdef DEBUG
      if (!pool){
      fprintf(stderr, "Fatal: trying to allocate to a pool that doesn't exist!\n");
      exit(EXIT_FAILURE);
      }
#endif
    void **block = pool->freeBlock;
    if (!block){
	return NULL;
    }
    pool->freeBlock = *block;
    return block;
}

void hpgDeleteFrom(void *block, HPGpool *pool){
    void **b = (void **)block;
    *b = pool->freeBlock;
    pool->freeBlock = b;
}

/*** Extension ***/
static HPGemitter *currentEmitter = NULL;

static int particleSort(const void *a, const void *b){
    int indexA = *((int *) a);
    int indexB = *((int *) b);
    char *data = (char *) currentEmitter->particleBuffer;
    size_t particleSize = currentEmitter->particleSize;
    float *pa = (float *) &data[indexA * particleSize];
    float *pb = (float *) &data[indexB * particleSize];
    int r = hpsFurtherFromCamera(hpsCurrentCamera(), pa, pb);
    return r;
}

void hpgInitParticles(void **data){
    *data = NULL;
}

void hpgDeleteParticles(void *data){
}

void hpgParticlesPreRender(void *data){
}

void hpgParticlesPostRender(void *data){
}

void hpgParticlesVisibleNode(void *data, HPSnode *node){
    HPGemitter *emitter = (HPGemitter *) hpsNodeData(node);
    currentEmitter = emitter;
    qsort(emitter->indexData, emitter->nParticles, sizeof(unsigned int), 
          &particleSort);
}

void hpgParticlesUpdateNode(void *data, HPSnode *node){
}

HPSextension particles = {hpgInitParticles,
                          hpgParticlesPreRender,
                          hpgParticlesPostRender,
                          hpgParticlesVisibleNode,
                          hpgParticlesUpdateNode,
                          hpgDeleteParticles};

HPSextension *hpgParticles = &particles; //external 


/*** Particles ***/
void hpgDeleteEmitter(void *emitter){
    free(emitter);
}

static void initParticles(HPGemitter *emitter){
    size_t particleI;
    char *data = (char *) emitter->particleBuffer;
    size_t particleSize = emitter->particleSize;
    for (particleI = 0; particleI < emitter->maxParticles; particleI++){
        float *position = (float *) &data[particleI * particleSize];
        position[2] = NAN;
    }
    emitter->indexData[0] = ~0; // End of index delimited by zero's compliment
}

// indexBuffer should be one longer than nParticles
HPSnode *hpgAddEmitter(HPSnode *parent, size_t nParticles, void* particleBuffer, size_t particleSize, void *indexBuffer){
    HPGemitter *emitter = malloc(sizeof(HPGemitter));
    HPSnode *node = hpsAddNode(parent, emitter, NULL, &hpgDeleteEmitter);
    hpsSetNodeExtension(node, hpgParticles);
    emitter->nParticles = 0;
    emitter->maxParticles = nParticles;
    emitter->particleSize = particleSize;
    emitter->particleBuffer = particleBuffer;
    emitter->indexData = (unsigned int *) indexBuffer;
    hpgInitPool(&emitter->particlePool, particleBuffer, particleSize, nParticles, "Particle pool");
    initParticles(emitter);
    return node;
}

void *hpgAddParticle(HPGemitter *emitter){
    if (emitter->nParticles == emitter->maxParticles) return NULL;
    void *particle = hpgAllocateFrom(&emitter->particlePool);
    float *p = (float *) particle;
    p[2] = 0;
    emitter->nParticles++;
    return (void *) particle;
}

void hpgDeleteParticle(HPGemitter *emitter, void *particle){
    float *p = (float *) particle;
    p[2] = NAN;
    hpgDeleteFrom(particle, &emitter->particlePool);
    emitter->nParticles--;
}

void hpgEmitterReset(HPGemitter *emitter){
    emitter->iterateIndex = 0;
}

void *hpgEmitterNextParticle(HPGemitter *emitter){
    char *data = (char *) emitter->particleBuffer;
    unsigned int i = emitter->indexData[emitter->iterateIndex];
    if (i == ~0){ return NULL; }
    emitter->iterateIndex++;
    return (void *) &data[i * emitter->particleSize];
}

void hpgUpdateEmitter(HPGemitter *emitter){
    size_t indexI = 0, particleI;
    char *data = (char *) emitter->particleBuffer;
    size_t particleSize = emitter->particleSize;
    for (particleI = 0; particleI < emitter->maxParticles; particleI++){
        float *position = (float *) &data[particleI * particleSize];
        if (!isnan(position[2])){
            emitter->indexData[indexI] = particleI;
            indexI++;
        }
    }
    emitter->indexData[indexI] = ~0; // End of index delimited by zero's compliment
}

size_t hpgEmitterNParticles(HPGemitter *emitter){
    return emitter->nParticles;
}

size_t hpgEmitterMaxParticles(HPGemitter *emitter){
    return emitter->maxParticles;
}
