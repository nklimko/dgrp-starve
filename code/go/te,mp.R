temp

rule go-tblup_m:
  input:
    script='code/go/goBlupTrial.R',
    data='snake/data/01_matched/m_starvation.Rds',
    goPath='snake/data/go/03_goterms/sexm/{goterm}.Rds'
  output:
    'snake/data/go/33_metric/sexm/tblup/{goterm}.Rds'
  params:
    goterm = lambda wildcards: str(wildcards.goterm),
    niter = 85000,
    burnin = 10000,
    thin = 50,
    RMAX=0.8,
    repCount = 25,
    sex='m'
  threads: 1
  resources:
    jobweight = 1,
    mem_mb=8000
  shell:
    """
    mkdir -p snake/data/bglr/m/gblup/{params.goterm}
    mkdir -p snake/data/go/24_goCor/m/tblup/{params.goterm}
    Rscript {input.script} \
      --inPath {input.data} \
      --goPath {input.goPath} \
      --outPath {output} \
      --goterm {params.goterm} \
      --sex {params.sex} \
      --nIter {params.niter} \
      --burnIn {params.burnin} \
      --thin {params.thin} \
      --repCount {params.repCount} \
      --RMAX {params.RMAX} \
      --model RKHS
    """
    #mkdir -p snake/data/bglr/sexf/go{params.rGO}/max{params.rMAX}/term{params.goterm}/id{params.goset}
rule combineBlupGO_M:
  input:
    script='code/go/combineBlupGO.R',
    data=expand('snake/data/go/33_metric/sexm/gblupTrial/{goterm}.Rds', goterm=BLUP_M)
  output:
      'snake/data/go/40_all/sexm/blup_allData.Rds'
  threads: 1
  shell:
    """
    Rscript {input.script} \
    --dataList {input.data} \
    --outPath {output}
    """
    
print(paste0('temp\n', 'tempagain')
cat(paste0('temp\n', 'tempagain')