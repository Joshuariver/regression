# Regression Analysis in R

R을 활용한 회귀분석 학습 자료 모음입니다.

## 개요

이 리포지토리는 다양한 회귀분석 기법을 R로 구현한 학습 자료를 포함하고 있습니다. 기초적인 단순회귀부터 고급 기법인 매개/조절 회귀까지 단계별로 학습할 수 있도록 구성되어 있습니다.

## 학습 내용

### 1. 단순회귀 (Simple Regression)
- `01_단순회귀.R` - 단순회귀분석 기본 개념
- `01_단순회귀실습.R` - 실습 예제
- `01_단순회귀과제.R` - 연습 과제

### 2. 다변량회귀 (Multiple Regression)
- `02_다변량회귀.R` - 다중회귀분석 이론
- `02_다변량회귀실습.R` - 실습 예제
- `02_다변량회귀과제.R`, `02_다변량회귀과제2.R` - 연습 과제

### 3. 단계적회귀 (Stepwise Regression)
- `03_단계적회귀.R` - 변수 선택 방법론
- `03_단계적회귀실습.R` - 실습 예제
- `03_단계적회귀실습과제.R` - 연습 과제

### 4. 위계적회귀 (Hierarchical Regression)
- `04_위계적회귀.R` - 위계적 회귀분석 이론
- `04_위계적회귀실습.R` - 실습 예제
- `04_위계적회귀과제.R` - 연습 과제

### 5. 더미회귀 (Dummy Regression)
- `05_더미회귀.R` - 범주형 변수를 활용한 회귀분석
- `05_더미회귀실습.R` - 실습 예제

### 6. 조절회귀 (Moderation Regression)
- `06_조절회귀.R` - 조절효과 분석
- `06_조절회귀실습.R` - 실습 예제
- `06_조절회귀과제.R` - 연습 과제

### 7. 매개회귀 (Mediation Regression)
- `07_매개회귀.R` - 매개효과 분석
- `07_매개회귀실습.R` - 실습 예제
- `07_매개회귀과제.R` - 연습 과제

## 추가 자료

### 고급 기법
- `elasticnet.R` - Elastic Net 정규화
- `ridge lasso.R` - Ridge/Lasso 회귀
- `Mean Centering.R` - 평균 중심화 기법

### 요인분석 (Factor Analysis)
- `00_요인분석.R` - 요인분석 기초
- `factor_analysis_1.R` - 요인분석 심화
- `fa_shiny.R` - Shiny 앱을 활용한 요인분석

### 기타
- `pca_umap.R` - PCA와 UMAP 차원축소
- `Healthcare.R` - 헬스케어 데이터 분석 예제
- `Confirmal_Prediction/` - Conformal Prediction 관련 자료

## 폴더 구조

```
├── Data/                 # 분석용 데이터셋 (.dta 형식)
├── exercise/             # 추가 연습 자료
├── factor analysis example/  # 요인분석 예제
└── Confirmal_Prediction/     # Conformal Prediction 자료
```

## 데이터셋

`Data/` 폴더에는 실습에 사용되는 Stata 데이터 파일(.dta)이 포함되어 있습니다:
- `AEJfigs.dta`
- `NHIS2009_clean.dta`
- `annual_spend.dta`
- `mdve.dta`
- `rand_initial_sample_2.dta`

## 요구 사항

- R (버전 4.0 이상 권장)
- 주요 패키지: `tidyverse`, `car`, `psych`, `lavaan`, `mediation` 등

## 라이선스

학습 및 교육 목적으로 사용할 수 있습니다.
