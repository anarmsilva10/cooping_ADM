# COPING, Fatores Psicossociais e Capacidade para o Trabalho
# Estudo da Escala Brief COPE em Profissionais de Saúde

## Descrição do Projeto
Este projeto teve como objetivo analisar as estratégias de coping adotadas por profissionais de saúde, explorando a relação entre os fatores psicossociais e a capacidade para o trabalho (ICT), com base na escala Brief COPE.
O estudo integrou metodologias de análise multivariada — nomeadamente Análise de Componentes Principais (PCA), Análise de Clusters (CA) e MANOVA — para identificar padrões de enfrentamento e associações com variáveis sociodemográficas.

O trabalho foi desenvolvido no âmbito da unidade curricular Análise de Dados Multivariados do Mestrado em Bioinformática Clínica (Universidade de Aveiro, 2024/2025) e pode ser lido o relatório completo em `Grupo1_projeto1.pdf`.

## Objetivos
- Avaliar as estratégias de coping (adaptativas e disfuncionais) em profissionais de saúde.
- Explorar as propriedades psicométricas da escala Brief COPE.
- Analisar a relação entre as dimensões de coping e a capacidade para o trabalho (ICT).
- Verificar diferenças entre grupos sociodemográficos (idade, sexo, literacia e anos de trabalho).
- Avaliar a robustez dos resultados em subpopulações (médicos, enfermeiros e técnicos).

## Metodologia
1. Base de Dados:
    Origem: Tese de doutoramento “Coping, Fatores Psicossociais e Capacidade para o Trabalho” (Ramos, 2014).
    Total inicial: 2960 observações e 117 variáveis.
    Amostra final: 909 profissionais de saúde.
    Software utilizado: R (v4.4.2).
    Ficheiro de dados: data2.sav (formato SPSS).

2. Pré-processamento:
    - Filtragem para incluir apenas profissionais de saúde.
    - Remoção de variáveis não relacionadas à escala Brief COPE.
    - Criação das 14 escalas de coping a partir dos 28 itens (Likert 1–4).
    - Cálculo de medidas de síntese (média, mediana e soma) — a soma foi utilizada nas análises.
    - Recodificação da variável ICT.
    - Remoção de outliers multivariados (distância de Mahalanobis, p < 0.01).
    - Divisão dos dados: 70% treino / 30% teste.

3. Análise Estatística:
    - Análise de Componentes Principais (PCA)
    - Análise de Clusters (CA)
    - MANOVA
    - Análise de Sensibilidade (PCA repetida para subgrupos)

## Principais Resultados
As estratégias de coping mais relevantes foram o coping ativo, planeamento, apoio instrumental e aceitação.
As dimensões de coping disfuncional, como autoculpabilização, negação e uso de substâncias, mostraram-se associadas a menor capacidade para o trabalho.
Diferenças observadas entre classes profissionais:
- Médicos: foco em coping proativo e planeamento.
- Enfermeiros: maior ênfase na regulação emocional e apoio social.
- Técnicos: estratégias mais operacionais e menos focadas na resolução de problemas.

## Estrutura do Código
O script principal encontra-se em `Grupo1.R` e segue as seguintes etapas:
1. Carregamento de pacotes e dados (data2.sav)
2. Transformação e limpeza dos dados
3. Construção das 14 escalas Brief COPE
4. Cálculo de PCA e regressão linear
4. Análise de clusters (Ward + K-means)
5. MANOVA e inferência estatística
6. Análise de sensibilidade (subgrupos profissionais)

Nota: As dependências encontram-se no script.

## Referência
Ramos, M. (2014). Coping, Fatores Psicossociais e Capacidade para o Trabalho. Universidade de Aveiro.

## License
This project is licensed under the terms of the MIT License.