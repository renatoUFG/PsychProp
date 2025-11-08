# PsycProp  um aplicativo shiny para análise  de propriedades psicométricas de escalas tipo Likert

Este aplicativo Shiny facilita a análise de propriedades psicométricas de instrumentos baseados em escalas tipo Likert. 
Desenvolvido por **Renato Rodrigues Silva**, utiliza diversas bibliotecas da linguagem R, incluindo `psych`, `lavaan`, `mirt`, entre outras.

##  Pacotes necessários

- shiny
- shinyWidgets)
- shinythemes)
- tidyverse
- DT
- psych
- lavaan
- EFA.MRFA
- mirt


##  Instalação

Para usar o aplicativo, é necessário ter o R instalado. Instale as dependências com o script:

```r
source("install_dependencies.R")
```

## Tipo de arquivos aceitos pelo app

Apenas arquivos com extensão .csv ou .xls, ou xlsx são aceitos no app

## Preparação da planilha

A planilha deve ser organizada da seguinte forma:
Cada coluna é um item da escala e cada linha é uma resposta dos participantes.
Os nomes das colunas deve ser consistente com a forma de nomear
variáveis no R.
A planilha deve conter apenas os itens da escala, qualquer outras variáveis
coletadas durante a pesquisa (variáveis demográficas por exemplo) devem
ser removidas.


##  Como executar

### Usando a IDE RStudio

Abra o arquivo `app.R` no RStudio e clique em **Run App**.

### Usando o terminal do Windows

1. Abra o Prompt de Comando (cmd.exe) ou PowerShell.
2. Navegue até a pasta onde está app.R
3. Execute: Rscript app.R

Obs: Se ao rodar Rscript você receber erro do tipo “comando não encontrado”, verifique se o R está no seu PATH do sistema.:

### Usando o terminal linux/macOS

1. Abra o terminal.
2. Vá até a pasta 
3. Execute:  Rscript app.R

### Usando arquivo executável no Windows

Clique  duas vezes nesse .bat, o app será executado automaticamente.

### Usando arquivo executável no Linux/MacOS

1. Abra o terminal
2. Vá até a pasta 
3.  Execute:
a. Somente na primeira vez, para tornar executável
chmod +x rodar_app.sh  
b. Nas outras vezes, execute:
./rodar_app.sh

##Utilização de PsychProp

A maioria das análises é intuitiva, basta clicar nos botões indicados. 
Para fazer análise fatorial confirmatória, a sintaxe é a mesma utilizada no software lavaan,
cujo maiores detalhes podem ser encontrados no endereço https://lavaan.ugent.be/tutorial/cfa.html


##  Licença

Este projeto está licenciado sob os termos da **GNU General Public License v3.0 (GPL-3)**.  
Consulte o cabeçalho do arquivo `app.R` ou o arquivo `LICENSE` para mais informações.
