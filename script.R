library(flexdashboard)
library(tidyverse)
library(plotly)
library(sidrar)
library(knitr)
library(kableExtra)
library(formattable)
library(tmap)
library(tmaptools)
library(sf)
library(geobr)
library(rgdal)

setwd('/home/heitor/ProjetosR/Shiny Porjects/Conjuntura/Conjt_board')

# Coletas: =====================================

# ipca  - Br   reg - último mês - mensal e ac. ano
c1 <- '/t/7060/n1/all/n7/all/n6/all/v/63,69/p/last%201/c315/7169/d/v63%202,v69%202'
# ipca  - Br   reg - histórico - ac. 12 meses
c2 <- '/t/7060/n1/all/n7/all/n6/all/v/2265/p/all/c315/7169/d/v2265%202'
# ipca - br reg - último mês - peso mensal
c3 <- '/t/7060/n1/all/n7/all/n6/all/v/66/p/last%201/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v66%204'
# ipp - br - hist - mes, mes ant, ac ano, indc
c4 <- '/t/6904/n1/all/v/all/p/all/c543/33579,33580,33583,33585,33586/d/v1394%202,v1395%202,v1396%202,v10008%205'
# ipp - br/CNAE - hist - mes, mes ant, ac ano
c5 <- '/t/6903/n1/all/v/1394,1395,1396/p/last%201/c842/all/d/v1394%202,v1395%202,v1396%202'
# pnad - br,reg,uf - MAIS RECENTE - desocup
c6 <- '/t/4099/n1/all/n2/all/n3/all/v/4099,4118/p/last%201/d/v4099%201,v4118%201'
# pnad - br,reg,uf - MAIS RECENTE - rend méd
c7 <- '/t/6469/n1/all/n2/all/n3/all/v/5931,5935/p/last%201'
# pnad - br,reg,uf - MAIS RECENTE - horas méd DESATIVADO
#c8 <- '/t/6371/n1/all/n2/all/n3/all/v/8192/p/last%201/c2/6794/d/v8192%201'
# pnad - br,reg - hist - desemprego
c8 <- '/t/4099/n1/all/n2/all/v/4099/p/all/d/v4099%201'
# pnad - br,reg - hist - rend méd real
c9 <- '/t/6469/n1/all/n2/all/v/5935/p/all'
# horas br
c91 <- '/t/6371/n1/all/v/8192/p/last%201/c2/6794/d/v8192%201'
# participação br
c92 <- '/t/6461/n1/all/v/4096/p/last%201/d/v4096%201'
# desocup br
c93 <- '/t/4099/n1/all/v/4099/p/last%201/d/v4099%201'
# rend médi nom
c94 <- '/t/6469/n1/all/v/5931/p/last%201'

# Tratamentos: ==================================

## c1 ----
d1 <- get_sidra(api = c1) |> as_tibble()

d1 |> View()
d1 |> glimpse()

d1 <- d1 |>
	dplyr::select('Var' = 'Variável', 
				  'Mês',
				  'Valor',
				  'Local' = 'Brasil, Região Metropolitana e Município') |>
	dplyr::mutate(Local,
				  'Reg' = case_when(
				  	TRUE ~ Local,
Local=="Belo Horizonte - MG" ~ 'Belo \nHorizonte - MG',
Local=="Grande Vitória - ES" ~ 'Grande \nVitória - ES',
Local=="Rio de Janeiro - RJ" ~ 'Rio de \nJaneiro - RJ',
Local=="Porto Alegre - RS" ~ 'Porto \nAlegre - RS',
Local=="Campo Grande - MS" ~ 'Campo \nGrande - MS'
))

#d1$Local <- NULL
d1$Var <- stringr::str_replace(d1$Var,
				pattern = 'IPCA - Variação ',
				replacement = 'Var. ')
d1 <- d1 |> dplyr::mutate(Local,
						  'br' = case_when(
						  	Local=="Brasil"~'País',
						  	Local!='Brasil'~'RM'))
d1$br <- d1$br |> as_factor()

## c2 ---
d2 <- get_sidra(api = c2) |> as_tibble()

d2 <- d2 |>
	dplyr::select('Var' = 'Variável', 
				  'Mês' = "Mês (Código)",
				  'Valor',
				  'Local' = 'Brasil, Região Metropolitana e Município') |>
	dplyr::filter(!is.na(Valor))

d2$Var <- stringr::str_replace(d2$Var,
					 pattern = 'IPCA - Variação acumulada em',
					 replacement = 'Var. Ac.')

d2$Mês <- d2$Mês |> lubridate::ym()

d2 |> View()
d2 |> glimpse()

## c3 ----
d3 <- get_sidra(api = c3) |> as_tibble()

d3 <- d3 |>
	dplyr::select('Valor',
				  'Local' = 'Brasil, Região Metropolitana e Município',
				  'Grupo' = 'Geral, grupo, subgrupo, item e subitem') |>
	dplyr::filter(!Grupo=='Índice geral') |>
	tidyr::pivot_wider(names_from = Grupo,
					   values_from = Valor)

View(d3)

## c4 ----
d4 <- get_sidra(api = c4) |> as_tibble()

d4 <- d4 |>
	dplyr::select('Variável',
				  'Categoria' = 'Grandes categorias econômicas',
				  'Mês' = 'Mês (Código)',
				  'Valor'
				  ) |>
	pivot_wider(names_from = Variável,
				values_from = Valor) |>
	dplyr::rename('Var. Ac. Ano' = 'IPP - Variação acumulada no ano (em relação a dezembro do ano anterior)',
				  'Var. Mensal Ano Ant.' = 'IPP - Variação mês/mesmo mês do ano anterior (M/M-12)',
				  'Var. Mensal Mês Ant.' = 'IPP - Variação mês/mês imediatamente anterior (M/M-1)',
				  'Índice' = 'IPP - Número-índice (dezembro de 2018 = 100)') |>
	dplyr::mutate(Mês = lubridate::ym(Mês)) #|> as_tsibble(index = Mês)

d4 <- d4 |>
	dplyr::mutate(Categoria,
				  'indg' = case_when(
				  	Categoria=="Indústria geral"~'Geral',
				  	Categoria!="Indústria geral"~'Bens'))

#d4$Categoria <-
#	stringr::str_replace(d4$Categoria,
#						 pattern     = 'Bens de consumo semiduráveis e ',
#						 replacement = 'Bens de cons. ')

d4 <- d4 |>
	dplyr::mutate(Categoria =
				  	case_when(
				  	Categoria=="Bens de consumo semiduráveis e não duráveis (BCND)"~'BCND',
				  	Categoria=="Bens de consumo duráveis (BCD)"~'BCD',
				  	Categoria=="Bens de capital (BK)"~'BK',
				  	Categoria=="Bens intermediários (BI)"~'BI',
				  	Categoria=="Indústria geral"~'Geral'))

d4 |> View()
d4 |> glimpse()

## c5 ----
d5 <- get_sidra(api = c5) |> as_tibble()
d5 <- d5 |>
	dplyr::select('Variável',
				  'Categoria' = 'Indústria geral, indústrias extrativas e indústrias de transformação e atividades (CNAE 2.0)',
				  'Valor') |>
	pivot_wider(names_from = Variável,
				values_from = Valor) |>
	dplyr::rename('Var. Ac. Ano' = 'IPP - Variação acumulada no ano (em relação a dezembro do ano anterior)',
				  'Var. Mensal Ano Ant.' = 'IPP - Variação mês/mesmo mês do ano anterior (M/M-12)',
				  'Var. Mensal Mês Ant.' = 'IPP - Variação mês/mês imediatamente anterior (M/M-1)') |>
	dplyr::relocate('Categoria',
					'Var. Mensal Mês Ant.',
					'Var. Mensal Ano Ant.',
					'Var. Ac. Ano')

d5$Categoria <- str_to_title(d5$Categoria,
							 locale = 'pt')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
						 pattern = ' De ',
						 replacement = ' de ')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
						 pattern = ' E ',
						 replacement = ' e ')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
						 pattern = 'B ',
						 replacement = '0B ')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
						 pattern = 'C ',
						 replacement = '0C ')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
			pattern = 'Indústria Geral',
			replacement = '0A Indústria Geral')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
			pattern = '20b ',
			replacement = '20 ')
d5$Categoria <-
	stringr::str_replace_all(d5$Categoria,
			pattern = '20c ',
			replacement = '20 ')
d5$Categoria <-
	stringr::str_sub(d5$Categoria,
					 start = 4)

d5 |> View()
d5 |> glimpse()

## c6 ----

d6 <- get_sidra(api = c6) |> as_tibble()

d6 |> View()
d6 |> glimpse()

d6 <- d6 |>
	dplyr::select(`Nível Territorial`,`Brasil, Grande Região e Unidade da Federação`, Variável, `Trimestre (Código)`, Valor) |>
	dplyr::rename('nivel' = `Nível Territorial`,
				 'regiao' = `Brasil, Grande Região e Unidade da Federação`,
				 'tri' = `Trimestre (Código)`) |>
	pivot_wider(names_from = Variável,
				values_from = Valor ) |>
	dplyr::rename('Desemprego' = `Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade`,
				  'Subutilização' = `Taxa composta de subutilização da força de trabalho, na semana de referência, das pessoas de 14 anos ou mais de idade`)

## M1 ----

shp_reg <- rgdal::readOGR(
	dsn= "/home/heitor/ProjetosR/Shiny Porjects/Conjuntura/Conjt_board/Reg/",
	layer= "regioes_2010") 
shp_est <- rgdal::readOGR(
	dsn= "/home/heitor/ProjetosR/Shiny Porjects/Conjuntura/Conjt_board/Uf/",
	layer= "estados_2010") 
shp_reg <- shp_reg |> st_as_sf()
shp_est <- shp_est |> st_as_sf()

shp_reg <-
	dplyr::left_join(shp_reg, d6 |> 
					 	dplyr::select(regiao,Desemprego, Subutilização),
					 by= c("nome" = "regiao"))
shp_est <-
	dplyr::left_join(shp_est, d6 |> 
					 	dplyr::select(regiao,Desemprego, Subutilização),
					 by= c("nome" = "regiao"))

## c7 ----

d7 <- get_sidra(api = c7) |> as_tibble()
d7 <- d7 |>
	dplyr::select(`Nível Territorial`,`Brasil, Grande Região e Unidade da Federação`, Variável, `Trimestre (Código)`, Valor) |>
	dplyr::rename('nivel' = `Nível Territorial`,
				 'regiao' = `Brasil, Grande Região e Unidade da Federação`,
				 'tri' = `Trimestre (Código)`) |>
	pivot_wider(names_from = Variável,
				values_from = Valor ) |>
	dplyr::rename('Renda Nominal' = `Rendimento médio nominal de todos os trabalhos, efetivamente recebido no mês de referência, pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho`,
				  'Renda Real' = `Rendimento médio real de todos os trabalhos, efetivamente recebido no mês de referência, pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho`)

shp_est <-
	dplyr::left_join(shp_est, d7 |> 
					 	dplyr::select(regiao,`Renda Real`, `Renda Nominal`),
					 by= c("nome" = "regiao"))

d7 |> View()
d7 |> glimpse()

## c8 ----
d8 <- get_sidra(api = c8) |> as_tibble()

d8 <- d8 |>
	dplyr::select(`Nível Territorial`,`Brasil e Grande Região`, Variável, `Trimestre (Código)`, Valor) |>
	dplyr::rename('nivel' = `Nível Territorial`,
				  'regiao' = `Brasil e Grande Região`,
				  'tri' = `Trimestre (Código)`) |>
	pivot_wider(names_from = Variável,
				values_from = Valor ) |>
	dplyr::rename('Desemprego' = `Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade`)|>
	dplyr::mutate(tri = lubridate::yq(tri))

d8 |> View()
d8 |> glimpse()

## c9 ----
d9 <- get_sidra(api = c9) |> as_tibble()
d9 <- d9 |>
	dplyr::select(`Nível Territorial`,`Brasil e Grande Região`, Variável, `Trimestre (Código)`, Valor) |>
	dplyr::rename('nivel' = `Nível Territorial`,
				  'regiao' = `Brasil e Grande Região`,
				  'tri' = `Trimestre (Código)`) |>
	pivot_wider(names_from = Variável,
				values_from = Valor ) |>
	dplyr::rename('Renda Real' = `Rendimento médio real de todos os trabalhos, efetivamente recebido no mês de referência, pelas pessoas de 14 anos ou mais de idade, ocupadas na semana de referência, com rendimento de trabalho`)|>
	dplyr::mutate(tri = lubridate::yq(tri))

d9 |> View()
d9 |> glimpse()

d91 <- get_sidra(api = c91) |>
	as_tibble() |> dplyr::select(Valor)
d92 <- get_sidra(api = c92) |>
	as_tibble() |> dplyr::select(Valor)
d93 <- get_sidra(api = c93) |>
	as_tibble() |> dplyr::select(Valor)
d94 <- get_sidra(api = c94) |>
	as_tibble() |> dplyr::select(Valor)
t2 <- tibble(
	'Participação (%)'=paste(d92$Valor,'%',
							 sep = ''),
	'Desocupação (%)'=paste(d93$Valor,'%',
							sep=''),
	'Renda Média Nominal' =paste('R$',d94$Valor,
								 sep = ''),
		   'Horas Sem. Méd.' = d91$Valor)

# Visualizações: ===============================

# G1 ggplot -------------------------------------
d1 |>
	dplyr::filter(Var == 'Var. mensal') |>
	ggplot(aes(x = Valor,
			   y = reorder(eg, Valor))) +
	geom_col(aes(fill= Local=="Brasil")) +
	geom_text( aes(label = Valor,
				   y = Reg),
		position = position_dodge(1),
		hjust = -0.125 ) +
	lims(x= c(min(d1$Valor[d1$Var == 'Var. mensal'])-.15,
			  max(d1$Valor[d1$Var == 'Var. mensal'])+.2)) +
	labs(x = 'Var. Mensal',
		 y = 'Região')+
	scale_fill_manual(
		values = c("dodgerblue", "green4")) +
	theme(legend.position = "none")

# G1 plotly -------------------------------------
d1 |>
	dplyr::filter(Var == 'Var. mensal') |>
	plot_ly(x = ~Valor,
			y = ~reorder(Local, Valor),
			type = 'bar',
			orientation = 'h',
			text = ~Valor,
			textposition = 'outside',
			color = ~br,
			colors = c('País' = '#117864',
					   'RM' = '#21618c')) |>
	layout(paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   xaxis = list(title = "Variação % Mês",
		   			 showgrid = FALSE,
		   			 showline = FALSE,
		   			 showticklabels = FALSE,
		   			 zeroline = FALSE),
		   yaxis = list(title = "",
		   			 showticklabels = TRUE),
		   showlegend = FALSE) 

# G2 ggplot -------------------------------------
d1 |>
	dplyr::filter(Var=='Var. acumulada no ano') |>
	ggplot(aes(x = reorder(Reg, Valor),
			   y = Valor)) +
	geom_col(aes(fill= Reg=="Brasil")) +
	geom_text( aes(label = Valor,
				   x = Reg),
			   position = position_dodge(1),
			   vjust = -0.125 ) +
	lims(y= c(0,
			  max(d1$Valor[d1$Var == 'Var. acumulada no ano'])+.2)) +
	labs(y = 'Var. Ac. Ano',
		 x = 'Região') +
	scale_fill_manual(
		values = c("dodgerblue", "green4")) +
	theme(legend.position = "none")

# G2 plotly -------------------------------------
d1 |>
	dplyr::filter(Var == 'Var. acumulada no ano') |>
	plot_ly(y = ~Valor,
			x = ~reorder(Local, Valor),
			type = 'bar',
			orientation = 'v',
			color = ~br,
			colors = c('País' = '#117864',
					   'RM' = '#21618c'),
			text = ~Valor,
			textposition = 'auto') |>
	layout(xaxis = list(title = ""),
		   yaxis = list(title = "Variação % Ac. Ano",
		   			 showgrid = FALSE,
		   			 showline = FALSE,
		   			 showticklabels = FALSE,
		   			 zeroline = FALSE),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = F)

# G3 plotly -------------------------------------
   # ordem decrescente das regiões
loc_decr <- d1 |>
	dplyr::filter(Var=='Var. mensal') |>
	dplyr::arrange(desc(Valor)) |>
	select(Reg) |>
	as_vector()

   # Gráfico
g3 <- plot_ly(data= d3,
			  y= ~reorder(Local,
			  			d1$Valor[
			  			d1$Var=='Var. mensal']),
			  x= ~`1.Alimentação e bebidas`,
			  type = 'bar',
			  orientation = 'h',
			  marker = list(color = 'rgba(38, 24, 74, 0.8)'),
			  line = list(color = 'rgb(248, 248, 249)', width = 1),
			  name='Alimentação')
	
g3 <- g3 |> add_trace(x = ~`2.Habitação`,
					  marker = list(
			color = 'rgba(218, 112, 214, 0.8)'),
			name='Habitação') 
g3 <- g3 |> add_trace(x = ~`3.Artigos de residência`,
					  marker = list(
			color = 'rgba(100, 149, 237, 0.8)'),
			name='Artigos Residenciais') 
g3 <- g3 |> add_trace(x = ~`4.Vestuário`,
					  marker = list(
			color = 'rgba(64, 224, 208, 0.8)'),
			name='Vestuário') 
g3 <- g3 |> add_trace(x = ~`5.Transportes`,
					  marker = list(
			color = 'rgba(159, 226, 191, 0.8)'),
			name='Transporte') 
g3 <- g3 |> add_trace(x = ~`6.Saúde e cuidados pessoais`,
					  marker = list(
			color = 'rgba(222, 49, 99, 0.8)'),
			name='Saúde e cuidados') 
g3 <- g3 |> add_trace(x = ~`7.Despesas pessoais`,
					  marker = list(
			color = 'rgba(255, 127, 80, 0.8)'),
			name='Despesas pessoais') 
g3 <- g3 |> add_trace(x = ~`8.Educação`,
					  marker = list(
			color = 'rgba(255, 191, 0, 0.8)'),
			name='Educação') 
g3 <- g3 |> add_trace(x = ~`9.Comunicação`,
					  marker = list(
			color = 'rgba(223, 255, 0, 0.8)'),
			name='Comunicação') 
g3 <- g3 |>
	layout(xaxis = list(title = "",
						showgrid = TRUE,
						showline = FALSE,
						showticklabels = FALSE,
						zeroline = FALSE,
						domain = c(0.15, 1)),
		   yaxis = list(title = "",
		   			 showticklabels = TRUE),
		   barmode = 'stack',
		   paper_bgcolor = 'rgb(248, 248, 255)',
		   plot_bgcolor = 'rgb(248, 248, 255)',
		   showlegend = TRUE,
		   legend = list(orientation = 'h')) 
config(g3, displaylogo = FALSE,
	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))

# G4 plotly -------------------------------------

plot_ly(data = d2,
		x= ~Mês,
		y= ~Valor,
		color = ~Local,
		type = 'scatter',
		mode = 'lines')|>
	layout(xaxis = list(title = "",
						showgrid = F),
		   yaxis = list(title = "Var. % Ac. 12 Meses",
		   			 showgrid = T,
		   			 showline = FALSE,
		   			 showticklabels = T,
		   			 zeroline = FALSE),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = T)
 
# G5 plotly == RADAR ==

g5 <- plot_ly(type  = 'scatterpolar',
			  r     = d4$`Var. Mensal Ano Ant.`[d4$Mês == max(d4$Mês)],
			  theta = d4$Categoria |> as_factor() |> levels(),
			  fill  = 'toself') 

g5 <- g5 |> layout(
	xaxis = list(title = "",
				 showgrid = F),
	yaxis = list(showgrid = T,
				 showline = FALSE,
				 showticklabels = T,
				 zeroline = FALSE),
	paper_bgcolor = '#f8f8ff',
	plot_bgcolor = '#f8f8ff',
	polar = list(radialaxis = list(
		visible = T#,
		#range   = c(0,50)
		)),
	showlegend = F)
g5 <- config(g5,displaylogo = FALSE,
	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))
g5

# G6 plotly -------------------------------------

g6 <- d4 |>
	dplyr::filter(Mês == max(Mês)) |>
	plot_ly(y = ~`Var. Mensal Ano Ant.`,
			x = ~reorder(Categoria, `Var. Mensal Ano Ant.`),
			type = 'bar',
			orientation = 'v',
			color = ~indg,
			colors = c('Geral' = '#21618c',
					   'Bens' = '#117864'),
			text = ~`Var. Mensal Ano Ant.`,
			textposition = 'outside') |>
	layout(title = 'Mês/Mês Ano Ant.',
		xaxis = list(title = ""),
		   yaxis = list(title = "Var% Mês/Mês Ano Ant.",
		   			 showgrid = FALSE,
		   			 showline = FALSE,
		   			 showticklabels = FALSE,
		   			 zeroline = FALSE,
		   			 range = c(0,
		   			 		  1.1*max(d4$`Var. Mensal Ano Ant.`[d4$Mês == max(d4$Mês)]))),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = F)
#g6 <- config(g6,displaylogo = FALSE,
#	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))
g6

# G7 plotly -------------------------------------

g7 <- d4 |>
	dplyr::filter(Mês == max(Mês)) |>
	plot_ly(y = ~`Var. Mensal Mês Ant.`,
			x = ~reorder(Categoria, `Var. Mensal Mês Ant.`),
			type = 'bar',
			orientation = 'v',
			color = ~indg,
			colors = c('Geral' = '#21618c',
					   'Bens' = '#117864'),
			text = ~`Var. Mensal Mês Ant.`,
			textposition = 'outside') |>
	layout(title = 'Mês/Mês Ant.',
		xaxis = list(title = ""),
		   yaxis = list(title = "Var% Mês/Mês Ant.",
		   			 showgrid = FALSE,
		   			 showline = FALSE,
		   			 showticklabels = FALSE,
		   			 zeroline = FALSE,
		   			 range = c(2.1*min(d4$`Var. Mensal Mês Ant.`[d4$Mês == max(d4$Mês)]),
		   			 		  1.1*max(d4$`Var. Mensal Mês Ant.`[d4$Mês == max(d4$Mês)]))),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = F)
#g7 <- config(g7,displaylogo = FALSE,
#	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))
g7

# G8 plotly -------------------------------------

g8 <- d4 |>
	dplyr::filter(Mês == max(Mês)) |>
	plot_ly(y = ~`Var. Ac. Ano`,
			x = ~reorder(Categoria, `Var. Ac. Ano`),
			type = 'bar',
			orientation = 'v',
			color = ~indg,
			colors = c('Geral' = '#21618c',
					   'Bens' = '#117864'),
			text = ~`Var. Ac. Ano`,
			textposition = 'outside') |>
	layout(title = paste('Mês/Mês Ano Ant.',
						 'Mês/Mês Ant.',
						 'Ac. Ano',
						 sep = '                               '),
		xaxis = list(title = ""),
		   yaxis = list(title = "Var% Ac. Ano",
		   			 showgrid = FALSE,
		   			 showline = FALSE,
		   			 showticklabels = FALSE,
		   			 zeroline = FALSE,
		   			 range = c(2.1*min(d4$`Var. Ac. Ano`[d4$Mês == max(d4$Mês)]),
		   			 		  1.1*max(d4$`Var. Ac. Ano`[d4$Mês == max(d4$Mês)]))),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = F)
#g8 <- config(g8,displaylogo = FALSE,
#	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))
g8

# JOIN: G8, G7, G6

g678 <- subplot(g6, g7, g8, nrows = 1)
g678 <- config(g678,
			   displaylogo = FALSE,
			   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))
g678

# G9 plotly -------------------------------------

g9 <- plot_ly(data = d4,
			  x= ~Mês,
			  y= ~Índice,
			  color = ~Categoria,
			  type = 'scatter',
			  mode = 'lines')|>
	layout(xaxis = list(title = "",
						showgrid = F),
		   yaxis = list(title = "Índice (2018/Dez = 100)",
		   			 showgrid = T,
		   			 showline = FALSE,
		   			 showticklabels = T,
		   			 zeroline = FALSE),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = T)

config(g9, displaylogo = FALSE,
	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))

# T1 -------------------------------------
d51 <- d5
d51$`Var. Ac. Ano` <-
	color_tile("white","steelblue3")(d51$`Var. Ac. Ano`)
d51$`Var. Mensal Mês Ant.` <-
	color_tile("white","steelblue3")(d51$`Var. Mensal Mês Ant.`)
d51$`Var. Mensal Ano Ant.` <-
	color_tile("white","steelblue3")(d51$`Var. Mensal Ano Ant.`)

d51 |>
	kbl(escape = F, align = 'c') |>
	kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
				  font_size = 11) %>%
	row_spec(1:3, bold = T)

# M1 ---------------------------------------

#par(mar=c(0,0,0,0))
#plot(shp_reg, col="#f2f2f2",bg="skyblue",lwd=.25, #border=0, usePolypath = FALSE)

#reg <- read_region(simplified = T, year = 2020)
#reg |> class()
#ggplot() + 
#	geom_sf(data=reg,
#			fill="#2D3E50",
#			color="#FEBF57",
#			size=.15, show.legend = FALSE) +
#	theme_minimal() +
#	theme(axis.title=element_blank(),
#		  axis.text=element_blank(),
#		  axis.ticks=element_blank())

tmap_mode("view") # Importante!!
#tm_shape(shp_reg) +
#	tm_fill('Desemprego',
#			palette = "Blues",
#			n = 7,contrast = c(0.05, 0.7),
#			legend.show = F,
#			id='nome') +
#	tm_borders()+
#	tm_text("Desemprego", size = 'AREA')

tm_shape(shp_est) +
	tm_fill('Desemprego',
			palette = "Blues",
			n = 7,contrast = c(0.05, 0.7),
			legend.show = F,
			id='nome',
			popup.vars = c('Desemprego','Subutilização','Renda Nominal')) +
	tm_borders()+
	tm_text("Desemprego", size = 'AREA') +
	tmap_options(check.and.fix = TRUE) 

tm_shape(shp_est) +
	tm_fill('Renda Nominal',
			palette = "Blues",
			n = 7,contrast = c(0.05, 0.7),
			legend.show = F,
			id='nome',
			popup.vars = c('Desemprego','Subutilização', 'Renda Nominal')) +
	tm_borders()+
	tm_text('Renda Nominal', size = 'AREA') +
	tmap_options(check.and.fix = TRUE) 

# G10 plotly -------------------------------------

g10 <- plot_ly(data = d8,
		x= ~tri,
		y= ~`Desemprego`,
		color = ~regiao,
		type = 'scatter',
		mode = 'lines')|>
	layout(xaxis = list(title = "",
						showgrid = F),
		   yaxis = list(title = "Desocupação (%)",
		   			 showgrid = T,
		   			 showline = FALSE,
		   			 showticklabels = T,
		   			 zeroline = FALSE),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = T)

config(g10,displaylogo = FALSE,
	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))

# G11 plotly -------------------------------------

g11 <- plot_ly(data = d9,
		x= ~tri,
		y= ~`Renda Real`,
		color = ~regiao,
		type = 'scatter',
		mode = 'lines')|>
	layout(xaxis = list(title = "",
						showgrid = F),
		   yaxis = list(title = "Renda Real (R$)",
		   			 showgrid = T,
		   			 showline = FALSE,
		   			 showticklabels = T,
		   			 zeroline = FALSE),
		   paper_bgcolor = '#f8f8ff',
		   plot_bgcolor = '#f8f8ff',
		   showlegend = T)

config(g11,displaylogo = FALSE,
	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))

# T2 -------------------------------------

kbl(t2, align = 'c')|>
	kable_styling(full_width = T) |>
	row_spec(0, font_size = 12,
			 bold = F) |> 
	row_spec(1, font_size = 30,
			 color ='#ffffff',
			 background = '#21618c')


#config(fig,displaylogo = FALSE,
#	   modeBarButtonsToAdd = list('drawopenpath', 'eraseshape'))