library(flexdashboard)
library(tidyverse)
library(plotly)
library(sidrar)

setwd('/home/heitor/ProjetosR/Shiny Porjects/Conjuntura/Conjt_board')

# Coletas: =====================================
# ipca  - Br   reg - último mês - mensal e ac. ano
c1 <- '/t/7060/n1/all/n7/all/n6/all/v/63,69/p/last%201/c315/7169/d/v63%202,v69%202'
# ipca  - Br   reg - histórico - ac. 12 meses
c2 <- '/t/7060/n1/all/n7/all/n6/all/v/2265/p/all/c315/7169/d/v2265%202'
# ipca - br reg - último mês - peso mensal
c3 <- '/t/7060/n1/all/n7/all/n6/all/v/66/p/last%201/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v66%204'

# Tratamentos: ==================================

## c1 ---
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

d2 |> View()
d2 |> glimpse()

d2 <- d2 |>
	dplyr::select('Var' = 'Variável', 
				  'Mês',
				  'Valor',
				  'Local' = 'Brasil, Região Metropolitana e Município') |>
	dplyr::filter(!is.na(Valor))

d2$Var <- stringr::str_replace(d2$Var,
					 pattern = 'IPCA - Variação acumulada em',
					 replacement = 'Var. Ac.')

## c3 ---
d3 <- get_sidra(api = c3) |> as_tibble()

d3 <- d3 |>
	dplyr::select('Valor',
				  'Local' = 'Brasil, Região Metropolitana e Município',
				  'Grupo' = 'Geral, grupo, subgrupo, item e subitem') |>
	dplyr::filter(!Grupo=='Índice geral') |>
	tidyr::pivot_wider(names_from = Grupo,
					   values_from = Valor)

View(d3)

# Visualizações: ================================
# G1 ggplot
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

# G1 plotly
d1 |>
	dplyr::filter(Var == 'Var. mensal') |>
	plot_ly(x = ~Valor,
			y = ~reorder(Local, Valor),
			type = 'bar',
			orientation = 'h',
			text = ~Valor,
			textposition = 'auto',
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

# G2 ggplot
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

# G2 plotly
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

# G3 plotly
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
			color = 'rgba(255, 3, 62, 0.8)'),
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
g3
