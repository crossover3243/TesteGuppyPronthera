<%@ LANGUAGE="VBSCRIPT" %>
<%
	'-----------------------------------------------------------
	'-  Pesquisa de Prévia de Reembolso para beneficiario, familia ou contrato
	'-  Autor: TopDown
	'-  Criação: 01/09/2008
	'-  Alterações:
	'-----------------------------------------------------------

	option explicit

	dim txt_usuario, txt_senha, txt_ip, txt_modulo, txt_sistema
	dim txt_msg, txt_subtitulo, x
	dim oPesquisa, rsPesquisa, ind_submit, nome_campo_cod
	Dim xml_pedido, txt_xml_filtro, oRegXML, oXML
	dim ind_tipo_pesquisa, ind_reembolso, num_previa_reembolso, nome_funcao_exec

	Dim vet_PL()
	Redim vet_PL(0)

	txt_usuario				= Session("ace_usuario")
	txt_senha				= Session("ace_senha")
	txt_ip					= Session("ace_ip")
	txt_modulo				= Session("ace_modulo")
	txt_sistema				= Session("ace_sistema")

	txt_msg					= Session("txt_msg")
	'txt_subtitulo			= Request.QueryString("PT")
	txt_msg = ""
	
	Session("txt_msg")		= ""

	'ind_submit				= Request("indsubmit")
	nome_campo_cod			= Request("nome_campo_cod")
	ind_tipo_pesquisa			= Request("ind_tipo_pesquisa")
	ind_reembolso				= Request("ind_reembolso")
	num_previa_reembolso	= Request("num_previa_reembolso")
	nome_funcao_exec		= Request("nome_funcao_exec")
		
	if ind_tipo_pesquisa = "B" then ' pesquisa por beneficiario
		txt_subtitulo = "Pesquisa de Prévias do Beneficiário"		
	elseif ind_tipo_pesquisa = "T" then ' pesquisa pelo titular
		txt_subtitulo = "Pesquisa de Prévias da Família"		
	elseif ind_tipo_pesquisa = "C" then ' pesquisa pelo contrato
		txt_subtitulo = "Pesquisa de Prévias do Contrato"		
	end if
	
	'session("pgm_retorno") = "../../gen/asp/gen0150a.asp?indsubmit=" & ind_submit & "&nome_campo_cod=" & nome_campo_cod & "&ind_volta=S"
	'session("pgm_retorno_erro") = ""
	
	'------------------------------------------------
	' Montar XML dos filtros
	'------------------------------------------------
	txt_xml_filtro = "<?xml version=""1.0"" encoding=""ISO-8859-1""?>"
	txt_xml_filtro = txt_xml_filtro & "<PREVIA>"
	txt_xml_filtro = txt_xml_filtro & "<FILTRO>"
	txt_xml_filtro = txt_xml_filtro & "<IND_SITUACAO>" & Request("ind_situacao") & "</IND_SITUACAO>"
    if ind_tipo_pesquisa = "B" then ' pesquisa por beneficiario
		'txt_xml_filtro = txt_xml_filtro & "<COD_TS>" & Request("cod_ts") & "</COD_TS>"
		txt_xml_filtro = txt_xml_filtro & "<NUM_ASSOCIADO>" & Request("num_associado") & "</NUM_ASSOCIADO>"
	elseif ind_tipo_pesquisa = "T" then ' pesquisa pelo titular
		'txt_xml_filtro = txt_xml_filtro & "<COD_TS_TIT>" & Request("cod_ts_tit") & "</COD_TS_TIT>"
		txt_xml_filtro = txt_xml_filtro & "<NUM_TITULAR>" & Request("num_titular") & "</NUM_TITULAR>"
	elseif ind_tipo_pesquisa = "C" then ' pesquisa pelo contrato
		'txt_xml_filtro = txt_xml_filtro & "<COD_TS_CONTRATO>" & Request("cod_ts_contrato") & "</COD_TS_CONTRATO>"
		txt_xml_filtro = txt_xml_filtro & "<NUM_CONTRATO>" & Request("num_contrato") & "</NUM_CONTRATO>"
	end if
    txt_xml_filtro = txt_xml_filtro & "</FILTRO>"
	txt_xml_filtro = txt_xml_filtro & "</PREVIA>"
	
	%>
	<!--Include do recordset oracle-->
	<!--#include file=..\..\gen\asp\gen0146a.asp-->
	<!--#include file=..\..\gen\asp\gen0146b.asp-->
	<%
	
	'------------------------------------------------
	' Recuperar xml das prévias
	'------------------------------------------------
	Redim vet_PL(3,4)
	
	vet_PL(1, 1) = "IN"
	vet_PL(1, 2) = "adDouble"
	vet_PL(1, 3) = "p_num_reembolso"

	vet_PL(2, 1) = "OUT"
	vet_PL(2, 2) = "adLongVarChar"
	vet_PL(2, 3) = "p_xml_retorno"

	vet_PL(3, 1) = "IN"
	vet_PL(3, 2) = "adLongVarChar"
	vet_PL(3, 3) = "p_xml_filtro"
	vet_PL(3, 4) = txt_xml_filtro
	
	Call ExecutaPLOracle (	CStr(session("ace_usuario")),_		  					CStr(session("ace_senha")),_							CStr(session("ace_ip")),_							CStr(session("ace_sistema")),_							CStr(session("ace_modulo")),_
							"RB_PREVIA_REEMBOLSO.RetornaPrevia", _
							vet_PL, _
							false )

	FechaConexao()

	xml_pedido = vet_PL(2, 4)
	
	if Trim(xml_pedido) = "" then
		txt_msg = "Nenhuma informação encontrada para os critérios informados."
		Set oRegXML = Nothing
		Set oXML = Nothing
	else
		
		Set oXML = CreateObject("Microsoft.XMLDOM") 
		oXML.async = False 
		oXML.loadXML(xml_pedido)
		Set oRegXML = oXML.getElementsByTagName("DADOS") 

		if oRegXML.Item(0).selectSingleNode("./COD_RETORNO").Text <> "0" then
			txt_msg = oRegXML.Item(0).selectSingleNode("./MSG_RETORNO").Text
			Set oRegXML = Nothing
			Set oXML = Nothing
		end if
	end if
	
%>

<html>
<head>
<title><%=Application("app")%></title>
<link href="\gen\css\css002.css" rel="stylesheet" type="text/css">
</head>

<!--#include file=..\..\gen\inc\inc0000.asp-->
<!--#include file=..\..\gen\inc\inc0001.asp-->
<!--#include file=..\..\gen\inc\inc0002.asp-->

<script language=javascript>

function acao_voltar()
{
	parent.self.close();
}

function selecionar(p_num_reembolso)
{
	var txt_chamada = '';

	try	{
		var oMyObject = window.dialogArguments;
		var txt_chamada = "oMyObject";
		var aux = oMyObject.document;
	} catch (e)	{
		txt_chamada = "window.parent.opener";
	}
	
	<%if trim(nome_campo_cod) <> "" then%>
		var onum_reembolso	= eval(txt_chamada + '.document.form01.<%=nome_campo_cod%>');
	<%else%>
		var onum_reembolso	= eval(txt_chamada + '.document.form01.num_reembolso');
	<%end if%>
	
	if (onum_reembolso != null )
		onum_reembolso.value = p_num_reembolso;
	
	  
    <%if trim(nome_funcao_exec)<>"" then%>
        try{eval(txt_chamada + ".<%=nome_funcao_exec%>");}
        catch(e){window.returnValue = 'window.<%=nome_funcao_exec%>';}
    <% end if %>
	
	parent.self.close();
}

function visualisarHistorico(p_num_reembolso){
	var strQueryString;
	strQueryString = '';
    strQueryString = strQueryString + 'num_reembolso=' + p_num_reembolso

	AbrePesquisa('../../rbm/asp/RBM1012a.asp?pt=Histórico Pedido&ind_forma_abertura=CO&botao_voltar=S&ind_popup=S&' + strQueryString,'','Pedido',1100,900,50,50);
}

</script>

<body>
<br>

<%
'VOLTAR/CONTINUAR/LIMPAR/INCLUIR/ALTERAR/EXCLUIR/EXECUTAR/POPUP
call MontaToolbar("S","N ","N","N","N","N","N","S")
%>

<%AbreTable()%>
<font class="subtitulos"><%=txt_subtitulo%></font>
<%FechaTable()%>

<form method="post" name="form01">

<%AbreTable()%>
 
<div id="txt_msg" class="msg" align="center"><%=txt_msg%></div>

<% if Trim(xml_pedido) <> "" then %>
	<table width="100%" align="center">
		<tr>
			<td class='grid_cabec' align="center" valign="bottom">Nº Prévia</td>
			<td class='grid_cabec' align="center" valign="bottom">Beneficiário</td>
			<td class='grid_cabec' align="center" valign="bottom">Data Solicitação</td>
			<% if ind_reembolso = "S" then %>
				<td class='grid_cabec' align="center" valign="bottom">Motivo Reembolso</td>
			<% else %>
				<td class='grid_cabec' align="center" valign="bottom">Valor Calculado (R$)</td>
			<% end if %>
			<td class='grid_cabec' align="center" valign="bottom">Valor Reembolsado (R$)</td>		
			<td class='grid_cabec' align="center" valign="bottom">Situação</td>
		</tr>
		<%For x = 0 To oRegXML.Length - 1 %>
			<tr>
				<td class='grid_left' nowrap>
					<% if ind_reembolso = "S" then %>
						<a href="javascript:selecionar('<%=oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text%>');"><%=oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text%></a>
						&nbsp;<img alt="Visualizar histórico da prévia." SRC="../../GEN/IMG/folha3.gif" onclick="javascript:visualisarHistorico('<%=oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text%>');" style="cursor:hand" >
					<% elseif oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text <>  num_previa_reembolso then %>
						<a href="javascript:visualisarHistorico('<%=oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text%>');"><%=oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text%></a>
					<% else %>
						&nbsp;<%=oRegXML.Item(x).selectSingleNode("./NUM_REEMBOLSO").Text%>
					<% end if %>
				</td>
				<td class='grid_left' nowrap>&nbsp;<%=oRegXML.Item(x).selectSingleNode("./NUM_ASSOCIADO").Text%>&nbsp;-&nbsp;<%=oRegXML.Item(x).selectSingleNode("./NOME_ASSOCIADO").Text%></td>
				<td class='grid_center' nowrap>&nbsp;<%=oRegXML.Item(x).selectSingleNode("./DT_INCLUSAO").Text%></td>
				<% if ind_reembolso = "S" then %>
					<td class='grid_left' nowrap>&nbsp;<%=oRegXML.Item(x).selectSingleNode("./DESC_MOTIVO_REEMBOLSO").Text%></td>
				<% else %>
					<td class='grid_left' nowrap>&nbsp;<%=oRegXML.Item(x).selectSingleNode("./VAL_CALCULADO").Text%></td>
				<% end if %>
				<td class='grid_left' nowrap>&nbsp;<%=oRegXML.Item(x).selectSingleNode("./VAL_REEMBOLSADO").Text%></td>
				<td class='grid_left' nowrap>&nbsp;<%=oRegXML.Item(x).selectSingleNode("./NOME_SITUACAO").Text%></td>
			</tr>
		<%NEXT
		Set oRegXML = Nothing
		Set oXML = Nothing
		%>
	</table>
	<input type="hidden" name="nome_campo_cod" value="<%=nome_campo_cod%>">

<%end if%>

</form>

<%FechaTable()%>

</body>
</html>