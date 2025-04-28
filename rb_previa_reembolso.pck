CREATE OR REPLACE PACKAGE rb_previa_reembolso is
    --
    -- Created : 01/09/2008 11:00
    -- Purpose : Package com o acesso ao banco utilizada para consultar / gravar
    --           Pr�via de reembolso
    --
    --pragma serially_reusable;
    --
    function p_versao return Varchar2;
    --
    ----------------------------------------------------------------------------
    -- Retorna a operadora - Acerto para Operadora ONE
    ----------------------------------------------------------------------------
    function retorna_cod_operadora ( p_cod_operadora in number, p_cod_rede_atendimento in number )
    return number;
    --
    function retorna_nome_operadora ( p_cod_operadora in number, p_cod_rede_atendimento in number )
    return varchar2;
    --
    procedure RetornaParametro                      ( p_cod_parametro       in  varchar2
                                                    , p_val_parametro       out varchar2
                                                    , p_val_default         in  varchar2 default ''
                                                    );
    procedure RetornaOcorrenciaAss                  ( p_num_associado       in varchar2,
                                                      p_ind_ocorrencia      out NUMBER );
    --
    procedure ReverteCancelamentoPrevia             ( p_num_reembolso        in  number
                                                    , p_cod_usuario         in  varchar2
                                                    , p_cod_motivo          in  number
                                                    , p_txt_obs_operadora    in  varchar2
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );

    procedure ReverteFinalizacaoPrevia              ( p_num_reembolso        in  number
                                                    , p_cod_usuario         in  varchar2
                                                    , p_cod_motivo          in  number
                                                    , p_txt_obs_operadora    in  varchar2
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );


    procedure CancelaPrevia                         ( p_num_reembolso        in  number
                                                    , p_cod_usuario         in  varchar2
                                                    , p_cod_motivo          in  number
                                                    , p_txt_obs_operadora    in  varchar2
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );

    procedure enviar_fax                            ( p_cod_retorno      out number
                                                     , p_msg_retorno      out varchar2
                                                     , p_xml_dados        in clob
                                                     , p_xml_arquivos     in clob
                                                     );
    --
    procedure RetornaData                           ( p_cod_formato         in  varchar2
                                                    , p_data                out varchar2
                                                    );
    --
    procedure RetornaMotivo                         ( p_cod_motivo          in  number
                                                    , p_xml_retorno         out clob
                                                    );
    --
    procedure RetornaHabilitacaoGlosa               ( p_xml_glosa           out clob );
    --
    procedure GravaMotivo                           ( p_ind_acao            in  varchar2
                                                    , p_cod_motivo          in  number
                                                    , p_ind_tipo            in  varchar2
                                                    , p_desc_motivo         in  varchar2
                                                    , p_txt_mensagem        in  varchar2
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );
    --
    procedure GravaMotivoNegItem                    ( p_num_reembolso       in  number
                                                    , p_item_medico         in  varchar2
                                                    , p_num_seq_item        in  number
                                                    , p_cod_motivo          in  number
                                                    , p_desc_motivo         in  varchar2
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );
    --
    procedure RetornaDadosAssociado                 ( p_num_associado        in  varchar2
                                                    , p_data_solicitacao     in  date
                                                    , p_ind_acao_judicial    in  varchar2
                                                    , p_xml_retorno          out clob
                                                    , p_cod_ts               in  number     default null
                                                    );
    --
    Procedure RetornaPrevia                         ( p_num_reembolso       in  varchar2
                                                    , p_xml_retorno         out clob
                                                    , p_xml_filtro          in  clob        default null
                                                    , p_ind_forma_abertura  in varchar2     default null
                                                    );
    --
    procedure RetornaAnexo                          ( p_num_reembolso       in  varchar2
                                                    , p_xml_retorno         out clob
                                                    , p_nome_arquivo        in  varchar2    default null
                                                    );
    --
    procedure RetornaOcorrencia                     ( p_num_reembolso       in  varchar2
                                                    , p_xml_retorno         out clob
                                                    );
    --
    procedure RetornaSolicitante                    ( p_xml_filtro          in  clob
                                                    , p_xml_retorno         out clob
                                                    );
    --
    function RetornaGlosa                           ( p_num_reembolso         in  varchar2
                                                    , p_num_seq_item          in  number
                                                    )
    return sys_refcursor;
    --
    function RetornaUltimoSeqItem                   ( p_num_reembolso       in varchar2 )
    return   sys_refcursor;
    --
    PROCEDURE GravaHabilitacaoGlosa                 ( p_xml_dados           in  clob
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );
    --
    procedure GravaAnexo                            ( p_xml_dados           in  clob
                                                    , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    );
    --
    function  RetornaCursor                         ( p_nome_tabela         in  varchar
                                                    , p_campo_value         in  varchar
                                                    , p_campo_desc          in  varchar
                                                    , p_order               in  varchar     default null
                                                    , p_where               in  varchar     default null
                                                    )
    return sys_refcursor;
    --
    procedure IncluirPrevia                         ( p_xml_dados                in clob
                                                    , p_num_reembolso            out number
                                                    , p_cod_retorno              out number
                                                    , p_msg_retorno              out varchar2
                                                    );
    --
    function  get_filial_unidade                    ( p_cod_retorno                 in out nocopy varchar2
                                                    , p_msg_retorno                 in out nocopy varchar2
                                                    , p_cod_usuario                 in out nocopy varchar2
                                                    )
    return sys_refcursor;
    --
    procedure RetornaProcedimento                   ( p_item_medico                in  varchar2
                                                    , p_num_associado              in  varchar2
                                                    , p_cod_ts_contrato            in  number
                                                    , p_num_contrato               in  varchar2
                                                    , p_cod_ts_tit                 in  number
                                                    , p_num_titular                in  varchar2
                                                    , p_cod_plano                  in  varchar2
                                                    , p_dt_nascimento              in  varchar2
                                                    , p_ind_sexo                   in  varchar2
                                                    , p_dt_atendimento             in  varchar2
                                                    , p_ind_tipo_reembolso         in  varchar2
                                                    , p_qtd_informado              in  number default 1
                                                    , p_ind_principal              in  varchar2 default 'N'
                                                    , p_ind_via                    in  varchar2 default null
                                                    , p_ind_doppler                in  varchar2 default null
                                                    , p_cod_motivo_reembolso       in  varchar2 default null
                                                    , p_ind_dobra_calculo          in  varchar2 default null
                                                    , p_ind_add_anestesista        in  varchar2 default null
                                                    , p_cod_inspetoria_ts          in  varchar2 default 4
                                                    , p_cod_operadora              in  number   default 1
                                                    , p_ind_apenas_consulta        in  varchar2  default 'N'
                                                    , p_xml_retorno                out clob
                                                    );
    --
    function get_previa_submodalidades              ( p_tipo_reembolso             in pls_integer
                                                    , p_ctr_logs                   in varchar2     default 'N'
                                                    )
    return sys_refcursor;
    --
    --
    function get_submodalidade_ocorrencia   ( p_ind_tipo_reembolso  in pls_integer
                        , p_ctr_logs          in varchar2     default 'N'
                        )
    return sys_refcursor;
    --
    --
    procedure GravaPreviaSubmodalidade              ( p_cod_retorno                out number
                                                    , p_msg_retorno                out varchar2
                                                    , p_xml_parametros             in clob
                                                    );
    --
    procedure GravaPrevia                           ( p_xml_dados               in clob
                                                    , p_num_reembolso           out number
                                                    , p_cod_retorno             out number
                                                    , p_msg_retorno             out varchar2
                                                    );
    --

    --
    procedure getQtdFamilia                         ( p_cod_ts                      in   number
                                                    , p_qtd_familia                 out  number
                                                    );
    --

    procedure RetornaItensNova                      ( p_num_reembolso           in  varchar2
                                                    , p_xml_retorno             out clob
                                                    );
    --
    procedure RetornaFuncoes                        ( p_num_reembolso           in number
                                                    , p_xml_retorno             out clob
                                                    );
    --
    procedure processa_reencaminhamento             ( p_cod_retorno             out number
                                                    , p_msg_retorno             out varchar2
                                                    , p_xml_parametros          in   clob
                                                    );
    --
    function get_rs_grupo_usuario                   ( p_cod_usuario             in varchar2
                                                    , p_num_pedido              in varchar2 default null
                                                    )
    return sys_refcursor;
    --
    function get_rs_pedido_por_grupo                ( p_xml_parametros          in clob )
    return sys_refcursor;
    --
    procedure get_xml_permissoes                    ( p_xml_retorno             out clob
                                                    , p_cod_retorno             out number
                                                    , p_msg_retorno             out varchar2
                                                    , p_cod_usuario             in varchar2
                                                    , p_cod_tipo_usuario        in varchar2
                                                    );
     --
    procedure processa_encaminhamento_mult          ( p_cod_retorno                out number
                                                    , p_msg_retorno                out varchar2
                                                    , p_xml_dados               in     clob
                                                    , p_num_pedido              in     varchar2
                                                    , p_txt_obs_operadora       in     varchar2
                                                    , p_cod_usuario             in     varchar2
                                                    );
    --
    function RetornaGrupoAnalisePrevia              ( p_num_reembolso       in  number )
    return sys_refcursor;
    --
    procedure ValidaPrevia                          ( p_num_reembolso        in number
                                                    , p_cod_usuario          in varchar2
                                                    , p_cod_retorno          out number
                                                    , p_msg_retorno          out varchar2
                                                    );
    --
    procedure RetornaAutorizacao                    ( p_num_associado       in  varchar2
                                                    , p_cod_tratamento      in  number default 0
                                                    , p_ind_internado       in  varchar2 default 'N'
                                                    , p_xml_retorno         out clob
                                                    , p_dt_inicio           in  date
                                                    , p_dt_fim              in  date
                                                    , p_cod_situacao        in  number default 0
                                                    );
    --
    procedure RetornaNumAutorizacao                 ( p_num_associado       in  varchar2
                                                    , p_num_pedido          in  varchar2
                                                    , p_msg_retorno         out varchar2);
    --
    function  RetornaOcorrenciaAssCon               ( p_cod_ts_contrato     in number, p_cod_ts in number)
    return sys_refcursor;
    --
    function RetornaContatoPrevia                   ( p_num_reembolso       in  varchar2) return sys_refcursor;
    --
    PROCEDURE RetornaDiasPrazo                      ( p_cod_inspetoria_ts     in     number
                                                    , p_cod_plano             in     number
                                                    , p_dias_prazo               out varchar2
                                                    , p_dias_uteis               out varchar2
                                                    , p_data_prazo               out varchar2
                                                    , p_cod_retorno              out number
                                                    , p_msg_retorno              out varchar2
                                                    , p_data_solicitacao      in     varchar2 default null
                                                    );

    PROCEDURE RetornaItensReemb                     ( p_num_reembolso         in     varchar2
                                                    , p_xml_retorno              out clob
                                                    );
    --
    procedure get_num_pedido_cam                    ( p_num_pedido                out    varchar2
                                                    , p_cod_situacao              out    pls_integer
                                                    , p_num_reembolso_operadora   in     varchar2
                                                    , p_num_associado             in     varchar2
                                                    );
    --
    procedure get_xml_associado                     ( p_xml_retorno             out clob
                                                    , p_num_associado        in     varchar2
                                                    , p_cod_ts               in     pls_integer  default 0
                                                    , p_ctr_logs             in     varchar2     default 'N'
                                                    );
    --
    procedure RetornaProcedimentoCAM                ( p_item_medico             in  varchar2
                                                    , p_num_associado           in  varchar2
                                                    , p_num_contrato               in  varchar2
                                                    , p_num_titular                in  varchar2
                                                    , p_cod_plano               in  varchar2
                                                    , p_dt_nascimento           in  varchar2
                                                    , p_ind_sexo                in  varchar2
                                                    , p_dt_atendimento          in  varchar2
                                                    , p_ind_tipo_reembolso      in  varchar2
                                                    , p_qtd_informado           in  number default 1
                                                    , p_ind_principal           in  varchar2 default 'N'
                                                    , p_ind_via                 in  varchar2 default null
                                                    , p_ind_doppler             in  varchar2 default null
                                                    , p_cod_motivo_reembolso    in  varchar2 default null
                                                    , p_ind_dobra_calculo       in  varchar2 default null
                                                    , p_ind_add_anestesista     in  varchar2 default null
                                                    , p_cod_inspetoria_ts          in  number   default 4
                                                    , p_cod_operadora              in  number   default 1
                                                    , p_ind_regulamentado          in  varchar2 default 'S'
                                                    , p_xml_retorno             out clob
                                                    );

    --
         procedure GeraOcorrencia                    (p_num_reembolso         in   number,
                                                     p_num_reembolso_ans     in   varchar2 DEFAULT null,
                                                     p_cod_ocorrencia        in   number,
                                                     p_txt_obs               in   varchar2,
                                                     p_txt_operadora         in   varchar2,
                                                     p_cod_usuario           in   varchar2,
                                                     p_cod_retorno           out  number,
                                                     p_msg_retorno           out  varchar2,
                                                     p_num_seq_item          in   number DEFAULT 0
                                                     );

    --
    function get_rs_memoria                         ( p_num_reembolso in  varchar2
                                                    , p_num_seq_item  in  number
                                                    )
    return sys_refcursor;
    --
    procedure getProtocolo(p_cod_operadora_atd in  varchar2
                          , p_cod_usuario_atd in  varchar2
                          , p_cod_ts_atd      in  varchar2
                        , p_cod_retorno    out varchar2
                        , p_msg_retorno    out varchar2
                        , p_num_protocolo  out varchar2
                        ) ;

    procedure gravaProtocolo ( p_num_protocolo                in  atd_protocolo_geral.num_protocolo%type
                           , p_cod_ts                       in  pedido_reembolso_previa.cod_ts%type
                           , p_cod_operadora                in  atd_protocolo_geral.cod_operadora%type
                           , p_nome_associado               in  beneficiario.nome_associado%type
                           , p_dt_geracao                   in  atd_protocolo_geral.dt_geracao%type
                           , p_cod_usuario      in  usuario.cod_usuario%type
                           ) ;
    --
end RB_PREVIA_REEMBOLSO;
/
CREATE OR REPLACE PACKAGE BODY rb_previa_reembolso is
    --pragma serially_reusable;
    --
    /****************************************************************************************
      ALTERA��O   : Inclus�o dos campos data_adaptacao e tem_aditivo
      SOLICITANTE : PRJ0111627#07
      ANALISTA    : F�bio Ferreira
      FUN��O      : RetornaDadosAssociado
      VERS�O      : 02/09/2018 16:05 - Vers�o SPC 1.30  / BDY 1.97
    ***************************************************************************************
      ALTERA��O   : Inclusao do c�lculo de coparticipa��o
      SOLICITANTE : Lidiane
      Data        : 22/06/2022 - Prj0157335 - Pbi-203659 - DET
    ***************************************************************************************
      ALTERA��O   : Carregar dados de copart na tela de m�moria de c�lculo de previa
      SOLICITANTE : Lidiane
      Data        : 07/07/2022 - Prj0157335 - Pbi-209563 - DET
    ***************************************************************************************/

    type param_xml_reg  is record ( nome     varchar2(50)
                                  , valor    varchar2(50)
                                  , tipo     varchar2(1)
                                  );
    --
    type param_xml_map  is table of param_xml_reg;
    --
    param_xml           param_xml_map := param_xml_map();
    --
    ----------------------------------------------------------------------------
    -- Retornar a vers�o da package no CVS
    ----------------------------------------------------------------------------
    function p_versao return Varchar2 is
    begin
        return '02/09/2018 16:05 - Vers�o SPC 1.30  / BDY 1.97';
    end;
    --
    ----------------------------------------------------------------------------
    -- Retorna a operadora - Acerto para Operadora ONE
    ----------------------------------------------------------------------------
    function retorna_cod_operadora ( p_cod_operadora in number, p_cod_rede_atendimento in number )
    return number
    is
        v_existe number;
    begin
        begin
           select 1
           into   v_existe
           from   rede_operadora
           where  cod_rede      = p_cod_rede_atendimento
           and    cod_operadora = 11;
        exception
        when no_data_found then
           v_existe := 0;
        end;
        --
        if v_existe = 1 and p_cod_operadora <> 19 then
           return 11;
        else
           return p_cod_operadora;
        end if;
    exception
    when no_data_found then
         return p_cod_operadora;
    end;
    --
    function retorna_nome_operadora ( p_cod_operadora in number, p_cod_rede_atendimento in number )
    return varchar2
    is
        v_existe number;
        v_nome_operadora operadora.nom_operadora%type;
        v_cod_operadora number;
    begin
        begin
           select 1
           into   v_existe
           from   rede_operadora
           where  cod_rede      = p_cod_rede_atendimento
           and    cod_operadora = 11;
        exception
        when no_data_found then
           v_existe := 0;
        end;
        --
        if v_existe = 1 and p_cod_operadora <> 19 then
           v_cod_operadora := 11;
        else
           v_cod_operadora  := p_cod_operadora;
        end if;
        --
        begin
            select nom_operadora
              into v_nome_operadora
              from operadora
             where cod_operadora = v_cod_operadora;
        exception
        when others then
           return null;
        end;
        --
        return v_nome_operadora;
        --
    exception
    when no_data_found then
        begin
            select nom_operadora
              into v_nome_operadora
              from operadora
             where cod_operadora = v_cod_operadora;
        exception
        when others then
           return null;
        end;
        --
        return v_nome_operadora;
        --
    end;
    --
    ----------------------------------------------------------------------------
    -- Retornar percentual de coparticipa��o - copart - det
    ----------------------------------------------------------------------------
        function retorna_per_copart(p_num_associado in varchar2,
                                    p_cod_procedimento in varchar2,
                                    v_per_copart_default in number
                                ) return number
    is
        v_per number:= NULL;
        v_xml_copart clob;
        v_doc_copart xmldom.DOMDocument;
        v_cod_retorno   number;
        v_msg_retorno   clob;

    begin
        ctm_copart_val_procedimento.CM_CALCULA_PRINCIPAL(
            'A'
            ,p_num_associado
            ,p_cod_procedimento,'','','',''
            ,v_xml_copart,v_msg_retorno,v_cod_retorno);
        if v_cod_retorno = 0 then
            --Ler informa��es do XML
            ts_cria_doc_xml(v_xml_copart, v_doc_copart, v_cod_retorno, v_msg_retorno);
            if v_cod_retorno = 0 then
                if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc_copart,'VALORACAO','DETALHE_1/PERCENTUAL_1')) then
                    v_per := ts_numero_web(ts_obtem_dados_xml(v_doc_copart,'VALORACAO','DETALHE_1/PERCENTUAL_1'),2)/100;
                end if;
            end if;
            xmldom.freeDocument(v_doc_copart);
        end if;

        if (v_per IS NULL OR v_per = 0) AND v_per_copart_default > 0 then
            v_per := v_per_copart_default;
        end if;
        return v_per;
    exception
    when others then
     ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Retorno:' || v_per
                        , 'func retorna_per_copart' );
         return 0;
    end;
    --
   ----------------------------------------------------------------------------
    -- Retornar valor limite de coparticipa��o - copart - det
    ----------------------------------------------------------------------------
    function retorna_val_limite_copart(p_cod_plano in varchar2,
                                       p_cod_ts_contrato in varchar2,
                                       p_cod_procedimento in varchar2,
                                       p_val_fixo out number,
                                       p_val_pct_participacao out number,
                                       p_ind_condicao_reembolso out varchar2
                                       ) return number
    is
        v_val_limite number:=0;

    begin

        WITH COPART_TIPO_COMPOSICAO AS
		(
			 SELECT NVL(pc.val_limite, pc.val_participacao) val_limite, pc.val_participacao val_fixo, pc.dt_ini_vigencia, pc.ind_condicao_reembolso, pc.pct_participacao, DECODE(pc.cod_participacao,p_cod_procedimento,0,1) tipo_composicao
					FROM participacao_contrato pc
					WHERE
                        cod_plano = p_cod_plano  AND cod_ts_contrato = p_cod_ts_contrato
                        AND SYSDATE  BETWEEN pc.dt_ini_validade AND NVL(pc.dt_fim_validade, SYSDATE)
						AND dt_ini_vigencia <= SYSDATE
                        AND
                        (
                            pc.cod_participacao = p_cod_procedimento
                            OR
                            pc.cod_participacao IN (SELECT gp.cod_participacao_ts FROM grupo_participacao gp WHERE gp.cod_participacao = p_cod_procedimento)
                        )
		),
		COPART_PROCEDIMENTO AS
		(
			SELECT val_limite, val_fixo, dt_ini_vigencia, ind_condicao_reembolso, pct_participacao
            FROM COPART_TIPO_COMPOSICAO
            WHERE tipo_composicao = 0
		),
		COPART_GRUPO_PROCEDIMENTO AS
		(
			SELECT val_limite, val_fixo, dt_ini_vigencia, ind_condicao_reembolso, pct_participacao
            FROM COPART_TIPO_COMPOSICAO
            WHERE NOT EXISTS(SELECT 1 FROM COPART_PROCEDIMENTO)
		),
		COPART AS
		(
			SELECT val_limite, val_fixo, dt_ini_vigencia, ind_condicao_reembolso, pct_participacao
            FROM COPART_PROCEDIMENTO
            UNION ALL
			SELECT val_limite, val_fixo, dt_ini_vigencia, ind_condicao_reembolso, pct_participacao
            FROM COPART_GRUPO_PROCEDIMENTO
		),
		COPART_VIGENTE AS
		(
			SELECT val_limite, val_fixo, ind_condicao_reembolso, pct_participacao
				FROM COPART
				WHERE dt_ini_vigencia IN (SELECT MAX(dt_ini_vigencia) FROM COPART)
		),
        COPART_MAIOR_LIMITE AS
        (
            SELECT  ind_condicao_reembolso, NVL(val_limite, 0) val_limite , val_fixo, pct_participacao
                FROM COPART_VIGENTE
                WHERE val_limite IS NULL OR val_limite IN (SELECT MAX(val_limite) FROM COPART_VIGENTE)
        ),
		COPART_MAIOR_PERC AS
		(
			SELECT  ind_condicao_reembolso, val_limite, val_fixo, pct_participacao
				FROM COPART_MAIOR_LIMITE
             WHERE pct_participacao IS NULL OR pct_participacao IN (SELECT MAX(pct_participacao) FROM COPART_MAIOR_LIMITE)
		)
		SELECT  ind_condicao_reembolso, val_limite, val_fixo, (pct_participacao / 100) INTO p_ind_condicao_reembolso, v_val_limite, p_val_fixo, p_val_pct_participacao
             FROM COPART_MAIOR_PERC
             WHERE ROWNUM = 1
             ORDER BY val_limite DESC, pct_participacao DESC;


        return v_val_limite;
    exception
        when no_data_found then
			p_ind_condicao_reembolso := 0;
            return 0;
        when others then
			p_ind_condicao_reembolso := 0;
            ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 11
                        , 'Erro n�o previsto ' || p_cod_plano || '- ' || p_cod_ts_contrato || '- ' || p_cod_procedimento
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Retorno:' || v_val_limite
                        , 'func retorna_val_limite_copart' );
            return 0;
    end;

    procedure preenche_dados_copart(p_cod_plano         IN  VARCHAR2,
                                    p_cod_ts_contrato   IN  VARCHAR2,
                                    p_num_associado     IN  VARCHAR2,
                                    p_cod_procedimento  IN  VARCHAR2,
                                    p_val_fixo          OUT NUMBER,
                                    p_val_per_copart    OUT NUMBER,
                                    p_val_limite_copart OUT NUMBER)
    is
        v_ind_condicao_reembolso         varchar2(1);
    begin
        p_val_limite_copart := retorna_val_limite_copart(p_cod_plano, p_cod_ts_contrato, p_cod_procedimento, p_val_fixo, p_val_per_copart, v_ind_condicao_reembolso);
        IF v_ind_condicao_reembolso <> 1 THEN -- S� calcula a copart se tiver a condi��o 'Abate reembolso' = 1
            p_val_per_copart := 0;
            p_val_fixo := NULL;
			p_val_limite_copart := NULL;
        ELSIF p_val_fixo IS NULL THEN -- Quando o valor j� vem preenchido � pq � fixo
            p_val_per_copart := retorna_per_copart(p_num_associado,p_cod_procedimento, p_val_per_copart);
        END IF;
    end;

    ----------------------------------------------------------------------------
    -- Retornar um parametro da controle sistema
    ----------------------------------------------------------------------------
    procedure RetornaParametro (p_cod_parametro   in varchar2,
                                p_val_parametro  out varchar2,
                                p_val_default     in varchar2 default '')
    is

    begin

        begin
            select val_parametro
            into   p_val_parametro
            from   controle_sistema
            where  cod_parametro = p_cod_parametro;
        exception
            when no_data_found then
                p_val_parametro := p_val_default;
        end;

        return;
    end;
    --
    --
    function get_valor_xml ( p_xml               in clob
                           , p_nome_path         in varchar2
                           , p_nome_no           in varchar2
                           )
    return varchar2
    is
        v_retorno               varchar2(4000);
    begin
        --
        select extractValue ( sys.XMLType ( p_xml )
                            , p_nome_path || '/' || p_nome_no
                            )
        into   v_retorno
        from   dual;
        --
        return v_retorno;
        --
    exception
        when others then
        --
        ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'XML:' || p_xml
                        , 'get_valor_xml' );
        --
        return '';
    end;
    --
    --
    -----------------------------------------------------------------
    -- tratamento dos erros, quanto a libera��o das glosas da previa
    -----------------------------------------------------------------
    procedure processa_liberacao_glosa ( p_cod_retorno                  out pls_integer
                                       , p_msg_retorno                  out varchar2
                                       , p_xml_dados                 in     clob
                                       , p_num_reembolso             in     varchar2
                                       , p_cod_usuario               in     varchar2
                                       )
    is
        --
        cursor cur_glosa      (pXML in sys.XMLType)
        is
        select nvl(extractValue( VALUE(T) , '//IND_ANALISE'     ),'N') IND_ANALISE
             , nvl(extractValue( VALUE(T) , '//COD_MOTIVO_GLOSA'),0  ) COD_MOTIVO_GLOSA
        from   table ( xmlsequence ( extract(pXML,'/LIBERACAO_GLOSA/GLOSA') ) ) T;
        --
        V_XML                           sys.XMLType;
        --
    begin
        --
        V_XML := sys.XMLType.createXML( p_xml_dados );
        --
        for rc_glosa in cur_glosa (V_XML) loop
            --
            if rc_glosa.cod_motivo_glosa > 0 then
                --
                if rc_glosa.ind_analise = 'S' then
                    -- Liberar a glosa informada
                    update /*  rb_previa_reembolso.processa_libera_glosa  */
                           reembolso_previa_glosa
                    set    ind_analisado         = rc_glosa.ind_analise
                         , dt_liberacao          = sysdate
                         , cod_usuario_liberacao = p_cod_usuario
                         , txt_obs               = null
                    where  num_reembolso         = p_num_reembolso
                    and    cod_motivo_glosa      = rc_glosa.cod_motivo_glosa;
                else
                    update /*  rb_previa_reembolso.processa_libera_glosa  */
                           reembolso_previa_glosa
                    set    ind_analisado         = rc_glosa.ind_analise
                         , dt_liberacao          = null
                         , cod_usuario_liberacao = null
                         , txt_obs               = null
                    where  num_reembolso         = p_num_reembolso
                    and    cod_motivo_glosa      = rc_glosa.cod_motivo_glosa;
                end if;
            end if;
        end loop;
        --
        p_cod_retorno := 0;
        p_msg_retorno := '';
    exception
    when others then
        p_cod_retorno:=9;
        p_msg_retorno:='Ocorreu um erro ao liberar as glosas da cabe�a: ' || chr(13) || sqlerrm;
    end;
    --
    -----------------------------------------------------------------
    procedure calcula_total_pedido     ( p_num_reembolso             in     number )
    is
        v_val_reembolsado   pedido_reembolso_previa.val_reembolsado%type;
        v_val_calculado     pedido_reembolso_previa.val_calculado%type;
        v_val_apresentado   pedido_reembolso_previa.val_informado%type;
    begin
        --
        begin
          select SUM(val_reembolsado-val_calculado_participacao)
             ,   SUM(val_calculado)
             ,   SUM(val_apresentado)
          into
               v_val_reembolsado
             , v_val_calculado
             , v_val_apresentado
          from procedimento_reembolso_previa
         where num_reembolso          = p_num_reembolso
           and ind_situacao_funcao    = 'A'
           and nvl(ind_situacao,'A')  = 'A'
           and val_reembolsado        > 0;

          UPDATE pedido_reembolso_previa SET
                 val_reembolsado             = v_val_reembolsado
               , val_calculado               = v_val_calculado
               , val_informado               = v_val_apresentado
          where  num_reembolso               = p_num_reembolso;
        exception
          when others then
             null;
        end;
        --
    exception
    when others then
        null;
    end;
    --
    --
    --
    /*procedure sur_obtem_prox_dia_util (p_Data_In IN varchar2,
                                       p_cod_municipio_in IN municipio.cod_municipio%TYPE,
                                       p_qtd_dias_uteis in number,
                                       p_qtd_dias_corridos out number,
                                       p_data_out out varchar2
                                       )
    AS
      ERR_TRATA_ERRO EXCEPTION;
      ERR_PREVISTO EXCEPTION;
      ERR_NAO_PREVISTO EXCEPTION;
      ERR_ADVERTENCIA EXCEPTION;

      v_Data            DATE;
      v_Achou           PLS_INTEGER;
      v_Sgl_UF          MUNICIPIO.SGL_UF%TYPE;
      v_Cod_Municipio   MUNICIPIO.COD_MUNICIPIO%TYPE;
      v_Is_Dia_Inutil   BOOLEAN;
      v_Posicao         PLS_INTEGER;
      v_Txt_Parametros  VARCHAR2(500);
      v_Msg_Retorno     VARCHAR2(500);
      v_Erro_Oracle     VARCHAR2(500);
      v_Ind_Erro        VARCHAR2(1);
      v_qtd_dias_corridos number;
      v_dias_controle     number;
    BEGIN
      BEGIN
        v_Txt_Parametros := 'p_Data_In: '||p_Data_In||
                            ' p_cod_municipio_in: '||p_cod_municipio_in;

        v_Data := TO_DATE(p_Data_In,'dd/mm/yyyy');

        v_qtd_dias_corridos := 0;
        v_Is_Dia_Inutil := TRUE;
        v_dias_controle := 1;

        WHILE v_Is_Dia_Inutil
        LOOP
          v_Is_Dia_Inutil := FALSE;
          -- Data caiu no s�bado ou domingo, ajusta data.
          IF TO_CHAR(v_Data,'D') = '7' THEN
            v_Data := v_Data + 2;
            v_Is_Dia_Inutil := TRUE;
            v_qtd_dias_corridos := v_qtd_dias_corridos + 2;
          ELSIF TO_CHAR(v_Data,'D') = '1' THEN
            v_Data := v_Data + 1;
            v_Is_Dia_Inutil := TRUE;
            v_qtd_dias_corridos := v_qtd_dias_corridos + 1;
          END IF;


          IF p_cod_municipio_in is not null THEN
             BEGIN
               -- Obtem o UF do munic�pio.
               BEGIN
                 SELECT M.SGL_UF
                    INTO  v_Sgl_UF
                       FROM  MUNICIPIO M
                             WHERE M.COD_MUNICIPIO = p_cod_municipio_in;
               EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                       v_Msg_Retorno := 'Munic�pio n�o encontrado.('||p_cod_municipio_in||').';
                       RAISE ERR_PREVISTO;
               END;

               SELECT 1
               INTO v_Achou
               FROM DUAL
               WHERE EXISTS(SELECT NULL
                            FROM FERIADO_SUR
                            WHERE DT_FERIADO = TRUNC(v_Data)
                              AND ((IND_TIPO_FERIADO = 'M' -- Municipal
                                    AND
                                    COD_MUNICIPIO = v_Cod_Municipio)
                                   OR
                                   (IND_TIPO_FERIADO = 'E' -- Estadual
                                    AND
                                    SGL_UF = v_Sgl_UF)
                                   OR
                                   IND_TIPO_FERIADO = 'N'));

               v_Data := v_Data + 1;
               v_qtd_dias_corridos := v_qtd_dias_corridos + 1;

               v_Is_Dia_Inutil := TRUE;
             EXCEPTION
             WHEN NO_DATA_FOUND THEN
               NULL;
             END;
          END IF;

          if v_dias_controle <= p_qtd_dias_uteis and v_Is_Dia_Inutil = false then
            v_Data := v_Data + 1;
            v_Is_Dia_Inutil := true;
            v_dias_controle := v_dias_controle + 1;
            v_qtd_dias_corridos := v_qtd_dias_corridos + 1;
          end if;
        END LOOP;

        p_qtd_dias_corridos := v_qtd_dias_corridos;
        p_data_out :=  to_char(v_Data,'DD/MM/YYYY');
        --RETURN to_char(v_Data,'DD/MM/YYYY');

      EXCEPTION
      WHEN ERR_ADVERTENCIA THEN
        v_Ind_Erro := '3';
      WHEN ERR_PREVISTO THEN
        v_Ind_Erro := '1';
        RAISE ERR_TRATA_ERRO;
      WHEN ERR_NAO_PREVISTO THEN
        v_Ind_Erro := '2';
        RAISE ERR_TRATA_ERRO;
      WHEN OTHERS THEN
        v_Msg_Retorno := SQLERRM;
        v_Erro_Oracle := SQLERRM;
        v_Ind_Erro := '2';
        RAISE ERR_TRATA_ERRO;
      END;
    EXCEPTION
    WHEN ERR_TRATA_ERRO THEN
      TS_LOG_EXECUCAO('RB_PREVIA_REEMBOLSO',
                       v_Posicao,
                       v_Erro_Oracle,
                       v_txt_parametros,
                       v_Msg_Retorno);
    END;*/
    --
    --

    procedure RetornaOcorrenciaAss (p_num_associado   in varchar2,
                                    p_ind_ocorrencia  out NUMBER )
    is
       v_ind_ocorrencia number;
    begin

        begin

            select count(*)
             into v_ind_ocorrencia
             from ts.ocorrencia_associado oa, associado a
             where  a.cod_ts = oa.cod_ts
             and oa.cod_ocorrencia  = 202
             and a.num_associado = p_num_associado;


             p_ind_ocorrencia:= v_ind_ocorrencia;

        exception
            when no_data_found then
                p_ind_ocorrencia := 0;
        end;

        return;
    end;
    --
    --



    ----------------------------------------------------------------------------
    -- Revalida os procedimentos do reembolso no momento de salvar
    ----------------------------------------------------------------------------
    procedure revalida_proc_reembolso ( p_cod_retorno                  out pls_integer
                           , p_msg_retorno                  out varchar2
                           , p_num_reembolso              in     varchar2
                           , p_cod_usuario          in  varchar2
                           )
    is
      --
      v_num_associado                 pedido_reembolso_previa.num_associado%type;
      v_num_contrato                  pedido_reembolso_previa.num_contrato%type;
      v_num_titular                   pedido_reembolso_previa.num_titular%type;
      v_cod_ts_contrato               pedido_reembolso_previa.cod_ts_contrato%type;
      v_cod_plano                     pedido_reembolso_previa.cod_plano%type;
      v_dt_nascimento                 pedido_reembolso_previa.data_nascimento%type;
      v_ind_sexo                      pedido_reembolso_previa.ind_sexo%type;
      v_dt_atendimento                pedido_reembolso_previa.dt_inclusao%type;
      v_ind_tipo_reembolso            pedido_reembolso_previa.ind_tipo_reembolso%type;
      v_cod_motivo_reembolso          pedido_reembolso_previa.cod_motivo_reembolso%type;
      v_ind_situacao                  reembolso_previa_situacao.ind_situacao%type;
      rsProcedimento                  procedimento_reembolso_previa%rowtype;
      v_ind_funcoes                   varchar(400);
      v_ind_achou                     varchar(1);
      v_qtd_funcoes                   number;
      v_qtd_funcoes_antigo            number;
      v_cod_ts_tit                    pedido_reembolso_previa.cod_ts_tit%type;
      v_num_reembolso                 number;
      v_cod_inspetoria_ts             number;
      v_cod_operadora                 number;
      rsMemoriaCalculo                memoria_previa_detalhe%rowtype;
      v_doc                           xmldom.DOMDocument;
      v_xml_memoria                   clob;
      v_cod_retorno                   number;
      v_msg_retorno                   clob;
      v_existe_glosa_341_analisada    number;
      v_posicao                       varchar2(1000);
      v_val_fixo                       procedimento_reembolso_previa.val_calculado%type;
      v_val_per_copart                 procedimento_reembolso_previa.val_calculado%type;
      v_val_limite_copart              procedimento_reembolso_previa.val_calculado%type;
      v_num_seq_grupo_procedimento     procedimento_reembolso_previa.num_seq_grupo_procedimento%type;
    begin
      --
      v_posicao := '1';
      v_num_seq_grupo_procedimento := 0;
      --
      select pr.num_associado
         , pr.num_titular
         , pr.num_contrato
         , pr.cod_ts_contrato
         , pr.cod_plano
         , pr.data_nascimento
         , pr.ind_sexo
         , pr.dt_inclusao
         , pr.ind_tipo_reembolso
         , pr.cod_motivo_reembolso
         , pr.cod_ts_tit
         , pr.cod_inspetoria_ts_abertura
         , pr.cod_operadora_contrato
         , pr.ind_situacao
        into v_num_associado
         , v_num_titular
         , v_num_contrato
         , v_cod_ts_contrato
         , v_cod_plano
         , v_dt_nascimento
         , v_ind_sexo
         , v_dt_atendimento
         , v_ind_tipo_reembolso
         , v_cod_motivo_reembolso
         , v_cod_ts_tit
         , v_cod_inspetoria_ts
         , v_cod_operadora
         , v_ind_situacao
        from pedido_reembolso_previa pr
       where num_reembolso = p_num_reembolso;
      --
      delete from memoria_reembolso_previa
       where num_reembolso = p_num_reembolso;
      --
      delete from memoria_previa_detalhe
       where num_reembolso = p_num_reembolso;
      --
      v_num_reembolso := p_num_reembolso;
      --
      for p in ( select  prp.cod_procedimento_cm
                       , prp.ind_via
                       , prp.ind_doppler
                       , prp.qtd_informado
                       , prp.ind_dobra_calculo
                       , prp.ind_principal
                       , prp.ind_add_anestesista
                       , prp.cod_especialidade
             from     procedimento_reembolso_previa prp
            where     prp.num_reembolso = p_num_reembolso
--            and       nvl(prp.ind_situacao_funcao,'A') = 'A'
            group by  prp.cod_procedimento_cm, prp.ind_via, prp.ind_doppler, prp.qtd_informado, prp.ind_dobra_calculo, prp.ind_principal, prp.ind_add_anestesista, prp.cod_especialidade
      ) loop
        --
        v_ind_funcoes := '';
        v_qtd_funcoes := 0;
        v_qtd_funcoes_antigo := 0;
        --
        begin
            select count(*)
              into v_qtd_funcoes_antigo
              from procedimento_reembolso_previa prp
             where prp.num_reembolso = p_num_reembolso
               and prp.cod_procedimento_cm = p.cod_procedimento_cm
               and ind_funcao is not null;
        exception
            when others then
                v_qtd_funcoes_antigo := 0;
        end;
        --
        --dbms_output.put_line('inicio');
        --
        for f in ( select fv.*
               from table (   ts.rb_calcula_previa_reembolso.retornafuncoesvalores(
                         /*p_num_associado          =>*/ v_num_associado
                       , /*p_dt_atendimento         =>*/ v_dt_atendimento
                       , /*p_cod_ts_contrato        =>*/ v_cod_ts_contrato
                       , /*p_num_contrato           =>*/ v_num_contrato
                       , /*p_cod_ts_tit             =>*/ v_cod_ts_tit
                       , /*p_num_titular            =>*/ v_num_titular
                       , /*p_item_medico            =>*/ p.cod_procedimento_cm
                       , /*p_cod_plano              =>*/ v_cod_plano
                       , /*p_dt_nascimento          =>*/ v_dt_nascimento
                       , /*p_ind_sexo               =>*/ v_ind_sexo
                       , /*p_ind_tipo_reembolso     =>*/ v_ind_tipo_reembolso
                       , /*p_qtd_realizada          =>*/ p.qtd_informado
                       , /*p_ind_via_acesso         =>*/ p.ind_via
                       , p.ind_doppler
                       , /*p_ind_principal          =>*/ p.ind_principal
                       , /*p_cod_motivo_reembolso   =>*/ v_cod_motivo_reembolso
                       , /*p_ind_dobra_calculo      =>*/ p.ind_dobra_calculo
                       , /*p_ind_add_anestesista    =>*/ p.ind_add_anestesista
                       , /*p_cod_inspetoria_ts      =>*/ v_cod_inspetoria_ts
                       , /*p_cod_operadora          =>*/ v_cod_operadora
                      )
                    )  fv
        ) loop
          v_ind_achou := 'N';
          v_posicao := '10';

          for cur_procedimento in ( select *
                                      from procedimento_reembolso_previa
                                     where num_reembolso       = p_num_reembolso
                                       and cod_procedimento_cm = p.cod_procedimento_cm
                                       and nvl(ind_via,'X')    = nvl(p.ind_via,'X')
                                       and ind_funcao          = lpad(NVL(f.ind_funcao,'99'),2,'0')
                                       and nvl(cod_especialidade, 'X') = nvl(p.cod_especialidade, 'X')
                                       order by num_seq_item
               )
          loop
          begin

            select *
              into rsProcedimento
              from procedimento_reembolso_previa
             where num_reembolso       = p_num_reembolso
               and cod_procedimento_cm = p.cod_procedimento_cm
               and nvl(ind_via,'X')    = nvl(p.ind_via,'X')
               and ind_funcao          = lpad(NVL(f.ind_funcao,'99'),2,'0')
               and nvl(cod_especialidade, 'X') = nvl(p.cod_especialidade, 'X')
               and num_seq_item        = cur_procedimento.num_seq_item
               ;

               v_ind_achou := 'S';
          exception
            when no_data_found then
              v_ind_achou := 'N';
            when others then

              p_cod_retorno := 9;
              p_msg_retorno := 'Erro ao revalidar a pr�via de reembolso. Posi��o: ' || v_posicao || ' - ' || sqlerrm;
              rollback;
              return;
          end;

          --dbms_output.put_line('f.val_calculado = ' || f.val_calculado);
          v_num_seq_grupo_procedimento              := v_num_seq_grupo_procedimento + 1;
          rsProcedimento.Num_Seq_Grupo_Procedimento := v_num_seq_grupo_procedimento;
          rsProcedimento.cod_procedimento_cm        := p.cod_procedimento_cm;
          rsProcedimento.cod_procedimento           := f.cod_procedimento;
          rsProcedimento.perc_funcao                := f.perc_funcao;
          rsProcedimento.cod_grupo_estatistico      := f.cod_grupo_estatistico;
          rsProcedimento.pct_cirurgia_multipla      := f.pct_cirurgia_multipla;
          rsProcedimento.ind_tipo_composicao        := f.ind_tipo_composicao;
          rsProcedimento.cod_reembolso              := f.cod_reembolso;
          rsProcedimento.cod_porte_rb               := f.cod_porte_rb;
          rsProcedimento.sigla_tabela_rb            := f.sigla_tabela_rb;
          rsProcedimento.sigla_tabela_taxas         := f.sigla_tabela_taxas;
          rsProcedimento.qtd_vezes_tabela           := f.qtd_vezes_tabela;
          rsProcedimento.pct_recibo                 := f.pct_recibo;
          rsProcedimento.val_calculado              := f.val_calculado;
          rsProcedimento.val_limite                 := f.val_limite;
          rsProcedimento.val_cotacao_rb             := f.val_cotacao_rb;
          rsProcedimento.val_cotacao_taxa           := f.val_cotacao_taxa;
          rsProcedimento.val_fixo                   := f.val_fixo;
          rsProcedimento.qtd_prazo_dias             := f.qtd_prazo_dias;
          rsProcedimento.sigla_moeda                := f.sigla_moeda;
          rsProcedimento.ind_cirurgia               := nvl(f.ind_cirurgia,'N');
          rsProcedimento.cod_concessao              := f.cod_concessao;
          rsProcedimento.ind_funcao                 := lpad(NVL(f.ind_funcao,'99'),2,'0');
          rsProcedimento.dt_atu                     := sysdate;
          rsProcedimento.cod_usuario_atu            := p_cod_usuario;
          rsProcedimento.cod_especialidade          := p.cod_especialidade;

          if v_ind_achou = 'S' then
            if ((v_ind_situacao = 3) or (v_ind_situacao = 4)) then
              rsProcedimento.val_reembolsado := 0;   
              rsProcedimento.val_calculado_participacao := 0;           
            elsif rsProcedimento.val_reembolsado > f.val_calculado then
              rsProcedimento.val_reembolsado := f.val_calculado;
            end if;


            update procedimento_reembolso_previa
               set row = rsProcedimento
             where num_reembolso       = p_num_reembolso
               and cod_procedimento_cm = p.cod_procedimento_cm
               and nvl(ind_via,'X')    = nvl(p.ind_via,'X')
               and ind_funcao          = lpad(NVL(f.ind_funcao,'99'),2,'0')
               and nvl(cod_especialidade, 'X') = nvl(p.cod_especialidade, 'X')
               and num_seq_item        = cur_procedimento.num_seq_item
               ;
             --
             if sql%rowcount = 0 then
               p_cod_retorno := 1;
               p_msg_retorno := 'Houve problema na tentativa de revaida os prodecimentos da previa de reembolso';
               rollback;
               return;
             end if;
         else
            --situacao_funcao = A
            if ((v_ind_situacao = 3) or (v_ind_situacao = 4)) then
              rsProcedimento.val_reembolsado := 0;
              rsProcedimento.val_calculado_participacao := 0;              
            else
              rsProcedimento.val_reembolsado          := f.val_calculado;
            end if;
            rsProcedimento.ind_situacao_funcao      := 'A';
            rsProcedimento.num_reembolso            := p_num_reembolso;
            rsProcedimento.ind_via                  := p.ind_via;
            rsProcedimento.qtd_informado            := p.qtd_informado;
            rsProcedimento.ind_dobra_calculo        := f.ind_dobra_calculo;
--            rsProcedimento.ind_exibe_dobra_calc     := f.ind_exibe_dobra_calc;
            rsProcedimento.ind_origem_anestesista   := f.ind_origem_anestesista;
            if f.ind_origem_anestesista = 'P' then
                rsProcedimento.ind_add_anestesista      := null;
            else
                rsProcedimento.ind_add_anestesista      := p.ind_add_anestesista;
            end if;
            rsProcedimento.ind_principal            := 'N';

            v_posicao := '100';

            SELECT max(num_seq_item) + 1
             into rsProcedimento.num_seq_item
             FROM procedimento_reembolso_previa
            Where num_reembolso = p_num_reembolso;

            begin
              insert into procedimento_reembolso_previa
              values rsProcedimento;
            exception
              when dup_val_on_index then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: posicao-' || v_posicao || ' - ' || sqlerrm;
                rollback;
                return;
              when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: ' || sqlerrm;
                rollback;
                return;
            end;
          end if;
          --
          begin

              v_posicao := '150';
              insert into ts.memoria_reembolso_previa
              (num_reembolso, num_seq_item, txt_memoria)
              VALUES
              (p_num_reembolso, rsProcedimento.num_seq_item, f.txt_memoria_calculo);
            exception
              when dup_val_on_index then
                --
                update ts.memoria_reembolso_previa
                   set txt_memoria = f.txt_memoria_calculo
                 where num_reembolso = p_num_reembolso
                   and num_seq_item  = rsProcedimento.num_seq_item;
                --
              when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: posicao-' || v_posicao || ' - ' || sqlerrm;
                rollback;
                return;
            end;
           --
           -- ######### INSERINDO DADOS DA MEMORIA DE CALCULO
           --
           begin
                --
                v_posicao := '200';
                v_xml_memoria := f.xml_memoria_calculo;
                --
                ts_cria_doc_xml(v_xml_memoria, v_doc, v_cod_retorno, v_msg_retorno);
                --
                --TS_LOG_EXECUCAO ( 'MEMORIA_PREVIA', 00, null, v_xml_memoria, 'debug');
                --
                rsMemoriaCalculo.num_reembolso    := p_num_reembolso;
                rsMemoriaCalculo.cod_usuario_atu  := p_cod_usuario;
                rsMemoriaCalculo.dt_atu           := sysdate;
                rsMemoriaCalculo.num_seq_item     := rsProcedimento.num_seq_item;
                rsMemoriaCalculo.cod_procedimento_calc := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PROCEDIMENTO');
                rsMemoriaCalculo.sigla_tabela_calc     := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PADRAO');
                rsMemoriaCalculo.cotacao_reembolso     := ts_obtem_dados_xml(v_doc,'MEMORIA','COTACAO_HONORARIO');
                rsMemoriaCalculo.ind_dobra_honorario   := ts_obtem_dados_xml(v_doc,'MEMORIA','IND_DOBRA_HONORARIO');
                rsMemoriaCalculo.pct_copart            := ts_obtem_dados_xml(v_doc,'MEMORIA','PCT_COPART');
                rsMemoriaCalculo.ind_calc_urgencia     := ts_obtem_dados_xml(v_doc,'MEMORIA','IND_URGENCIA');
                rsMemoriaCalculo.qtd_honorario         := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_HONORARIO');
                rsMemoriaCalculo.cod_porte             := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PORTE');
                rsMemoriaCalculo.val_porte             := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_PORTE'),2);
                rsMemoriaCalculo.cod_porte             := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PORTE');
                rsMemoriaCalculo.qtd_filme              := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_FILME');
                rsMemoriaCalculo.val_filme              := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_FILME'),2);
                rsMemoriaCalculo.cotacao_filme         := ts_obtem_dados_xml(v_doc,'MEMORIA','COTACAO_FILME');
                rsMemoriaCalculo.cod_padrao_valoracao  := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PADRAO_VALORACAO');
                rsMemoriaCalculo.cod_retorno           := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_RETORNO');
                rsMemoriaCalculo.msg_retorno           := ts_obtem_dados_xml(v_doc,'MEMORIA','MSG_RETORNO');
                rsMemoriaCalculo.val_tax_materiais   := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_TAXAS_MATERIAIS'),2);
                rsMemoriaCalculo.val_copart            := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_COPART'),2);
                rsMemoriaCalculo.qtd_vezes_tabela      := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_VEZES_TABELA');
                rsMemoriaCalculo.val_beneficio_anual   := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_TOTAL_ANUAL'),2);
                rsMemoriaCalculo.val_utilizado_beneficio := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_UTILIZADO_BENE'),2);
                rsMemoriaCalculo.saldo_beneficio_anual  := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','SALDO_ANUAL'),2);
                rsMemoriaCalculo.qtd_sessoes_anual      := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_SESSOES_ANUAL');
                rsMemoriaCalculo.qtd_utilizado_sessoes  := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_UTILIZADO_SESSOES');
                rsMemoriaCalculo.saldo_sessoes_anual    := ts_obtem_dados_xml(v_doc,'MEMORIA','SALDO_SESSOES_ANUAL');
                --
                rsMemoriaCalculo.val_reembolsado       := rsProcedimento.val_reembolsado;
                rsMemoriaCalculo.val_total             := rsProcedimento.val_calculado;
                rsMemoriaCalculo.qtd_informado         := rsProcedimento.qtd_informado;
                rsMemoriaCalculo.ind_via               := rsProcedimento.ind_via;
                rsMemoriaCalculo.pct_cirurgia_multipla := rsProcedimento.pct_cirurgia_multipla;
                rsMemoriaCalculo.ind_funcao            := rsProcedimento.ind_funcao;
                rsMemoriaCalculo.pct_funcao            := rsProcedimento.perc_funcao;
                rsMemoriaCalculo.pct_recibo            := rsProcedimento.pct_recibo;
                rsMemoriaCalculo.val_fixo              := rsProcedimento.val_fixo;
                rsMemoriaCalculo.val_limite            := rsProcedimento.val_limite;
                --
                --Tratar os dados de copart que est�o zerados
                preenche_dados_copart(v_cod_plano, v_cod_ts_contrato, v_num_associado, rsProcedimento.cod_procedimento_cm, v_val_fixo, v_val_per_copart, v_val_limite_copart);
                rsMemoriaCalculo.pct_copart := v_val_per_copart * 100;
                rsMemoriaCalculo.val_fixo   := v_val_fixo;
                rsMemoriaCalculo.val_limite := v_val_limite_copart;
                rsMemoriaCalculo.val_copart := rsProcedimento.val_calculado_participacao;
                --
                insert
                   into memoria_previa_detalhe
                 values rsMemoriaCalculo;
                --
           exception
            when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: posicao-' || v_posicao || ' - ' || top_utl_padrao.msgerro ;
                rollback;
                return;
           end;

           --
           -- guarda as funcoes para deletar depois as que n�o foram reincluidas
          v_ind_funcoes := v_ind_funcoes || lpad(NVL(f.ind_funcao,'99'),2,'0') || ',';
          v_qtd_funcoes := v_qtd_funcoes + 1;
          end loop;
          v_num_seq_grupo_procedimento := 0;
        end loop;
        --
        if v_qtd_funcoes > 0 or v_qtd_funcoes_antigo > 0 then
          for fe in ( select prp.num_seq_item, ind_funcao
                        from procedimento_reembolso_previa prp
                       where prp.num_reembolso          = p_num_reembolso
                         and prp.cod_procedimento_cm    = p.cod_procedimento_cm
                         and nvl(ind_via,'X')           = nvl(p.ind_via,'X')
                         and nvl(ind_funcao,'X')        not in ( select /*+cardinality(x,10)*/ nvl(x.column_value,'Z') from table( top_utl_padrao.split(v_ind_funcoes,',')) x )
          ) loop
          --
            begin
                delete from procedimento_reembolso_previa
                 where num_reembolso         = p_num_reembolso
                   and cod_procedimento_cm   = p.cod_procedimento_cm
                   and num_seq_item          = fe.num_seq_item;
                 --
                delete from reembolso_previa_glosa
                 where num_reembolso         = p_num_reembolso
                   and num_seq_item          = fe.num_seq_item;
                 --
                delete from memoria_reembolso_previa
                 where num_reembolso         = p_num_reembolso
                   and num_seq_item          = fe.num_seq_item;
            exception
                when others then
                    p_cod_retorno:=9;
                    p_msg_retorno:='Ocorreu um erro ao validar procedimentos do reembolso: ' || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || sqlerrm;
            end;
          --
          end loop;

        end if;

      end loop;
      --
      if nvl(v_ind_tipo_reembolso,0) = 1 then
          delete from procedimento_reembolso_previa
           where num_reembolso         =  p_num_reembolso
             and cod_procedimento_cm   != '10101012';
      end if;
      --
      p_cod_retorno := 0;
      p_msg_retorno := '';
    exception
    when others then
      p_cod_retorno:=9;
      p_msg_retorno:='Ocorreu um erro ao validar procedimentos do reembolso: ' || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || sqlerrm;
    end;
    --
    --
    procedure revalida_proc_reembolso_cam  ( p_cod_retorno                  out pls_integer
                           , p_msg_retorno                  out varchar2
                           , p_num_reembolso              in     varchar2
                           , p_cod_usuario          in  varchar2
                           )
    is
      --
      v_num_associado                 pedido_reembolso_previa.num_associado%type;
      v_num_contrato                  pedido_reembolso_previa.num_contrato%type;
      v_num_titular                   pedido_reembolso_previa.num_titular%type;
      v_cod_ts_contrato               pedido_reembolso_previa.cod_ts_contrato%type;
      v_cod_plano                     pedido_reembolso_previa.cod_plano%type;
      v_dt_nascimento                 pedido_reembolso_previa.data_nascimento%type;
      v_ind_sexo                      pedido_reembolso_previa.ind_sexo%type;
      v_dt_atendimento                pedido_reembolso_previa.dt_inclusao%type;
      v_ind_tipo_reembolso            pedido_reembolso_previa.ind_tipo_reembolso%type;
      v_cod_motivo_reembolso          pedido_reembolso_previa.cod_motivo_reembolso%type;
      rsProcedimento                  procedimento_reembolso_previa%rowtype;
      v_ind_funcoes                   varchar(400);
      v_ind_achou                     varchar(1);
      v_qtd_funcoes                   number;
      v_qtd_funcoes_antigo            number;
      v_cod_inspetoria_ts             pedido_reembolso_previa.cod_inspetoria_ts_abertura%type;
      v_cod_operadora                 pedido_reembolso_previa.cod_operadora_contrato%type;
      v_ind_regulamentado             pedido_reembolso_previa.ind_regulamentado%type;
      v_grupo_beneficio               procedimento_reembolso_previa.grupo_beneficio%type;
      rsMemoriaCalculo                memoria_previa_detalhe%rowtype;
      v_doc                           xmldom.DOMDocument;
      v_xml_memoria                   clob;
      v_cod_retorno                   number;
      v_msg_retorno                   clob;
      v_ind_situacao                  varchar2(2);
      v_posicao                       varchar2(1000);
      v_val_fixo                       procedimento_reembolso_previa.val_calculado%type;
      v_val_per_copart                 procedimento_reembolso_previa.val_calculado%type;
      v_val_limite_copart              procedimento_reembolso_previa.val_calculado%type;
    begin
      --
      v_posicao := '1';
      --
      select pr.num_associado
         , pr.num_contrato
         , pr.num_titular
         , pr.cod_ts_contrato
         , pr.cod_plano
         , pr.data_nascimento
         , pr.ind_sexo
         , pr.dt_inclusao
         , pr.ind_tipo_reembolso
         , pr.cod_motivo_reembolso
         , pr.cod_inspetoria_ts_abertura
         , pr.cod_operadora_contrato
         , pr.ind_regulamentado
         , pr.ind_situacao
        into v_num_associado
         , v_num_contrato
         , v_num_titular
         , v_cod_ts_contrato
         , v_cod_plano
         , v_dt_nascimento
         , v_ind_sexo
         , v_dt_atendimento
         , v_ind_tipo_reembolso
         , v_cod_motivo_reembolso
         , v_cod_inspetoria_ts
         , v_cod_operadora
         , v_ind_regulamentado
         , v_ind_situacao
        from pedido_reembolso_previa pr
       where num_reembolso = p_num_reembolso;
      --
      delete from memoria_reembolso_previa
       where num_reembolso = p_num_reembolso;
      --
      delete from memoria_previa_detalhe
       where num_reembolso = p_num_reembolso;

      --
      for p in ( select  prp.cod_procedimento_cm
                       , prp.cod_procedimento
                       , prp.ind_via
                       , prp.ind_doppler
                       , prp.qtd_informado
                       , prp.ind_dobra_calculo
                       , prp.ind_principal
                       , prp.ind_add_anestesista
                       , prp.grupo_beneficio
             from     procedimento_reembolso_previa prp
            where     prp.num_reembolso = p_num_reembolso
--            and       nvl(prp.ind_situacao_funcao,'A') = 'A'
            group by  prp.cod_procedimento_cm, prp.cod_procedimento, prp.ind_via, prp.ind_doppler, prp.qtd_informado, prp.ind_dobra_calculo, prp.ind_principal, prp.ind_add_anestesista, prp.grupo_beneficio
      ) loop
        --
        v_ind_funcoes := '';
        v_qtd_funcoes := 0;
        v_qtd_funcoes_antigo := 0;
        --
        begin
            select count(*)
              into v_qtd_funcoes_antigo
              from procedimento_reembolso_previa prp
             where prp.num_reembolso = p_num_reembolso
               and prp.cod_procedimento_cm = p.cod_procedimento_cm
               and ind_funcao is not null;
        exception
            when others then
                v_qtd_funcoes_antigo := 0;
        end;
        --
        if p.grupo_beneficio is null then
            begin
                SELECT CASE WHEN v_ind_regulamentado = 'S'
                        THEN to_char (a.cod_grupo_benef_atual)
                        ELSE to_char (a.cod_grupo_benef_velho)
                        END  grupo_beneficio
                INTO    v_grupo_beneficio
                FROM    amil_item_grupo_benef a
                WHERE  item_medico = p.cod_procedimento_cm;
            exception
                when no_data_found then
                    begin
                        SELECT CASE WHEN v_ind_regulamentado = 'S'
                                THEN to_char (a.cod_grupo_benef_atual)
                                ELSE to_char (a.cod_grupo_benef_velho)
                                END  grupo_beneficio
                        INTO    v_grupo_beneficio
                        FROM    amil_item_grupo_benef a
                        WHERE  item_medico = p.cod_procedimento;
                    exception
                    when no_data_found then
                        v_grupo_beneficio := null;
                    end;
            end;
        else
            v_grupo_beneficio := p.grupo_beneficio;
        end if;
        --
        for f in ( select fv.*
               from table (   ts.rb_calcula_previa_cam.retornafuncoesvalores(
                         /*p_num_associado          =>*/ v_num_associado
                       , /*p_num_contrato           =>*/ v_num_contrato
                       , /*p_num_titular            =>*/ v_num_titular
                       , /*p_dt_atendimentonvl      =>*/ v_dt_atendimento
                       , /*p_grupo_beneficio        =>*/ p.grupo_beneficio
                       , /*p_item_medico            =>*/ p.cod_procedimento_cm
                       , /*p_cod_plano              =>*/ v_cod_plano
                       , /*p_dt_nascimento          =>*/ v_dt_nascimento
                       , /*p_ind_sexo               =>*/ v_ind_sexo
                       , /*p_ind_tipo_reembolso     =>*/ v_ind_tipo_reembolso
                       , /*p_qtd_realizada          =>*/ p.qtd_informado
                       , /*p_ind_via_acesso         =>*/ p.ind_via
                       , /*ind_doppler              =>*/ p.ind_doppler
                       , /*p_ind_principal          =>*/ p.ind_principal
                       , /*p_cod_motivo_reembolso   =>*/ v_cod_motivo_reembolso
                       , /*p_ind_dobra_calculo      =>*/ p.ind_dobra_calculo
                       , /*p_ind_add_anestesista    =>*/ p.ind_add_anestesista
                       , /*p_cod_inspetoria_ts      =>*/ v_cod_inspetoria_ts
                       , /*p_cod_operadora          =>*/ v_cod_operadora
                      )
                    )  fv
        ) loop
          v_ind_achou := 'N';
          v_posicao   := '20';

          begin

            select *
              into rsProcedimento
              from procedimento_reembolso_previa
             where num_reembolso       = p_num_reembolso
               and cod_procedimento_cm = p.cod_procedimento_cm
               and nvl(ind_via,'X')    = nvl(p.ind_via,'X')
               and ind_funcao          = lpad(NVL(f.ind_funcao,'99'),2,'0');

               v_ind_achou := 'S';
          exception
            when no_data_found then
              v_ind_achou := 'N';
            when others then

              p_cod_retorno := 9;
              p_msg_retorno := 'Erro ao revalidar a pr�via de reembolso. Posi��o: ' || v_posicao || ' - ' || sqlerrm;
              rollback;
              return;
          end;

          rsProcedimento.cod_procedimento_cm        := p.cod_procedimento_cm;
          rsProcedimento.cod_procedimento           := f.cod_procedimento;
          rsProcedimento.perc_funcao                := f.perc_funcao;
          rsProcedimento.cod_grupo_estatistico      := f.cod_grupo_estatistico;
          rsProcedimento.pct_cirurgia_multipla      := f.pct_cirurgia_multipla;
          rsProcedimento.ind_tipo_composicao        := f.ind_tipo_composicao;
          rsProcedimento.cod_reembolso              := f.cod_reembolso;
          rsProcedimento.cod_porte_rb               := f.cod_porte_rb;
          rsProcedimento.sigla_tabela_rb            := f.sigla_tabela_rb;
          rsProcedimento.sigla_tabela_taxas         := f.sigla_tabela_taxas;
          rsProcedimento.qtd_vezes_tabela           := f.qtd_vezes_tabela;
          rsProcedimento.pct_recibo                 := f.pct_recibo;
          rsProcedimento.val_calculado              := f.val_calculado;
          rsProcedimento.val_limite                 := f.val_limite;
          rsProcedimento.val_cotacao_rb             := f.val_cotacao_rb;
          rsProcedimento.val_cotacao_taxa           := f.val_cotacao_taxa;
          rsProcedimento.val_fixo                   := f.val_fixo;
          rsProcedimento.qtd_prazo_dias             := f.qtd_prazo_dias;
          rsProcedimento.sigla_moeda                := f.sigla_moeda;
          rsProcedimento.ind_cirurgia               := nvl(f.ind_cirurgia,'N');
          rsProcedimento.cod_concessao              := f.cod_concessao;
          rsProcedimento.ind_funcao                 := lpad(NVL(f.ind_funcao,'99'),2,'0');
          rsProcedimento.dt_atu                     := sysdate;
          rsProcedimento.grupo_beneficio            := v_grupo_beneficio;
          rsProcedimento.cod_usuario_atu            := p_cod_usuario;

          if v_ind_achou = 'S' then
            if rsProcedimento.val_reembolsado > f.val_calculado then
              rsProcedimento.val_reembolsado := f.val_calculado;
            end if;

            update procedimento_reembolso_previa
               set row = rsProcedimento
             where num_reembolso       = p_num_reembolso
               and cod_procedimento_cm = p.cod_procedimento_cm
               and nvl(ind_via,'X')    = nvl(p.ind_via,'X')
               and ind_funcao          = lpad(NVL(f.ind_funcao,'99'),2,'0');
             --
             if sql%rowcount = 0 then
               p_cod_retorno := 1;
               p_msg_retorno := 'Houve problema na tentativa de revaida os prodecimentos da previa de reembolso';
               rollback;
               return;
             end if;
           else
            --situacao_funcao = A
            rsProcedimento.val_reembolsado          := f.val_calculado;
            rsProcedimento.ind_situacao_funcao      := 'A';
            rsProcedimento.num_reembolso            := p_num_reembolso;
            rsProcedimento.ind_via                  := p.ind_via;
            rsProcedimento.qtd_informado            := p.qtd_informado;
            rsProcedimento.ind_dobra_calculo        := f.ind_dobra_calculo;
            rsProcedimento.ind_exibe_dobra_calc     := f.ind_exibe_dobra_calc;
            rsProcedimento.ind_origem_anestesista   := f.ind_origem_anestesista;

            if f.ind_origem_anestesista = 'P' then
                rsProcedimento.ind_add_anestesista      := null;
            else
                rsProcedimento.ind_add_anestesista      := p.ind_add_anestesista;
            end if;
            rsProcedimento.ind_principal            := 'N';


            SELECT max(num_seq_item) + 1
             into rsProcedimento.num_seq_item
             FROM procedimento_reembolso_previa
            Where num_reembolso = p_num_reembolso;

            begin
              insert into procedimento_reembolso_previa
              values rsProcedimento;
            exception
              when dup_val_on_index then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: ' || sqlerrm;
                rollback;
                return;
              when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: ' || sqlerrm;
                rollback;
                return;
            end;
          end if;
          begin
              insert into ts.memoria_reembolso_previa
              (num_reembolso, num_seq_item, txt_memoria)
              VALUES
              (p_num_reembolso, rsProcedimento.num_seq_item, f.txt_memoria_calculo);
            exception
              when dup_val_on_index then
                --
                update ts.memoria_reembolso_previa
                   set txt_memoria = f.txt_memoria_calculo
                 where num_reembolso = p_num_reembolso
                   and num_seq_item  = rsProcedimento.num_seq_item;
                --
              when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: ' || sqlerrm;
                rollback;
                return;
            end;
             --
           -- ######### INSERINDO DADOS DA MEMORIA DE CALCULO
           --
           begin
                --
                v_xml_memoria := f.xml_memoria_calculo;
                --
                ts_cria_doc_xml(v_xml_memoria, v_doc, v_cod_retorno, v_msg_retorno);
                --
                rsMemoriaCalculo.num_reembolso    := p_num_reembolso;
                rsMemoriaCalculo.cod_usuario_atu  := p_cod_usuario;
                rsMemoriaCalculo.dt_atu           := sysdate;
                rsMemoriaCalculo.num_seq_item     := rsProcedimento.num_seq_item;
                rsMemoriaCalculo.cod_procedimento_calc := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PROCEDIMENTO');
                rsMemoriaCalculo.sigla_tabela_calc     := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PADRAO');
                rsMemoriaCalculo.cotacao_reembolso     := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','COTACAO_HONORARIO'),4);
                rsMemoriaCalculo.ind_dobra_honorario   := ts_obtem_dados_xml(v_doc,'MEMORIA','IND_DOBRA_HONORARIO');
                rsMemoriaCalculo.pct_copart            := ts_obtem_dados_xml(v_doc,'MEMORIA','PCT_COPART');
                rsMemoriaCalculo.ind_calc_urgencia     := ts_obtem_dados_xml(v_doc,'MEMORIA','IND_URGENCIA');
                rsMemoriaCalculo.qtd_honorario         := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_HONORARIO');
                rsMemoriaCalculo.cod_porte             := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PORTE');
                rsMemoriaCalculo.val_porte             := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_PORTE'),2);
                rsMemoriaCalculo.cod_porte             := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PORTE');
                rsMemoriaCalculo.qtd_filme              := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_FILME');
                rsMemoriaCalculo.val_filme              := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_FILME'),2);
                rsMemoriaCalculo.cotacao_filme         := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','COTACAO_FILME'),4);
                rsMemoriaCalculo.cod_padrao_valoracao  := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_PADRAO_VALORACAO');
                rsMemoriaCalculo.cod_retorno           := ts_obtem_dados_xml(v_doc,'MEMORIA','COD_RETORNO');
                rsMemoriaCalculo.msg_retorno           := ts_obtem_dados_xml(v_doc,'MEMORIA','MSG_RETORNO');
                rsMemoriaCalculo.val_tax_materiais   := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_TAXAS_MATERIAIS'),2);
                rsMemoriaCalculo.val_copart            := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_COPART'),2);
                rsMemoriaCalculo.qtd_vezes_tabela      := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_VEZES_TABELA');
                rsMemoriaCalculo.val_beneficio_anual   := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_TOTAL_ANUAL'),2);
                rsMemoriaCalculo.val_utilizado_beneficio := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','VAL_UTILIZADO_BENE'),2);
                rsMemoriaCalculo.saldo_beneficio_anual  := ts_numero_web(ts_obtem_dados_xml(v_doc,'MEMORIA','SALDO_ANUAL'),2);
                rsMemoriaCalculo.qtd_sessoes_anual      := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_SESSOES_ANUAL');
                rsMemoriaCalculo.qtd_utilizado_sessoes  := ts_obtem_dados_xml(v_doc,'MEMORIA','QTD_UTILIZADO_SESSOES');
                rsMemoriaCalculo.saldo_sessoes_anual    := ts_obtem_dados_xml(v_doc,'MEMORIA','SALDO_SESSOES_ANUAL');

                rsMemoriaCalculo.val_reembolsado       := rsProcedimento.val_reembolsado;
                rsMemoriaCalculo.val_total             := rsProcedimento.val_calculado;
                rsMemoriaCalculo.qtd_informado         := rsProcedimento.qtd_informado;
                rsMemoriaCalculo.ind_via               := rsProcedimento.ind_via;
                rsMemoriaCalculo.pct_cirurgia_multipla := rsProcedimento.pct_cirurgia_multipla;
                rsMemoriaCalculo.ind_funcao            := rsProcedimento.ind_funcao;
                rsMemoriaCalculo.pct_funcao            := rsProcedimento.perc_funcao;
                rsMemoriaCalculo.pct_recibo            := rsProcedimento.pct_recibo;
                rsMemoriaCalculo.val_fixo              := rsProcedimento.val_fixo;
                rsMemoriaCalculo.val_limite            := rsProcedimento.val_limite;

                --Tratar os dados de copart que est�o zerados
                preenche_dados_copart(v_cod_plano, v_cod_ts_contrato, v_num_associado, rsProcedimento.cod_procedimento_cm, v_val_fixo, v_val_per_copart, v_val_limite_copart);
                rsMemoriaCalculo.pct_copart := v_val_per_copart * 100;
                rsMemoriaCalculo.val_fixo   := v_val_fixo;
                rsMemoriaCalculo.val_limite := v_val_limite_copart;
                rsMemoriaCalculo.val_copart := rsProcedimento.val_calculado_participacao;
                insert
                   into memoria_previa_detalhe
                 values rsMemoriaCalculo;

                --TS_LOG_EXECUCAO ( 'MEMORIA_PREVIA', 00, null, v_xml_memoria, 'debug');

           exception
            when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao validar pr�via de reembolso: ' || top_utl_padrao.msgerro;
                rollback;
                return;
           end;
           --
           -- guarda as funcoes para deletar depois as que n�o foram reincluidas
          v_ind_funcoes := v_ind_funcoes || lpad(NVL(f.ind_funcao,'99'),2,'0') || ',';
          v_qtd_funcoes := v_qtd_funcoes + 1;
        end loop;
        --
        if v_qtd_funcoes > 0 or v_qtd_funcoes_antigo > 0 then
          for fe in ( select prp.num_seq_item, prp.ind_funcao
                        from procedimento_reembolso_previa prp
                       where prp.num_reembolso          = p_num_reembolso
                         and prp.cod_procedimento_cm    = p.cod_procedimento_cm
                         and nvl(ind_via,'X')           = nvl(p.ind_via,'X')
                         and nvl(ind_funcao,'X')        not in ( select /*+cardinality(x,10)*/ nvl(x.column_value,'Z') from table( top_utl_padrao.split(v_ind_funcoes,',')) x )
          ) loop
          --
            begin
                delete from procedimento_reembolso_previa
                 where num_reembolso         = p_num_reembolso
                   and cod_procedimento_cm   = p.cod_procedimento_cm
                   and num_seq_item          = fe.num_seq_item;
                 --
                delete from reembolso_previa_glosa
                 where num_reembolso         = p_num_reembolso
                   and num_seq_item          = fe.num_seq_item;
                 --
                delete from memoria_reembolso_previa
                 where num_reembolso         = p_num_reembolso
                   and num_seq_item          = fe.num_seq_item;

            exception
                when others then
                    p_cod_retorno:=9;
                    p_msg_retorno:='Ocorreu um erro ao validar procedimentos do reembolso: ' || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || sqlerrm;
            end;
          --
          end loop;
        end if;

      end loop;
      --
      if nvl(v_ind_tipo_reembolso,0) = 1 then
          delete from procedimento_reembolso_previa
           where num_reembolso         =  p_num_reembolso
             and cod_procedimento_cm   != '10101012';
      end if;
      --
      p_cod_retorno := 0;
      p_msg_retorno := '';
    exception
    when others then
      p_cod_retorno:=9;
      p_msg_retorno:='Ocorreu um erro ao validar procedimentos do reembolso: ' || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || sqlerrm;
    end;
    ----------------------------------------------------------------------------
    -- tratamento dos erros, quanto a libera��o das glosas dos itens do pedido
    ----------------------------------------------------------------------------
    procedure processa_liberacao_glosa_item ( p_cod_retorno                  out pls_integer
                                            , p_msg_retorno                  out varchar2
                                            , p_xml_dados                 in     clob
                                            , p_num_rembolso              in     varchar2
                                            , p_num_seq_item              in     pls_integer
                                            , p_cod_usuario               in     varchar2
                                            )
    is
        --
        cursor cur_glosa      (pXML in sys.XMLType)
        is
        select nvl(extractValue( VALUE(T) , '//IND_ANALISE'     ),'N') IND_ANALISE
             , nvl(extractValue( VALUE(T) , '//COD_MOTIVO_GLOSA'),0  ) COD_MOTIVO_GLOSA
        from   table ( xmlsequence ( extract(pXML,'/LIBERACAO_GLOSA_ITEM/GLOSA') ) ) T;
        --
        V_XML                           sys.XMLType;
        --
    begin
        --
        V_XML := sys.XMLType.createXML( p_xml_dados );
        --
        for rc_glosa in cur_glosa (V_XML) loop
            --
            if rc_glosa.cod_motivo_glosa > 0 then
                --
                if rc_glosa.ind_analise = 'S' then
                    -- Liberar a glosa informada
                    update /*  rb_previa_reembolso.processa_libera_glosa_item  */
                           reembolso_previa_glosa
                    set    ind_analisado            = rc_glosa.ind_analise
                         , dt_liberacao             = sysdate
                         , cod_usuario_liberacao    = p_cod_usuario
                    where  num_reembolso            = p_num_rembolso
                    and    num_seq_item             = p_num_seq_item
                    and    nvl(ind_analisado,'N')  != 'S'
                    and    cod_motivo_glosa         = rc_glosa.cod_motivo_glosa;
                    --
                else
                    --
                    update /*  rb_previa_reembolso.LiberacaoGlosaItem  */
                           reembolso_previa_glosa
                    set    ind_analisado            = rc_glosa.ind_analise
                         , dt_liberacao             = null
                         , cod_usuario_liberacao    = null
                    where  num_reembolso            = p_num_rembolso
                    and    num_seq_item             = p_num_seq_item
                    and    cod_motivo_glosa         = rc_glosa.cod_motivo_glosa;
                    --
                end if;
            end if;
        end loop;
        --
        p_cod_retorno := 0;
        p_msg_retorno := '';
    exception
    when others then
        p_cod_retorno:=9;
        p_msg_retorno:='Ocorreu um erro ao liberar as glosas dos itens: ' || chr(13) || sqlerrm;
    end;
    --
    -----------------------------------------------------------------
    -- valida a finaliza��o com aprova��o, verificando as glosas e grupos de analise
    -----------------------------------------------------------------
    procedure valida_finalizacao ( p_cod_retorno                  out pls_integer
                                 , p_msg_retorno                  out varchar2
                                 , p_num_reembolso             in     varchar2
                                 , p_cod_usuario               in     varchar2
                                 )
    is
        v_qtd_glosa_analise         number;
        v_qtd_proc_analise          number;
        v_qtd_proc_analisado        number;
        v_qtd_proc_zerado           number;
        v_qtd_procedimento          number;
        v_qtd_grupo_analise         number;
        v_qtd_proc_situ_recusado    number;
    begin

        p_cod_retorno := 0;
        p_msg_retorno := '';
        --
        -- valida se existe alguma glosa de cabe�a n�o liberada
        --
        begin
           select COUNT (*)
             into v_qtd_glosa_analise
             from reembolso_previa_glosa   a
            where a.num_reembolso          = p_num_reembolso
              and a.num_seq_item           = 0
              and nvl(a.ind_analisado,'N') = 'N';
        exception
        when others then
          v_qtd_glosa_analise   := 0;
        end;

        if v_qtd_glosa_analise > 0 then
            p_cod_retorno := 9;
            p_msg_retorno := 'A aprova��o n�o p�de ser realizada, exite(m) glosa(s) na cabe�a da pr�via pendente(s) de an�lise.';
            --return;
        end if;

        if p_cod_retorno = 0 then
            --
            -- valida se existe pelo menos um procedimento sem glosa ou com todas as glosas liberadas
            --
            v_qtd_procedimento          := 0;
            v_qtd_proc_analisado        := 0;
            v_qtd_proc_zerado           := 0;
            v_qtd_proc_situ_recusado    := 0;
            v_qtd_proc_analise          := 0;
            --
            for c_itens
               in (select distinct cod_procedimento_cm, num_seq_item, val_reembolsado, ind_situacao
                    from  ts.procedimento_reembolso_previa
                    where num_reembolso         = p_num_reembolso
                   )
            loop
                begin
                   select COUNT (*)
                     into v_qtd_glosa_analise
                     from reembolso_previa_glosa   a
                    where a.num_reembolso          = p_num_reembolso
                      and a.num_seq_item           = c_itens.num_seq_item
                      and nvl(a.ind_analisado,'N') = 'N';
                exception
                when others then
                  v_qtd_glosa_analise   := 0;
                end;
                --
                if v_qtd_glosa_analise > 0 then
                    v_qtd_proc_analise := v_qtd_proc_analise + 1;
                else
                    if c_itens.ind_situacao IN ('N','C') then
                        v_qtd_proc_situ_recusado := v_qtd_proc_situ_recusado + 1;
                    else
                        if c_itens.val_reembolsado <= 0 then
                            v_qtd_proc_zerado := v_qtd_proc_zerado + 1;
                        end if;
                    end if;
                    --
                    v_qtd_proc_analisado := v_qtd_proc_analisado + 1;
                end if;
                --
                v_qtd_procedimento := v_qtd_procedimento + 1;
                --
            end loop;
            --
            if v_qtd_procedimento = v_qtd_proc_analise then
                p_cod_retorno := 9;
                p_msg_retorno := 'A aprova��o n�o p�de ser realizada, pelo menos um procedimento n�o deve possuir glosas em an�lise.';
                --return;
            elsif v_qtd_proc_analisado = v_qtd_proc_situ_recusado then
                p_cod_retorno := 9;
                p_msg_retorno := 'A aprova��o n�o p�de ser realizada, pois o(s) procedimento(s) sem glosa(s) est�(�o) recusado(s) ou cancelado(s).';
                --return;
            elsif v_qtd_proc_analisado = v_qtd_proc_zerado then
                p_cod_retorno := 9;
                p_msg_retorno := 'A aprova��o n�o p�de ser realizada, pois o(s) procedimento(s) liberado(s) est�(�o) zerado(s).';
                --return;
            end if;
        end if;
        --
        -- valida se existe grupo n�o aprovado para o reembolso
        --
        begin
           select COUNT (*)
             into v_qtd_grupo_analise
             from ts.pedido_reembolso_previa_grupo   a
            where a.num_reembolso          = p_num_reembolso
              and nvl(a.ind_situacao,1)    in (1,3);
        exception
        when others then
          v_qtd_grupo_analise   := 0;
        end;
        --
        if v_qtd_grupo_analise > 0 then
            -- se n�o houver erros coloca a mensagem de grupo de an�lise, caso contr�rio, deixa a mensagem
            if p_cod_retorno = 0 then
                p_msg_retorno := 'A aprova��o n�o p�de ser realizada, exite(m) grupo(s) pendente(s) de an�lise.';
            end if;
            p_cod_retorno := 99;
        end if;
        --
        return;
        --
    exception
    when others then
        p_cod_retorno:=9;
        p_msg_retorno:='Ocorreu um erro ao analisar o pedido para finaliza��o: ' || chr(13) || sqlerrm;
    end;
    --
    ----------------------------------------------------------------------------
    -- Retornar um cursor vazio
    ----------------------------------------------------------------------------
    function  get_cursor_vazio
    return sys_refcursor
    --
    is
        c                         sys_refcursor;
    begin
        --
        open c
        for  select * from dual where 1 = 2;
        --
        return c;
        --
    exception
    when others then
        ts_log_execucao ( 'rb_previa_reembolso'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Erro-ORA:'               || sqlerrm
                        , 'get_cursor_vazio' );
        --
        raise_application_error( -20001 , 'rb_previa_reembolso.get_cursor_vazio - Ocorreu o seguinte erro: ' || ts.top_utl_padrao.msgerro);
    end;
    --
    ----------------------------------------------------------------------------
    -- Adicionar XML no XML informado
    ----------------------------------------------------------------------------
    procedure add_xml ( p_xml_completo      in out nocopy clob
                           , p_xml_add           in clob
                           , p_no_inclusao       in varchar2
                           )
    is
        vTipoXML                xmlType;
        vXMLRetorno             clob;
    begin
        --
        if nvl(p_xml_add,'X') = 'X'     then    return;
        end if;
        --
        if nvl(p_no_inclusao,'X') = 'X' then    return;
        end if;
        --
        vTipoXML := xmlType.appendChildXML ( sys.XMLType ( p_xml_completo )
                                           , p_no_inclusao
                                           , sys.XMLType ( replace(p_xml_add,'<?xml version="1.0"?>','') )
                                           );
        --
        p_xml_completo := vTipoXML.getclobval();
        --
        return;
        --
    exception
        when others then
        ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Erro-ORA:'     || sqlerrm
                                  || chr(13) || 'XML Completo:' || p_xml_completo
                                  || chr(13) || 'XML Add:'      || p_xml_add
                                  || chr(13) || 'p_no_inclusao:'|| p_no_inclusao
                        , 'add_xml' );
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Gerar XML do SQL / parametros informados
    ----------------------------------------------------------------------------
    function gerar_xml ( p_sql               in varchar2
                       , p_nome_cabecalho    in varchar2
                       , p_nome_coluna       in varchar2
                       , p_qtd_max_linhas    in number     default 0
                       )
    return clob
    is
        qryCtx                  DBMS_XMLGEN.ctxHandle;
        vXML                    clob;
        idx                     pls_integer := 0;
        i                       pls_integer := 0;
        v_cod_retorno           pls_integer := 0;
        v_msg_retorno           varchar2(4000);
    begin
        --
        ts_util.ValidarSQL ( p_cod_retorno  => v_cod_retorno
                           , p_msg_retorno  => v_msg_retorno
                           , stringSql      => p_sql
                           );
        --
        if v_cod_retorno != 0 then
            ts_log_execucao ( 'RB_PREVIA_REEMBOSLO'
                            , 999
                            , 'Erro ao ValidarSQL'
                            , 'Erro:' || chr(13) || v_msg_retorno
                                      || chr(13) || 'SQL:' || p_sql
                            , 'gerar_xml'
                            );

            return '';
        end if;
        --
        qryCtx := dbms_xmlgen.newContext     ( p_sql );
        -- Colocar os parametros
        idx := param_xml.first;
        while idx is not null loop
            --
            if param_xml ( idx ).tipo = 'D' then
                --
                dbms_xmlgen.setBindValue         ( qryCtx
                                                 , param_xml ( idx ).nome
                                                 , to_date(param_xml ( idx ).valor, 'dd/mm/yyyy')
                                                 );
                --
            else
                --
                dbms_xmlgen.setBindValue         ( qryCtx
                                                 , param_xml ( idx ).nome
                                                 , param_xml ( idx ).valor
                                                 );
                --
            end if;
            --
            idx := param_xml.next               ( idx );
        end loop;
        --
        param_xml.delete;
        --
        dbms_xmlgen.setRowSetTag                ( qryCtx , p_nome_cabecalho );
        dbms_xmlgen.setRowTag                   ( qryCtx , p_nome_coluna);
        --
        if p_qtd_max_linhas > 0 then
            dbms_xmlgen.setMaxRows              ( qryCtx , to_number(p_qtd_max_linhas));
        end if;
        --
        dbms_xmlgen.setCheckInvalidChars        ( qryCtx , TRUE );
        dbms_xmlgen.useNullAttributeIndicator   ( qryCtx , TRUE  );
        dbms_xmlgen.setConvertSpecialChars      ( qryCtx , TRUE );
        --dbms_xmlgen.setMaxRows(ctx IN ctxHandle, maxRows IN NUMBER);
        --dbms_xmlgen.setXSLT(ctx IN ctxType, name  IN VARCHAR2, value IN VARCHAR2);
        vXML := dbms_xmlgen.getXML              ( qryCtx );
        i := dbms_xmlgen.getNumRowsProcessed    ( qryCtx );
        dbms_xmlgen.closeContext                ( qryCtx );
        --
        -- Retirar xsi:nil = "true" pois d� erro ao gerar um cursor
        vXML := replace(vXML, ' xsi:nil = "true"', '');
        --
        return vXML;
        --
    exception
    when others then
        ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro || ' - ' || sqlerrm
                                  || chr(13) || 'SQL:' || p_sql
                        , 'gerar_xml' );
        return '';
    end;
    ----------------------------------------------------------------------------
    -- Add o parametro para gera��o do XML
    ----------------------------------------------------------------------------
    procedure add_parametro_sql ( p_nome_parametro      in varchar2
                                , p_valor_parametro     in varchar2
                                , p_tipo_parametro      in varchar2 default 'S'
                                )
    is
    begin
        param_xml.extend;
        param_xml(param_xml.count).tipo  := p_tipo_parametro;
        param_xml(param_xml.count).nome  := p_nome_parametro;
        param_xml(param_xml.count).valor := p_valor_parametro;
    end;
    ----------------------------------------------------------------------------
    -- Enviar fax
    ----------------------------------------------------------------------------
    procedure processar_comunicacao_fax ( p_cod_retorno          out pls_integer
                                        , p_msg_retorno          out varchar2
                                        , p_seq_comunicacao      out pls_integer
                                        , p_xml_parametro        in  clob
                                        )
    is
        v_cod_usuario       usuario.cod_usuario%type;
        v_num_pedido        autorizacao.num_pedido%type;
        v_num_ddd_fax       autorizacao.num_ddd_fax%type;
        v_num_fax           autorizacao.num_fax%type;
        tab_dados           top_utl_xml.tbl_fields;
    begin
        --
        --Passar o XML informado para uma cole��o
        tab_dados := ts.top_utl_xml.toCollection(p_xml_parametro);
        --
        v_num_pedido    := case when tab_dados.exists( 'numReembolso'   ) and tab_dados( 'numReembolso'  ).valor(1) is not null then tab_dados( 'numReembolso'   ).valor(1) else null end;
        v_num_ddd_fax   := case when tab_dados.exists( 'numDDDFax'   ) and tab_dados( 'numDDDFax'  ).valor(1) is not null then tab_dados( 'numDDDFax'   ).valor(1) else null end;
        v_num_fax       := case when tab_dados.exists( 'numFax'      ) and tab_dados( 'numFax'     ).valor(1) is not null then tab_dados( 'numFax'      ).valor(1) else null end;
        v_cod_usuario   := case when tab_dados.exists( 'codUsuario'  ) and tab_dados( 'codUsuario' ).valor(1) is not null then tab_dados( 'codUsuario'  ).valor(1) else null end;
        --
        --
        if v_num_fax is null then return;
        end if;
        --
        v_num_fax := to_number(replace(v_num_fax,'-'));
        --
        select com_registro_seq.nextval
        into   p_seq_comunicacao
        from   dual;
        --

        insert
        into com_registro   ( seq_comunicacao       , ind_situacao      , dat_registro      , dat_atu
                            , ind_enviar_email      , ind_enviar_sms    , ind_enviar_tweet  , ind_enviar_fax
                            , cod_tipo_comunicacao)
        values              ( p_seq_comunicacao     , '0'               , sysdate           , sysdate
                            , 'N'                   , 'N'               , 'N'               ,'S'
                            , 2);
        --




        insert
        into com_fax       ( seq_comunicacao        , ind_situacao      , dat_envio         , pais
                           , ddd                    , numero            , retorno)
        values             ( p_seq_comunicacao      , '0'               , null              , 55
                           , v_num_ddd_fax          , v_num_fax         , null);
        --
        commit;
        --
        p_cod_retorno := 0;
        p_msg_retorno := '';
    end;
    --
    ----------------------------------------------------------------------------
    -- Chamar WebService para envio de FAX
    ----------------------------------------------------------------------------
    procedure enviar_fax ( p_cod_retorno      out number
                         , p_msg_retorno      out varchar2
                         , p_xml_dados        in clob
                         , p_xml_arquivos     in clob
                         )
    is
        --
        v_seq_comunicacao       pls_integer;
        v_XML                   clob;
        v_resultado             varchar2(4000);
        v_wsdl_url              controle_sistema.val_parametro%type;
        v_data                  varchar2(20)   := to_char(sysdate,'dd/mm/yyyy hh24:mi:ss');
        v_hash                  varchar2(100)  := '';
        v_orientacao            varchar2(50)   := '';
        v_arquivo               varchar2(4000) := '';
        v_qtd_arquivo           pls_integer    := 0;
        tab_dados               top_utl_xml.tbl_fields;
        --
        cursor cur_arquivos      (pXML in sys.XMLType)
        is
        select /*+cardinality(T,10)*/ extractValue( VALUE(T) , '//nome_arquivo') nome_arquivo
             , extractValue( VALUE(T) , '//orientacao'  ) orientacao
        from   table ( xmlsequence ( extract(pXML,'/arquivos/detalhe') ) ) T;
        --
        V_XMLType                          sys.XMLType;
        --
    begin
        --
        --Passar o XML informado para uma cole��o
        tab_dados := ts.top_utl_xml.toCollection(p_xml_dados);
        --
        if not tab_dados.exists( 'numReembolso' ) or tab_dados( 'numReembolso' ).valor(1) is null then
            p_cod_retorno := 9;
            p_msg_retorno := 'Previa de reembolso n�o informada';
            return;
        end if;
        --
        if not tab_dados.exists( 'numDDDFax' ) or tab_dados( 'numDDDFax' ).valor(1) is null then
            p_cod_retorno := 9;
            p_msg_retorno := 'DDD do Fax n�o informado';
            return;
        end if;
        --
        if not tab_dados.exists( 'numFax' ) or tab_dados( 'numFax' ).valor(1) is null then
            p_cod_retorno := 9;
            p_msg_retorno := 'N�mero do Fax n�o informado';
            return;
        end if;
        --
        --
        tab_dados( 'numFax'    ).valor(1) := to_number(replace(tab_dados( 'numFax'    ).valor(1),'-'));
        --
        -- Atualizar FAX
        update pedido_reembolso_previa
        set    txt_num_fax      = tab_dados( 'numFax'    ).valor(1)
             , txt_ddd_fax      = tab_dados( 'numDDDFax' ).valor(1)
             , ind_tipo_emissao = 'F'
        where  num_reembolso    = tab_dados( 'numReembolso' ).valor(1);
        --
        -- Processar Comunica��o (Est� com erro ao enviar FAX)
        --processar_comunicacao ( p_cod_retorno, p_msg_retorno, v_seq_comunicacao, p_xml_dados );
        processar_comunicacao_fax ( p_cod_retorno, p_msg_retorno, v_seq_comunicacao, p_xml_dados );
        --
        --
        -- Montar o HASH
        select lower(TS_MD5('SISAMIL' || v_data || 'tld123'))
        into   v_hash
        from   dual;
        --
        -- Montar string dos arquivos + orienta��o
        --
        V_XMLType := sys.XMLType.createXML( p_xml_arquivos );
        --
        for rc_arquivos in cur_arquivos ( V_XMLType ) loop
            v_qtd_arquivo := v_qtd_arquivo + 1;
            v_arquivo     := v_arquivo    || case when v_qtd_arquivo > 1 then ';' else '' end || rc_arquivos.nome_arquivo;
            v_orientacao  := v_orientacao || case when v_qtd_arquivo > 1 then ';' else '' end || rc_arquivos.orientacao;
        end loop;
        --
        --Montar XML para chamada do WebService
        --http://tempuri.org/WSTeledata/recepcaofax
        v_xml := '<rec:DadosFax xmlns:rec="http://tempuri.org/WSTeledata/recepcaofax">
                     <rec:Login>SISAMIL</rec:Login>
                     <rec:DataHora>'      || v_data                           || '</rec:DataHora>
                     <rec:Hash>'          || v_hash                           || '</rec:Hash>
                     <rec:idSolicitacao>' || v_seq_comunicacao                || '</rec:idSolicitacao>
                     <rec:DDD>'           || tab_dados( 'numDDDFax').valor(1) || '</rec:DDD>
                     <rec:Tel>'           || tab_dados( 'numFax'   ).valor(1) || '</rec:Tel>
                     <rec:Ramal>'         || tab_dados( 'numRamal' ).valor(1) || '</rec:Ramal>
                     <rec:QdeDocs>'       || lpad(v_qtd_arquivo,2,'0')        || '</rec:QdeDocs>
                     <rec:Path>'          || v_arquivo                        || '</rec:Path>
                     <rec:Orientacao>'    || v_orientacao                     || '</rec:Orientacao>
                  </rec:DadosFax>';
        --
        ------------------------------------------------------------------------
        -- RECUPERAR A URL DO WSDL
        ------------------------------------------------------------------------
        RetornaParametro ('TS_WSDL_URL_FAX', v_wsdl_url, '');
        if v_wsdl_url = '' then
            p_cod_retorno := 9;
            p_msg_retorno := 'Par�metro "TS_WSDL_URL_FAX" n�o cadastrado na tabela de par�metros';
            return;
        end if;
        --
        --Chamar o WebService do FAX
        CALL_WS( p_wsdl_url     => v_wsdl_url
               , p_namespace    => 'http://tempuri.org/WSTeledata/recepcaofax'
               , p_service      => 'Enviofax'
               , p_port         => 'EnviofaxSoap'
               , p_operation    => 'DadosFax'
               , p_msg          => v_XML
               , p_resultado    => v_resultado
               , p_cod_retorno  => p_cod_retorno
               , p_msg_retorno  => p_msg_retorno
               );
        --
         ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                            , 10
                            , 'Erro n�o previsto ao enviar FAX'
                            , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                      || chr(13) || 'Erro-ORA:'  || sqlerrm
                                      || chr(13) || 'XML ENVIO:' || v_xml
                                      || chr(13) || 'WSDL:'      || v_wsdl_url
                                      || chr(13) || 'XML Dados:' || p_xml_dados
                                      || chr(13) || 'XML Arq:'   || p_xml_arquivos
                                      || chr(13) || 'MSGRetorno:'|| p_msg_retorno
                            , 'enviar_fax' );
        --
        if p_cod_retorno != 0 then
            ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                            , 10
                            , 'Erro n�o previsto ao enviar FAX'
                            , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                      || chr(13) || 'Erro-ORA:'  || sqlerrm
                                      || chr(13) || 'XML ENVIO:' || v_xml
                                      || chr(13) || 'WSDL:'      || v_wsdl_url
                                      || chr(13) || 'XML Dados:' || p_xml_dados
                                      || chr(13) || 'XML Arq:'   || p_xml_arquivos
                                      || chr(13) || 'MSGRetorno:'|| p_msg_retorno
                            , 'enviar_fax' );
            --
        end if;
        --
        commit;
        --
        --
    exception
    when others then
        --
        ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 10
                        , 'Erro n�o previsto ao enviar FAX'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Erro-ORA:'  || sqlerrm
                                  || chr(13) || 'XML ENVIO:' || v_xml
                                  || chr(13) || 'WSDL:'      || v_wsdl_url
                                  || chr(13) || 'XML Dados:' || p_xml_dados
                                  || chr(13) || 'XML Arq:'   || p_xml_arquivos
                                  || chr(13) || 'MSGRetorno:'|| p_msg_retorno
                        , 'enviar_fax' );
        --
        --
        p_cod_retorno := 9;
        p_msg_retorno := 'Erro ao enviar fax : ' || ts.top_utl_padrao.msgerro || ' ( ' || sqlerrm || ' ) ';
        rollback;
        return;
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Retornar a data atual do sistema
    ----------------------------------------------------------------------------
    procedure RetornaData (p_cod_formato in varchar2, p_data out varchar2)
    is

    begin

        begin
            SELECT TO_CHAR(SYSDATE, NVL(p_cod_formato,'DD/MM/YYYY')) into p_data
              FROM dual;
        exception
            when OTHERS then
                p_data := null;
        end;

        return;
    end;
    --
    ----------------------------------------------------------------------------
    -- Retorna xml com as informa��es da pr�via
    ----------------------------------------------------------------------------
    Procedure RetornaPrevia ( p_num_reembolso       in  varchar2
                            , p_xml_retorno         out clob
                            , p_xml_filtro          in  clob    default null
                            , p_ind_forma_abertura  in varchar2 default null) is
    --Declara��es:
    v_SQL                   varchar2(5000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    v_doc                   xmldom.DOMDocument;
    v_cod_retorno           number;
    v_msg_retorno           varchar2(4000);
    v_item_vazio            varchar2(3) := '���';
    v_cod_ts                pedido_reembolso_previa.cod_ts%type;
    v_ind_situacao          pedido_reembolso_previa.ind_situacao%type;
    v_cod_tratamento        pedido_reembolso_previa.cod_tratamento%type;
    v_dt_pedido_ini         pedido_reembolso_previa.dt_inclusao%type;
    v_dt_pedido_fim         pedido_reembolso_previa.dt_inclusao%type;
    v_num_insc_fiscal       pedido_reembolso_previa.num_insc_fiscal%type;
    v_nome_prestador        pedido_reembolso_previa.nome_prestador%type;
    v_sigla_conselho        pedido_reembolso_previa.sigla_conselho%type;
    v_num_crm               pedido_reembolso_previa.num_crm%type;
    v_uf_conselho           pedido_reembolso_previa.uf_conselho%type;
    v_cnes                  pedido_reembolso_previa.cnes%type;
    v_cod_ts_tit            pedido_reembolso_previa.cod_ts_tit%type;
    v_cod_ts_contrato       pedido_reembolso_previa.cod_ts_contrato%type;
    v_num_associado         pedido_reembolso_previa.num_associado%type;
    v_num_contrato          pedido_reembolso_previa.num_contrato%type;
    v_num_titular           pedido_reembolso_previa.num_titular%type;
    v_num_reembolso_ans     pedido_reembolso_previa.num_reembolso_ans%type;
    --
    v_tamanho_reembolso     number;
    v_num_reembolso         varchar(20);
    v_num_reembolso_ant     varchar2(20);
    begin
        --In�cio
        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        if v_tamanho_reembolso > 15 then
        begin
          select p.num_reembolso
            into v_num_reembolso
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        exception
          when no_data_found then
               p_xml_retorno := '<?xml version="1.0"?>';
               p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
               p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
               p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>N�mero da pr�via de reembolso n�o encontrado</MSG_RETORNO>';
               p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
               return;
        end;

        else
          v_num_reembolso := p_num_reembolso;
        end if;
        --

        --Valida��o
    if p_ind_forma_abertura is not null then
        begin
          begin
            select  ind_situacao
                 into v_ind_situacao
                 from  pedido_reembolso_previa
                 where num_reembolso = v_num_reembolso;
          exception
             when others then
                    begin
                       select  ind_situacao
                         into v_ind_situacao
                         from  pedido_reembolso_previa
                         where num_reembolso_ans = v_num_reembolso;
                    exception
                      when others then
                         p_xml_retorno := '<?xml version="1.0"?>';
                         p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                         p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                         p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>N�mero da pr�via de reembolso n�o encontrado</MSG_RETORNO>';
                         p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                         return;
                    end;
            end;
            if p_ind_forma_abertura = 'CA' then
                if v_ind_situacao in (2, 3, 4) then
                    p_xml_retorno := '<?xml version="1.0"?>';
                    p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                    p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Pr�via de reembolso ' || v_num_reembolso || ' n�o est� em situa��o que permita o seu cancelamento</MSG_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                    return;
                end if;
           end if;
           if p_ind_forma_abertura = 'RC' then
               if v_ind_situacao in (1, 2, 4, 5) then
                    p_xml_retorno := '<?xml version="1.0"?>';
                    p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                    p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Pr�via de reembolso ' || v_num_reembolso || ' n�o est� em situa��o que permita revers�o do cancelamento</MSG_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                    return;
               end if;
           end if;
           if p_ind_forma_abertura = 'RF' then
               if v_ind_situacao in (1,3,5) then
                    p_xml_retorno := '<?xml version="1.0"?>';
                    p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                    p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Pr�via de reembolso ' || v_num_reembolso || ' n�o est� em situa��o que permita a revers�o da finaliza��o</MSG_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                    return;
               end if;
           end if;
           if p_ind_forma_abertura = 'AL' then
               if v_ind_situacao in (2,3,4) then
                    p_xml_retorno := '<?xml version="1.0"?>';
                    p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                    p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Pr�via de reembolso ' || v_num_reembolso || ' n�o est� em situa��o que permita altera��o</MSG_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                    return;
               end if;
           end if;
           if p_ind_forma_abertura = 'EN' then
               if v_ind_situacao in (2,3,4) then
                    p_xml_retorno := '<?xml version="1.0"?>';
                    p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                    p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Pr�via de reembolso ' || v_num_reembolso || ' n�o est� em situa��o que permita encaminhar para an�lise.</MSG_RETORNO>';
                    p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                    return;
               end if;
           end if;
        exception
            when no_data_found then
               p_xml_retorno := '<?xml version="1.0"?>';
               p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
               p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
               p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>N�mero da pr�via de reembolso n�o encontrado</MSG_RETORNO>';
               p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
               return;
        end;
    end if;

        if nvl(p_xml_filtro,'X') <> 'X' then
            ts_cria_doc_xml(p_xml_filtro, v_doc, v_cod_retorno, v_msg_retorno);
            if v_cod_retorno <> 0 then
                p_xml_retorno := '<?xml version="1.0"?>';
                p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || v_msg_retorno || '</MSG_RETORNO>';
                p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                return;
            end if;
        else
            IF NVL(v_num_reembolso,0) = 0 THEN
                p_xml_retorno := '<?xml version="1.0"?>';
                p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
                p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
                p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>1 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
                p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
                return;
            END IF;
        end if;
        --
        select length(v_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        if v_tamanho_reembolso > 15 then
          BEGIN
          select p.num_reembolso
            into v_num_reembolso_ant
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = v_num_reembolso;
          exception
            when others then
               p_xml_retorno := '<?xml version="1.0"?>';
               p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
               p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
               p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>N�mero da pr�via de reembolso n�o encontrado</MSG_RETORNO>';
               p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
               return;
          end;
        end if;
        --Montar XML do pedidos
        v_SQL := '';
        v_SQL := v_SQL || ' SELECT DISTINCT 0 COD_RETORNO';
        v_SQL := v_SQL || '      , prp.num_reembolso';
        v_SQL := v_SQL || '      , prp.num_reembolso_ans';
        v_SQL := v_SQL || '      , to_char(SYSDATE,''DD/MM/YYYY'') data_atual';
        v_SQL := v_SQL || '      , prp.num_associado';
        v_SQL := v_SQL || '      , prp.nome_associado';
        v_SQL := v_SQL || '      , so.nome_social nome_Social';
        v_SQL := v_SQL || '      , prp.cod_ts';
        v_SQL := v_SQL || '      , prp.ind_situacao';
        v_SQL := v_SQL || '      , prp.ind_sexo';
        v_SQL := v_SQL || '      , prp.tipo_associado';
        v_SQL := v_SQL || '      , prp.cod_ts_resp';
        v_SQL := v_SQL || '      , prp.cod_dependencia';
        v_SQL := v_SQL || '      , prp.cod_ts_tit';
        v_SQL := v_SQL || '      , prp.cod_ts_contrato';
        v_SQL := v_SQL || '      , prp.num_contrato';
        v_SQL := v_SQL || '      , to_char(prp.val_calculado,''FM999G999G999G990D00'',''nls_numeric_characters='''',.'')   val_calculado';
        v_SQL := v_SQL || '      , to_char(prp.val_informado,''FM999G999G999G990D00'',''nls_numeric_characters='''',.'')   val_informado';
        v_SQL := v_SQL || '      , to_char(prp.val_reembolsado,''FM999G999G999G990D00'',''nls_numeric_characters='''',.'') val_reembolsado';
        v_SQL := v_SQL || '      , to_char(prp.data_nascimento,''DD/MM/YYYY'')     data_nascimento';
        v_SQL := v_SQL || '      , to_char(prp.dt_sit,''DD/MM/YYYY'')              dt_situacao_pedido';
        v_SQL := v_SQL || '      , to_char(prp.dt_inclusao,''DD/MM/YYYY'')         dt_inclusao';
        v_SQL := v_SQL || '      , to_char(prp.dt_analise,''DD/MM/YYYY'')          dt_analise';
        v_SQL := v_SQL || '      , to_char(prp.dt_indeferimento,''DD/MM/YYYY'')    dt_indeferimento';
        v_SQL := v_SQL || '      , prp.cod_tratamento';
        v_SQL := v_SQL || '      , prp.qtd_idade';
        v_SQL := v_SQL || '      , prp.cod_acomodacao ';
        v_SQL := v_SQL || '      , prp.txt_observacao';
        v_SQL := v_SQL || '      , prp.txt_observacao_operadora';
        v_SQL := v_SQL || '      , prp.ind_situacao';
        v_SQL := v_SQL || '      , prp.ind_insc_fiscal';
        v_SQL := v_SQL || '      , prp.nome_prestador';
        v_SQL := v_SQL || '      , prp.num_insc_fiscal';
        v_SQL := v_SQL || '      , prp.num_crm';
        v_SQL := v_SQL || '      , prp.uf_conselho';
        v_SQL := v_SQL || '      , prp.sigla_conselho';
        v_SQL := v_SQL || '      , prp.cnes';
        v_SQL := v_SQL || '      , prp.cod_cbo';
        v_SQL := v_SQL || '      , prp.cod_plano';
        v_SQL := v_SQL || '      , prp.cod_usuario_sit';
        v_SQL := v_SQL || '      , prp.ind_carater';
        v_SQL := v_SQL || '      , prp.cod_entidade_ts_tit';

        v_SQL := v_SQL || '      , prp.txt_num_fax';
        v_SQL := v_SQL || '      , prp.txt_ddd_fax';
        v_SQL := v_SQL || '      , prp.txt_ramal_fax';
        v_SQL := v_SQL || '      , prp.txt_email';
        v_SQL := v_SQL || '      , prp.ddd_residencial';
        v_SQL := v_SQL || '      , prp.tel_residencial';
        v_SQL := v_SQL || '      , prp.ddd_comercial';
        v_SQL := v_SQL || '      , prp.tel_comercial';
        v_SQL := v_SQL || '      , prp.ddd_celular';
        v_SQL := v_SQL || '      , prp.tel_celular';

        v_SQL := v_SQL || '      , prp.ind_acomodacao';
        v_SQL := v_SQL || '      , prp.cod_motivo';
        v_SQL := v_SQL || '      , cbo_s.nome_cbo';
        v_SQL := v_SQL || '      , prp.cod_motivo_reembolso';
        v_SQL := v_SQL || '      , srp.ind_cancelar';
        v_SQL := v_SQL || '      , srp.ind_negar';
        v_SQL := v_SQL || '      , srp.ind_excluir';
        v_SQL := v_SQL || '      , srp.ind_alterar';
        v_SQL := v_SQL || '      , srp.ind_autorizar';
        v_SQL := v_SQL || '      , srp.ind_usuario_analise';
        v_SQL := v_SQL || '      , prp.cod_origem';
        v_SQL := v_SQL || '      , prp.ind_tipo_reembolso';
        v_SQL := v_SQL || '      , prp.cod_inspetoria_ts_abertura';
        v_SQL := v_SQL || '      , prp.ind_tipo_emissao';
        v_SQL := v_SQL || '      , prp.qtd_dias_reembolso';
        v_SQL := v_SQL || '      , prp.qtd_dias_reemb_uteis';
        v_SQL := v_SQL || '      , to_char(prp.dt_provavel_reembolso,''DD/MM/YYYY'')    dt_provavel_reembolso';
        v_SQL := v_SQL || '      , prp.num_internacao';
        v_SQL := v_SQL || '      , ( srp.sgl_situacao || '' - '' || srp.nome_situacao ) nome_situacao';
        v_SQL := v_SQL || '      , rpmi.desc_motivo nome_motivo';
        v_SQL := v_SQL || '      , mr.desc_motivo_reembolso';
        v_SQL := v_SQL || '      , tr.nome_tipo_reembolso';

        --SOLICITANTE
        v_SQL := v_SQL || '      , prp.cod_solicitante';
        v_SQL := v_SQL || '      , prp.cod_cbo_solicitante ';
        v_SQL := v_SQL || '      , prp.cnes_solicitante ';
        v_SQL := v_SQL || '      , sl.nome_solicitante';
        v_SQL := v_SQL || '      , sl.num_crm num_crm_solicitante';
        v_SQL := v_SQL || '      , sl.ind_tipo_pessoa ind_tipo_pessoa_solicitante';
        v_SQL := v_SQL || '      , sl.num_insc_fiscal num_insc_fiscal_solicitante';
        v_SQL := v_SQL || '      , sl.sigla_conselho sigla_conselho_solicitante';
        v_SQL := v_SQL || '      , sl.sgl_uf_conselho uf_conselho_solicitante';
        v_SQL := v_SQL || '      , cbo_2.nome_cbo nome_cbo_solicitante';
        --
        if nvl(p_num_reembolso,0) > 0 then
            --GLOSAS CABE�A EM ANALISE
            v_SQL := v_SQL || ',(SELECT COUNT (*)';
            v_SQL := v_SQL || '  FROM   reembolso_previa_glosa a';
            v_SQL := v_SQL || '  WHERE  a.num_reembolso            = :num_reembolso ';
            v_SQL := v_SQL || '  AND    nvl(a.IND_ANALISADO,''N'') = ''N''';
            v_SQL := v_SQL || '  AND    a.num_seq_item             = 0';
            v_SQL := v_SQL || ') QTD_GLOSA_ANALISE ';
            --GLOSAS CABE�A ANALISADA
            v_SQL := v_SQL || ',(SELECT COUNT (*)';
            v_SQL := v_SQL || '  FROM   reembolso_previa_glosa a';
            v_SQL := v_SQL || '  WHERE  a.num_reembolso            = :num_reembolso ';
            v_SQL := v_SQL || '  AND    nvl(a.IND_ANALISADO,''N'') = ''S''';
            v_SQL := v_SQL || '  AND    a.num_seq_item             = 0';
            v_SQL := v_SQL || ') QTD_GLOSA_ANALISADA ';
            --GRUPO ANALISE
            v_SQL := v_SQL || ',(SELECT COUNT (*)';
            v_SQL := v_SQL || '    FROM pedido_reembolso_previa_grupo a';
            v_SQL := v_SQL || '   WHERE a.num_reembolso = :num_reembolso) QTD_GRUPO_ANALISE ';
        end if;
        --
        --
        v_SQL := v_SQL || ' FROM ts.pedido_reembolso_previa     prp';
        v_SQL := v_SQL || '    , ts.CBO_S';
        v_SQL := v_SQL || '    , CBO_S cbo_2';
        v_SQL := v_SQL || '    , ts.reembolso_previa_situacao   srp';
        v_SQL := v_SQL || '    , ts.reembolso_previa_motivo_indef  rpmi';
        v_SQL := v_SQL || '    , ts.motivo_reembolso  mr';
        v_SQL := v_SQL || '    , ts.tipo_reembolso  tr';
        v_SQL := v_SQL || '    , ts.associado_social  so';
        v_SQL := v_SQL || '    , solicitante sl';
        --
        v_SQL := v_SQL || ' WHERE prp.cod_cbo               = cbo_s.cod_cbo(+)';
        v_SQL := v_SQL || '   AND prp.ind_situacao          = srp.ind_situacao(+)';
        v_SQL := v_SQL || '   AND prp.cod_motivo            = rpmi.cod_motivo(+)';
        v_SQL := v_SQL || '   AND prp.cod_motivo_reembolso  = mr.cod_motivo_reembolso(+)';
        v_SQL := v_SQL || '   AND prp.ind_tipo_reembolso    = tr.ind_tipo_reembolso(+)';
        v_SQL := v_SQL || '   AND prp.cod_ts                = so.cod_ts(+)';
        v_SQL := v_SQL || '   AND prp.cod_solicitante       = sl.cod_solicitante(+)';
        v_SQL := v_SQL || '   AND prp.cod_cbo_solicitante    = cbo_2.cod_cbo(+)';
        --
        IF nvl(p_xml_filtro,'X') <> 'X' THEN --FILTRANDO PELA TELA GENERICA
            v_cod_ts            := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/COD_TS');
            v_num_associado     := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/NUM_ASSOCIADO');
            v_cod_ts_tit        := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/COD_TS_TIT');
            v_cod_ts_contrato   := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/COD_TS_CONTRATO');
            v_num_contrato      := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/NUM_CONTRATO');
            v_ind_situacao      := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/IND_SITUACAO');
            --
            if ts.TS_UTIL.IsDate(ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/DT_PEDIDO_INI'), 'DD/MM/YYYY') then
                v_dt_pedido_ini := to_date(ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/DT_PEDIDO_INI'), 'DD/MM/YYYY');
            end if;
            if ts.TS_UTIL.IsDate(ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/DT_PEDIDO_FIM'), 'DD/MM/YYYY') then
                v_dt_pedido_fim := to_date(ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/DT_PEDIDO_FIM'), 'DD/MM/YYYY');
            end if;
            --
            v_num_insc_fiscal   := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/NUM_INSC_FISCAL');
            v_nome_prestador    := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/NOME_PRESTADOR');
            v_sigla_conselho    := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/SIGLA_CONSELHO');
            v_num_crm           := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/NUM_CRM');
            v_uf_conselho       := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/UF_CONSELHO');
            v_cnes              := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/CNES');
            v_num_titular       := ts_obtem_dados_xml(v_doc,'PREVIA','FILTRO/NUM_TITULAR');
            --
            --
            xmldom.freeDocument(v_doc);
            --
           /* if nvl(v_cod_ts,0) <> 0 then v_SQL := v_SQL || ' AND prp.cod_ts = :cod_ts';
            end if; */
            --
            if nvl(v_num_associado,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND prp.num_associado = :num_associado';
            end if;
            --
            if nvl(v_cod_ts_tit,0) <> 0 then v_SQL := v_SQL || ' AND prp.cod_ts_tit = :cod_ts_tit';
            end if;
            --
            if nvl(v_num_titular,0) <> 0 then v_SQL := v_SQL || ' AND prp.num_titular = :num_titular';
            end if;
            --
            if nvl(v_cod_ts_contrato,0) <> 0 then v_SQL := v_SQL || ' AND prp.cod_ts_contrato = :cod_ts_contrato';
            end if;
            --
            if nvl(v_num_contrato,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND prp.num_contrato = :num_contrato';
            end if;
            --
            if nvl(v_ind_situacao,0) <> 0 then v_SQL := v_SQL || ' AND prp.ind_situacao = :ind_situacao';
            end if;
            --
            if nvl(v_cod_tratamento,0) <> 0 then v_SQL := v_SQL || ' AND prp.cod_tratamento = :cod_tratamento';
            end if;
            --
            if nvl(v_num_insc_fiscal,0) <> 0 then v_SQL := v_SQL || ' AND prp.num_insc_fiscal = :num_insc_fiscal';
            end if;
            --
            if nvl(v_nome_prestador,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND UPPER(prp.nome_prestador) LIKE ' || chr(39) || UPPER(v_nome_prestador) || '%' || chr(39);
            end if;
            --
            if nvl(v_sigla_conselho,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND prp.sigla_conselho = :sigla_conselho';
            end if;
            --
            if nvl(v_num_crm,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND prp.num_crm = :num_crm';
            end if;
            --
            if nvl(v_uf_conselho,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND prp.uf_conselho = :uf_conselho';
            end if;
            --
            if nvl(v_cnes,v_item_vazio) <> v_item_vazio then v_SQL := v_SQL || ' AND prp.cnes = :cnes';
            end if;
            --
            if v_dt_pedido_ini is not null and ts_util.isdate(to_char(v_dt_pedido_ini, 'dd/mm/yyyy'), 'dd/mm/yyyy') then v_SQL := v_SQL || ' AND TRUNC(prp.dt_inclusao) >= :dt_pedido_ini';
            end if;
            --
            if v_dt_pedido_fim is not null and ts_util.isdate(to_char(v_dt_pedido_fim, 'dd/mm/yyyy'), 'dd/mm/yyyy') then v_SQL := v_SQL || ' AND TRUNC(prp.dt_inclusao) <= :dt_pedido_fim';
            end if;
            --
        ELSE --FILTRANDO PELO NUMERO DE REEMBOLSO
            v_SQL := v_SQL || '   AND prp.num_reembolso = :num_reembolso';
        END IF;
        --
        v_SQL := v_SQL || ' ORDER BY prp.num_reembolso desc';
        --
        qryCtx := dbms_xmlgen.newContext(v_SQL);
        --

        if nvl(p_xml_filtro,'X') <> 'X' then
            /*if nvl(v_cod_ts,0) <> 0                                 then dbms_xmlgen.setBindValue(qryCtx, 'cod_ts', v_cod_ts);
            end if;*/
            --
            if nvl(v_num_associado,v_item_vazio) <> v_item_vazio    then dbms_xmlgen.setBindValue(qryCtx, 'num_associado', v_num_associado);
            end if;
            --
            if nvl(v_cod_ts_tit,0) <> 0                             then dbms_xmlgen.setBindValue(qryCtx, 'cod_ts_tit', v_cod_ts_tit);
            end if;
            --
            if nvl(v_num_titular,0) <> 0                            then dbms_xmlgen.setBindValue(qryCtx, 'num_titular', v_num_titular);
            end if;
            --
            if nvl(v_cod_ts_contrato,0) <> 0                        then dbms_xmlgen.setBindValue(qryCtx, 'cod_ts_contrato', v_cod_ts_contrato);
            end if;
            --
            if nvl(v_num_contrato,v_item_vazio) <> v_item_vazio     then dbms_xmlgen.setBindValue(qryCtx, 'num_contrato', v_num_contrato);
            end if;
            --
            if nvl(v_ind_situacao,0) <> 0                           then dbms_xmlgen.setBindValue(qryCtx, 'ind_situacao', v_ind_situacao);
            end if;
            --
            if nvl(v_cod_tratamento,0) <> 0                         then dbms_xmlgen.setBindValue(qryCtx, 'cod_tratamento', v_cod_tratamento);
            end if;
            --
            if nvl(v_num_insc_fiscal,0) <> 0                        then dbms_xmlgen.setBindValue(qryCtx, 'num_insc_fiscal', v_num_insc_fiscal);
            end if;
            --
            if nvl(v_sigla_conselho,v_item_vazio) <> v_item_vazio   then dbms_xmlgen.setBindValue(qryCtx, 'sigla_conselho', v_sigla_conselho);
            end if;
            --
            if nvl(v_num_crm,v_item_vazio) <> v_item_vazio          then dbms_xmlgen.setBindValue(qryCtx, 'num_crm', v_num_crm);
            end if;
            --
            if nvl(v_uf_conselho,v_item_vazio) <> v_item_vazio      then dbms_xmlgen.setBindValue(qryCtx, 'uf_conselho', v_uf_conselho);
            end if;
            --
            if nvl(v_cnes,v_item_vazio) <> v_item_vazio             then dbms_xmlgen.setBindValue(qryCtx, 'cnes', v_cnes);
            end if;
            --
            if v_dt_pedido_ini is not null and ts_util.isdate(to_char(v_dt_pedido_ini, 'DD/MM/YYYY'), 'DD/MM/YYYY') then dbms_xmlgen.setBindValue(qryCtx, 'dt_pedido_ini', v_dt_pedido_ini);
            end if;
            --
            if v_dt_pedido_fim is not null and ts_util.isdate(to_char(v_dt_pedido_fim, 'DD/MM/YYYY'), 'DD/MM/YYYY') then dbms_xmlgen.setBindValue(qryCtx, 'dt_pedido_fim', v_dt_pedido_fim);
            end if;
            --
        else
          if v_tamanho_reembolso <= 15 then
            dbms_xmlgen.setBindValue(qryCtx, 'num_reembolso', v_num_reembolso);
          else
            dbms_xmlgen.setBindValue(qryCtx, 'num_reembolso', v_num_reembolso_ant);
          end if;
        end if;
        --
        TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO.1', 0, 'DEBUG', sqlerrm || ' (' || ts.top_utl_padrao.msgerro  || ')' || chr(13) || v_SQL || chr(13) || p_xml_filtro || chr(13) || 'v_dt_pedido_ini=' || v_dt_pedido_ini || chr(13) || 'v_dt_pedido_fim=' || v_dt_pedido_fim, 'RetornaPrevia' );

        dbms_xmlgen.setCheckInvalidChars        ( qryCtx , true     );
        dbms_xmlgen.useNullAttributeIndicator   ( qryCtx , true     );
        dbms_xmlgen.setRowSetTag                ( qryCtx , 'PEDIDO' );
        dbms_xmlgen.setRowTag                   ( qryCtx , 'DADOS'  );
        p_xml_retorno                           := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext                ( qryCtx );
        --

    exception
        when others then
            --
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 99, 'Erro', sqlerrm || ' (' || ts.top_utl_padrao.msgerro  || ')' || chr(13) || v_SQL || chr(13) || p_xml_filtro || chr(13) || 'v_dt_pedido_ini=' || v_dt_pedido_ini || chr(13) || 'v_dt_pedido_fim=' || v_dt_pedido_fim, 'RetornaPrevia' );
            --
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PEDIDO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || sqlerrm || ' (' || ts.top_utl_padrao.msgerro  || ')' || '</MSG_RETORNO>';
            --p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS></PEDIDO>';
            return;
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Retorna xml com as informa��es das Ocorr�ncias
    ----------------------------------------------------------------------------
    procedure RetornaOcorrencia(p_num_reembolso in  varchar2, p_xml_retorno out clob) is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    v_tamanho_reembolso     number;
    v_num_reembolso         varchar2(20);

    begin
        --In�cio
        v_posicao := 1;

        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<OCORRENCIA><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>2 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</DADOS></OCORRENCIA>';
            return;
        END IF;
        --
        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        v_num_reembolso := p_num_reembolso;
        --
        if v_tamanho_reembolso > 15 then
          select p.num_reembolso
            into v_num_reembolso
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        end if;
        --
        v_posicao := 15;

        --Monta XML das participa��es
        v_SQL := '';
        v_SQL := v_SQL || 'SELECT DISTINCT a.cod_tipo_ocorrencia, a.cod_usuario, u.nom_usuario, a.txt_obs,';
        v_SQL := v_SQL || '         a.txt_obs_previa, b.nom_tipo_ocorrencia, a.dt_ocorrencia dt_ocorrencia_ordem,';
        v_SQL := v_SQL || '         to_char(a.dt_ocorrencia,'||chr(39)||'DD/MM/YYYY HH24:MI'||chr(39)||') dt_ocorrencia';
        v_SQL := v_SQL || '    FROM ts.REEMBOLSO_PREVIA_OCORRENCIA a, ts.REEMB_PREVIA_TIPO_OCORRENCIA b, usuario u';
        v_SQL := v_SQL || '   WHERE a.num_reembolso = :num_reembolso';
        v_SQL := v_SQL || '     AND a.cod_tipo_ocorrencia = b.cod_tipo_ocorrencia';
        v_SQL := v_SQL || '     AND a.cod_usuario   = u.cod_usuario(+)';
        v_SQL := v_SQL || ' ORDER BY dt_ocorrencia_ordem DESC';

        v_posicao := 16;

        qryCtx := dbms_xmlgen.newContext(v_SQL);
        dbms_xmlgen.setBindValue(qryCtx, 'num_reembolso', v_num_reembolso);
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'OCORRENCIA' );
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

        return;

    EXCEPTION
        WHEN OTHERS THEN

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<OCORRENCIA><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS></OCORRENCIA>';

            return;
    END;
    ----------------------------------------------------------------------------
    -- Gravar os motivos de indeferimento
    ----------------------------------------------------------------------------
    PROCEDURE GravaMotivo(p_ind_acao              in   varchar2,
                          p_cod_motivo            in   number,
                          p_ind_tipo              in   varchar2,
                          p_desc_motivo           in   varchar2,
                          p_txt_mensagem          in   varchar2,
                          p_cod_retorno           out  number,
                          p_msg_retorno           out  varchar2) is

    v_posicao       number;
    v_item_vazio    varchar2(3) := '���';
    v_qtd           number;

    begin

    v_posicao := 0;

    ---------------------------------------------------------------------------------

    v_posicao := 1;

    if nvl(p_cod_motivo,0) = 0 then
        p_cod_retorno := 1;
        p_msg_retorno := 'C�digo do motivo n�o informada';
        return;
    end if;

    v_posicao := 2;

    if nvl(p_desc_motivo,v_item_vazio) = v_item_vazio then
        p_cod_retorno := 1;
        p_msg_retorno := 'Descri��o do motivo n�o informado';
        return;
    end if;

    if nvl(p_ind_tipo,v_item_vazio) = v_item_vazio then
        p_cod_retorno := 1;
        p_msg_retorno := 'Tipo do motivo n�o informado';
        return;
    end if;

    if p_ind_tipo NOT in ('R', 'C', 'X', 'A') then
        p_cod_retorno := 1;
        p_msg_retorno := 'Indicador do tipo inv�lido (' || p_ind_tipo || ')';
        return;
    end if;

    if p_ind_acao NOT in ('A', 'I', 'E') then
        p_cod_retorno := 1;
        p_msg_retorno := 'Indicador do tipo de a��o inv�lido (' || p_ind_acao || ')';
        return;
    end if;

    v_posicao := 3;

    if p_ind_acao in ('A', 'I') then
        select count(*) into v_qtd
          from reembolso_previa_motivo_indef
           where UPPER(desc_motivo) = UPPER(p_desc_motivo)
             and cod_motivo <> p_cod_motivo;
        if v_qtd > 0 then
            p_cod_retorno := 1;
            p_msg_retorno := 'Descri��o j� utilizada para outro motivo';
            return;
        end if;
    end if;

    v_posicao := 4;

    if p_ind_acao = 'I' then

        begin
            insert into ts.reembolso_previa_motivo_indef
                            (cod_motivo,       desc_motivo,     ind_tipo,
                             txt_mensagem)
                 values     (p_cod_motivo,     p_desc_motivo,   p_ind_tipo,
                             p_txt_mensagem);
        exception
            when dup_val_on_index then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao realizar inclus�o do motivo: ' || sqlerrm;
                rollback;
        end;
    elsif p_ind_acao = 'A' then
        begin
            update ts.reembolso_previa_motivo_indef set
                   desc_motivo  = p_desc_motivo,
                   txt_mensagem = p_txt_mensagem,
                   ind_tipo     = p_ind_tipo
             where cod_motivo   = p_cod_motivo;
        exception
            when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao realizar altera��o do motivo: ' || sqlerrm;
                rollback;
        end;
    elsif p_ind_acao = 'E' then

        --Validar se existe algum reembolso com o motivo
        select count(*) into v_qtd
          from ts.pedido_reembolso_previa
           where cod_motivo = p_cod_motivo;
        if v_qtd > 0 then
            p_cod_retorno := 1;
            p_msg_retorno := 'Motivo n�o pode ser exclu�do pois est� sendo utilizado por uma pr�via de reembolso';
            return;
        end if;

        begin
            delete from ts.reembolso_previa_motivo_indef
             where cod_motivo = p_cod_motivo;
        exception
            when others then
                p_cod_retorno := 9;
                p_msg_retorno := 'Erro ao realizar exclus�o do motivo: ' || sqlerrm;
                rollback;
        end;
    end if;

    p_cod_retorno := 0;
    p_msg_retorno := null;

    commit;

    exception
        when others then

             p_cod_retorno := 9;
             p_msg_retorno := 'GravarMotivo - ' || v_posicao || ': ' || sqlerrm;
             rollback;

    end;

    ----------------------------------------------------------------------------
    -- Gerar ocorr�ncia na pr�via de reembolso informada
    ----------------------------------------------------------------------------
    PROCEDURE GeraOcorrencia(p_num_reembolso         in   number,
                             p_num_reembolso_ans     in   varchar2 DEFAULT null,
                             p_cod_ocorrencia        in   number,
                             p_txt_obs               in   varchar2,
                             p_txt_operadora         in   varchar2,
                             p_cod_usuario           in   varchar2,
                             p_cod_retorno           out  number,
                             p_msg_retorno           out  varchar2,
                             p_num_seq_item          in   number DEFAULT 0) is

    v_posicao                   number;

    begin

    v_posicao := 0;

    ---------------------------------------------------------------------------------

    v_posicao := 1;

    if nvl(p_num_reembolso,0) = 0 then
        p_cod_retorno := 1;
        p_msg_retorno := 'Pr�via de Reembolso n�o informada';
        return;
    end if;

    v_posicao := 2;

    if nvl(p_cod_ocorrencia,0) = 0 then
        p_cod_retorno := 1;
        p_msg_retorno := 'Tipo de ocorr�ncia n�o informado';
        return;
    end if;

    v_posicao := 3;

    begin
        insert into ts.REEMBOLSO_PREVIA_OCORRENCIA
                        (num_reembolso,       dt_ocorrencia,           cod_tipo_ocorrencia,
                         cod_usuario,         txt_obs,                 txt_obs_previa,
                         num_reembolso_ans
                         --num_seq_item
                         )
             values     (p_num_reembolso,     sysdate,                 p_cod_ocorrencia,
                         p_cod_usuario,       p_txt_obs,               p_txt_operadora,
                         p_num_reembolso_ans
                         --NVL(p_num_seq_item,0)
                         );
    exception
        when dup_val_on_index then
            null;
    end;

    p_cod_retorno := 0;
    p_msg_retorno := null;

    exception
        when others then

             p_cod_retorno := 9;
             p_msg_retorno := 'GeraOcorrencia - ' || v_posicao || ': ' || sqlerrm;
             rollback;

    end;
     ----------------------------------------------------------------------------
    -- Retornar RecordSet com o �ltimo n�mero da sequ�ncia dos itens para uma
    -- pr�via de reembolso informada
    ----------------------------------------------------------------------------
    function RetornaUltimoSeqItem (p_num_reembolso  in varchar2) return sys_refcursor
    is

    c   sys_refcursor;

    begin
    null;
        --
        open c for
            SELECT max(num_seq_item)
              FROM procedimento_reembolso_previa
             Where num_reembolso = p_num_reembolso
                or num_reembolso_ans = p_num_reembolso;
        --
        return c;
    end;

    ----------------------------------------------------------------------------
    -- Verifica se a glosa informada esta habilitada na data informada
    ----------------------------------------------------------------------------
    FUNCTION VerificaGlosa (p_cod_critica             in varchar2,
                            p_dt_transacao            in varchar2)
                            return number
    is

    v_posicao           number;
    v_ind_uso           motivo_glosa.ind_uso_prb%type;
    v_ind_implementado  motivo_glosa.ind_implementado_prb%type;
    v_desc_msg          motivo_glosa.desc_previa_reembolso%type;
    v_dt_ini_validade   motivo_glosa.dt_ini_prb%type;
    v_dt_fim_validade   motivo_glosa.dt_fim_prb%type;

    ------------------------------------------------------------------------

    BEGIN

        v_posicao     := 1;
        ----------------------------------------------------------------------
        --- Recupera a glosa informada
        ----------------------------------------------------------------------
        begin
            EXECUTE IMMEDIATE 'select ind_uso_prb, ind_implementado_prb, desc_previa_reembolso, dt_ini_prb, dt_fim_prb
                                 from motivo_glosa
                                where cod_motivo_glosa = :p_cod_critica'
              into  v_ind_uso,           v_ind_implementado,    v_desc_msg,
                    v_dt_ini_validade,   v_dt_fim_validade
            using p_cod_critica;
        exception
            when no_data_found then
                return 9;
        end;

        v_posicao     := 5;

        --- verifica se a cr�tica est� valida na data da transa��o
        if nvl(v_ind_implementado, 'N')  = 'S'  then
            if nvl(p_dt_transacao,trunc(sysdate)) between  nvl(v_dt_ini_validade,   nvl(p_dt_transacao,trunc(sysdate))) and
                                       nvl(v_dt_fim_validade,   nvl(p_dt_transacao,trunc(sysdate))) then

                --- verifica se a cr�tica est� dispon�vel para o tipo de cr�tica que est� sendo realizada
                if nvl(v_ind_uso,'N') = 'S' /*and p_tipo_critica = 1*/ then
                    null;
                else
                    return 9;  --- cr�tica n�o habilitada
                end if;
            else
                return 9;    --- cr�tica n�o v�lida na data
            end if;
        else
            return 9;  --- cr�tica n�o habilitada
        end if;

        v_posicao     := 10;

        return 0;

    exception
        when others then
             return 9;
    end;

    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
    procedure GravaGlosa (p_num_reembolso        in number,
                          p_cod_motivo_glosa     in number,
                          p_num_seq_item         in number,
                          p_cod_procedimento     in varchar2,
                          p_dt_inclusao          in date,
                          p_cod_usuario_inclusao in varchar2,
                          p_txt_complemento      in varchar2,
                          p_cod_retorno          out number,
                          p_msg_retorno          out varchar2)
    is

    v_posicao       number;
    v_item_vazio    varchar2(3) := '���';

    begin

    v_posicao := 0;

    if nvl(p_num_reembolso,0) = 0 then
        p_cod_retorno := 9;
        p_msg_retorno := 'Pr�via de Reembolso n�o informada';
        return;
    end if;

    if NVL(p_cod_motivo_glosa,0) = 0 then
        p_cod_retorno := 9;
        p_msg_retorno := 'Critica n�o informada';
        return;
    end if;

    begin
        insert into reembolso_previa_glosa (
                num_reembolso,                  cod_motivo_glosa,
                num_seq_item,
                dt_inclusao,                   cod_usuario_inclusao,
                txt_complemento)
       values  (p_num_reembolso,                p_cod_motivo_glosa,
                NVL(p_num_seq_item,0),
                NVL(p_dt_inclusao,sysdate),     p_cod_usuario_inclusao,
                p_txt_complemento);
    exception
        when dup_val_on_index then
            null;
    end;

    p_cod_retorno := 0;
    p_msg_retorno := '';

    end;

    --
    procedure getQtdFamilia                         ( p_cod_ts                      in   number
                                                    , p_qtd_familia                 out  number
                                                    )
    is
        v_cod_ts_tit number;
    begin
        --In�cio
        begin
         select cod_ts_tit
           into v_cod_ts_tit
           from associado
          where cod_ts = p_cod_ts;
        exception
            when others then
                p_qtd_familia := 0;
                return;
        end;
        --
        select count(*)
          into p_qtd_familia
          from associado
         where cod_ts_tit = v_cod_ts_tit;

        return;

    EXCEPTION
        WHEN OTHERS THEN
            p_qtd_familia := 0;
            return;
    END;
    --

    ----------------------------------------------------------------------------
    -- Realiza toda valida��o das glosas na pr�via de reembolso
    ----------------------------------------------------------------------------
    procedure ValidaPrevia (p_num_reembolso        in number,
                            p_cod_usuario          in varchar2,
                            p_cod_retorno          out number,
                            p_msg_retorno          out varchar2)
    is

    v_doc                           xmldom.DOMDocument;
     v_qtd                           pls_integer;
    v_posicao                       number;
    v_item_vazio                    varchar2(3) := '���';
    v_cod_motivo_glosa              motivo_glosa.cod_motivo_glosa%type;
     v_cod_grupo_cobertura_ass       varchar2(20);
     v_ind_tipo_pessoa_contrato      varchar2(10);

     v_num_associado_autorizacao     autorizacao.num_associado%type;
     v_cod_situacao_autorizacao      autorizacao.cod_situacao%type;



    v_num_insc_fiscal               pedido_reembolso_previa.num_insc_fiscal%type;
    v_num_crm                       pedido_reembolso_previa.num_crm%type;
    v_nome_prestador                pedido_reembolso_previa.nome_prestador%type;
    v_ind_insc_fiscal               pedido_reembolso_previa.ind_insc_fiscal%type;
    v_cod_tratamento                pedido_reembolso_previa.cod_tratamento%type;
    v_cod_acomodacao                pedido_reembolso_previa.cod_acomodacao%type;
    v_cnes                          pedido_reembolso_previa.cnes%type;
    v_cod_plano                     pedido_reembolso_previa.cod_plano%type;
    v_ind_acomodacao                pedido_reembolso_previa.ind_acomodacao%type;
    v_sigla_conselho                pedido_reembolso_previa.sigla_conselho%type;
    v_uf_conselho                   pedido_reembolso_previa.uf_conselho%type;
    v_cod_ts                        pedido_reembolso_previa.cod_ts%type;
    v_cod_ts_tit                    pedido_reembolso_previa.cod_ts_tit%type;
    v_ind_sexo                      pedido_reembolso_previa.ind_sexo%type;
    v_ind_sexo_proc                 pedido_reembolso_previa.ind_sexo%type;
    v_cod_ts_contrato               pedido_reembolso_previa.cod_ts_contrato%type;
     v_cod_operadora_contrato        pedido_reembolso_previa.cod_operadora_contrato%type;
    v_dt_ini_atendimento            date; --pedido_reembolso_previa.dt_ini_atendimento%type;
    v_dt_pedido                     date; --0pedido_reembolso_previa.dt_pedido%type;
    v_data_nascimento               pedido_reembolso_previa.data_nascimento%type;
    v_ind_internado                 tipo_tratamento.ind_internado%type;
    v_ind_utilizacao                tipo_tratamento.ind_utilizacao%type;
    v_ind_tipo                      tipo_tratamento.ind_tipo%type;
    v_ind_tem_cobertura             varchar2(1);
    v_ind_tem_carencia              varchar2(1);
    v_ind_reembolso                 varchar2(1);
    v_tipo_item                     varchar2(1);
    v_cod_procedimento_principal    procedimento_reembolso_previa.cod_procedimento%type;
    v_ind_rol_procedimentos         itens_medicos.ind_rol_procedimentos%type;
    v_cod_acomodacao_plano          plano_medico.cod_acomodacao%type;
    v_ind_tipo_acomodacao_plano     plano_medico.ind_acomodacao%type;
    v_sigla_tabela_hosp_reembolso   plano_medico.sigla_tabela_hosp_reembolso%type;
    v_sigla_tabela_reembolso        plano_medico.sigla_tabela_reembolso%type;
    v_cod_grupo_estatistico_pai     itens_medicos.cod_grupo_estatistico%type;
    v_ind_cobertura_procedimento    itens_medicos.ind_cobertura%type;

     v_nome_associado                varchar2(200);
     v_ind_tipo_carencia             varchar2(1);
    v_quantidade                    number;
    v_ind_erro_out                  varchar2(10);
    v_msg_retorno_out               varchar2(300);
    v_ind_situacao_sur              varchar2(2);
    v_cod_padrao                    itens_medicos.cod_padrao%type;
    v_ind_limite_idade_ate          itens_medicos.ind_limite_idade_ate%type;
    v_ind_limite_idade              itens_medicos.ind_limite_idade%type;
    v_rb_glosa_servicos_sem_cob     controle_sistema.val_parametro%type;
    v_val_calculado                 pedido_reembolso_previa.val_calculado%type;
    v_val_informado                 pedido_reembolso_previa.val_informado%type;
    v_ind_tipo_reembolso            pedido_reembolso_previa.ind_tipo_reembolso%type;
    v_num_associado                 pedido_reembolso_previa.num_associado%type;
    v_cod_motivo_reembolso          pedido_reembolso_previa.cod_motivo_reembolso%type;
    v_ind_origem_associado          pedido_reembolso_previa.ind_origem_associado%type;

    v_qtd_valida                    number;

    v_cod_retorno_xml               number;
    v_xml_calculo                   clob;
    v_node_list                     ts.xmldom.DOMNodeList;
    v_node                          ts.xmldom.DOMNode;
    v_data_teste                    date;
    v_num_pedido                    autorizacao.num_pedido%type;
    v_ind_carater                   pedido_reembolso_previa.ind_carater%type;
    --
    rsMemoriaPrevia                 memoria_previa_detalhe%rowtype;
    v_val_calculado_aux             number;
    v_qtd_informado_aux             number;
    v_num_insc_fiscal_solicitante   solicitante.num_insc_fiscal%type;
    --
    begin

        v_posicao := 0;

        if nvl(p_num_reembolso,0) = 0 then
            p_cod_retorno := 9;
            p_msg_retorno := 'Pr�via de Reembolso n�o informada';
            return;
        end if;

        v_posicao := 1;
        ------------------------------------------------------------------
        ---- Recuperar informa��es da pr�via
        ------------------------------------------------------------------
        begin
            select a.num_insc_fiscal,    a.num_crm,          a.ind_insc_fiscal,     a.cod_tratamento,
                   tt.ind_internado,     tt.ind_utilizacao,  tt.ind_tipo,           a.cod_acomodacao,     a.cnes,
                   a.cod_plano,          a.ind_acomodacao,   a.cod_ts,              a.ind_sexo,           a.cod_ts_contrato,
                   trunc(a.dt_inclusao), a.data_nascimento,
                   a.sigla_conselho,     a.uf_conselho,      a.nome_prestador,       a.val_calculado,     a.val_informado,
                   a.cod_operadora_contrato,
                   a.ind_tipo_reembolso,
                   a.num_associado,
                   a.cod_motivo_reembolso,
                   a.num_internacao,
                   a.ind_origem_associado,
                   a.ind_carater,
                   a.cod_ts_tit

              into v_num_insc_fiscal,    v_num_crm,          v_ind_insc_fiscal,      v_cod_tratamento,
                   v_ind_internado,      v_ind_utilizacao,   v_ind_tipo,             v_cod_acomodacao,    v_cnes,
                   v_cod_plano,          v_ind_acomodacao,   v_cod_ts,               v_ind_sexo,          v_cod_ts_contrato,
                   v_dt_pedido,          v_data_nascimento,
                   v_sigla_conselho,     v_uf_conselho,      v_nome_prestador,       v_val_calculado,     v_val_informado,
                   v_cod_operadora_contrato,
                   v_ind_tipo_reembolso,
                   v_num_associado,
                   v_cod_motivo_reembolso,
                   v_num_pedido,
                   v_ind_origem_associado,
                   v_ind_carater,
                   v_cod_ts_tit

                  from ts.pedido_reembolso_previa a, tipo_tratamento tt
                      where num_reembolso    = p_num_reembolso
                        and a.cod_tratamento = tt.cod_tratamento(+);
        exception
            when no_data_found then
                p_cod_retorno := 9;
                p_msg_retorno := 'Pr�via de Reembolso n�o encontrada (' || p_num_reembolso ||')';
                return;
        end;

        --dbms_output.put_line(' leitura realizada v_dt_pedido = ' || v_dt_pedido);

        v_posicao := 15;
        ------------------------------------------------------------------
        ---- Glosa: 511 - Aus�ncia Dos Dados Do Executante
          ---- glosa retirada da pr�via
        ------------------------------------------------------------------
        /*
        v_cod_motivo_glosa := 511;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            if (v_num_insc_fiscal is null AND v_num_crm is null) OR v_nome_prestador is null then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;*/

        v_posicao := 20;
        ------------------------------------------------------------------
        ---- Glosa: 340 - CNES do executante inv�lido
          ---- glosa retirada da pr�via
        ------------------------------------------------------------------
        /*
        v_cod_motivo_glosa := 340;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            if v_cnes is not null and ts_valida_cnes(v_cnes) = 'NOK' then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;
        */


       /* v_posicao := 25;
        --------------------------------------------------------------------
        ---- Glosa: 415 - Executante informado faz parte da rede credenciada
          ---- glosa retirada da pr�via
        --------------------------------------------------------------------
        v_cod_motivo_glosa := 415;

        v_data_teste := coalesce(v_dt_ini_atendimento,v_dt_pedido,trunc(sysdate));

        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then

            if nvl(v_num_insc_fiscal,0) <> 0 then
                --Procurar pelo CPF ou CGC
                if v_ind_insc_fiscal = 'F' then
                    SELECT COUNT (*) into v_qtd_valida
                      FROM ts.prestador_servico ps, ts.entidade_sistema es
                     WHERE ps.cod_entidade_ts = es.cod_entidade_ts
                       AND es.ind_tipo_pessoa = v_ind_insc_fiscal
                       AND es.num_cpf = v_num_insc_fiscal;
                      -- AND (nvl(ps.dt_exclusao_atd, ps.dat_exclusao) is null or v_data_teste <= nvl(ps.dt_exclusao_atd , ps.dat_exclusao) );
                else
                    SELECT COUNT (*) into v_qtd_valida
                      FROM ts.prestador_servico ps, ts.entidade_sistema es
                     WHERE ps.cod_entidade_ts = es.cod_entidade_ts
                       AND es.ind_tipo_pessoa = v_ind_insc_fiscal
                       AND es.num_cgc = v_num_insc_fiscal;
                      -- AND (nvl(ps.dt_exclusao_atd, ps.dat_exclusao) is null or v_data_teste <= nvl(ps.dt_exclusao_atd , ps.dat_exclusao) );
                end if;

            end if;

            if nvl(v_num_crm,v_item_vazio) <> v_item_vazio AND NVL(v_qtd_valida,0) = 0 then
                --Procurar pelo CRM
                SELECT COUNT (*) into v_qtd_valida
                  FROM ts.prestador_servico ps
                 WHERE ps.num_crm = v_num_crm
                   AND ps.sigla_conselho = NVL(v_sigla_conselho,ps.sigla_conselho)
                   AND ps.sgl_uf_conselho = NVL(v_uf_conselho,ps.sgl_uf_conselho);
                   -- AND (nvl(ps.dt_exclusao_atd, ps.dat_exclusao) is null or v_data_teste <= nvl(ps.dt_exclusao_atd , ps.dat_exclusao) );

            end if;

            if NVL(v_qtd_valida,0) > 0 then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;
        */


        v_posicao := 30;
        ------------------------------------------------------------------
        ---- Glosa: 723 - Tratamento ausente ou inv�lido
          ---- glosa retirada da pr�via
        ------------------------------------------------------------------
          /*
        v_cod_motivo_glosa := 723;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            if nvl(v_cod_tratamento,'0') = '0' then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;
       */


        v_posicao := 40;
        ------------------------------------------------------------------
        ---- Glosa: 724 - Acomoda��o ausente ou inv�lida
          ---- glosa retirada da pr�via
        ------------------------------------------------------------------
          /*
        v_cod_motivo_glosa := 724;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            if v_ind_internado = 'S' then
                if v_ind_utilizacao <> 7 then
                    if nvl(v_cod_acomodacao,'0') = '0'  then
                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;
                    end if;
                end if;
            end if;
        end if;
          */

        v_posicao := 50;
        ------------------------------------------------------------------
        ---- Verifica acomoda��o informada com a do benefici�rio
        ------------------------------------------------------------------

        begin
                select   ind_acomodacao
                    into v_ind_tipo_acomodacao_plano
                        from ts.plano_medico
                            where cod_plano = v_cod_plano;
            exception
                when no_data_found then
                     v_ind_tipo_acomodacao_plano := 'E';
            end;

          /*
        if v_ind_internado = 'S'  and  v_ind_utilizacao <> 7 then
            begin
                select distinct ind_acomodacao, cod_acomodacao
                    into v_ind_tipo_acomodacao_plano, v_cod_acomodacao_plano
                        from ts.plano_medico
                            where cod_plano = v_cod_plano;
            exception
                when no_data_found then
                    if v_cod_plano is null then
                        null;
                    else
                        p_cod_retorno := 9;
                        p_msg_retorno := 'Acomoda��o do plano inexistente.' || v_cod_plano;
                        return;
                    end if;
                when others then
                    p_cod_retorno := 9;
                    p_msg_retorno := 'Erro na obten��o da acomoda��o: ' || sqlerrm;
                    return;
            end;

            if v_ind_tipo_acomodacao_plano = 'E' and v_ind_acomodacao = 'A' then

                v_posicao := 60;
                ------------------------------------------------------------------
                ---- 731 - Acomodacao inferior com plano beneficiario
                ------------------------------------------------------------------
                v_cod_motivo_glosa := 731;
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;
                end if;

            else

                v_posicao := 70;
                ------------------------------------------------------------------
                ---- 730 - Acomodacao superior com plano beneficiario
                ------------------------------------------------------------------
                v_cod_motivo_glosa := 730;
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                    if v_ind_tipo_acomodacao_plano = 'A'  and  v_ind_acomodacao = 'E' then
                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;
                    end if;
                end if;
            end if;
        end if;
        */


        v_posicao := 80;

          /*
           IND_TIPO_REEMBOLSO   NOME_TIPO_REEMBOLSO
            ------------------  ----------------------
            1                   Consultas
            2                   Exames e Procedimentos
            3                   Interna��o

        */
          if    v_ind_tipo_reembolso = 1 then
                v_cod_tratamento := 5;  -- consulta
          elsif v_ind_tipo_reembolso = 2 then
                v_cod_tratamento := 15; -- ambulatorial
          elsif v_ind_tipo_reembolso = 3 then
                v_cod_tratamento := 9; -- interna��o
          else
                v_cod_tratamento := 15; -- ambulatorial
          end if;

        ---------------------------------------------------------------------------
        ---- 750 - Plano/contrato sem cobertura para o tipo de atendimento
        ---------------------------------------------------------------------------
        v_cod_motivo_glosa := 750;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            if v_cod_ts is not null then
                -- verifica se tem cobertura

                if v_cod_tratamento = 9 then

                    for t in ( select min(cod_tratamento) cod_tratamento
                                    from ts.tipo_tratamento
                                    where cod_tratamento_pai = v_cod_tratamento
                                    group by ind_tipo )
                    loop

                        crt_utl_base.CR_VER_TRATAMENTO_COBERTURA (t.cod_tratamento,      v_ind_sexo,
                                                                  v_cod_plano,           v_cod_ts_contrato,
                                                                  v_dt_pedido,           v_ind_tem_cobertura,
                                                                  p_msg_retorno,         p_cod_retorno);
                        if v_ind_tem_cobertura = 'S' then
                            exit;
                        end if;

                    end loop;


                else
                    crt_utl_base.CR_VER_TRATAMENTO_COBERTURA (v_cod_tratamento,      v_ind_sexo,
                                                              v_cod_plano,           v_cod_ts_contrato,
                                                              v_dt_pedido,           v_ind_tem_cobertura,
                                                              p_msg_retorno,         p_cod_retorno);
                end if;

                if p_cod_retorno = 9 then return;
                elsif v_ind_tem_cobertura = 'N' then
                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;
                end if;
             end if;
        end if;

        -------------------------------------------------------------
        ------- criticas de car�ncias
        -------------------------------------------------------------

        v_posicao := 90;

        -------------------------------------------------------------
        --- Glosa: 503 - benefici�rio com car�ncia de CPT
        -------------------------------------------------------------
        v_cod_motivo_glosa := 503;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then

            crt_utl_base.CR_VERIFICA_CPT(  v_dt_pedido, p_msg_retorno,      p_cod_retorno);

             if    p_cod_retorno  = 9 then return;
            elsif p_cod_retorno <> 0 then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;

        v_posicao := 100;

         -- verifica se benefic�rio tem alguma car�ncia
         crt_utl_base.CR_VER_CARENCIA (v_cod_ts,               null,
                                       v_dt_pedido,            v_ind_tem_carencia,
                                       p_msg_retorno,          p_cod_retorno);


        if p_cod_retorno = 9   then return;
        elsif v_ind_tem_carencia = 'S' then

            v_posicao := 110;
            -------------------------------------------------------------
            ------- Glosa: 760 - benefici�rio com car�ncia n�o cumprida
            -------------------------------------------------------------
            v_cod_motivo_glosa := 760;
            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                crt_utl_base.CR_VALIDA_CARENCIA( v_cod_ts,           v_dt_pedido,
                                    null,               null,
                                    v_cod_tratamento,   null,
                                    p_msg_retorno,      p_cod_retorno
                                    --,v_ind_carater,      v_ind_sexo
                                    );




                if p_cod_retorno = 9  then return;
                elsif p_cod_retorno <> 0 then
                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;
                end if;
            end if;

            v_posicao := 120;
            ------------------------------------------------------------------------
            ------- Glosa: 761 - benefici�rio com car�ncia de acomoda��o superior
            ------------------------------------------------------------------------
            v_cod_motivo_glosa := 761;
            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                if v_cod_acomodacao is not null and v_ind_internado = 'S' then
                    crt_utl_base.CR_VALIDA_CARENCIA_ACOMODACAO( v_cod_ts,                v_dt_pedido,
                                                                v_cod_acomodacao_plano,  v_cod_acomodacao,
                                                                p_msg_retorno,           p_cod_retorno);

                    if p_cod_retorno = 9 then return;
                    elsif p_cod_retorno <> 0 then
                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;
                    end if;
                end if;
            end if;
        end if;
        ----- fim verifica��o de car�ncias
        
        ------------------------------------------------------------------------
        ------- Glosa: 1572 - CPF do solicitante x CPF do benefici�rio titular\dependente
        ------------------------------------------------------------------------
        v_cod_motivo_glosa := 1572;
        v_posicao := 400;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
          
            BEGIN
              select s.num_insc_fiscal
                into v_num_insc_fiscal_solicitante
                from ts.pedido_reembolso_previa p 
                   , ts.solicitante s
               where p.num_reembolso = p_num_reembolso
                 and p.cod_solicitante = s.cod_solicitante;
            EXCEPTION
              WHEN OTHERS THEN
                 v_num_insc_fiscal_solicitante := null;
            END;
            --
            if ts.rb_trata_glosa.ValidaCpfExecBenef(v_cod_ts_tit,v_cod_ts,v_num_insc_fiscal_solicitante) = 0 then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;

        v_posicao := 130;
        ------------------------------------------------------------------------
        ------- Glosa: 408 - plano sem cobertura para reembolso
        ------------------------------------------------------------------------
        v_cod_motivo_glosa := 408;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            crt_utl_base.CR_PLANO_REEMBOLSO (v_cod_plano,                v_sigla_tabela_hosp_reembolso,
                                             v_sigla_tabela_reembolso,   v_ind_reembolso,
                                             p_msg_retorno,              p_cod_retorno) ;



            if p_cod_retorno = 9 then return;
            elsif p_cod_retorno <> 0 then
                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno = 9 then return; end if;
            end if;
        end if;
        v_posicao := 200;


          -- INCLUS�O GLOSAS 501, 502, 506


       if  nvl(aut_ctx_beneficiario.get_indOrigem,'DB') != 'WS' then
               crt_utl_base.CR_VALIDA_ELEGIBILIDADE ( p_tipo_critica                 => '4'
                                                    , p_dt_atendimento               => v_dt_pedido
                                                    , p_mes_ano_ref                  => v_dt_pedido
                                                    , p_num_associado                => v_num_associado
                                                    , p_cod_ts                       => v_cod_ts
                                                    , p_cod_prestador_ts             => null
                                                    , p_num_grd                      => null
                                                    , p_num_guia                     => null
                                                    , p_num_seq_guia                 => null
                                                    , p_num_anexo                    => null
                                                    , p_num_reembolso                => p_num_reembolso
                                                    , p_dt_nascimento                => v_data_nascimento
                                                    , p_ind_sexo                     => v_ind_sexo
                                                    , p_nome_associado               => v_nome_associado
                                                    , p_cod_plano                    => v_cod_plano
                                                    , p_cod_acomodacao               => v_ind_tipo_acomodacao_plano
                                                    , p_cod_motivo_glosa             => v_cod_motivo_glosa
                                                    , p_cod_ts_contrato              => v_cod_ts_contrato
                                                    , p_ind_tipo_carencia            => v_ind_tipo_carencia
                                                    , p_cod_grupo_cobertura          => v_cod_grupo_cobertura_ass
                                                    , p_ind_tipo_pessoa_contrato     => v_ind_tipo_pessoa_contrato
                                                    , p_msg_retorno                  => p_msg_retorno
                                                    , p_cod_retorno                  => p_cod_retorno
                                                         );
                if p_cod_retorno != 0 then
                    if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                   GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                   if p_cod_retorno = 9 then return; end if;
                     end if;
                end if;
        end if;


          select count(*)  into v_qtd
                 from ts.procedimento_reembolso_previa
                  where num_reembolso         = p_num_reembolso;


         ------------------------------------------------------------------------
         --- Glosa: 616 - servi�o / procedimento n�o informado
         ------------------------------------------------------------------------
         v_cod_motivo_glosa := 616;
         if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
             if nvl(v_qtd,0) = 0 then
                 GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,null,null,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                 if p_cod_retorno = 9 then return; end if;
             end if;
         end if;

         if  nvl(aut_ctx_beneficiario.get_indOrigem,'DB') != 'WS' then
              v_posicao := 210;
            ------------------------------------------------------------------------
            --- verifica se contrato estava inadimplente na data do atendimento
            --- de algum procedimento que tenha valor aprovado
            --- Glosa: 762
            ------------------------------------------------------------------------
            v_cod_motivo_glosa := 762;
            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                if v_dt_pedido is not null then

                    SUR_VERIFICA_INADIM_ASSOC (v_cod_ts,
                                               v_dt_pedido,
                                               v_ind_situacao_sur, -- A - Ativo,
                                                                   -- I - Inadimplente
                                               v_ind_erro_out,
                                               v_msg_retorno_out);

                    if v_ind_erro_out <> '0'  then
                        p_cod_retorno     := v_ind_erro_out;
                        p_msg_retorno     := v_msg_retorno_out;
                        return;
                    end if;

                    if v_ind_situacao_sur = 'I'  then
                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,NULL,NULL,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;
                    end if;

                end if;
            end if;
         end if;

          --- validar autoriza��o

          if v_num_pedido is not null then
          /*
            862 Senha inexistente
          864   Autoriza��o pr�via negada
          866   AP cancelada
          871   AP informada n�o pertence ao benefici�rio
        */
            begin
                select num_associado,                cod_situacao
                    into v_num_associado_autorizacao, v_cod_situacao_autorizacao
                          from autorizacao
                                 where num_pedido = v_num_pedido;


                if v_cod_situacao_autorizacao = 2 then
                    v_cod_motivo_glosa := 866;   -- AP cancelada
               if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                      GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,NULL,NULL,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                  if p_cod_retorno = 9 then return; end if;
               end if;
                elsif v_cod_situacao_autorizacao = 9 then
                    v_cod_motivo_glosa := 864; -- Autoriza��o pr�via negada
               if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                      GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,NULL,NULL,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                  if p_cod_retorno = 9 then return; end if;
               end if;
                elsif v_num_associado_autorizacao <> v_num_associado then
                    v_cod_motivo_glosa := 871; -- AP informada n�o pertence ao benefici�rio
                  if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                         GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,NULL,NULL,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                     if p_cod_retorno = 9 then return; end if;
                  end if;
                else
                   null;
                end if;

             exception
                when no_data_found then
                      v_cod_motivo_glosa := 862;
                  if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                         GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,NULL,NULL,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                     if p_cod_retorno = 9 then return; end if;
                  end if;
             end;
          end if;


        ------------------------------------------------------------------------
        --- VALIDAR OS PROCEDIMENTOS
        ------------------------------------------------------------------------
        v_cod_motivo_glosa := 599;
        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
            for c_proc in (select ce.ind_regulamentado,
                                  im.ind_rol_nreg,
                                  pc.num_seq_item,
                                  pc.cod_procedimento_cm
                           from ts.procedimento_reembolso_previa pc,
                                ts.pedido_reembolso_previa pr,
                                ts.itens_medicos im,
                                ts.contrato_empresa ce
                           where pc.num_reembolso = p_num_reembolso
                           and   pc.num_reembolso = pr.num_reembolso
                           and   pc.cod_procedimento_cm = im.item_medico
                           and   ce.ind_regulamentado != 'S'
                           and   ce.data_adaptacao is null
                           and ce.cod_ts_contrato = pr.cod_ts_contrato
                           and not exists (select 1 from contrato_beneficio cb
                                       where cb.cod_ts_contrato = ce.cod_ts_contrato
                                       and cb.cod_aditivo = 238))
              loop
                if nvl(c_proc.ind_regulamentado, 'N') = 'N' and nvl(c_proc.ind_rol_nreg, 'N') != 'S' then
                   GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_proc.num_seq_item,c_proc.cod_procedimento_cm,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                   if p_cod_retorno = 9 then return; end if;
                end if;
              end loop;
        end if;

        for c_itens
           in (select distinct cod_procedimento,     num_seq_item,
                               cod_procedimento_cm,  val_aprovado, cod_grupo_estatistico,
                               ind_tipo_composicao,   cod_reembolso, qtd_informado, val_calculado
                from ts.procedimento_reembolso_previa
                  where num_reembolso         = p_num_reembolso
                    and nvl(ind_situacao,'A') = 'A'
                  )
        loop

            v_posicao := 205;

            ------------------------------------------------------------------------
            --- Glosa: 341 - Cobertura Reembolso
            ------------------------------------------------------------------------


            if nvl(c_itens.cod_reembolso,0) = 0 then
                   if v_cod_motivo_reembolso = 'ANE'  then
                       -- n�o gera a glosa
                        null;
                    else
                    v_cod_motivo_glosa := 341;
                    if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa, c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;
                    end if;
                    end if;

            end if;

            ------------------------------------------------------------------------
            --- Glosa: 435 - Servi�o inexistente
            ------------------------------------------------------------------------
            v_cod_motivo_glosa := 435;
            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then

                select count(*) into v_qtd_valida
                  from vwm_procedimento
                    where item_medico = c_itens.cod_procedimento;
/*
                if v_qtd_valida = 0 then

                    select count(*) into v_qtd_valida
                      from itens_servicos
                        where item_servico = c_itens.cod_procedimento;

                    if v_qtd_valida = 0 then
                        select count(*) into v_qtd_valida
                          from itens_servicos
                            where item_servico = c_itens.cod_procedimento_cm;
                    end if;
                end if;
*/

                if v_qtd_valida = 0 then
                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;
                end if;
            end if;



            v_posicao := 220;
            ------------------------------------------------------------------------
            --- Glosa: 616 - servi�o / procedimento n�o informado
            ------------------------------------------------------------------------
            v_cod_motivo_glosa := 616;
            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                if (c_itens.cod_procedimento_cm is null) or (c_itens.cod_procedimento_cm = '00000000') then
                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;
                end if;
            end if;
            ------------------------------------------------------------------------
            --- Glosa: 760 - benefici�rio em car�ncia
            ------------------------------------------------------------------------
            v_cod_motivo_glosa := 760;
            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then

                --------------------------------------------------------------------------------
                --- Recuperar Grupo Estatistico PAI
                --------------------------------------------------------------------------------
                v_cod_grupo_estatistico_pai  := CM_OBTEM_GE_PAI(c_itens.cod_grupo_estatistico);
                if nvl(v_cod_grupo_estatistico_pai,v_item_vazio) = v_item_vazio then
                    v_cod_grupo_estatistico_pai := c_itens.cod_grupo_estatistico;
                end if;


                v_posicao := 250;
                --- verifica car�ncia para realiza��o do procedimento
                crt_utl_base.cr_valida_carencia  ( v_cod_ts,                   v_dt_pedido,
                                                   c_itens.cod_procedimento,   v_cod_grupo_estatistico_pai,
                                                   v_cod_tratamento,           null,
                                                   p_msg_retorno,              p_cod_retorno
                                                   --,v_ind_carater,           v_ind_sexo
                                                   );

                if p_cod_retorno = 9 then return;
                elsif p_cod_retorno <> 0 then

                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;

                end if;

                v_posicao := 260;
                ---- verifica com o grupo estat�stico do procedimento
                if c_itens.cod_grupo_estatistico <> v_cod_grupo_estatistico_pai then
                    crt_utl_base.cr_valida_carencia  ( v_cod_ts,                   v_dt_pedido,
                                                       c_itens.cod_procedimento,   c_itens.cod_grupo_estatistico,
                                                       v_cod_tratamento,           null,
                                                       p_msg_retorno,              p_cod_retorno
                                                       --,v_ind_carater,              v_ind_sexo
                                                     );

                    if p_cod_retorno = 9 then return;
                    elsif p_cod_retorno <> 0 then

                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;

                    end if;

                end if;
            end if;



            v_posicao := 280;
            begin
                ------------------------------------------------------------------------
                --- Recuperar informa��es do item
                ------------------------------------------------------------------------
                select ind_sexo,       ind_limite_idade_ate,    ind_limite_idade,   tipo_item
                  into v_ind_sexo_proc, v_ind_limite_idade_ate, v_ind_limite_idade, v_tipo_item
                    from ts.vwm_procedimento
                        where item_medico = c_itens.cod_procedimento;



                v_posicao := 290;



                v_posicao := 300;
                ------------------------------------------------------------------------
                --- Verificar Sexo
                ------------------------------------------------------------------------
                crt_utl_base.cr_valida_sexo (v_ind_sexo,                c_itens.cod_Procedimento,
                                             v_ind_sexo_proc,           v_cod_tratamento,
                                             p_msg_retorno,             p_cod_retorno);



                if p_cod_retorno = 9 then return;
                elsif p_cod_retorno <> 0 AND p_cod_retorno <> 1 then

                    if p_cod_retorno = 3 then

                        v_posicao := 310;
                        ------------------------------------------------------------------------
                        --- 567 - Sexo benefici�rio incompativel com especialidade/procedimento
                        ------------------------------------------------------------------------
                        v_cod_motivo_glosa := 567;
                        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                            GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                            if p_cod_retorno = 9 then return; end if;
                        end if;
                   /*
                          --- n�o faz mais sentido na pr�via, pois o tratamento n�o � informado
                    else

                        v_posicao := 320;
                        ------------------------------------------------------------------------
                        --- 566 - Tipo de tratamento incompat�vel com sexo do benefici�rio
                        ------------------------------------------------------------------------
                        v_cod_motivo_glosa := 566;
                        if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                            GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                            if p_cod_retorno = 9 then return; end if;
                        end if;
                                */

                    end if;
                end if;

                v_posicao := 330;
                ------------------------------------------------------------------------
                --- verifica limites de idade
                ------------------------------------------------------------------------
                if v_data_nascimento is not null then
                    crt_utl_base.cr_valida_idade (v_data_nascimento,          v_ind_limite_idade,
                                                  v_ind_limite_idade_ate,     c_itens.cod_procedimento,
                                                  p_msg_retorno,              p_cod_retorno);

                    if p_cod_retorno in(1,9) then return;
                    elsif p_cod_retorno <> 0 then

                        if p_cod_retorno = 2 then
                            v_posicao := 340;
                            ------------------------------------------------------------------------
                            --- 554 - Idade inferior ao permitido para o procedimento
                            ------------------------------------------------------------------------
                            v_cod_motivo_glosa := 554;
                            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                                if p_cod_retorno = 9 then return; end if;
                            end if;

                        else
                            v_posicao := 350;
                            ------------------------------------------------------------------------
                            --- 555 - Idade superior ao permitido para o procedimento
                            ------------------------------------------------------------------------
                            v_cod_motivo_glosa := 555;
                            if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                                GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                                if p_cod_retorno = 9 then return; end if;
                            end if;
                        end if;
                    end if;
                end if;

                v_posicao := 360;
                ------------------------------------------------------------------------
                --- 600 - Verifica se procedimento consta no ROL da ANS
                ------------------------------------------------------------------------
                v_cod_motivo_glosa := 600;
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                    if v_tipo_item = 'I' then
                        begin
                            select nvl(ind_rol_procedimentos,'N') into v_ind_rol_procedimentos
                              from itens_medicos
                                where item_medico = c_itens.cod_procedimento;
                        exception
                            when no_data_found then
                                v_ind_rol_procedimentos := 'N';
                        end;

                        if v_ind_rol_procedimentos = 'N' then
                            GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                            if p_cod_retorno = 9 then return; end if;
                        end if;
                    end if;
                end if;


                ------------------------------------------------------------------------
                --- Glosa: 586 - Procedimento sem cobertura pela operadora
                ------------------------------------------------------------------------
                v_cod_motivo_glosa := 586;
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then

                         begin
                       select nvl(ind_cobertura,'N') into v_ind_cobertura_procedimento
                           from itens_medicos
                                where item_medico = c_itens.cod_procedimento;
                    exception
                            when no_data_found then
                                v_ind_cobertura_procedimento := 'N';
                    end;

                    if nvl(v_ind_cobertura_procedimento,'N') = 'N' and nvl(v_ind_rol_procedimentos,'N') = 'N'  then
                        GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                        if p_cod_retorno = 9 then return; end if;
                   end if;
                end if;



            exception
                when no_data_found then
                    NULL;
            end;

            v_posicao := 400;

            v_posicao := 404;
            ------------------------------------------------------------------------
            --- Verificar as glosas realizadas para as fun��es do procedimento
            ------------------------------------------------------------------------
            v_node_list := xslprocessor.selectNodes(xmldom.makeNode(v_doc),'/PROCEDIMENTO/FUNCAO');
            FOR i IN 0 .. xmldom.getLength(v_node_list) - 1 LOOP
                v_node := xmldom.item(v_node_list, i);
                v_cod_motivo_glosa := NVL(xslprocessor.valueOf(v_node,'COD_GLOSA'),0);
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                    GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                    if p_cod_retorno = 9 then return; end if;
                end if;
            end loop;
/*
            rsMemoriaPrevia.num_reembolso := null;
            rsMemoriaPrevia.num_seq_item  := null;

            begin
                select *
                  into rsMemoriaPrevia
                  from memoria_previa_detalhe
                 where num_seq_item  = c_itens.num_seq_item
                   and num_reembolso = p_num_reembolso;
            exception
                when others then
                    rsMemoriaPrevia.num_seq_item := null;
                    rsMemoriaPrevia.num_reembolso := null;
            end;

            if  rsMemoriaPrevia.num_seq_item  = c_itens.num_seq_item
            and rsMemoriaPrevia.num_reembolso = p_num_reembolso then
                --
                v_val_calculado_aux := null;
                ------------------------------------------------------------------------
                --- Verificar as glosas de saldo do reembolso
                ------------------------------------------------------------------------
                v_cod_motivo_glosa := 1564;
                --
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then
                    if nvl(rsMemoriaPrevia.val_beneficio_anual,0) > 0 then
                        if nvl(rsMemoriaPrevia.saldo_beneficio_anual,0) < c_itens.val_calculado then
                            --
                            GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                            --
                            v_val_calculado_aux := c_itens.val_calculado - ( c_itens.val_calculado - nvl(rsMemoriaPrevia.saldo_beneficio_anual,0) );
                            if v_val_calculado_aux < 0 then
                                v_val_calculado_aux := 0;
                            end if;
                        end if;
                    end if;
                end if;

                ------------------------------------------------------------------------
                --- Verificar as glosas de saldo do sessoes
                ------------------------------------------------------------------------
                v_cod_motivo_glosa := 1565;
                --
                if VerificaGlosa(v_cod_motivo_glosa, v_dt_pedido) = 0 then

                    if nvl(rsMemoriaPrevia.qtd_sessoes_anual,0) > 0 then
                        if nvl(rsMemoriaPrevia.saldo_sessoes_anual,0) < c_itens.qtd_informado then
                            --
                            GravaGlosa(p_num_reembolso,v_cod_motivo_glosa,c_itens.num_seq_item,c_itens.cod_procedimento,SYSDATE,p_cod_usuario,null,p_cod_retorno,p_msg_retorno);
                            --
                            v_val_calculado_aux := ( c_itens.val_calculado / c_itens.qtd_informado ) * (c_itens.qtd_informado - (c_itens.qtd_informado - rsMemoriaPrevia.saldo_sessoes_anual));
                            if v_val_calculado_aux < 0 then
                                v_val_calculado_aux := 0;
                            end if;
                        end if;
                    end if;
                end if;
                --
                if v_val_calculado_aux is not null then
                    -- atualiza o procedimento com o novo valor calculado
                    begin
                        update procedimento_reembolso_previa
                           set val_calculado = v_val_calculado_aux
                         where num_seq_item  = c_itens.num_seq_item
                           and num_reembolso = p_num_reembolso;
                    exception
                        when others then
                            null;
                    end;
                end if;
                --
            end if;
*/
            xmldom.freeDocument(v_doc);

        end loop;
        v_posicao := 480;
        p_cod_retorno := 0;
        p_msg_retorno := '';

    exception
    when others then
        p_cod_retorno := 9;
        p_msg_retorno := 'RB_PREVIA_REEMBOLSO::ValidaPrevia -> ' || sqlerrm || ' (Posi��o: ' || v_posicao || ')' ;

    end;
    --
    --
    procedure IncluirPrevia  ( p_xml_dados                in clob
                             , p_num_reembolso            out number
                             , p_cod_retorno              out number
                             , p_msg_retorno              out varchar2)
    is
    --Declara��es:
    v_posicao                       number;
    v_item_vazio                    varchar2(3) := '���';
    v_doc                           xmldom.DOMDocument;
    --v_doc_procedimento              xmldom.DOMDocument;
    vXMLAnexo                       clob;
    p_xml_protocolo_ans             clob;
    --previa
    v_num_reembolso                 pedido_reembolso_previa.num_reembolso%type;
    --
    v_cod_ts                        pedido_reembolso_previa.cod_ts%type;
    v_num_associado                 pedido_reembolso_previa.num_associado%type;
    v_nome_associado                pedido_reembolso_previa.nome_associado%type;
    v_data_nascimento               pedido_reembolso_previa.data_nascimento%type;
    v_cod_rede                      pedido_reembolso_previa.cod_rede%type;
    v_cod_plano                     pedido_reembolso_previa.cod_plano%type;
    v_cod_ts_contrato               pedido_reembolso_previa.cod_ts_contrato%type;
    v_cod_operadora                 pedido_reembolso_previa.cod_operadora_contrato%type;
    v_cod_marca                     pedido_reembolso_previa.cod_marca_contrato%type;
    v_txt_observacao                pedido_reembolso_previa.txt_observacao%type;
    v_cod_inspetoria_ts             pedido_reembolso_previa.cod_inspetoria_ts_contrato%type;
    v_cod_inspetoria_ts_abertura    pedido_reembolso_previa.cod_inspetoria_ts_abertura%type;
    v_qtd_idade                     pedido_reembolso_previa.qtd_idade%type;
    v_ind_sexo                      pedido_reembolso_previa.ind_sexo%type;
    v_tipo_associado                pedido_reembolso_previa.tipo_associado%type;
    v_cod_ts_tit                    pedido_reembolso_previa.cod_ts_tit%type;
    v_cod_entidade_ts_tit           pedido_reembolso_previa.cod_entidade_ts_tit%type;
    v_num_contrato                  pedido_reembolso_previa.num_contrato%type;
    v_dt_sit                        pedido_reembolso_previa.dt_sit%type;
    v_dt_inclusao                   pedido_reembolso_previa.dt_inclusao%type;
    v_ind_tipo_reembolso            pedido_reembolso_previa.ind_tipo_reembolso%type;
    v_cod_origem                    pedido_reembolso_previa.cod_origem%type;
    v_txt_ramal_fax                 pedido_reembolso_previa.txt_ramal_fax%type;
    v_ind_tipo_emissao              pedido_reembolso_previa.ind_tipo_emissao%type;
    v_tipo_pessoa_contrato          pedido_reembolso_previa.tipo_pessoa_contrato%type;
    v_ind_origem_associado          pedido_reembolso_previa.ind_origem_associado%type;
    v_cod_motivo_reembolso          motivo_reembolso.cod_motivo_reembolso%type;
    v_num_titular                   pedido_reembolso_previa.num_titular%type;
    v_nome_titular                  pedido_reembolso_previa.nome_titular%type;
    v_nome_contrato                 pedido_reembolso_previa.nome_contrato%type;
    v_qtd_dias_reemb_uteis          pedido_reembolso_previa.qtd_dias_reemb_uteis%type;
    v_dt_provavel_reembolso         pedido_reembolso_previa.dt_provavel_reembolso%type;
    v_qtd_dias_reembolso            pedido_reembolso_previa.qtd_dias_reembolso%type;
    v_ind_regulamentado             pedido_reembolso_previa.ind_regulamentado%type;

    v_ddd_residencial               pedido_reembolso_previa.ddd_residencial%type;
    v_tel_residencial               pedido_reembolso_previa.tel_residencial%type;
    v_ddd_comercial                 pedido_reembolso_previa.ddd_comercial%type;
    v_tel_comercial                 pedido_reembolso_previa.tel_comercial%type;
    v_ddd_celular                   pedido_reembolso_previa.ddd_celular%type;
    v_tel_celular                   pedido_reembolso_previa.tel_celular%type;
    v_txt_ddd_fax                   pedido_reembolso_previa.txt_ddd_fax%type;
    v_txt_num_fax                   pedido_reembolso_previa.txt_num_fax%type;
    v_txt_email                     pedido_reembolso_previa.txt_email%type;
    --
    v_cod_usuario                   usuario.cod_usuario%type;
    v_nome_arquivo                  reembolso_previa_anexo.nom_arq_anexo%type;
    --
    v_qtd_anexo                     number;
    --
    v_val_reembolsado               procedimento_reembolso_previa.val_reembolsado%type;
    v_val_calculado                 procedimento_reembolso_previa.val_calculado%type;
    v_tipo_ocorrecia                number;
    v_ind_situacao                  number;
    xmlProcedimento                 clob;
    v_dt_ini_vigencia               date;
    --
    v_num_reembolso_ans             pedido_reembolso_previa.num_reembolso_ans%type;
    p_num_reembolso_ans             pedido_reembolso_previa.num_reembolso_ans%type;
    --
    cursor cur_procedimento          (pXML in sys.XMLType) is
    select extractValue( VALUE(T) , '//COD_RETORNO') COD_RETORNO
          , extractValue( VALUE(T) , '//CODIGO') ITEM_MEDICO
          , extractValue( VALUE(T) , '//CODIGO_PARA') COD_PROCEDIMENTO
          , extractValue( VALUE(T) , '//DESCRICAO') DESCRICAO
          , extractValue( VALUE(T) , '//QTD_INFORMADO') QTD_INFORMADO
          , extractValue( VALUE(T) , '//FUNCAO/IND_CIRURGIA') IND_CIRURGIA
          , extractValue( VALUE(T) , '//FUNCAO/COD_FUNCAO') COD_FUNCAO
          , extractValue( VALUE(T) , '//FUNCAO/NOME_FUNCAO') NOME_FUNCAO
          , extractValue( VALUE(T) , '//FUNCAO/PERC_FUNCAO') PERC_FUNCAO
          , extractValue( VALUE(T) , '//FUNCAO/COD_GRUPO_ESTATISTICO') COD_GRUPO_ESTATISTICO
          , extractValue( VALUE(T) , '//FUNCAO/PCT_CIRU_MULTIPLA') PCT_CIRU_MULTIPLA
          , ts_numero_web(extractValue( VALUE(T) , '//FUNCAO/VAL_CALCULADO'),2) VAL_CALCULADO
          , extractValue( VALUE(T) , '//FUNCAO/IND_TIPO_COMPOSICAO') IND_TIPO_COMPOSICAO
          , extractValue( VALUE(T) , '//FUNCAO/COD_REEMBOLSO') COD_REEMBOLSO
          , ts_numero_web(extractValue( VALUE(T) , '//FUNCAO/VAL_COTACAO_RB'),4) VAL_COTACAO_RB
          , extractValue( VALUE(T) , '//FUNCAO/COD_PORTE_RB') COD_PORTE_RB
          , extractValue( VALUE(T) , '//FUNCAO/SIGLA_TABELA_RB') SIGLA_TABELA_RB
          , extractValue( VALUE(T) , '//FUNCAO/SIGLA_TABELA_TAXAS') SIGLA_TABELA_TAXAS
          , extractValue( VALUE(T) , '//FUNCAO/QTD_VEZES_TABELA') QTD_VEZES_TABELA
          , extractValue( VALUE(T) , '//FUNCAO/PCT_RECIBO') PCT_RECIBO
          , ts_numero_web(extractValue( VALUE(T) , '//FUNCAO/VAL_COTACAO_TAXA'),4) VAL_COTACAO_TAXA
          , ts_numero_web(extractValue( VALUE(T) , '//FUNCAO/VAL_LIMITE'),2) VAL_LIMITE
          , ts_numero_web(extractValue( VALUE(T) , '//FUNCAO/VAL_FIXO'),2) VAL_FIXO
          , extractValue( VALUE(T) , '//FUNCAO/QTD_PRAZO_DIAS') QTD_PRAZO_DIAS
          , extractValue( VALUE(T) , '//FUNCAO/SIGLA_MOEDA') SIGLA_MOEDA
          , extractValue( VALUE(T) , '//FUNCAO/COD_CONCESSAO') COD_CONCESSAO
          , extractValue( VALUE(T) , '//FUNCAO/NOME_COBERTURA') NOME_COBERTURA
          , extractValue( VALUE(T) , '//FUNCAO/GRUPO_BENEFICIO') GRUPO_BENEFICIO
          , extractValue( VALUE(T) , '//FUNCAO/TXT_MEMORIA_CALCULO') TXT_MEMORIA_CALCULO
    from   table ( xmlsequence ( extract(pXML,'/PROCEDIMENTO') ) ) T;
    --
    V_XML                           sys.XMLType;
    --
    v_num_seq_item                  number;
    --
    begin
        --
        --In�cio
        v_posicao := 0;
        p_cod_retorno := 0;
        p_msg_retorno := '';

        --
        v_posicao := 1;
        --
        --Ler informa��es do XML
        ts_cria_doc_xml(p_xml_dados, v_doc, p_cod_retorno, p_msg_retorno);
        if p_cod_retorno <> 0 then
           p_msg_retorno := 'RB_PREVIA_REEMBOLSO.IncluirPrevia - '|| v_posicao || ': ' || p_msg_retorno;
           return;
        end if;
        --
        --Recuperar informa��es do XML
        --
        v_posicao := 2;
        --Associado
        v_num_associado         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_ASSOCIADO');
        v_nome_associado        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_ASSOCIADO');
        v_cod_ts                := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS');
        v_cod_ts_tit            := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS_TIT');
        v_cod_entidade_ts_tit   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_ENTIDADE_TS_TIT');
        v_num_titular           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_TITULAR');
        v_nome_titular          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_TITULAR');

        if TS_UTIL.IsDate(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_NASCIMENTO'), 'DD/MM/YYYY') then
            v_data_nascimento   := to_date(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_NASCIMENTO'), 'DD/MM/YYYY');
        end if;

        v_qtd_idade             := TO_NUMBER(ts_calcula_idade(v_data_nascimento,TRUNC(SYSDATE),'A'));
        if v_qtd_idade = -1 THEN
            v_qtd_idade := null;
        end if;
        --
        v_cod_rede              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_REDE');
        v_cod_plano             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_PLANO');
        v_cod_ts_contrato       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS_CONTRATO');
        v_num_contrato          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_CONTRATO');
        v_nome_contrato         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_CONTRATO');
        v_cod_inspetoria_ts     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_INSPETORIA_TS');
        v_cod_inspetoria_ts_abertura     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_INSPETORIA_TS_ABERTURA');
        v_cod_operadora         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_OPERADORA');
        v_cod_marca             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_MARCA');
        v_cod_motivo_reembolso  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_MOTIVO_REEMBOLSO');

        v_ind_sexo              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_SEXO');
        v_tipo_associado        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TIPO_ASSOCIADO');
        --
        v_posicao := 40;
        v_ind_tipo_reembolso    := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_TIPO_REEMBOLSO');
        v_cod_origem            := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_ORIGEM');
        v_ind_tipo_emissao      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_TIPO_EMISSAO');

        v_posicao := 42;
        v_txt_observacao        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TXT_OBSERVACAO');
        v_cod_usuario           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_USUARIO');
        v_txt_email             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TXT_EMAIL');

        v_tipo_pessoa_contrato  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TIPO_PESSOA_CONTRATO');
        v_ind_origem_associado  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_ORIGEM_ASSOCIADO');

        v_qtd_dias_reemb_uteis  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/QTD_DIAS_REEMB_UTEIS');
        --
        if TS_UTIL.IsDate(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DT_PROVAVEL_REEMBOLSO'), 'DD/MM/YYYY') then
            v_dt_provavel_reembolso   := to_date(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DT_PROVAVEL_REEMBOLSO'), 'DD/MM/YYYY');
        end if;
        --
        v_qtd_dias_reembolso    := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/QTD_DIAS_REEMBOLSO');

        v_ddd_residencial       := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_RESIDENCIAL');
        v_tel_residencial       := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_RESIDENCIAL'),'-','');
        v_ddd_comercial         := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_COMERCIAL');
        v_tel_comercial         := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_COMERCIAL'),'-','');
        v_ddd_celular           := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_CELULAR');
        v_tel_celular           := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_CELULAR'),'-','');
        v_txt_ddd_fax           := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TXT_DDD_FAX');
        v_txt_num_fax           := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TXT_NUM_FAX'),'-','');

        v_ind_regulamentado     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_REGULAMENTADO');

        v_posicao := 2;

         --------------------------------------------------
          --  Insclus�o do protocolo do reembolso
         --------------------------------------------------

          getProtocolo( p_cod_operadora_atd => v_cod_operadora
                   , p_cod_usuario_atd        => v_cod_usuario
                   , p_cod_ts_atd             => v_cod_ts
                   , p_cod_retorno    => p_cod_retorno
                   , p_msg_retorno    => p_msg_retorno
                   , p_num_protocolo  => p_num_reembolso_ans
                   ) ;
          --
        SELECT ts_sinistro_seq.NextVal
          INTO p_num_reembolso
             FROM DUAL;

            v_dt_inclusao := SYSDATE;

         v_posicao := 66;

         if v_cod_ts is null and v_num_associado is null then
             p_cod_retorno := 9;
             p_msg_retorno := 'Benefici�rio � obrigat�rio.';
             rollback;
             return;
         end if;
         --
         if v_data_nascimento is null then
            p_cod_retorno := 9;
            p_msg_retorno := 'Data de nascimento do Benefici�rio inv�lida'||v_data_nascimento;
           return;
         end if;
         --
         if v_cod_origem is null then
             p_cod_retorno := 9;
             p_msg_retorno := 'Origem da solicita��o � obrigat�ria.';
             rollback;
             return;
         end if;
         --
         if v_ind_tipo_reembolso is null then
             p_cod_retorno := 9;
             p_msg_retorno := 'Modalidade do reembolso � obrigat�ria.';
             rollback;
             return;
         end if;
         --
         if v_cod_motivo_reembolso is null then
             p_cod_retorno := 9;
             p_msg_retorno := 'Motivo do reembolso � obrigat�rio.';
             rollback;
             return;
         end if;
         --
         if v_ind_origem_associado = 'BD' and v_cod_ts_contrato is not null then
             begin
                 select max(dt_ini_cobranca)
                 into v_dt_ini_vigencia
                 from contrato_cobranca
                 where cod_ts_contrato    = v_cod_ts_contrato
                   and dt_ini_cobranca   <= SYSDATE;
             exception
                when others then
                    v_dt_ini_vigencia := null;
             end;
             --
             if v_dt_ini_vigencia is null and v_cod_motivo_reembolso <> 'ANE' then
                p_cod_retorno := 9;
                p_msg_retorno := 'N�o � poss�vel gravar a pr�via de reembolso. Benefici�rio n�o tem direito a reembolso.';
                rollback;
                return;
             end if;
         end if;
         --
         if v_ind_tipo_emissao <> 'I'  then
            if v_ind_tipo_emissao = 'E' and v_txt_email is null then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'E-mail para envio da pr�via � obrigat�rio.';
                 rollback;
                 return;
            end if;
            --
            if v_ind_tipo_emissao = 'F' and ( v_txt_num_fax is null or v_txt_ddd_fax is null ) then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'N�mero de Fax para envio da pr�via n�o informado ou inv�lido.';
                 rollback;
                 return;
            end if;
         end if;
         --
         begin
             insert into pedido_reembolso_previa (
                            num_reembolso
                          , num_associado
                          , nome_associado
                          , cod_ts
                          , cod_ts_tit
                          , cod_entidade_ts_tit
                          , data_nascimento
                          , qtd_idade
                          , cod_rede
                          , cod_plano
                          , cod_ts_contrato
                          , cod_inspetoria_ts_contrato
                          , cod_inspetoria_ts_abertura
                          , cod_operadora_contrato
                          , cod_marca_contrato
                          , ind_sexo
                          , tipo_associado
                          , ind_situacao
                          , ind_tipo_reembolso
                          , cod_origem
                          , txt_observacao
                          , txt_num_fax
                          , txt_ddd_fax
                          , txt_ramal_fax
                          , txt_email
                          , cod_usuario_inclusao
                          , dt_inclusao
                          , cod_usuario_sit
                          , dt_sit
                          , ind_tipo_emissao
                          , cod_motivo_reembolso
                          , tipo_pessoa_contrato
                          , ind_origem_associado
                          , num_contrato
                          , nome_contrato
                          , num_titular
                          , nome_titular
                          , qtd_dias_reemb_uteis
                          , dt_provavel_reembolso
                          , qtd_dias_reembolso
                          , ind_regulamentado
                          , ddd_celular
                          , tel_celular
                          , ddd_residencial
                          , tel_residencial
                          , ddd_comercial
                          , tel_comercial
                          , num_reembolso_ans
                         )
                   VALUES (
                            p_num_reembolso
                          , v_num_associado
                          , v_nome_associado
                          , v_cod_ts
                          , v_cod_ts_tit
                          , v_cod_entidade_ts_tit
                          , v_data_nascimento
                          , v_qtd_idade
                          , v_cod_rede
                          , v_cod_plano
                          , v_cod_ts_contrato
                          , v_cod_inspetoria_ts
                          , v_cod_inspetoria_ts_abertura
                          , v_cod_operadora
                          , v_cod_marca
                          , v_ind_sexo
                          , v_tipo_associado
                          , 5 -- situa��o pr�via cadastrada
                          , v_ind_tipo_reembolso
                          , v_cod_origem
                          , v_txt_observacao
                          , v_txt_num_fax
                          , v_txt_ddd_fax
                          , v_txt_ramal_fax
                          , v_txt_email
                          , v_cod_usuario
                          , v_dt_inclusao
                          , v_cod_usuario
                          , sysdate
                          , v_ind_tipo_emissao
                          , v_cod_motivo_reembolso
                          , v_tipo_pessoa_contrato
                          , v_ind_origem_associado
                          , v_num_contrato
                          , v_nome_contrato
                          , v_num_titular
                          , v_nome_titular
                          , v_qtd_dias_reemb_uteis
                          , v_dt_provavel_reembolso
                          , v_qtd_dias_reembolso
                          , v_ind_regulamentado
                          , v_ddd_celular
                          , v_tel_celular
                          , v_ddd_residencial
                          , v_tel_residencial
                          , v_ddd_comercial
                          , v_tel_comercial
                          , p_num_reembolso_ans
                         );
         exception
             when others then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'Erro ao incluir pr�via de reembolso: ' || sqlerrm;
                 rollback;
                 return;
         end;

         --Gerar Ocorr�ncia de INCLUS�O
         GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,1,v_txt_observacao,null,v_cod_usuario,p_cod_retorno,p_msg_retorno);
         if p_cod_retorno <> 0 then
             rollback;
             return;
         end if;

        --
        if v_cod_inspetoria_ts_abertura is null then
             p_cod_retorno := 9;
             p_msg_retorno := 'Filial / Unidade da abertura � obrigat�rio.';
             rollback;
             return;
        end if;


        v_posicao := 70;
        --gravar os anexos
        v_qtd_anexo := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/QTD_ANEXO');
        --
        /*if v_ind_tipo_reembolso != 1 and v_qtd_anexo < 1 then
             p_cod_retorno := 9;
             p_msg_retorno := 'Para as modalidades diferente de Consultas � obrigat�rio pelo menos um anexo.';
             rollback;
             return;
        end if;*/
        --
        for i in 1..v_qtd_anexo loop
            --
            if trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/NOM_ARQ_ANEXO_'||i)) is not null then
                --
                v_nome_arquivo := trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/NOM_ARQ_ANEXO_'||i));
                --
                -----------------------------------
                -- INCLUIR ANEXOS
                -----------------------------------
                vXMLAnexo := '<?xml version="1.0" encoding="ISO-8859-1" ?>' ||
                             '<ANEXO_PREVIA>'||
                             '<NUM_REEMBOLSO>'||p_num_reembolso||'</NUM_REEMBOLSO>'||
                             '<TXT_DESCRICAO>'||ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/TXT_DESCRICAO_'||i) ||'</TXT_DESCRICAO>'||
                             '<IND_NOTA_ORIGINAL>'||ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/IND_NOTA_ORIGINAL_'||i) ||'</IND_NOTA_ORIGINAL>'||
                             '<NOM_ARQ_ANEXO>'||v_nome_arquivo||'</NOM_ARQ_ANEXO>'||
                             '<COD_USUARIO>'||v_cod_usuario||'</COD_USUARIO>'||
                             '</ANEXO_PREVIA>';
                --
                GravaAnexo ( vXMLAnexo, p_cod_retorno, p_msg_retorno);
                if p_cod_retorno <> 0 then
                    rollback;
                    return;
                end if;
                --
            end if;
            --
        end loop;
        --
        -- se for consulta adiciona o procedimento de consulta
        if v_ind_tipo_reembolso = 1 then

        if nvl(v_ind_origem_associado,'BD') = 'WS' then
            rb_previa_reembolso.RetornaProcedimentoCAM ( '10101012'
                                                       , v_num_associado
                                                       , v_num_contrato
                                                       , v_num_titular
                                                       , v_cod_plano
                                                       , ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_NASCIMENTO')
                                                       , v_ind_sexo
                                                       , to_char( sysdate, 'dd/mm/yyyy' )
                                                       , v_ind_tipo_reembolso
                                                       , 1
                                                       , 'N'
                                                       , null
                                                       , null
                                                       , v_cod_motivo_reembolso
                                                       , null
                                                       , null
                                                       , v_cod_inspetoria_ts_abertura
                                                       , v_cod_operadora
                                                       , v_ind_regulamentado
                                                       , xmlProcedimento );
        else
            rb_previa_reembolso.RetornaProcedimento    ( '10101012'
                                                       , v_num_associado
                                                       , v_cod_ts_contrato
                                                       , v_num_contrato
                                                       , v_cod_ts_tit
                                                       , v_num_titular
                                                       , v_cod_plano
                                                       , ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_NASCIMENTO')
                                                       , v_ind_sexo
                                                       , to_char( sysdate, 'dd/mm/yyyy' )
                                                       , v_ind_tipo_reembolso
                                                       , 1
                                                       , 'N'
                                                       , null
                                                       , null
                                                       , v_cod_motivo_reembolso
                                                       , null
                                                       , null
                                                       , v_cod_inspetoria_ts_abertura
                                                       , v_cod_operadora
                                                       , 'N'
                                                       , xmlProcedimento );
        end if;
            --
            -- Retira o n� de xml da memoria de calculo para n�o causar problemas no extractvalue
            select REGEXP_REPLACE(xmlProcedimento,'<XML_MEMORIA_CALCULO>.*</XML_MEMORIA_CALCULO>','')
              into xmlProcedimento
              from dual;
            --
            V_XML := XMLType.createXML( xmlProcedimento );
            --
            v_num_seq_item := 1;
            --
            for rsPreviaProcedimento in cur_procedimento (V_XML) loop
                 --
                 if rsPreviaProcedimento.cod_retorno = 0 then
                     --
                     begin
                        insert into procedimento_reembolso_previa
                            (  num_reembolso
                            ,  num_seq_item
                            ,  cod_procedimento_cm
                            ,  cod_procedimento
                            ,  cod_grupo_estatistico
                            ,  ind_funcao
                            ,  ind_via
                            ,  pct_cirurgia_multipla
                            ,  qtd_informado
                            ,  val_calculado
                            ,  val_reembolsado
                            ,  ind_tipo_composicao
                            ,  cod_reembolso
                            ,  val_cotacao_rb
                            ,  cod_porte_rb
                            ,  sigla_tabela_rb
                            ,  dt_atu
                            ,  cod_usuario_atu
                            ,  sigla_tabela_taxas
                            ,  ind_situacao
                            ,  qtd_vezes_tabela
                            ,  pct_recibo
                            ,  val_cotacao_taxa
                            ,  qtd_prazo_dias
                            ,  sigla_moeda
                            ,  ind_principal
                            ,  cod_concessao
                            ,  ind_cirurgia
                            ,  perc_funcao
                            ,  ind_situacao_funcao
                            ,  val_fixo
                            ,  val_limite
                            ,  grupo_beneficio
                            ,  num_reembolso_ans    )
                          VALUES
                            ( p_num_reembolso
                            , v_num_seq_item
                            , rsPreviaProcedimento.item_medico
                            , rsPreviaProcedimento.cod_procedimento
                            , rsPreviaProcedimento.cod_grupo_estatistico
                            , rsPreviaProcedimento.cod_funcao
                            , null
                            , rsPreviaProcedimento.pct_ciru_multipla
                            , rsPreviaProcedimento.qtd_informado
                            , rsPreviaProcedimento.val_calculado
                            , rsPreviaProcedimento.val_calculado
                            , rsPreviaProcedimento.ind_tipo_composicao
                            , rsPreviaProcedimento.cod_reembolso
                            , rsPreviaProcedimento.val_cotacao_rb
                            , rsPreviaProcedimento.cod_porte_rb
                            , rsPreviaProcedimento.sigla_tabela_rb
                            , sysdate
                            , v_cod_usuario
                            , rsPreviaProcedimento.sigla_tabela_taxas
                            , null
                            , rsPreviaProcedimento.qtd_vezes_tabela
                            , rsPreviaProcedimento.pct_recibo
                            , rsPreviaProcedimento.val_cotacao_taxa
                            , rsPreviaProcedimento.qtd_prazo_dias
                            , rsPreviaProcedimento.sigla_moeda
                            , 'N'
                            , rsPreviaProcedimento.cod_concessao
                            , rsPreviaProcedimento.ind_cirurgia
                            , rsPreviaProcedimento.perc_funcao
                            , 'A'
                            , rsPreviaProcedimento.val_fixo
                            , rsPreviaProcedimento.val_limite
                            , rsPreviaProcedimento.grupo_beneficio
                            , p_num_reembolso_ans );

                    exception
                        when dup_val_on_index then
                            p_cod_retorno := 9;
                            p_msg_retorno := 'Erro ao incluir pr�via de reembolso. ' || ts.ts_util.msgerro || ' - ' || sqlerrm;
                            rollback;
                            return;
                        when others then
                            p_cod_retorno := 9;
                            p_msg_retorno := 'Erro ao incluir pr�via de reembolso. ' || ts.ts_util.msgerro || ' - '  || sqlerrm;
                            rollback;
                            return;
                    end;

                    if rsPreviaProcedimento.txt_memoria_calculo is not null then
                        begin
                            insert into ts.memoria_reembolso_previa
                            (num_reembolso, num_seq_item, txt_memoria)
                            VALUES
                            (p_num_reembolso, v_num_seq_item, rsPreviaProcedimento.txt_memoria_calculo);
                        exception
                            when dup_val_on_index then
                                p_cod_retorno := 9;
                                p_msg_retorno := 'Erro ao incluir pr�via de reembolso. ' || sqlerrm;
                                rollback;
                                return;
                            when others then
                                p_cod_retorno := 9;
                                p_msg_retorno := 'Erro ao incluir pr�via de reembolso. ' || sqlerrm;
                                rollback;
                                return;
                        end;
                    end if;

                    v_num_seq_item := v_num_seq_item + 1;
                 else
                     p_cod_retorno := 9;
                     p_msg_retorno := 'Erro ao incluir pr�via de reembolso. Procedimento de consulta n�o encontrado.';
                     rollback;
                     return;
                 end if;
             end loop;

        end if;
        --


        AUT_CTX_BENEFICIARIO.carga_por_uk ( p_cod_retorno
                                          , p_msg_retorno
                                          , v_num_associado
                                          , null
                                          );
        if p_cod_retorno <> 0 then
             rollback;
             return;
        end if;

        rb_previa_reembolso.ValidaPrevia ( p_num_reembolso
                                          , v_cod_usuario
                                          , p_cod_retorno
                                          , p_msg_retorno
                                          );

         if p_cod_retorno <> 0 then
             rollback;
             return;
         end if;

         rb_verifica_regra ( p_num_reembolso
                           , v_cod_usuario
                           , p_cod_retorno
                           , p_msg_retorno
                           );

         if p_cod_retorno <> 0 then
             rollback;
             return;
         end if;


/*         if v_ind_tipo_reembolso = 1 then

          -- ######################### valida��o deixada em stand by ####################################

          -- valida se sobrou alguma glosa n�o liberada na cabe�a
          -- ou se n�o h� pelo menos um procedimento liberado para finalizar
          -- valida finaliza��o se for consulta e inclus�o
          /*select SUM(val_reembolsado)
                  , SUM(val_calculado)
               into
                    v_val_reembolsado
                  , v_val_calculado
               from procedimento_reembolso_previa
              where num_reembolso = p_num_reembolso;

             rb_previa_reembolso.valida_finalizacao ( p_cod_retorno
                                                    , p_msg_retorno
                                                    , p_num_reembolso
                                                    , v_cod_usuario
                                                    );
              -- caso seja 99 existe grupo pentende
              -- ent�o n�o executar a��o, apenas cadastar a glosa
              if p_cod_retorno <> 99 then
                  if p_cod_retorno = 9 then
                      -- se cod_retorno = 9 recusar o pedido
                      -- existe glosa pendente ou valor � 0
                      v_tipo_ocorrecia                         := 5;
                      v_ind_situacao                    := 4;
                  elsif p_cod_retorno = 0 then
                      -- se for 0 finalizar e aprovar a pr�via
                      v_tipo_ocorrecia                       := 3;
                      v_ind_situacao                  := 2;
                  end if;
                   --
                   p_cod_retorno := 0;
                   begin
                     UPDATE pedido_reembolso_previa SET
                            dt_deferimento              = sysdate
                          , cod_usuario_deferimento     = v_cod_usuario
                          , dt_indeferimento            = sysdate
                          , cod_usuario_indeferimento   = v_cod_usuario
                          , ind_situacao                = v_ind_situacao
                          , val_reembolsado             = v_val_reembolsado
                          , val_calculado               = v_val_calculado
                     where  num_reembolso               = p_num_reembolso;
                   exception
                       when others then
                           p_cod_retorno := 9;
                           p_msg_retorno := 'Erro ao incluir a pr�via de reembolso: ' || sqlerrm;
                           rollback;
                           return;
                   end;
                  --
                  if p_cod_retorno <> 0 then
                      rollback;
                      return;
                  end if;

                  GeraOcorrencia( p_num_reembolso
                                , v_tipo_ocorrecia
                                , v_txt_observacao
                                , null
                                , v_cod_usuario
                                , p_cod_retorno
                                , p_msg_retorno);
              end if;
         end if;
*/
        xmldom.freeDocument(v_doc);

        p_cod_retorno := 0;
        p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso_ans || ' inclu�da';

        commit;

    exception
    when others then

        TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 00, 'ERRO', p_xml_dados || chr(13) || ts.ts_util.msgerro, 'IncluirPrevia - XML' );
        if nvl(v_posicao,0) > 1 then xmldom.freeDocument(v_doc);
        end if;
        rollback;

        p_cod_retorno := 9;
        p_msg_retorno := 'RB_PREVIA_REEMBOLSO::IncluirPrevia -> ' || sqlerrm || ' - ' || ts.ts_util.msgerro || ' (Posi��o: ' || v_posicao || ')' ;
    end;
    --
    procedure gravaMotivoIndef (  p_cod_retorno       out pls_integer
                                , p_msg_retorno       out varchar2
                                , p_xml_dados         in  clob
                                , p_num_reembolso     in  varchar2
                                , p_num_reembolso_ans in  varchar2
                                , p_cod_usuario       in  varchar2
                                , p_ind_tipo_emissao  in  varchar2
                                , p_txt_email         in  varchar2
                                )
    is
        ---
        cursor cur_motivo      (pXML in sys.XMLType)
        is
        select extractValue( VALUE(T) , '//COD_MOTIVO_INDEF') COD_MOTIVO_INDEF
        from   table ( xmlsequence ( extract(pXML,'/MOTIVOS/ITEM') ) ) T;
        --
        V_XML                           sys.XMLType;
        v_retorno               varchar2(3000);
        v_nom_ocorrencia        varchar2(4000);
        v_nom_temp_ocorrencia   varchar2(4000);
        --
    begin
        --
        V_XML := sys.XMLType.createXML( p_xml_dados );
        --
        IF V_XML is not null THEN
            FOR rc_motivo in cur_motivo (V_XML) LOOP
                --
                BEGIN
                    INSERT INTO REEMBOLSO_PREVIA_PEDIDO_INDEF( cod_motivo
                                                             , num_reembolso
                                                             , num_reembolso_ans
                                                             , cod_usuario_atu
                                                             , dt_atu)
                                                      VALUES( rc_motivo.COD_MOTIVO_INDEF
                                                            , p_num_reembolso
                                                            , p_num_reembolso_ans
                                                            , p_cod_usuario
                                                            , SYSDATE);
                    SELECT desc_motivo
                     INTO v_nom_ocorrencia
                    FROM reembolso_previa_motivo_indef
                    WHERE cod_motivo = rc_motivo.COD_MOTIVO_INDEF;

                    v_nom_temp_ocorrencia := 'Motivo: '|| v_nom_ocorrencia || '<br>' || v_nom_temp_ocorrencia;
                    v_nom_temp_ocorrencia := RTRIM(v_nom_temp_ocorrencia, '<br>');

                EXCEPTION
                    WHEN dup_val_on_index THEN
                    raise;
                END;
            END LOOP;

            BEGIN
                GeraOcorrencia ( p_num_reembolso,p_num_reembolso_ans,5,v_nom_temp_ocorrencia ,case when p_ind_tipo_emissao = 'E' then  ' Solicita��o de envio de Email foi cadastrada com sucesso. Enviado para o email: '||p_txt_email else null end,p_cod_usuario,p_cod_retorno,p_msg_retorno);
            EXCEPTION
                WHEN dup_val_on_index THEN
                raise;
            END;
        END IF;
        --
        p_cod_retorno := 0;
        p_msg_retorno := v_retorno;
    exception
    when others then
        p_cod_retorno:=9;
        p_msg_retorno:=ts_util.MsgErro;
    end;
    ------------------------------------------------------------------------------
    -- Incluir o solicitante informado na previa
    ------------------------------------------------------------------------------
    procedure grava_solicitante_previa_reembolso ( p_cod_solicitante out number
                                                  , p_cod_retorno     out pls_integer
                                                  , p_msg_retorno     out varchar2
                                                  , p_xml_parametros  in  clob
                                                  )
    is
        tab_parans              top_utl_xml.tbl_fields;
        str_sql                 varchar2(32000);
        v_cod_solicitante       solicitante.cod_solicitante%type;
        v_num_reembolso         number(15,0);
    begin
        --
        tab_parans := ts.top_utl_xml.toCollection( p_xml_parametros ); -- Retorna um MAP ( chave , valor ) com os n�s do XML;
        --
        v_cod_solicitante := tab_parans('cod_solicitante').valor(1);
        v_num_reembolso   := tab_parans('num_reembolso').valor(1);
        --
        if v_cod_solicitante is null then
            --
            str_sql :=  rtrim (' insert into solicitante                      ')
                    ||rtrim ('         ( cod_solicitante                      ')
                    ||rtrim ('         , nome_solicitante                     ')
                    ||rtrim ('         , ind_tipo_pessoa                      ')
                    ||rtrim ('         , num_crm                              ')
                    ||rtrim ('         , num_insc_fiscal                      ')
                    ||rtrim ('         , sgl_uf_conselho                      ')
                    ||rtrim ('         , sigla_conselho                       ')
                    ||rtrim ('         , cod_municipio_execucao               ')
                    ||rtrim ('         , ind_situacao                         ')
                    ||rtrim ('         , txt_origem                           ')
                    ||rtrim ('         , dt_atu                               ')
                    ||rtrim ('         , cod_usuario_atu                      ')
                    ||rtrim (') values ( :cod_solicitante                     ')
                    ||rtrim ('         , :nome_solicitante                    ')
                    ||rtrim ('         , :ind_tipo_pessoa                     ')
                    ||rtrim ('         , :num_crm                             ')
                    ||rtrim ('         , :num_insc_fiscal                     ')
                    ||rtrim ('         , :sgl_uf_conselho                     ')
                    ||rtrim ('         , :sigla_conselho                      ')
                    ||rtrim ('         , :cod_municipio_execucao              ')
                    ||rtrim ('         , :ind_situacao                        ')
                    ||rtrim ('         , :txt_origem                          ')
                    ||rtrim ('         , :dt_atu                              ')
                    ||rtrim ('         , :cod_usuario_atu                     ')
                    ||rtrim ('         )                                      ');
            --
            select ts_solicitante_seq.nextval
            into   v_cod_solicitante
            from   dual;
            --
            execute immediate str_sql
                      using v_cod_solicitante
                          , tab_parans('nome_solicitante').valor(1)
                          , tab_parans('ind_tipo_pessoa').valor(1)
                          , tab_parans('num_crm').valor(1)
                          , tab_parans('num_insc_fiscal').valor(1)
                          , tab_parans('sgl_uf_conselho').valor(1)
                          , tab_parans('sigla_conselho').valor(1)
                          , ''
                          , 1
                          , 'WEB'
                          , sysdate
                          , tab_parans('cod_usuario').valor(1);
            --
            BEGIN
              update ts.pedido_reembolso_previa pr
                 set pr.cod_solicitante = v_cod_solicitante
               where pr.num_reembolso   = v_num_reembolso;
             EXCEPTION
                WHEN OTHERS THEN
                    --
                    ts_log_execucao ('RB_PREVIA_REEMBOLSO'
                                    , NULL
                                    , 'Erro grava solicitante'
                                    , p_xml_parametros || chr(13) || str_sql || chr(13) || ts_util.MsgErro || chr(13) || ts.top_utl_padrao.msgerro
                                    , 'Erro'
                                    ) ;

                    p_cod_retorno := 9;
                    p_msg_retorno := 'Erro: ' || ts.top_utl_padrao.msgerro ;
                    return;
             END;

        else

                str_sql :=  rtrim (' update solicitante                                                       ')
                          ||rtrim ('        set nome_solicitante           = :nome_solicitante                ')
                          ||rtrim ('          , ind_tipo_pessoa            = :ind_tipo_pessoa                 ')
                          ||rtrim ('          , num_insc_fiscal            = :num_insc_fiscal                 ')
                          ||rtrim ('          , num_crm                    = :num_crm                         ')
                          ||rtrim ('          , sgl_uf_conselho            = :sgl_uf_conselho                 ')
                          ||rtrim ('          , sigla_conselho             = :sigla_conselho                  ')
                          ||rtrim ('          , cod_municipio_execucao     = :cod_municipio_execucao          ')
                          ||rtrim ('          , dt_atu                     = :dt_atu                          ')
                          ||rtrim ('          , cod_usuario_atu            = :cod_usuario_atu                 ')
                          ||rtrim (' where      cod_solicitante            = :cod_solicitante                 ');

                execute immediate str_sql
                      using tab_parans('nome_solicitante').valor(1)
                          , tab_parans('ind_tipo_pessoa').valor(1)
                          , tab_parans('num_insc_fiscal').valor(1)
                          , tab_parans('num_crm').valor(1)
                          , tab_parans('sgl_uf_conselho').valor(1)
                          , tab_parans('sigla_conselho').valor(1)
                          , ''
                          , sysdate
                          , tab_parans('cod_usuario').valor(1)
                          , tab_parans('cod_solicitante').valor(1);

        end if;
        --
        p_cod_solicitante := v_cod_solicitante;
        p_cod_retorno := 0;
        p_msg_retorno := 'Opera��o realizada com sucesso';
        return;
        --
    exception
    when others then
        --
        ts_log_execucao ('RBM_PROTOCOLO'
                        , NULL
                        , 'Erro'
                        , p_xml_parametros || chr(13) || str_sql || chr(13) || ts_util.MsgErro || chr(13) || ts.top_utl_padrao.msgerro
                        , 'Erro'
                        ) ;

        p_cod_retorno := 9;
        p_msg_retorno := 'Erro: ' || ts.top_utl_padrao.msgerro ;
        return;
    end;
    --
    procedure GravaPreviaSubmodalidade( p_cod_retorno          out number
                                       , p_msg_retorno         out varchar2
                                       , p_xml_parametros      in clob
                                       ) 
      is
      -----  documento transformado
      v_doc               xmldom.DOMDocument;

      -----  retorno
      v_cod_retorno       number;
      v_msg_retorno       varchar2(200);
      v_posicao           number;
      
      v_qtd_submodalidade        number;
      v_ind_tipo_submodalidade   number;
      v_ind_tipo_reembolso       varchar2(2);
      v_nome_submodalidade       varchar2(60);
      --v_ordem_exibicao           number;
      v_dt_inicio                date;
      v_dt_fim                   date;
      v_cod_usuario_atu          varchar2(20);
      v_ind_habilitado           varchar2(1);
      v_ind_acao_submodalidade   varchar2(1);
      v_num_seq_ocorrencia       number;
      
      v_ind_operacao_insere      varchar2(1);
      v_ind_operacao_altera      varchar2(1);
      v_ind_operacao_habilita    varchar2(1);
      
      begin
      
      TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO.GRAVAPREVIA', 00, 'Entrada', p_xml_parametros, 'grava_previa - XML' );
      ts_cria_doc_xml(p_xml_parametros,v_doc,v_cod_retorno,v_msg_retorno);
      if v_cod_retorno <> 0 then
          p_cod_retorno := v_cod_retorno;
          p_msg_retorno := v_msg_retorno;
          return;
      end if;
      
      v_ind_tipo_reembolso    := to_number(ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','IND_TIPO_REEMBOLSO'));
      v_qtd_submodalidade     := to_number(ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','QTD_SUBMODALIDADE'));
      v_cod_usuario_atu       := ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','COD_USUARIO');
      
      if NVL(v_qtd_submodalidade,0) = 0 then
          p_cod_retorno := 9;
          p_msg_retorno := 'Quantidade de submodalidade inv�lida';
          return;
      end if;      
      
      for i in 1 .. v_qtd_submodalidade loop
        
        v_ind_tipo_submodalidade  := to_number(ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/IND_TIPO_SUBMODALIDADE_' || i));
        v_nome_submodalidade      := ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/NOME_SUBMODALIDADE_' || i);
        --v_ordem_exibicao          := to_number(ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/ORDEM_EXIBICAO_' || i));
        v_dt_inicio               := to_date(ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/DT_INICIO_' || i), 'DD/MM/YYYY');
        v_dt_fim                  := to_date(ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/DT_FIM_' || i), 'DD/MM/YYYY');
        v_ind_habilitado          := ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/IND_HABILITADO_' || i);
        v_ind_acao_submodalidade  := ts_obtem_dados_xml(v_doc,'SUBMODALIDADE','DADOS_' || i||'/IND_ACAO_SUBMODALIDADE_' || i);
        
        if v_nome_submodalidade is not null then
          
          if v_ind_habilitado = 'S' then
            v_ind_operacao_habilita := 'S';
          end if;
          
          -- ALTERA
          if v_ind_acao_submodalidade = 'A' then          
              
              UPDATE REEMBOLSO_PREVIA_SUBMODALIDADE
                 SET NOME_SUBMODALIDADE = v_nome_submodalidade,
                     --ORDEM_EXIBICAO     = v_ordem_exibicao,
                     DT_INICIO          = v_dt_inicio,
                     DT_FIM             = v_dt_fim,
                     COD_USUARIO_ATU    = v_cod_usuario_atu,
                     IND_HABILITADO     = v_ind_habilitado
               WHERE IND_TIPO_SUBMODALIDADE = v_ind_tipo_submodalidade
                 AND IND_TIPO_REEMBOLSO = v_ind_tipo_reembolso;
                 
              IF SQL%ROWCOUNT > 0 THEN
                 v_ind_operacao_altera := 'S';
              END IF;           
                 
          -- INSERE
          else
            
            v_ind_operacao_insere := 'S';
            --
            BEGIN
              SELECT max(ind_tipo_submodalidade) +1 
                INTO v_ind_tipo_submodalidade
                FROM ts.REEMBOLSO_PREVIA_SUBMODALIDADE;
            EXCEPTION
              WHEN OTHERS THEN
                v_ind_tipo_submodalidade := null;
            END;
            
            IF v_ind_tipo_submodalidade IS NULL THEN
                v_ind_tipo_submodalidade := 1;
            END IF;
            --
            INSERT INTO TS.REEMBOLSO_PREVIA_SUBMODALIDADE
              (IND_TIPO_SUBMODALIDADE,
               IND_TIPO_REEMBOLSO,
               NOME_SUBMODALIDADE,
               --ORDEM_EXIBICAO,
               DT_INICIO,
               DT_FIM,
               COD_USUARIO_ATU)
            VALUES
              (v_ind_tipo_submodalidade,
               v_ind_tipo_reembolso,
               v_nome_submodalidade,
               --v_ordem_exibicao,
               v_dt_inicio,
               v_dt_fim,
               v_cod_usuario_atu);
               
          end if;
        end if;
        
      end loop;
      
      if v_ind_operacao_insere = 'S' then
        
          -------------------------
          -- GRAVANDO OCORR�NCIA
          -------------------------          
          --
          BEGIN
            SELECT max(num_seq_ocorrencia) +1 
              INTO v_num_seq_ocorrencia
              FROM ts.previa_submodalidade_ocorrencia;
          EXCEPTION
            WHEN OTHERS THEN
              v_num_seq_ocorrencia := null;
          END;
          
          IF v_num_seq_ocorrencia IS NULL THEN
              v_num_seq_ocorrencia := 1;
          END IF;
          --
          
           INSERT INTO ts.previa_submodalidade_ocorrencia
             (num_seq_ocorrencia,
              ind_tipo_reembolso,
              dt_ocorrencia,
              desc_ocorrencia,
              dt_atu,
              cod_usuario_atu)
           VALUES
             (v_num_seq_ocorrencia,
              v_ind_tipo_reembolso,
              sysdate,
              'Cria��o da Submodalidade',
              sysdate,
              v_cod_usuario_atu);
      
      end if;
      
      if v_ind_operacao_altera = 'S' then
        
          -----------------------------
          -- GRAVANDO OCORR�NCIA UPDATE
          -----------------------------
          --
          BEGIN
            SELECT max(num_seq_ocorrencia) +1 
              INTO v_num_seq_ocorrencia
              FROM ts.previa_submodalidade_ocorrencia;
          EXCEPTION
            WHEN OTHERS THEN
              v_num_seq_ocorrencia := null;
          END;
          
          IF v_num_seq_ocorrencia IS NULL THEN
              v_num_seq_ocorrencia := 1;
          END IF;
          --          
         INSERT INTO ts.previa_submodalidade_ocorrencia
           (num_seq_ocorrencia,
            ind_tipo_reembolso,
            dt_ocorrencia,
            desc_ocorrencia,
            dt_atu,
            cod_usuario_atu)
         VALUES
           (v_num_seq_ocorrencia,
            v_ind_tipo_reembolso,
            sysdate,
            'Submodalidade Alterada',
            sysdate,
            v_cod_usuario_atu);
          ---------------------------------
          -- FIM GRAVANDO OCORR�NCIA UPDATE
          ---------------------------------   
      
      end if;
      
      if v_ind_operacao_habilita = 'S' then
        
          -----------------------------
          -- GRAVANDO OCORR�NCIA HABILITA
          -----------------------------
          --
          BEGIN
            SELECT max(num_seq_ocorrencia) +1 
              INTO v_num_seq_ocorrencia
              FROM ts.previa_submodalidade_ocorrencia;
          EXCEPTION
            WHEN OTHERS THEN
              v_num_seq_ocorrencia := null;
          END;
          
          IF v_num_seq_ocorrencia IS NULL THEN
              v_num_seq_ocorrencia := 1;
          END IF;
          --          
         INSERT INTO ts.previa_submodalidade_ocorrencia
           (num_seq_ocorrencia,
            ind_tipo_reembolso,
            dt_ocorrencia,
            desc_ocorrencia,
            dt_atu,
            cod_usuario_atu)
         VALUES
           (v_num_seq_ocorrencia,
            v_ind_tipo_reembolso,
            sysdate,
            'Habilita��o da Submodalidade',
            sysdate,
            v_cod_usuario_atu);
          ---------------------------------
          -- FIM GRAVANDO OCORR�NCIA HABILITA
          ---------------------------------  
      
      end if;
      
      xmldom.freeDocument(v_doc);
      commit;
      
      p_cod_retorno       := 0;
      p_msg_retorno       := 'Opera��o realizada.';

      exception
        when others then

           p_cod_retorno := 9;
           p_msg_retorno := p_msg_retorno || 'GravaPreviaSubmodalidade-' || v_posicao || ': ' || sqlerrm;
           TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO.GRAVAPREVIA', 00, 'Entrada', p_xml_parametros, 'grava_previa - XML' );
      
           rollback;

           if v_posicao > 1 then
              xmldom.freeDocument(v_doc);
           end if;

    end;
    --
    --
    function get_submodalidade_ocorrencia(p_ind_tipo_reembolso in pls_integer,
                                          p_ctr_logs           in varchar2 default 'N')
      return sys_refcursor is
      result sys_refcursor;
    begin
      --
      if nvl(p_ind_tipo_reembolso, 0) = 0 then
        return get_cursor_vazio;
      end if;
      --
      OPEN RESULT FOR
        SELECT pso.num_seq_ocorrencia,
               pso.dt_ocorrencia,
               pso.desc_ocorrencia,
               pso.cod_usuario_atu
          FROM ts.previa_submodalidade_ocorrencia pso
         WHERE pso.ind_tipo_reembolso = p_ind_tipo_reembolso
         ORDER BY pso.num_seq_ocorrencia desc;
      --
      return result;
      --
    exception
      when others then
        if nvl(p_ctr_logs, 'N') = 'S' then
          --
          ts_log_execucao('RB_PREVIA_REEMBOLSO',
                          'get_submodalidade_ocorrencia',
                          ts_util.MsgErro,
                          'p_ind_tipo_reembolso = ' || p_ind_tipo_reembolso,
                          'Erro');
        end if;
        return get_cursor_vazio;
    end;
    --
    --
    function get_previa_submodalidades(p_tipo_reembolso in pls_integer,
                                       p_ctr_logs       in varchar2 default 'N')
      return sys_refcursor is
      result sys_refcursor;
    begin
      --
    
      --
      open result for
        select ps.ind_tipo_submodalidade,
               ps.nome_submodalidade,
               ps.ordem_exibicao,
               TO_CHAR(ps.dt_inicio, 'DD/MM/YYYY') AS dt_inicio,
               TO_CHAR(ps.dt_fim, 'DD/MM/YYYY') AS dt_fim,
               ps.ind_habilitado
          from reembolso_previa_submodalidade ps
         where ps.ind_tipo_reembolso = p_tipo_reembolso;
    
      --
      return result;
      --
    exception
      when others then
        if nvl(p_ctr_logs, 'N') = 'S' then
          --
          ts_log_execucao('TS.rbm_previa_reembolso',
                          'get_previa_submodalidades',
                          sqlerrm,
                          'p_tipo_reembolso = ' || p_tipo_reembolso,
                          'Erro');
        end if;
        return get_cursor_vazio;
    end;
    --
    --
    procedure GravaPrevia  ( p_xml_dados                in clob
                           , p_num_reembolso            out number
                           , p_cod_retorno              out number
                           , p_msg_retorno              out varchar2)
    is
    --Declara��es:
    v_posicao                       number;
    v_item_vazio                    varchar2(3) := '���';
    v_doc                           xmldom.DOMDocument;
    vXMLAnexo                       clob;
    --previa
    v_num_reembolso                 pedido_reembolso_previa.num_reembolso%type;
    --
    rsPrevia                        pedido_reembolso_previa%rowtype;
    rsSolicitante                   ts.solicitante%rowtype;
    --
    rsPreviaProcedimento            procedimento_reembolso_previa%rowtype;
    ind_excluir                     varchar2(1);
    qtd_participante                number;
    --
    v_cod_usuario                   usuario.cod_usuario%type;
    v_nome_arquivo                  reembolso_previa_anexo.nom_arq_anexo%type;
    --
    v_qtd_anexo                     number;
    v_result                        varchar2(10);
    v_qtd_procedimento              number;
    v_cod_procedimento_principal    procedimento_reembolso_previa.cod_procedimento%type;
    v_ind_acao                      varchar2(2);
    v_cod_procedimento_princ_ant    procedimento_reembolso_previa.cod_procedimento%type;
    v_qtd_participante              number;
    v_ind_excluir                   varchar2(1);
    --
    v_txt_memoria_calculo           memoria_reembolso_previa.txt_memoria%type;
    v_num_seq_item_memoria          memoria_reembolso_previa.num_seq_item%type;
    --
    rsPreviaAux                     pedido_reembolso_previa%rowtype;
    rsPreviaProcedimentoAux         procedimento_reembolso_previa%rowtype;
    --
    v_legenda_situacao_01           varchar2(50);
    v_legenda_situacao_02           varchar2(50);
    --
    --
    v_cod_procedimento_aux          procedimento_reembolso_previa.cod_procedimento%type;
    v_cod_procedimento_cm_aux       procedimento_reembolso_previa.cod_procedimento%type;
    --
    v_ind_procedimento_alterado     varchar2(1);
    v_ind_tipo_finalizacao          varchar2(1);
    v_cod_grupo_encaminhamento      varchar2(500);
    v_cod_grupo_encaminhamento_de   varchar2(20);
    v_tipo_ocorrecia                number;
    --
    v_cod_retorno                   number;
    v_msg_retorno                   varchar2(400);
    v_msg_retorno_sucesso           varchar2(400);
    v_msg_retorno_erro              varchar2(400);
    --
    txtXML                          clob;
    --
    v_xml_glosa                     clob;
    v_xml_motivo                    clob;
    v_ind_acao_procedimento         varchar2(1);
    v_ind_tipo_encaminhamento       varchar2(1);
    --
    v_desc_motivo                   varchar2(400);
    num_seq_itens_proc              varchar(400);
    v_cod_tipo_usuario              pls_integer;
    --
    v_val_reembolsado               procedimento_reembolso_previa.val_reembolsado%type;
    v_val_calculado                 procedimento_reembolso_previa.val_calculado%type;
    --
    v_ind_finalizado                varchar2(1);
    v_qtd_dias_reembolso            number;
    v_count_proc_iguais             number;
    v_count_proc_null               number;
    v_count_proc_not_null           number;
    v_qtd_glosa_analise             number;
    --
    v_ind_situacao_associado        varchar(1);
    v_num_dias_exclusao             number;
    v_data_exclusao                 date;
    --
    v_ind_erro_ws                   varchar2(1);
    v_msg_erro_ws                   clob;
    --
    v_ind_excluir_proc              varchar2(1);
    --
    p_num_reembolso_ans             pedido_reembolso_previa.num_reembolso_ans%type;
    p_xml_protocolo_ans             clob;
    v_tamanho_reembolso             number;
    --
    v_cod_operadora_atd             varchar2(20);
    v_cod_usuario_atd               varchar2(20);
    v_cod_ts_atd                    varchar2(20);
    v_dt_provavel_reembolso         varchar2(20);
    --
    v_val_fixo                       procedimento_reembolso_previa.val_calculado%type;
    v_val_per_copart                 procedimento_reembolso_previa.val_calculado%type;
    v_val_limite_copart              procedimento_reembolso_previa.val_calculado%type;
    v_valor_valido                   procedimento_reembolso_previa.val_calculado%type;
    --
    v_xml_solicitante               clob;
    --
    v_ind_vazio                     varchar2(1);
    v_num_seq_item_aux              number;
    --
    begin
        --
        --In�cio
        v_posicao := 0;
        p_cod_retorno := 0;
        p_msg_retorno := '';
        --
        v_ind_finalizado := 'N';
        v_desc_motivo := null;
        --
        v_posicao := 1;
        --
        --Ler informa��es do XML
        ts_cria_doc_xml(p_xml_dados, v_doc, p_cod_retorno, p_msg_retorno);
        if p_cod_retorno <> 0 then
           p_msg_retorno := 'RB_PREVIA_REEMBOLSO.GravaPreviaReembolso-'|| v_posicao || ': ' || p_msg_retorno;
           return;
        end if;
        --
        --
        TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO.GRAVAPREVIA', 00, 'Entrada', p_xml_dados, 'grava_previa - XML' );
        --
        --Recuperar informa��es do XML
        --
        v_ind_acao                  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_ACAO');
        v_ind_tipo_finalizacao      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_TIPO_FINALIZACAO');
        v_ind_tipo_encaminhamento   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_TIPO_ENCAMINHAMENTO');
        v_qtd_procedimento          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/QTD_PROCEDIMENTO');
        v_cod_usuario               := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_USUARIO');
        --
        v_posicao := 2;
        --Associado
        if v_ind_acao = 'I' then

            rsPrevia.qtd_dias_reembolso    := to_number(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/QTD_DIAS_REEMBOLSO'));
            rsPrevia.qtd_dias_reemb_uteis  := to_number(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/QTD_DIAS_REEMB_UTEIS'));
            --
            if TS_UTIL.IsDate(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DT_PROVAVEL_REEMBOLSO'), 'DD/MM/YYYY') then
                rsPrevia.dt_provavel_reembolso   := to_date(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DT_PROVAVEL_REEMBOLSO'), 'DD/MM/YYYY');
            end if;
            --
            --------------------------------------------------
             --  Insclus�o do protocolo do reembolso
             --------------------------------------------------
            v_cod_operadora_atd := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_OPERADORA');
            v_cod_usuario_atd := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_USUARIO_SITUACAO');
            v_cod_ts_atd := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS');
            --
            getProtocolo( p_cod_operadora_atd => v_cod_operadora_atd
                   , p_cod_usuario_atd        => v_cod_usuario_atd
                   , p_cod_ts_atd             => v_cod_ts_atd
                   , p_cod_retorno            => p_cod_retorno
                   , p_msg_retorno            => p_msg_retorno
                   , p_num_protocolo          => p_num_reembolso_ans
                   ) ;
            --
            SELECT ts_sinistro_seq.NextVal
            INTO p_num_reembolso
            FROM DUAL;
            rsPrevia.num_reembolso         := p_num_reembolso;
            rsPrevia.Num_Reembolso_Ans     := p_num_reembolso_ans;
            --
            rsPrevia.dt_inclusao           := SYSDATE;
            rsPrevia.ind_situacao          := 5;
            rsPrevia.cod_usuario_sit       := v_cod_usuario;
            rsPrevia.dt_sit                := sysdate;
            rsPrevia.cod_usuario_inclusao  := v_cod_usuario;
        else

            p_num_reembolso := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_REEMBOLSO');

            select length(p_num_reembolso)
              into v_tamanho_reembolso
              from dual;
           --
           --
           if v_tamanho_reembolso <= 15 then
            select *
              into rsPrevia
              from pedido_reembolso_previa
             where num_reembolso = p_num_reembolso;
           else
             select *
              into rsPrevia
              from pedido_reembolso_previa
             where num_reembolso_ans = p_num_reembolso;
           end if;

            p_num_reembolso                := rsPrevia.Num_Reembolso;
            p_num_reembolso_ans            := rsPrevia.Num_Reembolso_ans;
            rsPrevia.cod_usuario_sit       := v_cod_usuario;
            rsPrevia.dt_sit                := sysdate;

            rsPrevia.dt_deferimento             := null;
            rsPrevia.cod_usuario_deferimento    := null;
            rsPrevia.dt_indeferimento           := null;
            rsPrevia.cod_usuario_indeferimento  := null;
            rsPrevia.dt_cancelamento            := null;
            rsPrevia.cod_usuario_cancelamento   := null;
            rsPrevia.cod_motivo                 := null;

            --Executa valida��es em rel��o ao tipo de a��o
            if v_ind_acao = 'A' then -- altera��o
                v_msg_retorno_sucesso          := ' alterada com sucesso.';
                v_msg_retorno_erro             := 'Erro ao alterar a pr�via ';
                if v_qtd_procedimento > 0 then
                    rsPrevia.ind_situacao          := 1;
                else
                    rsPrevia.ind_situacao          := 5;
                end if;
                v_tipo_ocorrecia               := 2;
            elsif v_ind_acao = 'FN' then -- finalizacao
                v_msg_retorno_sucesso          := ' finalizada com sucesso.';
                v_msg_retorno_erro             := 'Erro ao finalizar a pr�via ';

                if v_ind_tipo_finalizacao = 'A' then -- aprovacao
                    v_tipo_ocorrecia                       := 3;
                    rsPrevia.ind_situacao                  := 2;
                    rsPrevia.cod_usuario_deferimento       := v_cod_usuario;
                    rsPrevia.dt_deferimento                := sysdate;
                elsif v_ind_tipo_finalizacao = 'R' then -- recusa
                    v_tipo_ocorrecia                         := 5;
                    rsPrevia.ind_situacao                    := 4;
                    rsPrevia.cod_usuario_indeferimento       := v_cod_usuario;
                    rsPrevia.dt_indeferimento                := sysdate;
                    --rsPrevia.cod_motivo                      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_MOTIVO');

                begin

                select desc_motivo
                  into v_desc_motivo
                  from reembolso_previa_motivo_indef
                 where cod_motivo = rsPrevia.cod_motivo;

                exception
                   when others then
                   v_desc_motivo := null;
                end;
                end if;
            elsif v_ind_acao = 'CA' then -- cancelamento

                v_msg_retorno_sucesso          := ' cancelada com sucesso.';
                v_msg_retorno_erro             := 'Erro ao cancelar a pr�via ';

                v_tipo_ocorrecia                         := 4;
                rsPrevia.ind_situacao                    := 3;
                rsPrevia.cod_usuario_indeferimento       := v_cod_usuario;
                rsPrevia.dt_cancelamento                 := sysdate;
                rsPrevia.cod_motivo                      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_MOTIVO');

                begin

                    select desc_motivo
                      into v_desc_motivo
                      from reembolso_previa_motivo_indef
                     where cod_motivo = rsPrevia.cod_motivo;

                exception
                   when others then
                   v_desc_motivo := null;
                end;
            elsif v_ind_acao = 'EN' then -- transfer�ncia
                if v_ind_tipo_encaminhamento = 'E' then
                    v_msg_retorno_sucesso          := ' encaminhada com sucesso.';
                    v_msg_retorno_erro             := 'Erro ao encaminhar a pr�via ';
                else
                    v_msg_retorno_sucesso          := ' transferida com sucesso.';
                    v_msg_retorno_erro             := 'Erro ao tranferir a pr�via ';
                end if;

                v_tipo_ocorrecia                   := 20;
                rsPrevia.ind_situacao              := 1;
            else
                p_cod_retorno := 9;
                p_msg_retorno := 'A��o n�o informada ou inv�lida.';
                return;
            end if;
        end if;
        --
        if nvl(rsPrevia.qtd_dias_reembolso,0) = 0
        or nvl(rsPrevia.qtd_dias_reemb_uteis,0) = 0
        or nvl(rsPrevia.dt_provavel_reembolso,trunc(sysdate)) = trunc(sysdate) then
        --
            RetornaDiasPrazo  ( rsPrevia.cod_inspetoria_ts_abertura
                              , rsPrevia.cod_plano
                              , rsPrevia.qtd_dias_reembolso
                               , rsPrevia.qtd_dias_reemb_uteis
                              , v_dt_provavel_reembolso
                              , p_cod_retorno
                              , p_msg_retorno
                              , to_char(rsPrevia.dt_inclusao, 'DD/MM/YYYY')
                              );


           rsPrevia.dt_provavel_reembolso := to_date(v_dt_provavel_reembolso,'dd/mm/yyyy');
        --
        end if;
        --
        --dados associado
        rsPrevia.num_associado         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_ASSOCIADO');
        rsPrevia.nome_associado        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_ASSOCIADO');
        rsPrevia.cod_ts                := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS');
        rsPrevia.cod_ts_tit            := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS_TIT');
        rsPrevia.cod_entidade_ts_tit   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_ENTIDADE_TS_TIT');
        if TS_UTIL.IsDate(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_NASCIMENTO'), 'DD/MM/YYYY') then
            rsPrevia.data_nascimento   := to_date(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_NASCIMENTO'), 'DD/MM/YYYY');
        end if;

        rsPrevia.qtd_idade             := TO_NUMBER(ts_calcula_idade(rsPrevia.data_nascimento,TRUNC(SYSDATE),'A'));
        if rsPrevia.qtd_idade = -1 THEN
            rsPrevia.qtd_idade := null;
        end if;
        --
        rsPrevia.cod_rede                       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_REDE');
        rsPrevia.cod_plano                      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_PLANO');
        rsPrevia.cod_ts_contrato                := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_TS_CONTRATO');
        rsPrevia.num_contrato                   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_CONTRATO');
        rsPrevia.cod_inspetoria_ts_contrato     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_INSPETORIA_TS');
        rsPrevia.cod_operadora_contrato         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_OPERADORA');
        rsPrevia.cod_marca_contrato             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_MARCA');
        --
        rsPrevia.ind_sexo                       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_SEXO');
        rsPrevia.tipo_associado                 := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TIPO_ASSOCIADO');
        --
        rsPrevia.ind_tipo_reembolso             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_TIPO_REEMBOLSO');
        rsPrevia.cod_origem                     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_ORIGEM');
        rsPrevia.cod_inspetoria_ts_abertura     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_INSPETORIA_TS_ABERTURA');
        --
        rsPrevia.ind_tipo_emissao               := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_TIPO_EMISSAO');
        rsPrevia.num_internacao                 := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_INTERNACAO');
        rsPrevia.ind_carater                    := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_CARATER');
        rsPrevia.ind_acomodacao                 := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_ACOMODACAO');
        --
        rsPrevia.tipo_pessoa_contrato           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TIPO_PESSOA_CONTRATO');
        rsPrevia.ind_origem_associado           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_ORIGEM_ASSOCIADO');
        --
        v_posicao := 40;
        -- dados do executante
        --rsPrevia.ind_insc_fiscal       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_INSC_FISCAL');
        --rsPrevia.num_insc_fiscal       := replace(replace(replace(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_INSC_FISCAL'),'.',''),'-',''),'/','');
        --rsPrevia.nome_prestador        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_PRESTADOR');
        --rsPrevia.sigla_conselho        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/SIGLA_CONSELHO');
        --rsPrevia.cod_cbo               := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_CBO');
        --rsPrevia.uf_conselho           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/UF_CONSELHO');
        --rsPrevia.num_crm               := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_CRM');
        --rsPrevia.cnes                  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/CNES');

        -- dados do solicitante
        rsPrevia.cod_solicitante           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_SOLICITANTE');
        rsPrevia.cnes_solicitante          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/CNES_SOLICITANTE');
        rsPrevia.cod_cbo_solicitante       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_CBO_SOLICITANTE');
        rsSolicitante.ind_tipo_pessoa      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_INSC_FISCAL_SOLICITANTE');
        rsSolicitante.nome_solicitante     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_PRESTADOR_SOLICITANTE');
        rsSolicitante.num_insc_fiscal      := replace(replace(replace(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_INS_FISCAL_SOLICITANTE'),'.',''),'-',''),'/','');
        rsSolicitante.sigla_conselho       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/SIGLA_CONSELHO_SOLICITANTE');
        rsSolicitante.num_crm              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_CRM_SOLICITANTE');
        rsSolicitante.sgl_uf_conselho      := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/UF_CONSELHO_SOLICITANTE');
        --

        v_posicao := 42;

        rsPrevia.cod_motivo_reembolso       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_MOTIVO_REEMBOLSO');
        rsPrevia.txt_observacao             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TXT_OBSERVACAO');
        rsPrevia.txt_observacao_operadora   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TXT_OBSERVACAO_OPERADORA');

        rsPrevia.txt_email                  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/TXT_EMAIL');

      --*****************************************************************************************************************
        --CL00541137 - ALTERA��O DA GRAVA��O DO N�MERO DE TELEFONE PARA COLUNA CELULAR NA TABELA PEDIDO_REEMBOLSO_PREVIA
        --*****************************************************************************************************************
        if (rsPrevia.cod_origem = 11 and v_ind_acao = 'I') then
          rsPrevia.ddd_residencial            := null;
          rsPrevia.tel_residencial            := null;
          rsPrevia.ddd_comercial              := null;
          rsPrevia.tel_comercial              := null;
          rsPrevia.ddd_celular                := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_RESIDENCIAL');
          rsPrevia.tel_celular                := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_RESIDENCIAL'),'-','');
        elsif (rsPrevia.cod_origem = 11 and v_ind_acao = 'A') then
          rsPrevia.ddd_residencial            := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_RESIDENCIAL');
          rsPrevia.tel_residencial            := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_RESIDENCIAL'),'-','');
          rsPrevia.ddd_comercial              := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_COMERCIAL');
          rsPrevia.tel_comercial              := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_COMERCIAL'),'-','');
          rsPrevia.ddd_celular                := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_CELULAR');
          rsPrevia.tel_celular                := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_CELULAR'),'-','');
        else
          rsPrevia.ddd_residencial            := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_RESIDENCIAL');
          rsPrevia.tel_residencial            := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_RESIDENCIAL'),'-','');
          rsPrevia.ddd_comercial              := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_COMERCIAL');
          rsPrevia.tel_comercial              := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_COMERCIAL'),'-','');
          rsPrevia.ddd_celular                := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/DDD_CELULAR');
          rsPrevia.tel_celular                := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TEL_CELULAR'),'-','');
        end if;
        --*****************************************************************************************************************


        rsPrevia.txt_ddd_fax                := ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TXT_DDD_FAX');
        rsPrevia.txt_num_fax                := replace(ts_obtem_dados_xml(v_doc, 'REEMBOLSO', 'PREVIA/TXT_NUM_FAX'),'-','');

        rsPrevia.num_titular                := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NUM_TITULAR');
        rsPrevia.nome_titular               := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_TITULAR');
        rsPrevia.nome_contrato              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/NOME_CONTRATO');

        rsPrevia.ind_regulamentado          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_REGULAMENTADO');

        v_ind_erro_ws                       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_ERRO_WS');
        v_msg_erro_ws                       := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/MSG_ERRO_WS');

        v_xml_motivo                            := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/INDEFERIMENTO');
        --
        -- Gravar o(s) motivo(s) de recusa da prevoa
        IF v_ind_tipo_finalizacao = 'R' and v_ind_acao = 'FN' THEN
          gravaMotivoIndef( p_cod_retorno
                            , p_msg_retorno
                            , v_xml_motivo
                            , rsPrevia.Num_Reembolso
                            , rsPrevia.Num_Reembolso_Ans
                            , v_cod_usuario
                            , rsPrevia.Ind_Tipo_Emissao
                            , rsPrevia.Txt_Email);


        END IF;
        --
        if    rsPrevia.ind_tipo_reembolso = 1 then
           rsPrevia.cod_tratamento := 5;  -- consulta
        elsif rsPrevia.ind_tipo_reembolso = 2 then
           rsPrevia.cod_tratamento := 15; -- ambulatorial
        elsif rsPrevia.ind_tipo_reembolso = 3 then
           rsPrevia.cod_tratamento := 9; -- interna��o
        else
           rsPrevia.cod_tratamento := 15; -- ambulatorial
        end if;

        v_posicao := 2;

        if not ( v_ind_acao = 'CA' or ( v_ind_acao = 'FN' and v_ind_tipo_finalizacao = 'R' ) ) then

            v_posicao := 60;

             if rsPrevia.cod_ts is null and rsPrevia.num_associado is null then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'Benefici�rio � obrigat�rio.';
                 rollback;
                 return;
             end if;
            --
            v_ind_situacao_associado     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/IND_SITUACAO');
            v_data_exclusao              := to_date(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/DATA_EXCLUSAO'), 'DD/MM/YYYY');
            --
            select to_number(v_data_exclusao - trunc(sysdate))
              into v_num_dias_exclusao
              from dual;
            --
            if v_ind_situacao_associado = 'E' or v_num_dias_exclusao < 1 then
               p_cod_retorno := 9;
               p_msg_retorno := 'A��o n�o permitida, o benefici�rio informado est� exclu�do.';
               return;
            end if;
            --
            if rsPrevia.data_nascimento is null then
               p_cod_retorno := 9;
               p_msg_retorno := 'Data de nascimento do Benefici�rio inv�lida'||rsPrevia.data_nascimento;
               return;
            end if;
             --
             if rsPrevia.cod_origem is null then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'Origem da solicita��o � obrigat�ria.';
                 rollback;
                 return;
             end if;
             --
             if rsPrevia.ind_tipo_reembolso is null then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'Modalidade do reembolso � obrigat�ria.';
                 rollback;
                 return;
             end if;
             --
             if rsPrevia.cod_motivo_reembolso is null then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'Motivos do reembolso � obrigat�rio.';
                 rollback;
                 return;
             end if;
             --
             if rsPrevia.ind_tipo_emissao <> 'I'  then
                if rsPrevia.ind_tipo_emissao = 'E' and rsPrevia.txt_email is null then
                     p_cod_retorno := 9;
                     p_msg_retorno := 'E-mail para envio da pr�via � obrigat�rio.';
                     rollback;
                     return;
                end if;
                --
                if rsPrevia.ind_tipo_emissao = 'F' and ( rsPrevia.txt_num_fax is null or rsPrevia.txt_ddd_fax is null ) then
                     p_cod_retorno := 9;
                     p_msg_retorno := 'N�mero de Fax para envio da pr�via n�o indormado ou inv�lido.';
                     rollback;
                     return;
                end if;
             end if;
             --
             if rsPrevia.cod_inspetoria_ts_abertura is null then
                 p_cod_retorno := 9;
                 p_msg_retorno := 'Filial / Unidade da abertura � obrigat�ria.';
                 rollback;
                 return;
             end if;
             --Validar informa��es do Solicitante
            if (rsSolicitante.nome_solicitante is null and rsSolicitante.num_insc_fiscal is not null ) or
                (rsSolicitante.nome_solicitante is not null and rsSolicitante.num_insc_fiscal is null) then
              p_cod_retorno := 9;
              p_msg_retorno := 'Dados do Solicitante incompleto.';
              rollback;
              return;
            end if;

            if rsSolicitante.num_insc_fiscal is not null then
                 if rsSolicitante.ind_tipo_pessoa = 'F' AND NVL(rsSolicitante.num_insc_fiscal,0) <> 0 then
                     --
                     select /*rb_reembolso.GravaReembolso*/
                            TS_VALIDA_CPF(rsSolicitante.num_insc_fiscal)
                     into   v_result
                     from   dual;
                     --
                     if v_result = 'NOK' then
                         p_cod_retorno := 9;
                         p_msg_retorno := 'CPF do solicitante inv�lido.';
                         xmldom.freeDocument(v_doc);
                         return;
                     end if;
                     --
                 elsif rsSolicitante.ind_tipo_pessoa = 'J' AND NVL(rsSolicitante.num_insc_fiscal,0) <> 0 then
                     --
                     select /*rb_reembolso.GravaReembolso*/ TS_VALIDA_CNPJ(lpad(rsSolicitante.num_insc_fiscal,14,'0'))
                     into   v_result
                     from   dual;
                     --
                     if v_result = 'NOK' then
                         p_cod_retorno := 9;
                         p_msg_retorno := 'CNPJ do solicitante inv�lido.';
                         xmldom.freeDocument(v_doc);
                         return;
                     end if;
                 end if;
                 --
                 if rsSolicitante.Sigla_Conselho = 'CRM' then
                   --
                   if rsSolicitante.ind_tipo_pessoa = 'F' AND rsSolicitante.Num_Crm is null then
                       p_cod_retorno := 9;
                       p_msg_retorno := 'N�mero do CRM n�o informado';
                       xmldom.freeDocument(v_doc);
                       return;
                   end if;
                   --
                   if rsSolicitante.sgl_uf_conselho is null then
                       p_cod_retorno := 9;
                       p_msg_retorno := 'UF do conselho n�o informado';
                       xmldom.freeDocument(v_doc);
                       return;
                   end if;
                   --
                 end if;
                 --
            end if;

        end if;
         -- ##############################################################################
         --                           INCLUSAO / ALTERA��O DA PREVIA
         -- ##############################################################################
         if v_ind_acao = 'I' then
             begin
                 INSERT INTO pedido_reembolso_previa
                 VALUES rsPrevia;
                 TS.AGENDA_NOTIFICACAO_PREVIA_RB(rsPrevia.num_reembolso, 2,rsPrevia.ind_situacao);
             exception
                 when others then
                     p_cod_retorno := 9;
                     p_msg_retorno := 'Erro ao incluir pr�via de reembolso: ' || sqlerrm;
                     rollback;
                     return;
             end;

             --Gerar Ocorr�ncia de INCLUS�O
             GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,1,rsPrevia.txt_observacao,rsPrevia.txt_observacao_operadora,v_cod_usuario,v_cod_retorno,v_msg_retorno);
             if v_cod_retorno <> 0 then
                 p_cod_retorno := v_cod_retorno;
                 p_msg_retorno := v_msg_retorno;
                 rollback;
                 return;
             end if;


         else
             begin

                 update /*rb_previa_reembolso.GravaPrevia*/
                        pedido_reembolso_previa
                 set    row = rsPrevia
                 where  num_reembolso = rsPrevia.num_reembolso;
                 TS.AGENDA_NOTIFICACAO_PREVIA_RB(rsPrevia.num_reembolso, 2,rsPrevia.ind_situacao);
             exception
                 when others then
                     p_cod_retorno := 9;
                     p_msg_retorno := v_msg_retorno_erro || p_num_reembolso || ': ' || sqlerrm;
                     rollback;
                     return;
             end;
            --
            processa_liberacao_glosa ( p_cod_retorno
                                     , p_msg_retorno
                                     , ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/XML_GLOSA')
                                     , p_num_reembolso
                                     , v_cod_usuario );

            if p_cod_retorno <> 0 then
                rollback;
                return;
            end if;

            -- se n�o for aprova��o gera ocorrencia
            if not ( v_ind_acao = 'FN' ) then

                GeraOcorrencia( p_num_reembolso
                              , p_num_reembolso_ans
                              , v_tipo_ocorrecia
                              , rsPrevia.txt_observacao
                              , 'Motivo : '||v_desc_motivo||'. '||rsPrevia.txt_observacao_operadora || case when rsPrevia.Ind_Tipo_Emissao = 'E' then  ' Solicita��o de envio de Email foi cadastrada com sucesso. Enviado para o email: '||rsPrevia.txt_email else null end
                              , v_cod_usuario
                              , v_cod_retorno
                              , v_msg_retorno);
            end if;

            if v_cod_retorno <> 0 then
                p_cod_retorno := v_cod_retorno;
                p_msg_retorno := v_msg_retorno;
                rollback;
                return;
            end if;
         end if;
         --
         v_posicao := 45;
         --
--        if v_ind_acao in ('I','A') then

            --ITENS
            ------------------------------------------------------------------------
            --- Verificar Procedimento Principal e validar duplicidade de item
            ------------------------------------------------------------------------
            for x in 1 .. nvl(v_qtd_procedimento,0) loop
                if ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_PRINCIPAL_' || x) = 'S' then
                    v_cod_procedimento_principal := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/ITEM_MEDICO_' || x);
                end if;
            end loop;
            v_posicao := 46;
            ------------------------------------------------------------------------
            --- Recuperar o procedimento principal antigo
            ------------------------------------------------------------------------
            if v_ind_acao <> 'I' then

                begin
                    select distinct cod_procedimento
                      into v_cod_procedimento_princ_ant
                      from procedimento_reembolso_previa
                     WHERE num_reembolso = p_num_reembolso
                       AND ind_principal = 'S';
                exception
                    when no_data_found then
                        v_cod_procedimento_princ_ant := '';
                end;

                v_posicao := 47;
                --Gerar ocorr�ncia do indicador de procedimento principal
                if NVL(v_cod_procedimento_princ_ant,'X') <> 'X' AND NVL(v_cod_procedimento_princ_ant,'X') <> NVL(v_cod_procedimento_principal,'X') then
                    GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,15,rsPrevia.txt_observacao,'O procedimento ' || v_cod_procedimento_princ_ant || ' deixou de ser o procedimento principal' || chr(13) || rsPrevia.txt_observacao_previa, v_cod_usuario,v_cod_retorno,v_msg_retorno);
                    if v_cod_retorno <> 0 then
                        p_cod_retorno := v_cod_retorno;
                        p_msg_retorno := v_msg_retorno;
                        rollback;
                        return;
                    end if;
                end if;
            end if;

            v_posicao := 48;

            for x in 1 .. nvl(v_qtd_procedimento,0) loop

                v_posicao := 71;
                v_ind_vazio := 'N';

                v_ind_acao_procedimento                         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_ACAO_PROCEDIMENTO_' || x);

                rsPreviaProcedimento.cod_procedimento_cm        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/ITEM_MEDICO_' || x);
                rsPreviaProcedimento.cod_procedimento           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_PROCEDIMENTO_' || x);
                num_seq_itens_proc                              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/NUM_SEQ_ITENS_PROC_' || x);
                v_ind_excluir_proc                              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_EXCLUIR_PROC_' || x);

                --caso o procedimento venha vazio verifica se tinha procedimento antes e exclui os procedimento relacionados ao campo
                if NVL(rsPreviaProcedimento.cod_procedimento_cm, v_item_vazio) = v_item_vazio or nvl(v_ind_excluir_proc,'N') = 'S' then
                     -- verifica se � diferente de inclus�o e o procedimento n�o � novo
                     if v_ind_acao <> 'I' and v_ind_acao_procedimento <> 'I' then
                        --
                        delete from ts.memoria_reembolso_previa
                         where num_reembolso  = p_num_reembolso
                           and num_seq_item  in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x );
                        --
                        -- deleta todas as glosas e procedimentos pois o cod_procedimento foi alterado e passado em branco
                        --
                        delete from reembolso_previa_glosa
                         where num_reembolso  = p_num_reembolso
                           and num_seq_item  in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x );
                        --
                        -- delete todos os procedimentos caso o procedimento seja alterado e passado em branco
                        --
                        delete from ts.procedimento_reembolso_previa
                         where num_reembolso = p_num_reembolso
                           and num_seq_item in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x );

                     end if;

                    --vai para o proximo item
                    GOTO proximo_item;
                end if;

                --rsPreviaProcedimento.cod_motivo_glosa_man           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_MOTIVO_GLOSA_MAN_' || x);
                --rsPreviaProcedimento.txt_motivo_glosa_man           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/TXT_MOTIVO_GLOSA_MAN_' || x);

                rsPreviaProcedimento.qtd_informado                  := NVL(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/QTD_INFORMADO_' || x),1);
                rsPreviaProcedimento.ind_via                        := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_VIA_' || x);
                rsPreviaProcedimento.ind_doppler                    := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_DOPPLER_' || x);
                rsPreviaProcedimento.cod_grupo_estatistico          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_GRUPO_ESTATISTICO_' || x );
                rsPreviaProcedimento.ind_principal                  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_PRINCIPAL_' || x);
                rsPreviaProcedimento.ind_cirurgia                   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_CIRURGIA_' || x);
                rsPreviaProcedimento.ind_situacao                   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_SITUACAO_' || x);
                rsPreviaProcedimento.ind_dobra_calculo              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_DOBRA_CALCULO_' || x);
                rsPreviaProcedimento.ind_add_anestesista            := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_ADD_ANESTESISTA_' || x);
                rsPreviaProcedimento.ind_origem_anestesista         := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_ORIGEM_ANESTESISTA_' || x);
                rsPreviaProcedimento.ind_exibe_dobra_calc           := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_EXIBE_DOBRA_CALC_' || x);
                rsPreviaProcedimento.grupo_beneficio                := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/GRUPO_BENEFICIO_' || x);
                rsPreviaProcedimento.cod_especialidade              := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_ESPECIALIDADE_' || x);

                v_qtd_participante                                  := NVL(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/QTD_PARTICIPANTE_' || x),1);
                v_posicao := 72;
                rsPreviaProcedimento.cod_usuario_atu                := v_cod_usuario;
                rsPreviaProcedimento.dt_atu                         := sysdate;
                -- Verificar se todas as vias vem preenchidas
                for i in 1 .. nvl(v_qtd_procedimento,0) loop
                    if ((num_seq_itens_proc <> ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/NUM_SEQ_ITENS_PROC_' || i))
                      and (rsPreviaProcedimento.cod_procedimento_cm = ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/ITEM_MEDICO_' || i))) then
                      --
                      if (ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_VIA_' || i) is null) then
                         v_ind_vazio := 'S';
                      end if;
                      --
                    end if;
                end loop;
                --
                v_count_proc_iguais := 0;
                -- verifica se tem algum procedimento igual ou j� existe um em branco, mesmo cod_procedimento e via
                begin
                    select sum( case when ind_via is null       then 1 else 0 end )
                         , sum( case when nvl(a.ind_via,'X') =  nvl(rsPreviaProcedimento.ind_via,'X')
                                then case when nvl(a.cod_especialidade,'X') =  nvl(rsPreviaProcedimento.cod_especialidade,'X') then 1 else 0 end
                                else 0 end )
                         , sum( case when ind_via is not null   then 1 else 0 end )
                      into v_count_proc_null
                         , v_count_proc_iguais
                         , v_count_proc_not_null
                      from procedimento_reembolso_previa a
                     where a.cod_procedimento_cm  = rsPreviaProcedimento.cod_procedimento_cm
                       and a.num_reembolso        = p_num_reembolso
                       and num_seq_item           not in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x )
                  group by a.cod_procedimento_cm;
                exception
                    when others then
                        v_count_proc_iguais     := 0;
                        v_count_proc_null       := 0;
                        v_count_proc_not_null   := 0;
                end;

                -- se tiver procedimentos iguais ou o procedimento estiver em branco e j� tiver algum em branco cadastrado
                /*if v_count_proc_iguais > 0 then
                    p_cod_retorno := '9';
                    p_msg_retorno := 'O Procedimento '||rsPreviaProcedimento.cod_procedimento_cm||' n�o pode ser inclu�do mais de uma vez com o mesmo tipo de via ou especialidade.';
                    rollback;
                    return;
                end if;*/

                -- caso esteja tentando adicionar algum procedimento com um procedimento em branco ou o contr�rio
                if ( rsPreviaProcedimento.ind_via is not null and v_count_proc_null > 0 and nvl(v_ind_vazio,'N') = 'S')
                or ( rsPreviaProcedimento.ind_via is null and v_count_proc_not_null > 0 and nvl(v_ind_vazio,'S') = 'N')  then
                    p_cod_retorno := '9';
                    p_msg_retorno := 'O Procedimento '||rsPreviaProcedimento.cod_procedimento_cm||' n�o pode ser inclu�do com o tipo de via em branco junto com outro tipo de via.';
                    rollback;
                    return;
                end if;

                if v_ind_acao_procedimento <> 'I' then
                    begin
                        SELECT DISTINCT cod_procedimento
                                      , cod_procedimento_cm
                                      , qtd_informado
                                      , ind_via
                                      , ind_situacao
                                   into rsPreviaProcedimentoAux.cod_procedimento
                                      , rsPreviaProcedimentoAux.cod_procedimento_cm
                                      , rsPreviaProcedimentoAux.qtd_informado
                                      , rsPreviaProcedimentoAux.ind_via
                                      , rsPreviaProcedimentoAux.ind_situacao
                                   FROM ts.procedimento_reembolso_previa
                                  WHERE num_reembolso = p_num_reembolso
                                    AND num_seq_item  in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x )
                                    AND rownum = 1;
                        --
                        v_ind_procedimento_alterado := 'N';
                        --
                        --Gerar ocorr�ncia de troca de procedimento
                        if rsPreviaProcedimentoAux.cod_procedimento_cm <> rsPreviaProcedimento.cod_procedimento_cm then
                            GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,6, rsPrevia.txt_observacao,rsPreviaProcedimentoAux.cod_procedimento_cm || ' foi alterado para : ' || rsPreviaProcedimento.cod_procedimento_cm || chr(13) || rsPrevia.txt_observacao_operadora, v_cod_usuario,v_cod_retorno,v_msg_retorno);
                            v_ind_procedimento_alterado := 'S';
                            if v_cod_retorno <> 0 then
                                p_cod_retorno := v_cod_retorno;
                                p_msg_retorno := v_msg_retorno;
                                rollback;
                                return;
                            end if;
                        end if;

                        --Gerar ocorr�ncia de altera��o de situa��o
                        if NVL(rsPreviaProcedimentoAux.ind_situacao,'A') <> NVL(rsPreviaProcedimento.ind_situacao,'A') then
                            if NVL(rsPreviaProcedimento.ind_situacao,'A')    = 'A' then
                                v_legenda_situacao_01 := 'Aprovada';
                            elsif NVL(rsPreviaProcedimento.ind_situacao,'A') = 'C' then
                                v_legenda_situacao_01 := 'Cancelada';
                            elsif NVL(rsPreviaProcedimento.ind_situacao,'A') = 'N' then
                                v_legenda_situacao_01 := 'Negada';
                            end if;

                            if NVL(rsPreviaProcedimentoAux.ind_situacao_funcao,'A')    = 'A' then
                                v_legenda_situacao_02 := 'Aprovada';
                            elsif NVL(rsPreviaProcedimentoAux.ind_situacao_funcao,'A') = 'C' then
                                v_legenda_situacao_02 := 'Cancelada';
                            elsif NVL(rsPreviaProcedimentoAux.ind_situacao_funcao,'A') = 'N' then
                                v_legenda_situacao_02 := 'Negada';
                            end if;

                            GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,13,rsPrevia.txt_observacao, 'Procedimento ' || rsPreviaProcedimento.cod_procedimento_cm || ' (' || v_legenda_situacao_01 || ') foi alterada para ' || v_legenda_situacao_02 || chr(13) || rsPrevia.txt_observacao_operadora, v_cod_usuario,v_cod_retorno,v_msg_retorno,rsPreviaProcedimento.num_seq_item);
                            if v_cod_retorno <> 0 then
                                p_cod_retorno := v_cod_retorno;
                                p_msg_retorno := v_msg_retorno;
                                rollback;
                                return;
                            end if;
                        end if;

                        --Gerar ocorr�ncia de altera�a� de quantidade
                        if rsPreviaProcedimentoAux.qtd_informado <> rsPreviaProcedimento.qtd_informado then
                            GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,12,rsPrevia.txt_observacao,'Procedimento ' || rsPreviaProcedimento.cod_procedimento_cm || ' (' || rsPreviaProcedimentoAux.qtd_informado || ') foi alterada para ' || rsPreviaProcedimento.qtd_informado || chr(13) || rsPrevia.txt_observacao_operadora, v_cod_usuario,v_cod_retorno,v_msg_retorno,rsPreviaProcedimento.num_seq_item);
                            if v_cod_retorno <> 0 then
                                p_cod_retorno := v_cod_retorno;
                                p_msg_retorno := v_msg_retorno;
                                rollback;
                                return;
                            end if;
                        end if;

                        --Gerar ocorr�ncia de altera�a� de via
                        if NVL(rsPreviaProcedimentoAux.ind_via,'X') <> NVL(rsPreviaProcedimento.ind_via,'X') then

                            if NVL(rsPreviaProcedimentoAux.ind_via,'X')    = 'M' then
                                v_legenda_situacao_01 := 'Mesma Via';
                            elsif NVL(rsPreviaProcedimentoAux.ind_via,'X')  = 'D' then
                                v_legenda_situacao_01 := 'Diferentes Vias';
                            elsif NVL(rsPreviaProcedimentoAux.ind_via,'X') = 'U' then
                                v_legenda_situacao_01 := 'Via �nica';
                            else
                                v_legenda_situacao_01 := 'Em branco';
                            end if;

                            if NVL(rsPreviaProcedimento.ind_via,'X')    = 'M' then
                                v_legenda_situacao_02 := 'Mesma Via';
                            elsif NVL(rsPreviaProcedimento.ind_via,'X') = 'D' then
                                v_legenda_situacao_02 := 'Diferentes Vias';
                            elsif NVL(rsPreviaProcedimento.ind_via,'X') = 'U' then
                                v_legenda_situacao_02 := 'Via �nica';
                            else
                                v_legenda_situacao_02 := 'Em branco';
                            end if;

                            GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,14, rsPrevia.txt_observacao, 'Procedimento ' || rsPreviaProcedimento.cod_procedimento_cm || ' (' || v_legenda_situacao_01 || ') foi alterada para ' || v_legenda_situacao_02 || chr(13) || rsPrevia.txt_observacao_operadora, v_cod_usuario,v_cod_retorno,v_msg_retorno,rsPreviaProcedimento.num_seq_item);
                            if v_cod_retorno <> 0 then
                                p_cod_retorno := v_cod_retorno;
                                p_msg_retorno := v_msg_retorno;
                                rollback;
                                return;
                            end if;
                        end if;

                    exception
                        when no_data_found then
                            null;
                    end;
                end if;

                v_posicao := 73;
                v_qtd_participante      := NVL(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/QTD_PARTICIPANTE_' || x),1);

                if v_qtd_participante = 0 then v_qtd_participante := 1; end if;
                --
                --apagar todas as mem�rias de calculo, as mesmas ser�o recolocadas, caso necess�rio
                delete from ts.memoria_reembolso_previa
                where  num_reembolso  = p_num_reembolso
                  and  num_seq_item in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x );

                if v_ind_acao <> 'I' and v_ind_acao_procedimento <> 'I' then
                    if v_ind_procedimento_alterado = 'S' then
                          -- deleta todas as glosas e procedimentos pois o cod_procedimento foi alterado
                          delete from reembolso_previa_glosa
                          where  num_reembolso  = p_num_reembolso
                            and  num_seq_item  in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x );

                          -- delete todos os procedimentos caso o procedimento seja alterado
                          DELETE FROM ts.procedimento_reembolso_previa
                                WHERE num_reembolso = p_num_reembolso
                                  AND num_seq_item in ( select /*+cardinality(x,10)*/ x.column_value from table( top_utl_padrao.split(num_seq_itens_proc,',')) x );
                          --
                          -- caso o procedimento seja alterado a a��o � incluir um procedimento novo
                          v_ind_acao_procedimento := 'I';
                          --
                    end if;
                end if;

                --copart
                v_posicao := 74;

                preenche_dados_copart(rsPrevia.cod_plano, rsPrevia.cod_ts_contrato, rsPrevia.num_associado, rsPreviaProcedimento.cod_procedimento_cm, v_val_fixo, v_val_per_copart, v_val_limite_copart);
                --
                for k in 1 .. nvl(v_qtd_participante,1) loop

                    v_posicao := 75;
                    rsPreviaProcedimento.num_seq_item                   := nvl(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/NUM_SEQ_ITEM_' || x || '_' || k),num_seq_itens_proc);
                    rsPreviaProcedimento.ind_funcao                     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_FUNCAO_' || x || '_' || k);
                    rsPreviaProcedimento.perc_funcao                    := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/PCT_PARTICIPACAO_' || x || '_' || k);
                    rsPreviaProcedimento.ind_tipo_composicao            := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_TIPO_COMPOSICAO_' || x || '_' || k);
                    rsPreviaProcedimento.sigla_tabela_rb                := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/SIGLA_TABELA_RB_' || x || '_' || k);
                    rsPreviaProcedimento.cod_porte_rb                   := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_PORTE_RB_' || x || '_' || k);
                    rsPreviaProcedimento.sigla_tabela_taxas             := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/SIGLA_TABELA_TAXAS_' || x || '_' || k);
                    rsPreviaProcedimento.pct_cirurgia_multipla          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/PCT_CIRURGIA_MULTIPLA_' || x || '_' || k);
                    rsPreviaProcedimento.qtd_vezes_tabela               := ts_numero_web_ponto(replace(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/QTD_VEZES_TABELA_' || x || '_' || k),',','.'),4);
                    rsPreviaProcedimento.qtd_prazo_dias                 := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/QTD_PRAZO_DIAS_' || x || '_' || k);
                    rsPreviaProcedimento.sigla_moeda                    := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/SIGLA_MOEDA_' || x || '_' || k);
                    rsPreviaProcedimento.cod_concessao                  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/COD_CONCESSAO_' || x || '_' || k);
                    rsPreviaProcedimento.pct_recibo                     := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/PCT_RECIBO_' || x || '_' || k);

                    v_ind_excluir                                  := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/IND_EXCLUIR_' || x || '_' || k);

                    if v_ind_excluir = 'S'  then
                      rsPreviaProcedimento.ind_situacao_funcao := 'E';
                    else
                      rsPreviaProcedimento.ind_situacao_funcao := 'A';
                    end if;

                    v_posicao := 76;

                    rsPreviaProcedimento.val_apresentado     := 0;
                    if v_qtd_participante = 1 then
                       if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_APRESENTADO_' || x)) then
                          rsPreviaProcedimento.val_apresentado     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_APRESENTADO_' || x),2);
                       end if;
                    else
                       if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_APRESENTADO_' || x || '_' || k)) then
                          rsPreviaProcedimento.val_apresentado     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_APRESENTADO_' || x || '_' || k),2);
                       end if;
                    end if;

                    if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_CALCULADO_' || x || '_' || k)) then
                        rsPreviaProcedimento.val_calculado     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_CALCULADO_' || x || '_' || k),2);
                    else
                        rsPreviaProcedimento.val_calculado     := 0;
                    end if;

                    if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_INFORMADO_' || x || '_' || k)) then
                        rsPreviaProcedimento.val_reembolsado     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_INFORMADO_' || x || '_' || k),2);
                    else
                        rsPreviaProcedimento.val_reembolsado     := 0;
                    end if;

                    if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_COTACAO_RB_' || x || '_' || k)) then
                        rsPreviaProcedimento.val_cotacao_rb     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_COTACAO_RB_' || x || '_' || k),4);
                    end if;

                    if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_COTACAO_TAXA_' || x || '_' || k)) then
                        rsPreviaProcedimento.val_cotacao_taxa     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_COTACAO_TAXA_' || x || '_' || k),4);
                    end if;

                    if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_LIMITE_' || x || '_' || k)) then
                        rsPreviaProcedimento.val_limite     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_LIMITE_' || x || '_' || k),2);
                    end if;

                    if TS_UTIL.IsNumber(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_FIXO_' || x || '_' || k)) then
                        rsPreviaProcedimento.val_fixo     := ts_numero_web(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/VAL_FIXO_' || x || '_' || k),2);
                    end if;

                    v_posicao := 78;
                    --
                    rsPreviaProcedimento.num_reembolso := p_num_reembolso;
                    rsPreviaProcedimento.num_reembolso_ans := p_num_reembolso_ans;

                    --copart
                    if v_val_limite_copart > 0 then
                        rsPreviaProcedimento.val_limite   :=v_val_limite_copart;
                    end if;
                    --Tratar valor de apresentado
                    v_valor_valido := rsPreviaProcedimento.val_calculado;
                    if rsPreviaProcedimento.val_apresentado > 0 then
                        if rsPreviaProcedimento.val_apresentado < rsPreviaProcedimento.val_calculado then
                           v_valor_valido := rsPreviaProcedimento.val_apresentado;
                        end if;
                    end if;

                    if v_val_fixo IS NOT NULL then
                        rsPreviaProcedimento.val_calculado_participacao := v_val_fixo;
                    else
                        rsPreviaProcedimento.val_calculado_participacao := v_valor_valido * v_val_per_copart;
                    end if;

                    if rsPreviaProcedimento.val_calculado_participacao > v_val_limite_copart and v_val_limite_copart > 0 then
                        rsPreviaProcedimento.val_calculado_participacao:= v_val_limite_copart;
                    end if;

                    if v_valor_valido >= rsPreviaProcedimento.val_calculado_participacao then
						rsPreviaProcedimento.val_reembolsado   := v_valor_valido-rsPreviaProcedimento.val_calculado_participacao;
					ELSE
						rsPreviaProcedimento.val_reembolsado   := 0;
                    end if;

                    -- #######################################################################
                    --          INCLUS�O / ALTERA��O DOS PROCEDIMENTOS
                    -- #######################################################################
                    if v_ind_acao_procedimento = 'I' then

                        select max(num_seq_item)+1
                        into v_num_seq_item_aux
                        from procedimento_reembolso_previa
                       where num_reembolso = p_num_reembolso;

                      --
                      rsPreviaProcedimento.num_seq_item :=  nvl(v_num_seq_item_aux,1);

                        begin
                            insert into procedimento_reembolso_previa
                            VALUES rsPreviaProcedimento;
                        exception
                            when dup_val_on_index then
                                p_cod_retorno := 9;
                                p_msg_retorno := 'Erro ao incluir pr�via de reembolso: ' || sqlerrm;
                                rollback;
                                return;
                            when others then
                                p_cod_retorno := 9;
                                p_msg_retorno := 'Erro ao incluir pr�via de reembolso: ' || sqlerrm;
                                rollback;
                                return;
                        end;
                    else
                         begin
                            --
                            rsPreviaProcedimento.dt_atu             := sysdate;
                            rsPreviaProcedimento.cod_usuario_atu    := v_cod_usuario;
                            --
                            UPDATE /*rb_previa_reembolso.GravaPrevia*/
                                   procedimento_reembolso_previa
                              set  row = rsPreviaProcedimento
                            WHERE  num_reembolso           = p_num_reembolso
                              AND  num_seq_item            = rsPreviaProcedimento.num_seq_item;
                        exception
                            when dup_val_on_index then
                                p_cod_retorno := 9;
                                p_msg_retorno := v_msg_retorno_erro || p_num_reembolso || ': ' || sqlerrm;
                                rollback;
                                return;
                            when others then
                                p_cod_retorno := 9;
                                p_msg_retorno := v_msg_retorno_erro || p_num_reembolso || ': ' || sqlerrm;
                                rollback;
                                return;
                        end;

                        processa_liberacao_glosa_item ( p_cod_retorno
                                                      , p_msg_retorno
                                                      , ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/XML_GLOSA_' || x || '_' || k)
                                                      , p_num_reembolso
                                                      , rsPreviaProcedimento.num_seq_item
                                                      , v_cod_usuario );
                        if p_cod_retorno <> 0 then
                            rollback;
                            return;
                        end if;

                    end if;
                    --
                    v_txt_memoria_calculo          := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ITEM/TXT_MEMORIA_CALCULO_' || x || '_' || k);
                    --
                    if v_txt_memoria_calculo is not null then
                        begin
                            insert into ts.memoria_reembolso_previa
                            (num_reembolso, num_seq_item, txt_memoria)
                            VALUES
                            (p_num_reembolso, rsPreviaProcedimento.num_seq_item, v_txt_memoria_calculo);
                        exception
                            when dup_val_on_index then
                                p_cod_retorno := 9;
                                p_msg_retorno := 'Erro ao incluir mem�ria da pr�via de reembolso: ' || sqlerrm;
                                rollback;
                                return;
                            when others then
                                p_cod_retorno := 9;
                                p_msg_retorno := 'Erro ao incluir mem�ria da pr�via de reembolso: ' || sqlerrm;
                                rollback;
                                return;
                        end;
                    end if;
                end loop;

                <<proximo_item>>
                null; -- passa para o pr�ximo item
            end loop;
            --
            -- REVALIDA TODOS OS PROCEDIMENTOS COM AS INFORMACOES DO BANCO
            --
            if nvl(rsPrevia.ind_origem_associado,'BD') = 'WS' then
                revalida_proc_reembolso_cam ( p_cod_retorno
                                            , p_msg_retorno
                                            , p_num_reembolso
                                            , v_cod_usuario
                                            );
            else
                revalida_proc_reembolso ( p_cod_retorno
                                        , p_msg_retorno
                                        , p_num_reembolso
                                        , v_cod_usuario
                                        );
            end if;

            if p_cod_retorno <> 0 then
                rollback;
                return;
            end if;
        --end if;
        -- GERA OCORRENCIA EM CASO DE ERRO NA CONSULTA DO WS
        if v_ind_erro_ws = 'S' then
            GeraOcorrencia(p_num_reembolso,p_num_reembolso_ans,56,'Erro ocorrido na chamada do WS de dados complementares do benefici�rio: ' || v_msg_erro_ws,null, v_cod_usuario,p_cod_retorno,p_msg_retorno);
            if p_cod_retorno <> 0 then
                rollback;
                return;
            end if;
        end if;
        --
        v_posicao := 70;
        --#################################################################
        --              Prepara e executa grava��o dos anexos
        --#################################################################
        v_qtd_anexo := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/QTD_ANEXO');
        --
        /*if rsPrevia.ind_tipo_reembolso != 1 and v_qtd_anexo < 1 then
             p_cod_retorno := 9;
             p_msg_retorno := 'Para as modalidades diferente de Consultas � obrigat�rio pelo menos um anexo.';
             rollback;
             return;
        end if;*/
        --
        for i in 1..v_qtd_anexo loop
            --
            if trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/NOM_ARQ_ANEXO_'||i)) is not null then
                --
                if  trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/IND_ALTERAR_'||i)) = 'S' and
                    trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/IND_EXCLUIR_'||i)) = 'S' then

                    v_nome_arquivo := trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/NOM_ARQ_ANEXO_'||i));

                    delete from reembolso_previa_anexo
                    where num_reembolso = p_num_reembolso
                      and nom_arq_anexo = v_nome_arquivo;

                elsif trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/IND_ALTERAR_'||i)) = 'N' then
                    v_nome_arquivo := trim(ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/NOM_ARQ_ANEXO_'||i));
                    --
                    -----------------------------------
                    -- INCLUIR ANEXOS
                    -----------------------------------
                    vXMLAnexo := '<?xml version="1.0" encoding="ISO-8859-1" ?>' ||
                                 '<ANEXO_PREVIA>'||
                                 '<NUM_REEMBOLSO>'||p_num_reembolso||'</NUM_REEMBOLSO>'||
                                 '<TXT_DESCRICAO>'||ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/TXT_DESCRICAO_'||i) ||'</TXT_DESCRICAO>'||
                                 '<IND_NOTA_ORIGINAL>'||ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA_ANEXO/IND_NOTA_ORIGINAL_'||i) ||'</IND_NOTA_ORIGINAL>'||
                                 '<NOM_ARQ_ANEXO>'||v_nome_arquivo||'</NOM_ARQ_ANEXO>'||
                                 '<COD_USUARIO>'||v_cod_usuario||'</COD_USUARIO>'||
                                 '</ANEXO_PREVIA>';
                    --
                    GravaAnexo ( vXMLAnexo, p_cod_retorno, p_msg_retorno);
                    if p_cod_retorno <> 0 then
                        rollback;
                        return;
                    end if;
                    --
                end if;
            end if;
            --
        end loop;

         -- monta xml para rotina de salvar o solicitante
         v_xml_solicitante := '<parametros>';
         v_xml_solicitante := v_xml_solicitante || '<cod_solicitante>'            || rsPrevia.cod_solicitante              || '</cod_solicitante>';
         v_xml_solicitante := v_xml_solicitante || '<num_reembolso>'              || rsPrevia.num_reembolso                || '</num_reembolso>';
         v_xml_solicitante := v_xml_solicitante || '<ind_tipo_pessoa>'            || rsSolicitante.ind_tipo_pessoa            || '</ind_tipo_pessoa>';
         v_xml_solicitante := v_xml_solicitante || '<num_insc_fiscal>'            || rsSolicitante.num_insc_fiscal            || '</num_insc_fiscal>';
         v_xml_solicitante := v_xml_solicitante || '<nome_solicitante>'           || rsSolicitante.nome_solicitante           || '</nome_solicitante>';
         v_xml_solicitante := v_xml_solicitante || '<sigla_conselho>'             || rsSolicitante.sigla_conselho             || '</sigla_conselho>';
         v_xml_solicitante := v_xml_solicitante || '<sgl_uf_conselho>'            || rsSolicitante.sgl_uf_conselho            || '</sgl_uf_conselho>';
         v_xml_solicitante := v_xml_solicitante || '<num_crm>'                    || rsSolicitante.num_crm                    || '</num_crm>';
         v_xml_solicitante := v_xml_solicitante || '<cod_usuario>'                || v_cod_usuario                            || '</cod_usuario>';
         --v_xml_solicitante := v_xml_solicitante || '<cod_municipio_execucao>'     || rsPrevia.cod_municipio_execucao       || '</cod_municipio_execucao>';
         v_xml_solicitante := v_xml_solicitante || '</parametros>';

         --v_posicao := 510;

         grava_solicitante_previa_reembolso  ( rsPrevia.cod_solicitante
                                              , v_cod_retorno
                                             , v_msg_retorno
                                              , v_xml_solicitante
                                              );

        --#################################################################
        --              Prepara e executa transferencia ou encamninhamento de grupo, se necess�rio
        --#################################################################
        if v_ind_acao = 'EN' then

           v_cod_grupo_encaminhamento := ts_obtem_dados_xml(v_doc,'REEMBOLSO','PREVIA/COD_GRUPO_ENCAMINHAMENTO');

           if v_cod_grupo_encaminhamento is null then
                v_cod_retorno := 9;
                v_msg_retorno := 'Grupo de encaminhamento n�o informado. ' || sqlerrm;
           else

               txtXML := '<ROOT><ENCAMINHAMENTO>' || chr(13);
               txtXML := txtXML || '<IND_TIPO_ENCAMINHAMENTO>' || v_ind_tipo_encaminhamento || '</IND_TIPO_ENCAMINHAMENTO>' || chr(13);

               for grupo
                   in ( select /*+cardinality(x,10)*/ x.column_value cod_grupo
                        from table( top_utl_padrao.split( v_cod_grupo_encaminhamento , ',' ) ) x
                      )
               loop

                txtXML := txtXML || '<ITEM>' || chr(13);
                txtXML := txtXML || '<COD_GRUPO_ENCAMINHAMENTO>' || grupo.cod_grupo  || '</COD_GRUPO_ENCAMINHAMENTO>' || chr(13);
                txtXML := txtXML || '</ITEM>' || chr(13);

               end loop;
               txtXML := txtXML || '</ENCAMINHAMENTO></ROOT>' || chr(13);

               rb_previa_reembolso.processa_encaminhamento_mult ( v_cod_retorno
                                                                , v_msg_retorno
                                                                , txtXML
                                                                , p_num_reembolso
                                                                , rsPrevia.txt_observacao_operadora
                                                                , v_cod_usuario
                                                                );


           end if;

           if v_cod_retorno <> 0 then
                p_cod_retorno := v_cod_retorno;
                p_msg_retorno := v_msg_retorno;
                rollback;
                return;
           end if;


        end if;

        --#################################################################
        --              Verifica se existe algum grupo para o usu�rio que est� analisando e processa a an�lise
        --#################################################################
        --
        begin
            select cod_tipo_usuario
            into   v_cod_tipo_usuario
            from   usuario
            where  cod_usuario = v_cod_usuario;
        exception
        when no_data_found then
            v_cod_tipo_usuario := 0;
        end;
        --
        for C_item in ( select /* rb_reembolso.processa_encaminhamento */
                                  cod_grupo_previa
                                , num_seq_liberacao
                                , rowid
                           from   pedido_reembolso_previa_grupo
                           where  ind_situacao     = '1'
                           and    num_reembolso    = p_num_reembolso
                           and    cod_grupo_previa in ( select pgp.cod_grupo_previa
                                                        from   reemb_previa_grupo_perfil pgp
                                                        where  exists        ( select cod_perfil
                                                                               from   ( select pu.cod_perfil
                                                                                        from   perfil_usuario pu
                                                                                        where  pu.cod_usuario = v_cod_usuario
                                                                                        union
                                                                                        select ptu.cod_perfil
                                                                                        from   perfil_tipo_usuario ptu
                                                                                        where  ptu.cod_tipo_usuario = v_cod_tipo_usuario
                                                                                      ) x
                                                                               where  x.cod_perfil = pgp.cod_perfil
                                                                             )
                                                     )
                         )
           loop
               --Caso exista, colocar como analisado, por�m colocando uma obs
                 update /* rb_previa_reembolso.processa_encaminhamento_mult */
                        pedido_reembolso_previa_grupo
                 set    cod_usuario_liberacao     = v_cod_usuario
                      , dt_liberacao              = sysdate
                      , txt_obs                   = substr(rsPrevia.txt_observacao,1,500)
                      , ind_situacao              = 2
                 where  rowid                     = C_item.rowid;
                 --
                 --
               if p_cod_retorno != 0 then
                  rollback;
                  return;
               end if;
               --
           end loop;

        --apagar as cr�ticas n�o liberadas  @@ faltava a exclus�o da glosa
         delete from reembolso_previa_glosa
          where num_reembolso        = p_num_reembolso
          --and  nvl(num_seq_item,0)  = 0
            and  nvl(ind_analisado,'N') = 'N';

        --#################################################################
        --              Executa valida��o de glosas, regras e de finaliza��o
        --#################################################################
        if not ( v_ind_acao = 'CA' or ( v_ind_acao = 'FN' and v_ind_tipo_finalizacao = 'R' ) ) then

            AUT_CTX_BENEFICIARIO.carga_por_uk ( p_cod_retorno
                                              , p_msg_retorno
                                              , rsPrevia.num_associado
                                              , null
                                              );
            if p_cod_retorno <> 0 then
                 rollback;
                 return;
            end if;

            rb_previa_reembolso.ValidaPrevia ( p_num_reembolso
                                             , v_cod_usuario
                                             , p_cod_retorno
                                             , p_msg_retorno
                                             );

             if p_cod_retorno <> 0 then
                 rollback;
                 return;
             end if;

             rb_verifica_regra ( p_num_reembolso
                               , v_cod_usuario
                               , p_cod_retorno
                               , p_msg_retorno
                               );

             if p_cod_retorno <> 0 then
                 rollback;
                 return;
             end if;
            --
            -- valida se sobrou alguma glosa n�o liberada na cabe�a
            -- ou se n�o h� pelo menos um procedimento liberado para finalizar
            -- valida finaliza��o se for consulta e inclus�o
            if ( v_ind_acao = 'FN' and v_ind_tipo_finalizacao = 'A' )
            or ( rsPrevia.ind_tipo_reembolso = 1 and v_ind_acao = 'I' ) then

                rb_previa_reembolso.valida_finalizacao ( p_cod_retorno
                                                       , p_msg_retorno
                                                       , p_num_reembolso
                                                       , v_cod_usuario
                                                       );

                -- se ocorrer erro e n�o for consulta e finaliza��o aprova.
                if ( v_ind_acao = 'FN' and v_ind_tipo_finalizacao = 'A' ) then

                    if p_cod_retorno <> 0 then
                        --
                        v_tipo_ocorrecia   := 2;
                        p_cod_retorno      := 2; -- coloca o c�digo retorno para ser sempre 2 neste caso de erro de finaliza��o, para atualizar a tela
                        --
                        update pedido_reembolso_previa
                        set    ind_situacao            = 1
                             , cod_usuario_deferimento = null
                             , dt_deferimento          = null
                        where  num_reembolso           = p_num_reembolso;
                    else
                        v_tipo_ocorrecia   := 3;
                        --
                        --PASSA O CAMPO VAL_REEMBOLSADO PARA 0 DE TODOS OS PROCEDIMENTOS QUE FORAM DEIXADOS GLOSADOS
                        --
                        for c_itens
                           in (select distinct cod_procedimento_cm, num_seq_item, val_reembolsado, ind_situacao, ind_situacao_funcao
                                from  ts.procedimento_reembolso_previa
                                where num_reembolso         = p_num_reembolso
                               )
                        loop
                            begin
                               --
                               v_qtd_glosa_analise := 0;
                               --
                               select COUNT (*)
                                 into v_qtd_glosa_analise
                                 from reembolso_previa_glosa   a
                                where a.num_reembolso          = p_num_reembolso
                                  and a.num_seq_item           = c_itens.num_seq_item
                                  and nvl(a.ind_analisado,'N') = 'N';
                            exception
                            when others then
                              v_qtd_glosa_analise   := 0;
                            end;
                            --
                            if v_qtd_glosa_analise > 0 or c_itens.ind_situacao IN ('N','C') or c_itens.ind_situacao_funcao = 'E' then
                               begin
                                   update procedimento_reembolso_previa
                                      set val_reembolsado  = 0
                                   where num_reembolso     = p_num_reembolso
                                     and num_seq_item      = c_itens.num_seq_item;
                               exception
                                 when others then
                                    null;
                               end;
                            end if;
                        end loop;

                        p_msg_retorno := 'Pr�via de reembolso ' || rsPrevia.Num_Reembolso_Ans || ' finalizada com sucesso.';
                    end if;
                     --
                     --

                     GeraOcorrencia( p_num_reembolso
                                   , p_num_reembolso_ans
                                   , v_tipo_ocorrecia
                                   , null
                                   , p_msg_retorno
                                   , v_cod_usuario
                                   , v_cod_retorno
                                   , v_msg_retorno);
                        --
                        if v_cod_retorno <> 0 then
                            p_cod_retorno := v_cod_retorno;
                            p_msg_retorno := v_msg_retorno;
                            rollback;
                            return;
                        end if;

                     calcula_total_pedido(p_num_reembolso);
                     --
                     commit;
                     return;
                     --
                elsif rsPrevia.ind_tipo_reembolso = 1 and v_ind_acao = 'I' then
                    -- caso seja consulta validar
                    -- caso seja 99 existe grupo pentende
                    -- ent�o n�o executar a��o, apenas cadastar a previa
                    if p_cod_retorno <> 99 then
                        if p_cod_retorno = 9 then
                            -- se cod_retorno = 9 recusar o pedido
                            -- existe glosa pendente ou valor � 0
                            v_tipo_ocorrecia                         := 5;
                            rsPrevia.ind_situacao                    := 4;
                            rsPrevia.cod_usuario_indeferimento       := v_cod_usuario;
                            rsPrevia.dt_indeferimento                := sysdate;
                        elsif p_cod_retorno = 0 then
                            -- se for 0 finalizar e aprovar a pr�via
                            v_tipo_ocorrecia                       := 3;
                            rsPrevia.ind_situacao                  := 2;
                            rsPrevia.cod_usuario_deferimento       := v_cod_usuario;
                            rsPrevia.dt_deferimento                := sysdate;
                        end if;
                         --
                         p_cod_retorno := 0;
                         begin
                           UPDATE pedido_reembolso_previa SET
                                  dt_deferimento              = rsPrevia.dt_deferimento
                                , cod_usuario_deferimento     = rsPrevia.cod_usuario_deferimento
                                , dt_indeferimento            = rsPrevia.dt_indeferimento
                                , cod_usuario_indeferimento   = rsPrevia.cod_usuario_indeferimento
                                , ind_situacao                = rsPrevia.ind_situacao
                           where  num_reembolso               = p_num_reembolso;
                         exception
                             when others then
                                 p_cod_retorno := 9;
                                 p_msg_retorno := v_msg_retorno_erro || p_num_reembolso || ': ' || sqlerrm;
                                 rollback;
                                 return;
                         end;
                        --
                        if p_cod_retorno <> 0 then
                            rollback;
                            return;
                        end if;
                        --
                        for c_itens
                           in (select distinct cod_procedimento_cm, num_seq_item, val_reembolsado, ind_situacao, ind_situacao_funcao
                                from  ts.procedimento_reembolso_previa
                                where num_reembolso         = p_num_reembolso
                               )
                        loop
                            begin
                               --
                               v_qtd_glosa_analise := 0;
                               --
                               select COUNT (*)
                                 into v_qtd_glosa_analise
                                 from reembolso_previa_glosa   a
                                where a.num_reembolso          = p_num_reembolso
                                  and a.num_seq_item           = c_itens.num_seq_item
                                  and nvl(a.ind_analisado,'N') = 'N';
                            exception
                            when others then
                              v_qtd_glosa_analise   := 0;
                            end;
                            --
                            if v_qtd_glosa_analise > 0 or c_itens.ind_situacao IN ('N','C') or c_itens.ind_situacao_funcao = 'E' then
                               begin
                                   update procedimento_reembolso_previa
                                      set val_reembolsado  = 0
                                   where num_reembolso     = p_num_reembolso
                                     and num_seq_item      = c_itens.num_seq_item;
                               exception
                                 when others then
                                    null;
                               end;
                            end if;
                        end loop;
                        --
                        GeraOcorrencia( p_num_reembolso
                                      , p_num_reembolso_ans
                                      , v_tipo_ocorrecia
                                      , null
                                      , 'Finaliza��o autom�tica. ' || case when v_tipo_ocorrecia = 5 then 'Pr�via de reembolso recusada por existir glosa(s) na cabe�a e/ou em procedimento(s).' else null end
                                      , v_cod_usuario
                                      , v_cod_retorno
                                      , v_msg_retorno);
                        --
                        if v_cod_retorno <> 0 then
                            p_cod_retorno := v_cod_retorno;
                            p_msg_retorno := v_msg_retorno;
                            rollback;
                            return;
                        end if;
                        --
                        v_ind_finalizado := 'S';

                    end if;
                else
                     p_cod_retorno := 9;
                     p_msg_retorno := 'Ocorreu um erro ao finalizar a pr�via.';
                     rollback;
                     return;
                end if;

            end if;

        end if;
        --
        calcula_total_pedido(p_num_reembolso);
        --
        xmldom.freeDocument(v_doc);

        p_cod_retorno := 0;
        if v_ind_acao = 'I' then
            if v_ind_finalizado = 'S' then
                p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso_ans || ' inclu�da e finalizada com sucesso.';
            else
                p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso_ans || ' inclu�da com sucesso.';
            end if;
        else
            p_msg_retorno := 'Pr�via de reembolso ' || rsPrevia.Num_Reembolso_Ans || v_msg_retorno_sucesso;
        end if;
        commit;
    exception
    when others then
        TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 00, 'ERRO', p_xml_dados || chr(13) || ts.ts_util.msgerro, 'IncluirPrevia - XML' );
        if nvl(v_posicao,0) > 1 then xmldom.freeDocument(v_doc);
        end if;
        rollback;

        p_cod_retorno := 9;
        p_msg_retorno := 'RB_PREVIA_REEMBOLSO::GravaPrevia -> ' || sqlerrm || ' - ' || ts.ts_util.msgerro || ' (Posi��o: ' || v_posicao || ')' ;
    end;
    ----------------------------------------------------------------------------
    ----------------------------------------------------------------------------
        ------------------------------------------------------------------------------
    -- Validar inclus�o ou altera��o do solicitante (duplicidade)
    ------------------------------------------------------------------------------
    function valida_gravacao_solicitante ( p_tab_parans  in top_utl_xml.tbl_fields )
    return pls_integer
    is
        str_sql         varchar2(4000);
        v_qtd           pls_integer;
    begin
        --
        --
        -- Valida��o de Inscri��o + Conselho
        --
        str_sql :=  rtrim (' select count(*)                                              ')
                  ||rtrim (' from   solicitante                                           ')
                  ||rtrim (' where                                                        ');
        --
        if p_tab_parans('num_insc_fiscal').valor(1) is not null then
            str_sql := str_sql || rtrim (' num_insc_fiscal        = :num_insc_fiscal      ');
        else
            str_sql := str_sql || rtrim (' 1                      = :num_insc_fiscal      '); --Deve ser 1=0 mesmo para n�o achar e passar na valida��o
        end if;
        --
        execute immediate str_sql
                     into v_qtd
                    using to_number(nvl(p_tab_parans('num_insc_fiscal').valor(1),0));
        --
        if v_qtd > 0 then return 1;
        end if;
        --
        return 0; --Valida��o OK
        --
    exception
    when others then
        ts_log_execucao ('RBM_PROTOCOLO'
                        , NULL
                        , ts_util.MsgErro
                        , str_sql
                        , 'Erro'
                        ) ;
        return 1;
    end;
     ----------------------------------------------------------------------------
    procedure RetornaDadosAssociado( p_num_associado          in  varchar2
                                   , p_data_solicitacao       in  date
                                   , p_ind_acao_judicial      in  varchar2
                                   , p_xml_retorno            out clob
                                   , p_cod_ts                 in  number default null)

    is

    --Declara��es:
    v_posicao               number;
    v_parametro             varchar2(4000);
    v_SQL                   varchar2(32000);

    v_item_vazio            varchar2(3) := '���';

    v_cod_ts                associado.cod_ts%type;
    v_cod_entidade_ts       associado.cod_entidade_ts%type;
    v_cod_entidade_ts_tit   associado.cod_entidade_ts%type;
    v_data_exclusao         associado.data_exclusao%type;
    v_ind_situacao          associado.ind_situacao%type;
    v_cod_ts_destino        associado.cod_ts_destino%type;

    v_cod_tipo_contrato     contrato_empresa.cod_tipo_contrato%type;
    v_cod_titular_contrato  contrato_empresa.cod_titular_contrato%type;
    v_cod_empresa           associado.cod_empresa%type;

    v_cod_retorno           number;
    v_msg_retorno           varchar2(4000);
    v_complemento_erro      varchar2(10000);

    qryCtx                  DBMS_XMLGEN.ctxHandle;

    xml_contato clob;
    num_ddd_tel     beneficiario_contato.num_ddd%type;
    num_tel         beneficiario_contato.num_telefone%type;
    num_ddd_cel     beneficiario_contato.num_ddd%type;
    num_cel         beneficiario_contato.num_telefone%type;
    num_ddd_fax     beneficiario_contato.num_ddd%type;
    num_fax         beneficiario_contato.num_telefone%type;
    end_email       beneficiario_contato.end_email%type;
    end_email_tit   beneficiario_contato.end_email%type;

    qtd_contato            number;
    qtd_reemb_beneficiario  number;
    v_ind_acao_judicial     varchar2(1);
    begin

        --In�cio
        v_posicao := 0;
        v_parametro := 'p_num_associado = ' || p_num_associado;
        v_parametro := v_parametro || CHR(13) || 'p_cod_ts = ' || p_cod_ts;
        v_parametro := v_parametro || CHR(13) || 'p_data_solicitacao = ' || p_data_solicitacao;
        v_parametro := v_parametro || CHR(13) || 'p_ind_acao_judicial= ' || p_ind_acao_judicial;

        if NVL(p_cod_ts,0) = 0 then --Recuperar o COD-TS
            v_posicao := 1;
            --Valida��o
            IF NVL(p_num_associado,v_item_vazio) = v_item_vazio THEN
                v_msg_retorno := 'N�mero do Benefici�rio n�o informado';
                goto trata_retorno_erro;
            END IF;

            v_posicao := 2;
            --Recuperar os parametros
            v_posicao := 3;
            --Recuperar o COD_TS
            begin
                begin
                    select cod_ts, cod_entidade_ts, cod_entidade_ts_tit
                    into   v_cod_ts, v_cod_entidade_ts, v_cod_entidade_ts_tit
                    from   beneficiario
                    where  num_associado = p_num_associado;
                exception
                when no_data_found then
                        null;
                end;
            end;
        else
            v_cod_ts := p_cod_ts;
            --
            begin
                select cod_entidade_ts, cod_entidade_ts_tit
                into   v_cod_entidade_ts, v_cod_entidade_ts_tit
                from   beneficiario
                where  cod_ts = v_cod_ts;
            exception
                when others then
                    null;
            end;
            --
        end if;

        v_ind_acao_judicial := nvl(p_ind_acao_judicial,'N');

        IF NVL(v_cod_ts,0) = 0 THEN
            v_msg_retorno := 'Benefici�rio n�o encontrado..';
            goto trata_retorno_erro;
        END IF;


        begin
           select
              COUNT(1) QtdReembolso
              into
              qtd_reemb_beneficiario
          from
              TS.PEDIDO_REEMBOLSO
          where
            ind_situacao not in (26, 32, 33, 34, 35, 36, 39, 42, 46)
            and num_associado = p_num_associado
            and (dt_pedido >= sysdate - 90)
            and dt_pedido <= sysdate;
        exception
        when no_data_found then
          null;
        end;

        v_posicao := 10;
        --Recuperar os dados do associado

        BEGIN
            v_SQL := v_SQL || ' SELECT 0 COD_RETORNO';
            v_SQL := v_SQL || '      , ''BD'' IND_ORIGEM_ASSOCIADO';
            v_SQL := v_SQL || '      , trunc(sysdate) data_atual';
            --DADOS DO ASSOCIADO
            v_SQL := v_SQL || '      , a.cod_ts';
            v_SQL := v_SQL || '      , a.num_associado';
            v_SQL := v_SQL || '      , a.nome_associado';
            v_SQL := v_SQL || '      , a.ind_apos_dem';
            v_SQL := v_SQL || '      , ' || qtd_reemb_beneficiario || ' qtd_reemb_beneficiario';
            v_SQL := v_SQL || '      , ''' || v_ind_acao_judicial    || ''' indAcaoJudicialCliente';
            --v_SQL := v_SQL || '      , d.nome_entidade nome_associado ';
            v_SQL := v_SQL || '      , d.ind_sexo';
            v_SQL := v_SQL || '      , TO_CHAR(d.num_cpf) num_cpf';
            v_SQL := v_SQL || '      , a.ind_situacao';
            v_SQL := v_SQL || '      , a.tipo_associado';
            v_SQL := v_SQL || '      , sa.nom_situacao nom_situacao_associado';
            v_SQL := v_SQL || '      , a.cod_lotacao_ts';
            v_SQL := v_SQL || '      , TO_CHAR (a.data_inclusao, '||chr(39)||'DD/MM/YYYY'||chr(39)||') data_inclusao';
            v_SQL := v_SQL || '      , TO_CHAR (a.data_exclusao, '||chr(39)||'DD/MM/YYYY'||chr(39)||') data_exclusao';
            v_SQL := v_SQL || '      , TO_CHAR (a.data_admissao, '||chr(39)||'DD/MM/YYYY'||chr(39)||') data_admissao';
            v_SQL := v_SQL || '      , TO_CHAR (d.data_nascimento, '||chr(39)||'DD/MM/YYYY'||chr(39)||') data_nascimento';
            v_SQL := v_SQL || '      , ts_calcula_idade (d.data_nascimento,TRUNC (SYSDATE),'||chr(39)||'A'||chr(39)||') idade_associado';
            --DADOS DO TITULAR
            v_SQL := v_SQL || '      , atit.num_associado num_associado_tit';
            v_SQL := v_SQL || '      , atit.nome_associado nome_associado_tit';
            v_SQL := v_SQL || '      , TO_CHAR(betit.num_cpf) num_cpf_tit ';
            v_SQL := v_SQL || '      , a.cod_ts_tit';
            v_SQL := v_SQL || '      , a.cod_entidade_ts_tit';
            v_SQL := v_SQL || '      , TO_CHAR (betit.data_nascimento, '||chr(39)||'DD/MM/YYYY'||chr(39)||') data_nascimento_tit';
            v_SQL := v_SQL || '      , ts_calcula_idade (betit.data_nascimento,TRUNC (SYSDATE),'||chr(39)||'A'||chr(39)||') idade_titular';
            -- DADOS DO PLANO
            v_SQL := v_SQL || '      , a.cod_plano';
            v_SQL := v_SQL || '      , p.nome_plano';
            v_SQL := v_SQL || '      , p.ind_cobertura_internacional';
            v_SQL := v_SQL || '      , p.ind_acomodacao';
            v_SQL := v_SQL || '      , p.cod_marca';
            v_SQL := v_SQL || '      , p.ind_regulamentado';
            v_SQL := v_SQL || '      , c.data_adaptacao';
            v_SQL := v_SQL || '      , (select ''S'' ';
            v_SQL := v_SQL || '           from contrato_beneficio cb';
            v_SQL := v_SQL || '          where cb.cod_ts_contrato = c.cod_ts_contrato';
            v_SQL := v_SQL || '            and cb.cod_aditivo = 238';
            v_SQL := v_SQL || '            and rownum <= 1) tem_aditivo';
            --DADOS BANC�RIOS ASSOCIADO
            v_SQL := v_SQL || '      , ba.nome_banco';
            v_SQL := v_SQL || '      , a.ind_tipo_conta_reemb';
            v_SQL := v_SQL || '      , a.cod_banco_reemb             cod_banco';
            v_SQL := v_SQL || '      , a.cod_agencia_reemb           cod_agencia_bancaria';
            v_SQL := v_SQL || '      , a.num_dv_agencia_reemb        num_dv_agencia';
            v_SQL := v_SQL || '      , a.num_conta_corrente_reemb    num_cco';
            v_SQL := v_SQL || '      , a.num_dv_cc_reemb             num_cco_dv';
            --DADOS BANC�RIOS TITULAR
            v_SQL := v_SQL || '      , ba_tit.nome_banco             nome_banco_tit';
            v_SQL := v_SQL || '      , atit.ind_tipo_conta_reemb     ind_tipo_conta_reemb_tit';
            v_SQL := v_SQL || '      , atit.cod_banco_reemb          cod_banco_tit';
            v_SQL := v_SQL || '      , atit.cod_agencia_reemb        cod_agencia_bancaria_tit';
            v_SQL := v_SQL || '      , atit.num_dv_agencia_reemb     num_dv_agencia_tit';
            v_SQL := v_SQL || '      , atit.num_conta_corrente_reemb num_cco_tit';
            v_SQL := v_SQL || '      , atit.num_dv_cc_reemb          num_cco_dv_tit';
            v_SQL := v_SQL || '      , a.cod_dependencia';
            -- DADOS DE SITUA��O ESPECIAL
            v_SQL := v_SQL || '      , a.cod_situacao_esp';
            v_SQL := v_SQL || '      , se.nome_situacao_esp';
            v_SQL := v_SQL || '      , se.nom_imagem';
            -- REDE, OPERADORA, UNIDADE E FILIAL DO ASSOCIADO
            v_SQL := v_SQL || '      , r.nom_rede';
            v_SQL := v_SQL || '      , r.cod_rede';

            --v_SQL := v_SQL || '      , ts.rb_previa_reembolso.retorna_cod_operadora(a.cod_operadora,r.cod_rede) cod_operadora';   -- retorna o c�digo da operadora
            --v_SQL := v_SQL || '      , ts.rb_previa_reembolso.retorna_nome_operadora(a.cod_operadora,r.cod_rede) nom_operadora'; -- retorna o nome da operadora

            v_SQL := v_SQL || '      , ts.rb_previa_reembolso.retorna_cod_operadora(p.cod_operadora,r.cod_rede) cod_operadora';   -- retorna o c�digo da operadora
            v_SQL := v_SQL || '      , ts.rb_previa_reembolso.retorna_nome_operadora(p.cod_operadora,r.cod_rede) nom_operadora'; -- retorna o nome da operadora


            v_SQL := v_SQL || '      , i.cod_inspetoria_ts';
            v_SQL := v_SQL || '      , i.nome_inspetoria';
            v_SQL := v_SQL || '      , s.nome_sucursal';
            -- DADOS DE EMPRESA E CONTRATO
            v_SQL := v_SQL || '      , a.ind_tipo_pessoa_contrato tipo_pessoa_contrato';
            v_SQL := v_SQL || '      , a.cod_ts_contrato';
            v_SQL := v_SQL || '      , c.num_contrato';
            v_SQL := v_SQL || '      , nvl(c.cod_grupo_empresa,0) cod_grupo_empresa';
            v_SQL := v_SQL || '      , a.cod_empresa';
            v_SQL := v_SQL || '      , c.tipo_preco ind_tipo_preco';
            v_SQL := v_SQL || '      , c.cod_tipo_contrato';
            v_SQL := v_SQL || '      , ge.nome_grupo_empresa';
            v_SQL := v_SQL || '      , ( select es.nome_entidade';
            v_SQL := v_SQL || '            from entidade_sistema es';
            v_SQL := v_SQL || '           where es.cod_entidade_ts = c.cod_titular_contrato';
            v_SQL := v_SQL || '           union';
            v_SQL := v_SQL || '          select be.nome_entidade';
            v_SQL := v_SQL || '            from beneficiario_entidade be';
            v_SQL := v_SQL || '           where be.cod_entidade_ts = c.cod_titular_contrato_pf ) nome_contrato';
            v_SQL := v_SQL || '      , NVL (c.ind_retroatividade, '||chr(39)||'N'||chr(39)||') ind_retroatividade';
            v_SQL := v_SQL || '      , c.cod_titular_contrato';
            v_SQL := v_SQL || '      , NVL (c.qtd_prazo_entrega_recibo,0) qtd_prazo_entrega_recibo';
            v_SQL := v_SQL || '      , TO_CHAR (c.data_inicio_vigencia, '||chr(39)||'DD/MM/YYYY'||chr(39)||') data_inicio_vigencia';
            v_SQL := v_SQL || '      , re.ind_classificacao';
            v_SQL := v_SQL || '      , re.nome_tipo_empresa';
            -- DADOS DO WS, COLOCADO AQUI COMO NULL PARA N�O GERAR ERRO
            v_SQL := v_SQL || '      , null cod_acao_ts';
            v_SQL := v_SQL || '      , null num_processo';
            v_SQL := v_SQL || '      , null txt_obs_judicial';
            v_SQL := v_SQL || '      , null ind_pode_courrier';
            v_SQL := v_SQL || '      , null num_centro_custo';
            v_SQL := v_SQL || '      , null ind_erro_ws';
            v_SQL := v_SQL || '      , null msg_erro_ws';
            v_SQL := v_SQL || '      , null cep_courrier';
            v_SQL := v_SQL || '      , null uf_courrier';
            v_SQL := v_SQL || '      , null endereco_courrier';
            v_SQL := v_SQL || '      , null cidade_courrier';
            v_SQL := v_SQL || '      , null cod_cidade_courrier';
            v_SQL := v_SQL || '      , null bairro_courrier';
            v_SQL := v_SQL || '      , null cod_bairro_courrier';
            v_SQL := v_SQL || '      , null complemento_courrier';
            v_SQL := v_SQL || '      , null cep_courrier_cb';
            v_SQL := v_SQL || '      , null uf_courrier_cb';
            v_SQL := v_SQL || '      , null endereco_courrier_cb';
            v_SQL := v_SQL || '      , null cidade_courrier_cb';
            v_SQL := v_SQL || '      , null cod_cidade_courrier_cb';
            v_SQL := v_SQL || '      , null bairro_courrier_cb';
            v_SQL := v_SQL || '      , null cod_bairro_courrier_cb';
            v_SQL := v_SQL || '      , null complemento_courrier_cb';

            -- DATA DE VIGENCIA DO CONTRATO
            /*
            v_SQL := v_SQL || '      , to_char( ( select max(dt_ini_cobranca)';
            v_SQL := v_SQL || '            from contrato_cobranca';
            v_SQL := v_SQL || '           where cod_ts_contrato    = a.cod_ts_contrato';
            v_SQL := v_SQL || '             and dt_ini_cobranca   <= sysdate ), '||chr(39)||'DD/MM/YYYY'||chr(39)||')  dt_ini_vigencia';
            */
            v_SQL := v_SQL || '      , to_char(  decode( a.cod_operadora,';
            v_SQL := v_SQL || '                        14, ( select max(dt_ini_cobranca)';
            v_SQL := v_SQL || '                                from ts.contrato_cobranca cc';
            v_SQL := v_SQL || '                               where cc.cod_ts_contrato in nvl(';
            v_SQL := v_SQL || '                                     ( select cod_ts_contrato ';
            v_SQL := v_SQL || '                                         from ts.grupo_empresa';
            v_SQL := v_SQL || '                                        where cod_ts_contrato in ( ';
            v_SQL := v_SQL || '                                                    select cod_ts_contrato ';
            v_SQL := v_SQL || '                                                      from contrato_empresa ';
            v_SQL := v_SQL || '                                                     where cod_grupo_empresa = c.cod_grupo_empresa )';
            v_SQL := v_SQL || '                              ), a.cod_ts_contrato ) ) ';
            v_SQL := v_SQL || '                           ,( select max(dt_ini_cobranca)';
            v_SQL := v_SQL || '                                from contrato_cobranca';
            v_SQL := v_SQL || '                               where cod_ts_contrato    = a.cod_ts_contrato';
            v_SQL := v_SQL || '                                and dt_ini_cobranca   <= sysdate )';
            v_SQL := v_SQL || '                        ) , ''DD/MM/YYYY'' )   dt_ini_vigencia';

            -- VERIFICA SE EXISTE REGRAS DE REEMBOLSO PARA O CONTRATO
            v_SQL := v_SQL || '      , case when ( select count(*) from reembolso_contrato ';
            v_SQL := v_SQL || '                    where cod_ts_contrato = a.cod_ts_contrato ';
            v_SQL := v_SQL || '                    and cod_plano = a.cod_plano ) = 0 then ''N'' ';
            v_SQL := v_SQL || '        else ''S'' end ind_plano_com_reembolso ';
            -- TABELAS
            v_SQL := v_SQL || '  FROM  associado a';
            v_SQL := v_SQL || '      , contrato_empresa c';
            v_SQL := v_SQL || '      , beneficiario_entidade d';
            v_SQL := v_SQL || '      , beneficiario_entidade betit';
            v_SQL := v_SQL || '      , plano_medico p';
            v_SQL := v_SQL || '      , situacao_contrato sc';
            v_SQL := v_SQL || '      , situacao_especial se';
            v_SQL := v_SQL || '      , situacao_associado sa';
            --v_SQL := v_SQL || '      , operadora o';
            v_SQL := v_SQL || '      , plano_rede_atendimento pr';
            v_SQL := v_SQL || '      , rede_atendimento r';
--            v_SQL := v_SQL || '      , entidade_sistema estit';
            v_SQL := v_SQL || '      , associado atit';
            v_SQL := v_SQL || '      , regra_empresa re';
            v_SQL := v_SQL || '      , grupo_empresa ge';
            v_SQL := v_SQL || '      , contrato_grupo cg';
            v_SQL := v_SQL || '      , inspetoria i';
            v_SQL := v_SQL || '      , sucursal s';
--            v_SQL := v_SQL || '      , acao_jud_pgto aj';
            v_SQL := v_SQL || '      , banco ba';
            v_SQL := v_SQL || '      , banco ba_tit';
            v_SQL := v_SQL || ' WHERE a.cod_ts                  = :cod_ts';
            v_SQL := v_SQL || '   AND a.cod_ts_contrato         = c.cod_ts_contrato';
            v_SQL := v_SQL || '   AND a.cod_entidade_ts         = d.cod_entidade_ts';
            v_SQL := v_SQL || '   AND a.cod_plano               = p.cod_plano';
            v_SQL := v_SQL || '   AND c.ind_situacao            = sc.ind_situacao';
            v_SQL := v_SQL || '   AND a.ind_situacao            = sa.ind_situacao';
            v_SQL := v_SQL || '   AND a.cod_situacao_esp        = se.cod_situacao_esp(+)';
            v_SQL := v_SQL || '   AND c.tipo_empresa            = re.tipo_empresa(+)';
            --v_SQL := v_SQL || '   AND a.cod_operadora           = o.cod_operadora(+)';
            v_SQL := v_SQL || '   AND c.cod_ts_contrato         = cg.cod_ts_contrato(+)';
            v_SQL := v_SQL || '   AND cg.cod_grupo_empresa      = ge.cod_grupo_empresa(+)';
            v_SQL := v_SQL || '   AND a.cod_plano               = pr.cod_plano';
            v_SQL := v_SQL || '   AND pr.cod_rede               = r.cod_rede';
            v_SQL := v_SQL || '   AND a.cod_inspetoria_ts       = i.cod_inspetoria_ts';
            v_SQL := v_SQL || '   AND a.cod_sucursal            = s.cod_sucursal';
            v_SQL := v_SQL || '   AND a.cod_ts_tit              = atit.cod_ts';
--            v_SQL := v_SQL || '   AND atit.cod_entidade_ts      = estit.cod_entidade_ts';
            v_SQL := v_SQL || '   AND atit.cod_entidade_ts      = betit.cod_entidade_ts';
            v_SQL := v_SQL || '   AND a.cod_banco_reemb         = ba.cod_banco(+)';
            v_SQL := v_SQL || '   AND atit.cod_banco_reemb      = ba_tit.cod_banco(+)';
--            v_SQL := v_SQL || '   AND a.cod_ts                  = aj.cod_ts(+)';
--            v_SQL := v_SQL || '   AND rownum                    = 1';
            v_posicao := 12;

            qryCtx := dbms_xmlgen.newContext(v_SQL);
            dbms_xmlgen.setBindValue(qryCtx, 'cod_ts', v_cod_ts);
            dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
            dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
            --dbms_xmlgen.setRowTag (qryCtx, 'EMPLOYEE');
            p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
            dbms_xmlgen.closeContext(qryCtx);

            -- DADOS DE CONTATO DO ASSOCIADO
            begin
                for c in ( select * from beneficiario_contato where cod_entidade_ts = v_cod_entidade_ts ) loop

                    CASE c.ind_class_contato
                       WHEN 'T' THEN
                        num_ddd_tel := c.num_ddd;
                        num_tel     := c.num_telefone;
                       WHEN 'C' THEN
                        num_ddd_cel := c.num_ddd;
                        num_cel     := c.num_telefone;
                       WHEN 'F' THEN
                        num_ddd_fax := c.num_ddd;
                        num_fax     := c.num_telefone;
                       WHEN 'E' THEN
                        end_email   := c.end_email;
                       ELSE null;
                    END CASE;
                end loop;
            --
            -- Caso o associado n�o tenha dados do contrato, ser� atribuido os dados do titular
            --
              select count(*) into qtd_contato from  beneficiario_contato where cod_entidade_ts = v_cod_entidade_ts;

              if qtd_contato = 0 then
                 for c in ( select * from beneficiario_contato where cod_entidade_ts = v_cod_entidade_ts_tit ) loop

                    CASE c.ind_class_contato
                       WHEN 'T' THEN
                        num_ddd_tel := c.num_ddd;
                        num_tel     := c.num_telefone;
                       WHEN 'C' THEN
                        num_ddd_cel := c.num_ddd;
                        num_cel     := c.num_telefone;
                       WHEN 'F' THEN
                        num_ddd_fax := c.num_ddd;
                        num_fax     := c.num_telefone;
                       WHEN 'E' THEN
                        end_email   := c.end_email;
                       ELSE null;
                    END CASE;
                end loop;
             end if;

            exception
                when others then
                null;
            end;

            if v_cod_entidade_ts_tit <> v_cod_entidade_ts and end_email is null then
                begin
                    select end_email
                      into end_email_tit
                      from beneficiario_contato
                     where cod_entidade_ts = v_cod_entidade_ts_tit
                       and ind_class_contato = 'E';
                exception
                    when others then
                    null;
                end;
            end if;
            --
            xml_contato := '<CONTATO>';
            xml_contato := xml_contato || '<NUM_DDD_TELEFONE>'  || num_ddd_tel      || '</NUM_DDD_TELEFONE>';
            xml_contato := xml_contato || '<NUM_TELEFONE>'      || num_tel          || '</NUM_TELEFONE>';
            xml_contato := xml_contato || '<DDD_COMERCIAL></DDD_COMERCIAL>';
            xml_contato := xml_contato || '<TEL_COMERCIAL></TEL_COMERCIAL>';
            xml_contato := xml_contato || '<NUM_DDD_CELULAR>'   || num_ddd_cel      || '</NUM_DDD_CELULAR>';
            xml_contato := xml_contato || '<NUM_CELULAR>'       || num_cel          || '</NUM_CELULAR>';
            xml_contato := xml_contato || '<NUM_DDD_FAX>'       || num_ddd_fax      || '</NUM_DDD_FAX>';
            xml_contato := xml_contato || '<NUM_FAX>'           || num_fax          || '</NUM_FAX>';
            xml_contato := xml_contato || '<END_EMAIL>'         || end_email        || '</END_EMAIL>';
            xml_contato := xml_contato || '<END_EMAIL_TIT>'     || end_email_tit    || '</END_EMAIL_TIT>';
            xml_contato := xml_contato || '</CONTATO>';
            --

            add_xml(p_xml_retorno,xml_contato, 'ROWSET/ROW');

            --
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                v_msg_retorno := 'Benefici�rio n�o encontrado.';
                goto trata_retorno_erro;
        END;

        v_posicao := 20;

        return;

        <<trata_retorno_erro>>

        v_cod_retorno := '9';

        if ts.top_utl_padrao.msgerro is not null then
            v_complemento_erro := ' - ' || ts.top_utl_padrao.msgerro;
        end if;

        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<ROWSET>';
        p_xml_retorno := p_xml_retorno || '<ROW>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>' || v_cod_retorno || '</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || v_msg_retorno || v_complemento_erro  || '</MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '</ROW>';
        p_xml_retorno := p_xml_retorno || '</ROWSET>';

        return;

    EXCEPTION
        WHEN OTHERS THEN

            --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || 'Parametros:' || chr(13) || v_parametro, 'RetornaDadosAssociado' );
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', v_SQL, 'RetornaDadosAssociado - SQL' );

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<ROWSET>';
            p_xml_retorno := p_xml_retorno || '<ROW>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</ROW>';
            p_xml_retorno := p_xml_retorno || '</ROWSET>';

            return;
    END;
    --
    --
    ----------------------------------------------------------------------------
    -- Retorna xml com as informa��es do anexo da pr�via
    ----------------------------------------------------------------------------
    procedure RetornaAnexo(p_num_reembolso in  varchar2
                          ,p_xml_retorno   out clob
                          ,p_nome_arquivo  in  varchar2 default null)

    is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    v_item_vazio            varchar2(3) := '���';
    v_tamanho_reembolso     number;
    v_num_reembolso         varchar2(20);

    begin
        --In�cio
        v_posicao := 0;

        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<ANEXO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>3 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</DADOS></ANEXO>';
            return;
        END IF;
        --
        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        v_num_reembolso := p_num_reembolso;
        --
        if v_tamanho_reembolso > 15 then
          select p.num_reembolso
            into v_num_reembolso
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        end if;
        --
        --
        v_posicao := 15;

        --Monta XML de anexos
        v_SQL := '';
        v_SQL := v_SQL || 'SELECT   0 cod_retorno, a.nom_arq_anexo, a.cod_usuario, a.txt_descricao, decode(ind_nota_original,''S'',''Sim'',''N'',''N�o'','''') as ind_nota_original,';
        v_SQL := v_SQL || '         to_char(a.dt_anexado,'||chr(39)||'DD/MM/YYYY HH24:MI'||chr(39)||') dt_anexado';
        v_SQL := v_SQL || '    FROM ts.REEMBOLSO_PREVIA_ANEXO a';
        v_SQL := v_SQL || '   WHERE num_reembolso = :num_reembolso';
        if NVL(p_nome_arquivo, v_item_vazio) <> v_item_vazio then
            v_SQL := v_SQL || ' AND nom_arq_anexo = :nom_arq_anexo';
        end if;
        v_SQL := v_SQL || ' ORDER BY a.nom_arq_anexo';

        v_posicao := 16;

        qryCtx := dbms_xmlgen.newContext(v_SQL);
        dbms_xmlgen.setBindValue(qryCtx, 'num_reembolso', v_num_reembolso);
        if NVL(p_nome_arquivo, v_item_vazio) <> v_item_vazio then
            dbms_xmlgen.setBindValue(qryCtx, 'nom_arq_anexo', p_nome_arquivo);
        end if;
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'ANEXO' );
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

        return;

    EXCEPTION
        WHEN OTHERS THEN

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<ANEXO>';
            p_xml_retorno := p_xml_retorno || '<DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</DADOS>';
            p_xml_retorno := p_xml_retorno || '</ANEXO>';
            return;
    END;

    ----------------------------------------------------------------------------
    -- Retorna xml com as glosas da pr�via informada
    ----------------------------------------------------------------------------
    function RetornaGlosa ( p_num_reembolso         in  varchar2
                          , p_num_seq_item          in  number
                          )
    return sys_refcursor
    is
        --Declara��es:
        v_posicao               number;
        v_parametro             varchar2(4000);
        v_SQL                   varchar2(4000);
        v_cod_retorno           number;
        v_msg_retorno           varchar2(4000);
        c                       sys_refcursor;
    begin
        --In�cio
        v_posicao := 0;
        v_parametro := 'p_num_reembolso = ' || p_num_reembolso;
        v_parametro := 'p_num_seq_item = ' || p_num_seq_item;
        --
        v_posicao := 1;
        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            return get_cursor_vazio;
        END IF;
        --
        v_posicao := 10;
        --
        --Montar XML das glosas
        v_SQL :=  trim(' select a.cod_motivo_glosa                                                     ')
              || rtrim('      , nvl(b.desc_previa_reembolso,b.desc_motivo_glosa) desc_motivo_glosa     ')
              || rtrim('      , num_seq_item                                                           ')
              || rtrim('      , a.txt_complemento                                                      ')
              || rtrim('      , a.cod_usuario_inclusao                                                 ')
              || rtrim('      , to_char(a.dt_inclusao,''dd/mm/yyyy'') dt_inclusao                      ')
              || rtrim('      , b.ind_tipo_glosa                                                       ')
              || rtrim('      , b.ind_implementado_rb                                                  ')
              || rtrim('      , b.ind_liberacao                                                        ')
              || rtrim('      , a.txt_obs                                                              ')
              || rtrim('      , nvl(a.ind_analisado,''N'')  ind_analisado                              ')
              || rtrim('      , a.cod_usuario_liberacao                                                ')
              || rtrim('      , a.dt_liberacao                                                         ')
              || rtrim(' from   reembolso_previa_glosa                a                                ')
              || rtrim('      , motivo_glosa                          b                                ')
              || rtrim(' where  a.cod_motivo_glosa                    = b.cod_motivo_glosa             ')
              || rtrim(' and    a.num_reembolso                       = :num_reembolso                 ')
              || rtrim(' and    a.num_seq_item                        = :num_seq_item                  ')
              || rtrim(' order by a.cod_motivo_glosa                                                   ');
        --
        v_posicao := 12;
        --
        open  c
        for   v_Sql
        using p_num_reembolso
            , nvl(p_num_seq_item,0);
        --
        return c;
        --
    EXCEPTION
    WHEN OTHERS THEN
        TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || 'Parametros:' || chr(13) || v_parametro, 'RetornaGlosa' );
        --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', v_SQL, 'RetornaGlosa - SQL' );
        --
        return get_cursor_vazio;
    END;

    ----------------------------------------------------------------------------
    -- Retorna xml com as glosas implementadas para a pr�via de reembolso
    ----------------------------------------------------------------------------
    procedure RetornaHabilitacaoGlosa(p_xml_glosa out clob)

    is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    v_item_vazio            varchar2(3) := '���';
    v_cod_retorno           number;
    v_msg_retorno           varchar2(4000);

    qryCtx                  DBMS_XMLGEN.ctxHandle;

    begin
        --In�cio
        v_posicao := 0;

        --Valida��o

        v_posicao := 10;

        --Montar XML das glosas
        v_SQL := '';

        v_SQL := v_SQL || 'select 0 COD_RETORNO, IND_USO_PRB HABILITADO, ';
        v_SQL := v_SQL || '       to_char(DT_INI_PRB,'||chr(39)||'MM/YYYY'||chr(39)||') DT_INICIO_VALIDADE,';
        v_SQL := v_SQL || '       to_char(DT_FIM_PRB,'||chr(39)||'MM/YYYY'||chr(39)||') DT_FIM_VALIDADE,';
        v_SQL := v_SQL || '       COD_MOTIVO_GLOSA, ind_nega_previa_prb, ind_nega_item_prb,';
        v_SQL := v_SQL || '       DESC_MOTIVO_GLOSA, IND_IMPEDE_AP_PRB,';
        v_SQL := v_SQL || '       NVL(DESC_PREVIA_REEMBOLSO, DESC_MOTIVO_GLOSA) as desc_exibicao ';
        v_SQL := v_SQL || '  from MOTIVO_GLOSA ';
        v_SQL := v_SQL || ' where IND_TIPO_GLOSA <> '||chr(39)||'1'||chr(39);
        v_SQL := v_SQL || '   and IND_IMPLEMENTADO_PRB = '||chr(39)||'S'||chr(39);
        v_SQL := v_SQL || ' order by DESC_MOTIVO_GLOSA asc';

        v_posicao := 12;

        qryCtx := dbms_xmlgen.newContext(v_SQL);
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'MOTIVO_GLOSA' );
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_glosa := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

        return;

        <<trata_retorno_erro>>

        v_cod_retorno := 9;
        p_xml_glosa := '<?xml version="1.0"?>';
        p_xml_glosa := p_xml_glosa || '<MOTIVO_GLOSA>';
        p_xml_glosa := p_xml_glosa || '<DADOS>';
        p_xml_glosa := p_xml_glosa || '<COD_RETORNO>' || v_cod_retorno || '</COD_RETORNO>';
        p_xml_glosa := p_xml_glosa || '<MSG_RETORNO>' || v_msg_retorno || '</MSG_RETORNO>';
        p_xml_glosa := p_xml_glosa || '<SQL>' || v_SQL || '</SQL>';
        p_xml_glosa := p_xml_glosa || '</DADOS>';
        p_xml_glosa := p_xml_glosa || '</MOTIVO_GLOSA>';

        return;

    EXCEPTION
        WHEN OTHERS THEN

            p_xml_glosa := '<?xml version="1.0"?>';
            p_xml_glosa := p_xml_glosa || '<MOTIVO_GLOSA>';
            p_xml_glosa := p_xml_glosa || '<DADOS>';
            p_xml_glosa := p_xml_glosa || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_glosa := p_xml_glosa || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_glosa := p_xml_glosa || '<SQL>' || v_SQL || '</SQL>';
            p_xml_glosa := p_xml_glosa || '</DADOS>';
            p_xml_glosa := p_xml_glosa || '</MOTIVO_GLOSA>';

            return;
    END;
    ----------------------------------------------------------------------------
    -- Retorna xml com os motivos de recusa / negativa
    ----------------------------------------------------------------------------
    procedure RetornaMotivo(p_cod_motivo            in  number
                           ,p_xml_retorno           out clob)

    is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    v_item_vazio            varchar2(3) := '���';
    v_cod_retorno           number;
    v_msg_retorno           varchar2(4000);

    qryCtx                  DBMS_XMLGEN.ctxHandle;

    begin
        --In�cio

        v_posicao := 1;
        --Valida��o
        IF NVL(p_cod_motivo,0) = 0 THEN
            v_msg_retorno := 'C�digo do motivo n�o informado';
            goto trata_retorno_erro;
        END IF;

        v_posicao := 10;

        --Montar XML dos motivos
        v_SQL := '';
        v_SQL := v_SQL || ' SELECT  0 COD_RETORNO,';
        v_SQL := v_SQL || '         a.cod_motivo, a.desc_motivo, a.txt_mensagem, a.ind_tipo';
        v_SQL := v_SQL || '    FROM ts.reembolso_previa_motivo_indef a';
        v_SQL := v_SQL || '   WHERE a.cod_motivo = :cod_motivo';

        v_posicao := 12;

        qryCtx := dbms_xmlgen.newContext(v_SQL);
        dbms_xmlgen.setBindValue(qryCtx, 'cod_motivo', p_cod_motivo);
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'MOTIVO');
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

        return;

        <<trata_retorno_erro>>

        v_cod_retorno := 9;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<MOTIVO>';
        p_xml_retorno := p_xml_retorno || '<DADOS>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>' || v_cod_retorno || '</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || v_msg_retorno || '</MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
        p_xml_retorno := p_xml_retorno || '</DADOS>';
        p_xml_retorno := p_xml_retorno || '</MOTIVO>';

        return;

    EXCEPTION
        WHEN OTHERS THEN

            --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || 'Parametros:' || chr(13) || v_parametro, 'RetornaGlosa' );
            --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', v_SQL, 'RetornaGlosa - SQL' );

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<MOTIVO>';
            p_xml_retorno := p_xml_retorno || '<DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS>';
            p_xml_retorno := p_xml_retorno || '</MOTIVO>';

            return;
    END;

    ----------------------------------------------------------------------------
    -- Retorna xml com a vincula��o de Usu�rio x Situa��o
    ----------------------------------------------------------------------------
    procedure RetornaUsuarioSituacao(p_cod_usuario    in  varchar2
                                    ,p_xml_retorno    out clob)

    is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    v_item_vazio            varchar2(3) := '���';
    v_cod_retorno           number;
    v_msg_retorno           varchar2(4000);

    qryCtx                  DBMS_XMLGEN.ctxHandle;

    begin
        --In�cio

        v_posicao := 1;
        --Valida��o
        IF NVL(p_cod_usuario,v_item_vazio) = v_item_vazio THEN
            v_msg_retorno := 'C�digo do usu�rio n�o informado';
            goto trata_retorno_erro;
        END IF;
        --
        v_posicao := 10;
        --
        --Montar XML
        v_SQL := 'select 0 cod_retorno
                       , a.cod_usuario
                       , b.ind_situacao
                       , b.nome_situacao
                       , decode (a.cod_usuario, NULL, '''', ''CHECKED'') ind_usuario
                  from   reemb_previa_usuario_sit  a
                       , reembolso_previa_situacao b
                  where  a.cod_usuario(+)       = :cod_usuario
                  and    a.ind_situacao(+)      = b.ind_situacao
                  and    b.ind_usuario_analise  = ''S''
                  order by 1';
        --
        v_posicao := 12;

        qryCtx := dbms_xmlgen.newContext(v_SQL);
        dbms_xmlgen.setBindValue(qryCtx, 'cod_usuario', p_cod_usuario);
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'USUARIO_SITUACAO');
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

        return;

        <<trata_retorno_erro>>

        v_cod_retorno := 9;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<USUARIO_SITUACAO>';
        p_xml_retorno := p_xml_retorno || '<DADOS>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>' || v_cod_retorno || '</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || ts.top_utl_padrao.msgerro || '</MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
        p_xml_retorno := p_xml_retorno || '</DADOS>';
        p_xml_retorno := p_xml_retorno || '</USUARIO_SITUACAO>';

        return;

    EXCEPTION
        WHEN OTHERS THEN

            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto - USUARIO ' || p_cod_usuario, 'Erro:' || chr(13) || sqlerrm || chr(13), 'RetornaUsuarioSituacao' );

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<USUARIO_SITUACAO>';
            p_xml_retorno := p_xml_retorno || '<DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || ts.top_utl_padrao.msgerro || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS>';
            p_xml_retorno := p_xml_retorno || '</USUARIO_SITUACAO>';

            return;
    END;
    --
    --
    ----------------------------------------------------------------------------
    -- Retornar um cursor com os parametros informados
    ----------------------------------------------------------------------------
    function  RetornaCursor( p_nome_tabela       in varchar
                            ,p_campo_value       in varchar
                            ,p_campo_desc        in varchar
                            ,p_order             in varchar default null
                            ,p_where             in varchar default null
                            )
    return sys_refcursor
    is
        c                         sys_refcursor;
        v_Sql                     Varchar2(4000) := '';
        v_item_vazio              varchar2(3) := '���';
    begin
        --
        v_Sql := v_Sql || ' SELECT ';
        --
        if    NVL(p_campo_value,v_item_vazio) <> v_item_vazio
           or NVL(p_campo_desc,v_item_vazio) <> v_item_vazio then
            --
            if NVL(p_campo_value,v_item_vazio) <> v_item_vazio then
                v_Sql := v_Sql || p_campo_value;
            end if;
            --
            if NVL(p_campo_desc,v_item_vazio) <> v_item_vazio then
                --
                if NVL(p_campo_value,v_item_vazio) <> v_item_vazio then v_Sql := v_Sql || ' ,';
                end if;
                --
                v_Sql := v_Sql || p_campo_desc;
                --
            end if;
            --
        else
            --
            v_Sql := v_Sql || ' * ';
            --
        end if;
        --
        v_Sql := v_Sql || ' From ' || p_nome_tabela;
        --
        if p_where is not null then
            v_Sql := v_Sql || ' ' || p_where;
        end if;
        --
        if p_order is not null then
            v_Sql := v_Sql || ' ' || p_order;
        elsif p_campo_desc is not null then
            v_Sql := v_Sql || ' order by ' || p_campo_desc;
        else
            v_Sql := v_Sql || ' order by ' || p_campo_value;
        end if;
        --
        --
        open c for v_Sql;
        --
        return c;
        --
    exception
        when others then
            raise_application_error( -20001 , 'RB_PREVIA_REEMBOLSO.RetornaCursor - Ocorreu o seguinte erro: ' || sqlerrm || v_Sql);
            return null;
    end;
    --
    ----------------------------------------------------------------------------
    -- Retorna uma combo da situa��o
    ----------------------------------------------------------------------------
    procedure RetornaCboSituacao(p_nome_combo           in varchar2,
                                 p_ind_situacao_atual   in number,
                                 p_html_retorno         out clob)
    --
    is
        --Declara��es:
        v_posicao               number;
    begin
        --In�cio
        v_posicao := 1;

        --Montar String de retorno

        p_html_retorno := '<select name="' || p_nome_combo || '">';
        p_html_retorno := p_html_retorno || '<OPTION></OPTION>';

        for c_item in
           (select ind_situacao, nome_situacao
              from reembolso_previa_situacao
             where ind_encaminhamento = 'S'
               and ind_situacao <> nvl(p_ind_situacao_atual,0)
          order by nome_situacao)
        loop
            p_html_retorno := p_html_retorno || '<OPTION VALUE="' || C_Item.ind_situacao || '">' || C_Item.nome_situacao || '</OPTION>';
        end loop;

        p_html_retorno := p_html_retorno || '</select>';

    EXCEPTION
        WHEN OTHERS THEN

            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, '', 'Erro:' || chr(13) || sqlerrm || chr(13), 'RetornaCboSituacao' );
            p_html_retorno := 'Erro ao montar a combo de situa��o: ' || SQLERRM;
            return;
    END;
    --
    ----------------------------------------------------------------------------
    -- Anexar / Excluir arquivos de uma pr�via de reembolso informada
    ----------------------------------------------------------------------------
    PROCEDURE GravaAnexo ( p_xml_dados       in   clob, -- xml contendo a informa��o a ser gravada/atualizada/exclu�da
                           p_cod_retorno     out  number,
                           p_msg_retorno     out  varchar2)   is
    --
    -----  documento transformado
    v_doc          xmldom.DOMDocument;
    --
    -----  retorno
    v_cod_retorno     number;
    v_msg_retorno     varchar2(200);
    --
    v_posicao         number;
    v_qtd             number;
    v_num_reembolso       REEMBOLSO_PREVIA_ANEXO.num_reembolso%type;
    v_nom_arq_anexo       REEMBOLSO_PREVIA_ANEXO.nom_arq_anexo%type;
    v_ind_nota_original   REEMBOLSO_PREVIA_ANEXO.ind_nota_original%type;
    v_nom_arquivo_aux     REEMBOLSO_PREVIA_ANEXO.nom_arq_anexo%type;
    v_txt_descricao       REEMBOLSO_PREVIA_ANEXO.txt_descricao%type;
    v_cod_usuario         REEMBOLSO_PREVIA_ANEXO.cod_usuario%type;
    v_txt_obs_exc         REEMBOLSO_PREVIA_OCORRENCIA.txt_obs%type;
    v_txt_obs_inc         REEMBOLSO_PREVIA_OCORRENCIA.txt_obs%type;
    v_num_reembolso_ans   PEDIDO_REEMBOLSO_PREVIA.num_reembolso_ans%type;
    v_ind_exclusao    varchar2(1);
    v_qtd_exclusao    number;
    v_qtd_inclusao    number;
    i                 number;
    v_ind_anexo       varchar2(1);
    v_qtd_anexo       number;
    v_ind_excluir     varchar2(1);
    v_ind_altera      varchar2(1);
    v_tamanho_reembolso number;
    v_dir             REEMBOLSO_PREVIA_ANEXO.nom_arq_anexo%type;
    v_arq             REEMBOLSO_PREVIA_ANEXO.nom_arq_anexo%type;
    --------------------------------------------------------------------------------
    begin
        --
        v_posicao := 0;
        --
        ts_log_execucao ( 'RB_PREVIA_REEMBOLSO', v_posicao, '', 'p_xml_dados:' || chr(13) || p_xml_dados || chr(13), 'GravaAnexo' );
        --
        v_posicao := 1;
        --
        ts_cria_doc_xml(p_xml_dados,v_doc,v_cod_retorno,v_msg_retorno);
        if v_cod_retorno <> 0 then
            p_cod_retorno := v_cod_retorno;
            p_msg_retorno := v_msg_retorno;
            return;
        end if;
        --
        v_posicao := 2;
        --
        v_num_reembolso_ans := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','NUM_REEMBOLSO');
        --
        if NVL(v_num_reembolso_ans,0) = 0 then
            p_cod_retorno := 9;
            p_msg_retorno := 'N� da pr�via de reembolso n�o informado';
            return;
        end if;
        --
        v_posicao := 3;
        --
        select length(v_num_reembolso_ans)
          into v_tamanho_reembolso
          from dual;
        --
        v_posicao := 4;
        --
        if v_tamanho_reembolso > 15 then
            select p.num_reembolso
              into v_num_reembolso
              from ts.pedido_reembolso_previa p
             where p.num_reembolso_ans = v_num_reembolso_ans;
        else
          v_num_reembolso := v_num_reembolso_ans;
        end if;
        --
        v_posicao := 5;
        --
        v_qtd_anexo         := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','QTD_ANEXO');
        v_ind_anexo         := nvl(ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','IND_ANEXO'),'N'); --Verifica se veio da fun��o Anexo
        --
        if v_ind_anexo = 'S' then
            --
            v_txt_obs_exc := '';
            v_txt_obs_inc := '';
            v_qtd_exclusao := 0;
            v_qtd_inclusao := 0;
            --
            for i in 1..v_qtd_anexo loop
                --
                v_ind_excluir       := nvl(ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','IND_EXCLUIR_'||i),'N');
                v_ind_nota_original := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','IND_NOTA_ORIGINAL_'||i);
                v_ind_altera        := nvl(ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','IND_ALTERA_'||i),'N');
                v_txt_descricao     := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','TXT_DESCRICAO_'||i);
                v_nom_arq_anexo     := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','NOM_ARQ_ANEXO_'||i);
                v_cod_usuario       := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','COD_USUARIO');
                v_nom_arquivo_aux   := v_nom_arq_anexo;
                --
                if v_ind_excluir = 'S' then
                    --
                    v_posicao := 10;
                    if (upper(v_cod_usuario) <> 'SITE') then
                        v_arq := substr(v_nom_arq_anexo, instr(v_nom_arq_anexo, '\', -1) + 1);
                        v_nom_arquivo_aux := v_arq;
                    end if;
                    --
                    v_posicao := 11;
                    --
                    delete from REEMBOLSO_PREVIA_ANEXO
                    where  num_reembolso        = v_num_reembolso
                    and    upper(nom_arq_anexo) = upper(v_nom_arq_anexo);
                    --
                    if v_qtd_exclusao > 0 then
                        v_txt_obs_exc := v_txt_obs_exc || ', ';
                    end if;
                    --
                    v_posicao := 12;
                    --
                    v_txt_obs_exc := v_txt_obs_exc || v_nom_arquivo_aux;
                    --
                    v_qtd_exclusao := v_qtd_exclusao + 1;
                    --
                else
                    --
                    v_posicao := 15;
                    --
                    if (upper(v_cod_usuario) <> 'SITE') then
                        v_dir := substr(v_nom_arq_anexo, 0, instr(v_nom_arq_anexo, '\', -1));
                        v_arq := substr(v_nom_arq_anexo, instr(v_nom_arq_anexo, '\', -1) + 1);
                        v_nom_arquivo_aux := v_num_reembolso || '_' || v_arq;
                        v_nom_arq_anexo := v_dir || v_nom_arquivo_aux;
                    end if;
                    --
                    if v_ind_altera = 'S' then
                        --
                        v_posicao := 16;
                        --
                        select count(*)
                        into   v_qtd
                        from   REEMBOLSO_PREVIA_ANEXO
                        where  num_reembolso        = v_num_reembolso
                        and    Upper(nom_arq_anexo) = upper(v_nom_arq_anexo);
                        --
                        v_posicao := 17;
                        --
                        if v_qtd > 0 then
                            p_cod_retorno := 9;
                            p_msg_retorno := 'Arquivo ' || v_nom_arquivo_aux || ' j� anexado na pr�via.';
                            return;
                        end if;
                        --
                    end if;
                    --
                    begin
                        --
                        v_posicao := 20;
                        --
                        insert into
                            REEMBOLSO_PREVIA_ANEXO (num_reembolso,       nom_arq_anexo,      txt_descricao,
                                                    cod_usuario,         dt_anexado, Ind_Nota_Original)
                                            values (v_num_reembolso,     v_nom_arq_anexo,    v_txt_descricao,
                                                    v_cod_usuario,       sysdate, v_ind_nota_original);
                        --
                    exception
                        when dup_val_on_index then
                            p_cod_retorno := 9;
                            p_msg_retorno := 'Anexo j� cadastrado';
                            return;
                    end;
                    --
                    if v_qtd_inclusao > 0 then
                        v_txt_obs_inc := v_txt_obs_inc || ', ';
                    end if;
                    --
                    v_posicao := 21;
                    --
                    v_txt_obs_inc := v_txt_obs_inc || v_nom_arquivo_aux;
                    --
                    v_qtd_inclusao := v_qtd_inclusao + 1;
                    --
                end if;
                --
            end loop;
            --
            v_posicao := 30;
            --
            if v_qtd_inclusao > 0 then
                --
                v_posicao := 31;
                --
                v_txt_obs_inc := 'Arquivo(s):' || v_txt_obs_inc;
                --
                -- gera ocorrencia de inclus�o de anexo
                GeraOcorrencia(v_num_reembolso,null,8,null,v_txt_obs_inc,v_cod_usuario,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno <> 0 then
                    xmldom.freeDocument(v_doc);
                    rollback;
                    return;
                end if;
                --
            end if;
            --
            v_posicao := 40;
            --
            if v_qtd_exclusao > 0 then
                --
                v_posicao := 41;
                --
                v_txt_obs_exc := 'Arquivo(s):' || v_txt_obs_exc;
                --
                -- gera ocorrencia de exclus�o de anexo
                GeraOcorrencia(v_num_reembolso,null,9,null,v_txt_obs_exc,v_cod_usuario,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno <> 0 then
                    xmldom.freeDocument(v_doc);
                    rollback;
                    return;
                end if;
                --
            end if;
            --
        else
            v_txt_descricao     := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','TXT_DESCRICAO');
            v_ind_nota_original    := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','IND_NOTA_ORIGINAL');
            v_nom_arq_anexo     := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','NOM_ARQ_ANEXO');
            v_cod_usuario       := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','COD_USUARIO');
            v_nom_arquivo_aux   := v_nom_arq_anexo;
            --
            v_posicao := 50;
            --
            if not v_nom_arq_anexo is null then
                --
                v_posicao := 51;
                --
                if (upper(v_cod_usuario) <> 'SITE') then
                  v_dir := substr(v_nom_arq_anexo, 0, instr(v_nom_arq_anexo, '\', -1));
                  v_arq := substr(v_nom_arq_anexo, instr(v_nom_arq_anexo, '\', -1) + 1);
                  v_nom_arquivo_aux := v_num_reembolso || '_' || v_arq;
                  v_nom_arq_anexo := v_dir || v_nom_arquivo_aux;
                end if;
                --
                v_posicao := 52;
                --
                select count(*)
                into   v_qtd
                from   REEMBOLSO_PREVIA_ANEXO
                where  num_reembolso        = v_num_reembolso
                and    Upper(nom_arq_anexo) = upper(v_nom_arq_anexo);
                --
                v_posicao := 53;
                --
                if v_qtd > 0 then
                    p_cod_retorno := 9;
                    p_msg_retorno := 'Arquivo ' || v_nom_arquivo_aux || ' j� anexado na pr�via.';
                    return;
                end if;
                --
                begin
                    --
                    v_posicao := 54;
                    --
                    insert into
                            REEMBOLSO_PREVIA_ANEXO (num_reembolso,       nom_arq_anexo,      txt_descricao,
                                                    cod_usuario,         dt_anexado, ind_nota_original)
                                            values (v_num_reembolso,     v_nom_arq_anexo,    v_txt_descricao,
                                                    v_cod_usuario,       sysdate, v_ind_nota_original);
                    --
                exception
                    when dup_val_on_index then
                        p_cod_retorno := 9;
                        p_msg_retorno := 'Anexo j� cadastrado';
                        return;
                end;
                --
                v_posicao := 55;
                --
                -- gera ocorrencia de exclus�o de anexo
                GeraOcorrencia(v_num_reembolso,null,8,null,'Arquivo:'||v_nom_arquivo_aux,v_cod_usuario,p_cod_retorno,p_msg_retorno);
                if p_cod_retorno <> 0 then
                    xmldom.freeDocument(v_doc);
                    rollback;
                    return;
                end if;
                --
            end if;
            --
            v_posicao := 60;
            --
            v_qtd_exclusao      := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','EXCLUSAO/QTD_EXCLUSAO');
            --
            if v_qtd_exclusao is null then v_qtd_exclusao:=0;
            end if;
            --
            -- deletar os marcados
            --
            v_txt_obs_exc := '';
            --
            for i in 1 .. v_qtd_exclusao loop
                --
                v_posicao := 61;
                --
                v_nom_arq_anexo   := ts_obtem_dados_xml(v_doc,'ANEXO_PREVIA','EXCLUSAO/NOM_ARQ_ANEXO_' || i);
                v_nom_arquivo_aux := v_nom_arq_anexo;
                --
                v_posicao := 62;
                --
                if (upper(v_cod_usuario) <> 'SITE') then
                    v_arq := substr(v_nom_arq_anexo, instr(v_nom_arq_anexo, '\', -1) + 1);
                    v_nom_arquivo_aux := v_arq;
                end if;
                --
                v_posicao := 63;
                --
                delete from REEMBOLSO_PREVIA_ANEXO
                where  num_reembolso        = v_num_reembolso
                and    upper(nom_arq_anexo) = upper(v_nom_arq_anexo);
                --
                if i > 1 then
                    v_txt_obs_exc := v_txt_obs_exc || ', ';
                end if;
                --
                v_txt_obs_exc := v_txt_obs_exc || v_nom_arquivo_aux;
                --
            end loop;
            --
        end if;
        --
        v_posicao := 70;
        --
        if v_qtd_exclusao > 0 then
            --
            v_posicao := 71;
            --
            v_txt_obs_exc := 'Arquivo(s):' || v_txt_obs_exc;
            --
            -- gera ocorrencia de exclus�o de anexo
            GeraOcorrencia(v_num_reembolso,null,9,v_txt_obs_exc,null,v_cod_usuario,p_cod_retorno,p_msg_retorno);
            if p_cod_retorno <> 0 then
                xmldom.freeDocument(v_doc);
                rollback;
                return;
            end if;
            --
        end if;
        --
        -- libera��o do xml ------------------------------------------------------------
        --
        v_posicao := 90;
        xmldom.freeDocument(v_doc);
        --
        commit;
        --
        p_cod_retorno       := 0;
        p_msg_retorno       := 'Opera��o realizada.';
        --
    exception
        when others then
            --
            p_cod_retorno := 9;
            p_msg_retorno := 'GravaAnexo-' || v_posicao || ': ' || sqlerrm;
            --
            rollback;
            --
            if v_posicao > 1 then xmldom.freeDocument(v_doc);
            end if;
            --
    end;
    --
    ----------------------------------------------------------------------------
    -- Gravar o motivo da negativa para pr�vias recusadas
    ----------------------------------------------------------------------------
    PROCEDURE GravaMotivoNegItem ( p_num_reembolso        in number,
                                   p_item_medico          in varchar2,
                                   p_num_seq_item         in number,
                                   p_cod_motivo           in number,
                                   p_desc_motivo          in varchar2,
                                   p_cod_retorno          out number,
                                   p_msg_retorno          out varchar2)   is

    -----  retorno
    v_posicao         number;
    v_item_vazio      varchar2(3) := '���';
    --------------------------------------------------------------------------------

    BEGIN

    v_posicao := 0;

    --------------------------------------------------------------------------------

    if NVL(p_num_reembolso,0) = 0 then
        return;
    end if;

    if NVL(p_num_seq_item,0) = 0 then
        return;
    end if;

    if NVL(p_item_medico,v_item_vazio) = v_item_vazio then
        return;
    end if;

    v_posicao := 2;

    UPDATE ts.procedimento_reembolso_previa
       SET cod_motivo_glosa_man = p_cod_motivo,
           txt_motivo_glosa_man = p_desc_motivo
     WHERE num_reembolso = p_num_reembolso
       AND cod_procedimento = p_item_medico;
       --AND num_seq_item = p_num_seq_item;

    commit;

    p_cod_retorno       := 0;
    p_msg_retorno       := 'Motivo da negativa gravado';

    exception
        when others then

           p_cod_retorno := 9;
           p_msg_retorno := 'GravaMotivoNegItem-' || v_posicao || ': ' || sqlerrm;
           rollback;
    end;

    ----------------------------------------------------------------------------
    -- Gravar habilita��o das glosas implementadas para pr�via de reembolso
    ----------------------------------------------------------------------------
    PROCEDURE GravaHabilitacaoGlosa ( p_xml_dados       in   clob,
                                      p_cod_retorno     out  number,
                                      p_msg_retorno     out  varchar2)   is

    -----  documento transformado
    v_doc               xmldom.DOMDocument;

    -----  retorno
    v_cod_retorno       number;
    v_msg_retorno       varchar2(200);

    v_posicao           number;

    v_qtd_total         number;
    v_cod_motivo_glosa  motivo_glosa.cod_motivo_glosa%type;
    v_ind_uso_prb       motivo_glosa.ind_uso_prb%type;
    v_ind_nega_previa   motivo_glosa.ind_nega_previa_prb%type;
    v_ind_nega_item     motivo_glosa.ind_nega_item_prb%type;
    v_ind_impede_ap_prb motivo_glosa.ind_impede_ap_prb%type;
    v_dt_ini_prb        varchar2(20);
    v_dt_fim_prb        varchar2(20);

    --------------------------------------------------------------------------------

    BEGIN

    v_posicao := 0;

    --------------------------------------------------------------------------------

    ts_cria_doc_xml(p_xml_dados,v_doc,v_cod_retorno,v_msg_retorno);
    if v_cod_retorno <> 0 then
        p_cod_retorno := v_cod_retorno;
        p_msg_retorno := v_msg_retorno;
        return;
    end if;

    v_posicao := 2;

    v_qtd_total     := NVL(ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/TOTAL'),0);
    if NVL(v_qtd_total,0) = 0 then
        p_cod_retorno := 9;
        p_msg_retorno := 'Quantidade de cr�tica inv�lida';
        return;
    end if;

    v_posicao := 3;

    for i in 1 .. v_qtd_total loop
        v_cod_motivo_glosa  := ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/COD_MOTIVO_GLOSA_' || i);
        v_ind_uso_prb       := ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/SELECAO_' || i);
        if v_ind_uso_prb = 'S' then
            v_ind_nega_previa   := ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/IND_NEGA_PREVIA_' || i);
            v_ind_nega_item     := ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/IND_NEGA_ITEM_' || i);
        else
            v_ind_nega_item   := 'N';
            v_ind_nega_previa := 'N';
        end if;
        v_dt_ini_prb :='';
        v_dt_fim_prb :='';

        v_ind_impede_ap_prb  := ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/IND_IMPEDE_AP_PRB_' || i);

        if nvl(ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/DT_INI_' || i),'N') <> 'N' then
            v_dt_ini_prb    := to_date('01/' || ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/DT_INI_' || i),'dd/mm/yyyy');
        end if;
        if nvl(ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/DT_FIM_' || i),'N') <> 'N' then
            v_dt_fim_prb    := to_date('01/' || ts_obtem_dados_xml(v_doc,'HABILITACAO_GLOSA','DADOS/DT_FIM_' || i),'dd/mm/yyyy');
        end if;

        v_posicao := 5;
        UPDATE motivo_glosa SET
               ind_uso_prb = nvl(v_ind_uso_prb,'N'),
               ind_nega_previa_prb = nvl(v_ind_nega_previa,'N'),
               ind_nega_item_prb = nvl(v_ind_nega_item,'N'),
               dt_ini_prb = v_dt_ini_prb,
               dt_fim_prb = v_dt_fim_prb,
               ind_impede_ap_prb = v_ind_impede_ap_prb
         WHERE cod_motivo_glosa = v_cod_motivo_glosa;

    end loop;

    v_posicao := 6;

    -- libera��o do xml ------------------------------------------------------------

    v_posicao := 10;
    xmldom.freeDocument(v_doc);

    commit;

    p_cod_retorno       := 0;
    p_msg_retorno       := 'Opera��o realizada.';

    exception
        when others then

           p_cod_retorno := 9;
           p_msg_retorno := p_msg_retorno || 'GravaHabilitacaoGlosa-' || v_posicao || ': ' || sqlerrm;

           rollback;

           if v_posicao > 1 then
              xmldom.freeDocument(v_doc);
           end if;

    end;

    ----------------------------------------------------------------------------
    -- Retorna xml com as informa��es do solicitante informado
    ----------------------------------------------------------------------------
    procedure RetornaSolicitante(p_xml_filtro       in clob
                                ,p_xml_retorno      out clob)

    is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    v_cod_retorno           number;
    v_msg_retorno           varchar2(4000);
    v_doc                   xmldom.DOMDocument;
    v_cod_solicitante       solicitante.cod_solicitante%type;
    v_nome_solicitante      solicitante.nome_solicitante%type;
    v_sigla_conselho        solicitante.sigla_conselho%type;
    v_num_crm               solicitante.num_crm%type;
    v_sgl_uf_conselho       solicitante.sgl_uf_conselho%type;
    v_num_insc_fiscal       solicitante.num_insc_fiscal%type;
    v_ind_tipo_pessoa       solicitante.ind_tipo_pessoa%type;
    v_item_vazio            varchar2(3) := '���';
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    v_busca_inscricao       varchar2(1) := 'N';

    begin
        --Recuperar parametros do filtro
        ts_cria_doc_xml(p_xml_filtro, v_doc, v_cod_retorno, v_msg_retorno);
        if v_cod_retorno <> 0 then
            goto trata_retorno_erro;
        end if;

        v_posicao := 2;

        v_cod_solicitante   := ts_obtem_dados_xml(v_doc,'SOLICITANTE','COD_SOLICITANTE');
        v_nome_solicitante  := ts_obtem_dados_xml(v_doc,'SOLICITANTE','NOME_SOLICITANTE');
        v_posicao := 3;
        v_sigla_conselho    := ts_obtem_dados_xml(v_doc,'SOLICITANTE','SIGLA_CONSELHO');
        v_num_crm           := ts_obtem_dados_xml(v_doc,'SOLICITANTE','NUM_CRM');
        v_posicao := 5;
        v_sgl_uf_conselho   := ts_obtem_dados_xml(v_doc,'SOLICITANTE','SGL_UF_CONSELHO');
        v_num_insc_fiscal   := replace(replace(replace(ts_obtem_dados_xml(v_doc,'SOLICITANTE','NUM_INSC_FISCAL'),'.',''),'-',''),'/','');
        v_ind_tipo_pessoa   := ts_obtem_dados_xml(v_doc,'SOLICITANTE','IND_TIPO_PESSOA');

        xmldom.freeDocument(v_doc);

        v_posicao := 10;

        --Montar XML
        v_SQL := '';
        v_SQL := v_SQL || ' SELECT  0 cod_retorno, s.cod_solicitante, nome_solicitante, s.sigla_conselho, s.num_crm,';
        v_SQL := v_SQL || '         s.sgl_uf_conselho, s.num_insc_fiscal, s.ind_tipo_pessoa,';
        v_SQL := v_SQL || '         s.cod_municipio_execucao, m.nom_municipio nome_municipio_execucao,';
        --CPF
        v_SQL := v_SQL || '         DECODE(s.num_insc_fiscal,null,'||chr(39)||chr(39)||',';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,11,'||chr(39)||0||chr(39)||'),1,3)||'||chr(39)||'.'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,11,'||chr(39)||0||chr(39)||'),4,3)||'||chr(39)||'.'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,11,'||chr(39)||0||chr(39)||'),7,3)||'||chr(39)||'-'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,11,'||chr(39)||0||chr(39)||'),10,2)) num_cpf,';
        --CNPJ
        v_SQL := v_SQL || '         DECODE(s.num_insc_fiscal,null,'||chr(39)||chr(39)||',';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,14,'||chr(39)||0||chr(39)||'),1,2)||'||chr(39)||'.'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,14,'||chr(39)||0||chr(39)||'),3,3)||'||chr(39)||'.'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,14,'||chr(39)||0||chr(39)||'),6,3)||'||chr(39)||'/'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,14,'||chr(39)||0||chr(39)||'),9,4)||'||chr(39)||'-'||chr(39)||'||';
        v_SQL := v_SQL || '         SUBSTR(lpad(s.num_insc_fiscal,14,'||chr(39)||0||chr(39)||'),13,2)) num_cnpj';

        v_SQL := v_SQL || '    FROM ts.solicitante s, ts.municipio m';
        v_SQL := v_SQL || '   WHERE s.ind_tipo_pessoa = :ind_tipo_pessoa';
        v_SQL := v_SQL || '     AND s.cod_municipio_execucao = m.cod_municipio(+)';


        if nvl(v_cod_solicitante,0) <> 0 then
            v_SQL := v_SQL || ' and s.cod_solicitante = :cod_solicitante';
            v_busca_inscricao := 'S';
        end if;
        if nvl(v_num_insc_fiscal,0) <> 0 then
            v_SQL := v_SQL || ' and s.num_insc_fiscal = :num_insc_fiscal';
        end if;

        if nvl(v_nome_solicitante,v_item_vazio) <> v_item_vazio then
            v_SQL := v_SQL || ' and UPPER(s.nome_solicitante) like ' || chr(39) || '%' || UPPER(v_nome_solicitante) || '%' || chr(39);
            v_busca_inscricao := 'S';
        end if;

        if v_ind_tipo_pessoa = 'F' then
            if nvl(v_sigla_conselho,v_item_vazio) <> v_item_vazio then
                v_SQL := v_SQL || ' and s.sigla_conselho = :sigla_conselho';
                v_busca_inscricao := 'S';
            end if;
            if nvl(v_num_crm,v_item_vazio) <> v_item_vazio then
                v_SQL := v_SQL || ' and s.num_crm = :num_crm';
                v_busca_inscricao := 'S';
            end if;
            if nvl(v_sgl_uf_conselho,v_item_vazio) <> v_item_vazio then
                v_SQL := v_SQL || ' and s.sgl_uf_conselho = :sgl_uf_conselho';
                v_busca_inscricao := 'S';
            end if;
        end if;
        --
        if nvl(v_num_insc_fiscal,0) <> 0 and v_busca_inscricao = 'N' then
           v_SQL := v_SQL || ' and rownum = 1 and num_insc_fiscal is not null ORDER BY nome_solicitante';
        else
           v_SQL := v_SQL || ' and rownum <= 100 and num_insc_fiscal is not null ORDER BY nome_solicitante';
        end if;
        --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 00, 'Debug', v_SQL, 'INI' );

        v_posicao := 12;

        qryCtx := dbms_xmlgen.newContext(v_SQL);

        dbms_xmlgen.setBindValue(qryCtx, 'ind_tipo_pessoa', v_ind_tipo_pessoa);

        if nvl(v_cod_solicitante,0) <> 0 then
            dbms_xmlgen.setBindValue(qryCtx, 'cod_solicitante', v_cod_solicitante);
        end if;
        if nvl(v_num_insc_fiscal,0) <> 0 then
            dbms_xmlgen.setBindValue(qryCtx, 'num_insc_fiscal', v_num_insc_fiscal);
        end if;

        if v_ind_tipo_pessoa = 'F' then
            if nvl(v_sigla_conselho,v_item_vazio) <> v_item_vazio then
                dbms_xmlgen.setBindValue(qryCtx, 'sigla_conselho', v_sigla_conselho);
            end if;
            if nvl(v_num_crm,v_item_vazio) <> v_item_vazio then
                dbms_xmlgen.setBindValue(qryCtx, 'num_crm', v_num_crm);
            end if;
            if nvl(v_sgl_uf_conselho,v_item_vazio) <> v_item_vazio then
                dbms_xmlgen.setBindValue(qryCtx, 'sgl_uf_conselho', v_sgl_uf_conselho);
            end if;
        end if;
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'SOLICITANTE' );
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

        return;

        <<trata_retorno_erro>>

        v_cod_retorno := 9;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<SOLICITANTE>';
        p_xml_retorno := p_xml_retorno || '<DADOS>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || v_msg_retorno || '</MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
        p_xml_retorno := p_xml_retorno || '</DADOS>';
        p_xml_retorno := p_xml_retorno || '</SOLICITANTE>';

        return;

    EXCEPTION
        WHEN OTHERS THEN

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<SOLICITANTE>';
            p_xml_retorno := p_xml_retorno || '<DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS>';
            p_xml_retorno := p_xml_retorno || '</SOLICITANTE>';

            return;
    END;
    --
    --
    procedure ReverteCancelamentoPrevia             ( p_num_reembolso       in  number
                                                     , p_cod_usuario         in  varchar2
                                                     , p_cod_retorno         out number
                                                    , p_msg_retorno         out varchar2
                                                    ) is

   v_ind_situacao      pedido_reembolso_previa.ind_situacao%type;
    v_cod_usuario       usuario.cod_usuario%type;
    v_posicao           pls_integer;

   begin

       v_posicao  := 1;

       if p_num_reembolso is null then
           p_msg_retorno := 'Pr�via de reembolso n�o informada, revers�o do cancelamento impossibilitada.';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  ind_situacao
                    into v_ind_situacao
                         from  pedido_reembolso_previa
                                  where num_reembolso = p_num_reembolso;
            exception
               when no_data_found then
                      p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o foi identificada, revers�o do cancelamento impossibilitada.';
                      p_cod_retorno := 1;
                      return;
            end;

            if v_ind_situacao not in (3)  then
               p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o est� em situa��o que permita revers�o do cancelamento.';
               p_cod_retorno := 1;
               return;
           end if;

       end if;

       v_posicao  := 2;

       if p_cod_usuario is null then
           p_msg_retorno := 'Usu�rio n�o informado';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  cod_usuario
                    into v_cod_usuario
                         from  usuario
                                  where cod_usuario = p_cod_usuario;
            exception
               when no_data_found then
                      p_msg_retorno := 'Usu�rio ' || p_cod_usuario || ' n�o foi identificado, revers�o do cancelamento impossibilitada.';
                      p_cod_retorno := 1;
                      return;
            end;


       end if;

     v_posicao  := 3;
      GeraOcorrencia(p_num_reembolso,null,18,null,null,p_cod_usuario,p_cod_retorno,p_msg_retorno);
     if p_cod_retorno <> 0 then
         rollback;
         return;
     end if;

      v_posicao  := 4;

      update pedido_reembolso_previa
        set   ind_situacao              = 1
              ,dt_deferimento            = null
                ,cod_usuario_deferimento   = null
                ,dt_indeferimento          = null
                ,cod_usuario_indeferimento = null
              ,dt_cancelamento           = null
                ,cod_usuario_cancelamento  = null
                ,dt_devolucao              = null
                ,cod_usuario_devolucao     = null
                ,dt_sit                    = sysdate
                ,cod_usuario_sit           = p_cod_usuario
                     ,cod_motivo                = null
              where num_reembolso        = p_num_reembolso;

      commit;

    p_msg_retorno := 'Opera��o realizada com sucesso';
    p_cod_retorno := 0;

    exception
      when others then
       p_cod_retorno := 9;
       p_msg_retorno := 'RB_PREVIA_REEMBOLSO.ReverteCancelamentoPrevia-' || v_posicao || ': ' || sqlerrm;
       rollback;
    end;
  --
  --
   procedure CancelaPrevia                         ( p_num_reembolso        in  number
                                                     , p_cod_usuario         in  varchar2
                                                     , p_cod_motivo          in  number
                                                     ,p_txt_obs_operadora    in  varchar2
                                                     , p_cod_retorno         out number
                                                     , p_msg_retorno         out varchar2
                                                     ) is
      v_ind_situacao      pedido_reembolso_previa.ind_situacao%type;
      v_cod_usuario       usuario.cod_usuario%type;
      v_posicao           pls_integer;
      v_data_cancelamento date;
      v_txt_obs           pedido_reembolso_previa.txt_observacao_previa%type;
      v_desc_motivo       reembolso_previa_motivo_indef.desc_motivo%type;
      v_num_reembolso_ans pedido_reembolso_previa.num_reembolso_ans%type;
   begin

       v_posicao  := 1;


       if p_num_reembolso is null then
           p_msg_retorno := 'Pr�via de reembolso n�o informada, cancelamento impossibilitado.';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  ind_situacao, txt_observacao_previa
                    into v_ind_situacao, v_txt_obs
                         from  pedido_reembolso_previa
                                  where num_reembolso = p_num_reembolso;
            exception
               when no_data_found then
                      p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o foi identificada, cancelamento impossibilitado.';
                      p_cod_retorno := 1;
                      return;
            end;

            if v_ind_situacao not in (1,5)  then
               p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o est� em situa��o que permita o seu cancelamento.';
               p_cod_retorno := 1;
               return;
           end if;

       end if;
       --
       select nvl(prp.num_reembolso_ans,prp.num_reembolso)
         into v_num_reembolso_ans
         from ts.pedido_reembolso_previa prp
        where prp.num_reembolso = p_num_reembolso;
       --
       v_posicao  := 2;

       if p_cod_usuario is null then
           p_msg_retorno := 'Usu�rio n�o informado';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  cod_usuario
                    into v_cod_usuario
                         from  usuario
                                  where cod_usuario = p_cod_usuario;
            exception
               when no_data_found then
                      p_msg_retorno := 'Usu�rio ' || p_cod_usuario || ' n�o foi identificado, cancelamento impossibilitado.';
                      p_cod_retorno := 1;
                      return;
            end;

       end if;

     if p_cod_motivo is null then
           p_msg_retorno := 'C�digo do motivo n�o informado';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  desc_motivo
                    into v_desc_motivo
                         from  reembolso_previa_motivo_indef
                                  where cod_motivo = p_cod_motivo;
            exception
               when no_data_found then
                      p_msg_retorno := 'Motivo ' || p_cod_motivo || ' n�o foi identificado, cancelamento impossibilitado.';
                      p_cod_retorno := 1;
                      return;
            end;

       end if;

     v_posicao  := 3;
      GeraOcorrencia(p_num_reembolso,null,4, v_txt_obs,'Motivo Cancelamento: ' ||v_desc_motivo || '. ' || p_txt_obs_operadora,p_cod_usuario,p_cod_retorno,p_msg_retorno);
     if p_cod_retorno <> 0 then
         rollback;
         return;
     end if;




     v_posicao  := 4;
      v_data_cancelamento := sysdate;

      update pedido_reembolso_previa
        set   ind_situacao              = 3
              ,dt_deferimento            = null
                ,cod_usuario_deferimento   = null
                ,dt_indeferimento          = null
                ,cod_usuario_indeferimento = null
              ,dt_cancelamento           = v_data_cancelamento
                ,cod_usuario_cancelamento  = p_cod_usuario
                ,dt_devolucao              = null
                ,cod_usuario_devolucao     = null
                ,dt_sit                    = v_data_cancelamento
                ,cod_usuario_sit           = p_cod_usuario
                , txt_observacao_operadora = p_txt_obs_operadora
                ,cod_motivo                = p_cod_motivo
              where num_reembolso        = p_num_reembolso;

      commit;

    p_msg_retorno := 'Pr�via de reembolso ' || v_num_reembolso_ans || ' cancelada com sucesso.';
    p_cod_retorno := 0;


    exception
          when others then
               p_cod_retorno := 9;
               p_msg_retorno := 'RB_PREVIA_REEMBOLSO.CancelaPrevia-' || v_posicao || ': ' || sqlerrm;
               rollback;
    end;
    --
    --
    procedure ReverteCancelamentoPrevia             ( p_num_reembolso        in  number
                                                     , p_cod_usuario         in  varchar2
                                                     , p_cod_motivo          in  number
                                                     ,p_txt_obs_operadora    in  varchar2
                                                     , p_cod_retorno         out number
                                                     , p_msg_retorno         out varchar2
                                                     )  is

    v_ind_situacao      pedido_reembolso_previa.ind_situacao%type;
    v_cod_usuario       usuario.cod_usuario%type;
    v_posicao           pls_integer;
    v_txt_obs           pedido_reembolso_previa.txt_observacao_previa%type;
    v_num_reembolso_ans pedido_reembolso_previa.num_reembolso_ans%type;

    begin

       v_posicao  := 1;

       if p_num_reembolso is null then
           p_msg_retorno := 'Pr�via de reembolso n�o informada, revers�o do cancelamento impossibilitada.';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  ind_situacao, txt_observacao_previa
                    into v_ind_situacao, v_txt_obs
                         from  pedido_reembolso_previa
                                  where num_reembolso = p_num_reembolso;
            exception
               when no_data_found then
                      p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o foi identificada, revers�o do cancelamento impossibilitada.';
                      p_cod_retorno := 1;
                      return;
            end;

            if v_ind_situacao not in (3)  then
               p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o est� em situa��o que permita revers�o do cancelamento.';
               p_cod_retorno := 1;
               return;
           end if;

       end if;
       --
       select nvl(prp.num_reembolso_ans,prp.num_reembolso)
         into v_num_reembolso_ans
         from ts.pedido_reembolso_previa prp
        where prp.num_reembolso = p_num_reembolso;
       --
       v_posicao  := 2;

       if p_cod_usuario is null then
           p_msg_retorno := 'Usu�rio n�o informado';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  cod_usuario
                    into v_cod_usuario
                         from  usuario
                                  where cod_usuario = p_cod_usuario;
            exception
               when no_data_found then
                      p_msg_retorno := 'Usu�rio ' || p_cod_usuario || ' n�o foi identificado, revers�o do cancelamento impossibilitada.';
                      p_cod_retorno := 1;
                      return;
            end;


       end if;

     v_posicao  := 3;
      GeraOcorrencia(p_num_reembolso,null,18,v_txt_obs,p_txt_obs_operadora,p_cod_usuario,p_cod_retorno,p_msg_retorno);

     if p_cod_retorno <> 0 then
         rollback;
         return;
     end if;

      v_posicao  := 4;

      update pedido_reembolso_previa
        set      ind_situacao              = 1
                ,dt_deferimento            = null
                ,cod_usuario_deferimento   = null
                ,dt_indeferimento          = null
                ,cod_usuario_indeferimento = null
                ,dt_cancelamento           = null
                ,cod_usuario_cancelamento  = null
                ,dt_devolucao              = null
                ,cod_usuario_devolucao     = null
                ,dt_sit                    = sysdate
                ,cod_usuario_sit           = p_cod_usuario
                ,cod_motivo                = null
                ,txt_observacao_operadora  = p_txt_obs_operadora
              where num_reembolso        = p_num_reembolso;

      commit;

    p_msg_retorno := 'Opera��o realizada com sucesso';
    p_cod_retorno := 0;

    exception
      when others then
       p_cod_retorno := 9;
       p_msg_retorno := 'RB_PREVIA_REEMBOLSO.ReverteCancelamentoPrevia-' || v_posicao || ': ' || sqlerrm;
       rollback;
    end;
    --
    --
    procedure ReverteFinalizacaoPrevia             ( p_num_reembolso        in  number
                                                     , p_cod_usuario         in  varchar2
                                                     , p_cod_motivo          in  number
                                                     ,p_txt_obs_operadora    in  varchar2
                                                     , p_cod_retorno         out number
                                                     , p_msg_retorno         out varchar2
                                                     )  is
     v_ind_situacao      pedido_reembolso_previa.ind_situacao%type;
     v_cod_usuario       usuario.cod_usuario%type;
     v_posicao           pls_integer;
     v_txt_obs           pedido_reembolso_previa.txt_observacao_previa%type;
    begin

       v_posicao  := 1;

       if p_num_reembolso is null then
           p_msg_retorno := 'Pr�via de reembolso n�o informada, revers�o da finaliza��o impossibilitada.';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  ind_situacao
                    into v_ind_situacao
                         from  pedido_reembolso_previa
                                  where num_reembolso = p_num_reembolso;
            exception
               when no_data_found then
                      p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o foi identificada, revers�o da finaliza��o impossibilitada.';
                      p_cod_retorno := 1;
                      return;
            end;

            if v_ind_situacao not in (2,4)  then
               p_msg_retorno := 'Pr�via de reembolso ' || p_num_reembolso || ' n�o est� em situa��o que permita a revers�o da finaliza��o.';
               p_cod_retorno := 1;
               return;
           end if;

       end if;

       v_posicao  := 2;

       if p_cod_usuario is null then
           p_msg_retorno := 'Usu�rio n�o informado';
            p_cod_retorno := 1;
            return;
       else
           begin
                select  cod_usuario
                    into v_cod_usuario
                         from  usuario
                                  where cod_usuario = p_cod_usuario;
            exception
               when no_data_found then
                      p_msg_retorno := 'Usu�rio ' || p_cod_usuario || ' n�o foi identificado, revers�o da finaliza��o impossibilitada.';
                      p_cod_retorno := 1;
                      return;
            end;


       end if;

     v_posicao  := 3;
      GeraOcorrencia(p_num_reembolso,null,19,null,null,p_cod_usuario,p_cod_retorno,p_msg_retorno);
     if p_cod_retorno <> 0 then
         rollback;
         return;
     end if;

     v_posicao  := 4;
      update pedido_reembolso_previa
        set   ind_situacao              = 1
              ,dt_deferimento            = null
              ,cod_usuario_deferimento   = null
              ,dt_indeferimento          = null
              ,cod_usuario_indeferimento = null
              ,dt_cancelamento           = null
              ,cod_usuario_cancelamento  = null
              ,dt_devolucao              = null
              ,cod_usuario_devolucao     = null
              ,dt_sit                    = sysdate
              ,cod_usuario_sit           = p_cod_usuario
              ,cod_motivo                = null
              ,txt_observacao_operadora  = p_txt_obs_operadora
              where num_reembolso        = p_num_reembolso;

      delete from ts.reembolso_previa_pedido_indef
             where num_reembolso = p_num_reembolso;
    commit;

    p_msg_retorno := 'Opera��o realizada com sucesso';
    p_cod_retorno := 0;


    exception
      when others then
       p_cod_retorno := 9;
       p_msg_retorno := 'RB_PREVIA_REEMBOLSO.ReverteFinalizacaoPrevia-' || v_posicao || ': ' || sqlerrm;
       rollback;
    end;
    --
    --
    function get_filial_unidade ( p_cod_retorno         in out nocopy varchar2
                               , p_msg_retorno         in out nocopy varchar2
                               , p_cod_usuario         in out nocopy varchar2
                               )
    return sys_refcursor
    is
        result                          sys_refcursor;
        v_qtd_filial                    pls_integer;
        v_qtd_unidade                   pls_integer;
        v_cod_tipo_usuario              usuario.cod_tipo_usuario%type;
    begin
        --
        begin
            select cod_tipo_usuario
            into   v_cod_tipo_usuario
            from   usuario
            where  cod_usuario            = upper(p_cod_usuario);
        exception
        when others then
            null;
        end;
        --
        --UNIDADE
        select count(*)
        into   v_qtd_unidade
        from   autorizacao_perfil_nivel apn
        where  apn.cod_tipo_nivel       =  '12'
        and (  exists                  (select null
                                        from   perfil_usuario      pu
                                        where  pu.cod_usuario      = p_cod_usuario
                                        and    pu.cod_perfil       = apn.cod_perfil
                                       )
            or exists                  (select null
                                        from   perfil_tipo_usuario pu
                                        where  pu.cod_tipo_usuario = v_cod_tipo_usuario
                                        and    pu.cod_perfil       = apn.cod_perfil
                                       )
           );
        --
        --FILIAL
        select count(*)
        into   v_qtd_filial
        from   autorizacao_perfil_nivel apn
        where  apn.cod_tipo_nivel       =  '10'
        and (  exists                  (select null
                                        from   perfil_usuario      pu
                                        where  pu.cod_usuario      = p_cod_usuario
                                        and    pu.cod_perfil       = apn.cod_perfil
                                       )
            or exists                  (select null
                                        from   perfil_tipo_usuario pu
                                        where  pu.cod_tipo_usuario = v_cod_tipo_usuario
                                        and    pu.cod_perfil       = apn.cod_perfil
                                       )
           );
        --
        --  Verifica se existe filial e unidade
        if v_qtd_unidade = 0 and v_qtd_filial = 0 then
            -- se nao existir nenhum dos dois, exibe todas as cadastradas no sistema
            open  result
            for   select i.cod_inspetoria_ts
                       --, s.cod_sucursal
                       , i.nome_inspetoria || ' (' || s.nome_sucursal || ')' nome_filial_unidade
                  from   inspetoria i
                       , sucursal   s
                  where  i.cod_sucursal = s.cod_sucursal
                  order by nome_filial_unidade;

        elsif nvl(v_qtd_unidade,0) > 0 then
            -- se tiver algum caso de unidade, exibe apenas as unidades cadastradas, ignorandos as filiais
            open  result
            for   select i.cod_inspetoria_ts
                       --, s.cod_sucursal
                       , i.nome_inspetoria || ' (' || s.nome_sucursal || ')' nome_filial_unidade
                  from   inspetoria i
                       , sucursal   s
                  where  i.cod_sucursal = s.cod_sucursal
                  and    i.cod_inspetoria_ts in (select apn.val_permitido
                                                from   autorizacao_perfil_nivel apn
                                                where  apn.cod_tipo_nivel       =  '12'
                                                and (  exists                   (select null
                                                                                 from   perfil_usuario      pu
                                                                                 where  pu.cod_usuario      = p_cod_usuario
                                                                                 and    pu.cod_perfil       = apn.cod_perfil
                                                                                )
                                                    or exists                   (select null
                                                                                 from   perfil_tipo_usuario pu
                                                                                 where  pu.cod_tipo_usuario = v_cod_tipo_usuario
                                                                                 and    pu.cod_perfil       = apn.cod_perfil
                                                                                )
                                                    )
                                                )
                  order by nome_filial_unidade;

        else
            -- caso contr�rio, ele tem filiais cadastradas, exibe todas as unidades das filiais encontradas.
            open  result
            for   select i.cod_inspetoria_ts
                       --, s.cod_sucursal
                       , i.nome_inspetoria || ' (' || s.nome_sucursal || ')' nome_filial_unidade
                  from   inspetoria i
                       , sucursal   s
                  where  i.cod_sucursal = s.cod_sucursal
                  and    i.cod_sucursal in (select apn.val_permitido
                                                from   autorizacao_perfil_nivel apn
                                                where  apn.cod_tipo_nivel       =  '10'
                                                and (  exists                   (select null
                                                                                 from   perfil_usuario      pu
                                                                                 where  pu.cod_usuario      = p_cod_usuario
                                                                                 and    pu.cod_perfil       = apn.cod_perfil
                                                                                )
                                                    or exists                   (select null
                                                                                 from   perfil_tipo_usuario pu
                                                                                 where  pu.cod_tipo_usuario = v_cod_tipo_usuario
                                                                                 and    pu.cod_perfil       = apn.cod_perfil
                                                                                )
                                                    )
                                                )
                  order by nome_filial_unidade;
        end if;

        return result;

    exception
    when others then
        ts_log_execucao ('RB_PREVIA_REEMBOLSO.get_filial_unidade'
                        , NULL
                        , sqlerrm
                        , 'p_cod_usuario  = ' || p_cod_usuario
                        , 'Erro'
                        ) ;
       --
       return get_cursor_vazio;
    end;
      --
      --
    procedure RetornaProcedimento    ( p_item_medico                in  varchar2
                                     , p_num_associado              in  varchar2
                                     , p_cod_ts_contrato            in  number
                                     , p_num_contrato               in  varchar2
                                     , p_cod_ts_tit                 in  number
                                     , p_num_titular                in  varchar2
                                     , p_cod_plano                  in  varchar2
                                     , p_dt_nascimento              in  varchar2
                                     , p_ind_sexo                   in  varchar2
                                     , p_dt_atendimento             in  varchar2
                                     , p_ind_tipo_reembolso         in  varchar2
                                     , p_qtd_informado              in  number default 1
                                     , p_ind_principal              in  varchar2 default 'N'
                                     , p_ind_via                    in  varchar2 default null
                                     , p_ind_doppler                in  varchar2 default null
                                     , p_cod_motivo_reembolso       in  varchar2 default null
                                     , p_ind_dobra_calculo          in  varchar2 default null
                                     , p_ind_add_anestesista        in  varchar2 default null
                                     , p_cod_inspetoria_ts          in  varchar2 default 4
                                     , p_cod_operadora              in  number   default 1
                                     , p_ind_apenas_consulta        in  varchar2  default 'N'
                                     , p_xml_retorno                out clob
                                     )
    is
    --
    --
    --Declara��es:
    v_posicao                   number;
    v_parametro                 varchar2(4000);
    v_item_vazio                varchar2(3) := '���';
    --
    v_cod_retorno               number;
    v_msg_retorno               varchar2(4000);
    v_nome_item                 itens_medicos.nome_item%type;
    v_ind_cirurgia              itens_medicos.ind_cirurgia%type;
    v_item_medico               itens_medicos.item_medico%type;
    v_xml_funcao                clob;
    --
    v_num_associado             associado.num_associado%type;
    v_dt_atendimento            date;
    v_cod_ts_contrato           associado.cod_ts_contrato%type;
    v_cod_plano                 associado.cod_plano%type;
    v_dt_nascimento             date;
    v_ind_sexo                  entidade_sistema.ind_sexo%type;
    v_ind_tipo_reembolso        pedido_reembolso_previa.ind_tipo_reembolso%type;
    v_cod_procedimento_para     itens_medicos.item_medico%type;
    v_memoria_calculo           varchar2(4000);
    v_qtd_informado             procedimento_reembolso_previa.qtd_informado%type;
    v_ind_via                   procedimento_reembolso_previa.ind_via%type;
    v_ind_principal             procedimento_reembolso_previa.ind_principal%type;
    v_cod_motivo_reembolso      motivo_reembolso.cod_motivo_reembolso%type;
    v_ind_dobra_calculo         procedimento_reembolso_previa.ind_dobra_calculo%type;
    v_ind_add_anestesista       procedimento_reembolso_previa.ind_add_anestesista%type;
    v_ind_origem_anestesista    procedimento_reembolso_previa.ind_origem_anestesista%type;
    v_ind_exibe_dobra_calc      procedimento_reembolso_previa.ind_exibe_dobra_calc%type;
    v_cod_inspetoria_ts         pedido_reembolso_previa.cod_inspetoria_ts_abertura%type;
    v_cod_operadora             pedido_reembolso_previa.cod_operadora_contrato%type;
    v_ind_doppler               procedimento_reembolso_previa.ind_doppler%type;
    v_cod_grupo_estatistico     procedimento_reembolso_previa.cod_grupo_estatistico%type;
    v_cod_ts_tit                pedido_reembolso_previa.cod_ts_tit%type;
    v_num_titular               pedido_reembolso_previa.num_titular%type;
    v_num_contrato              pedido_reembolso_previa.num_contrato%type;
    v_ind_apenas_consulta       varchar2(1);
    --
    v_ind_rol_procedimentos     itens_medicos.ind_rol_procedimentos%type;
    v_ind_genetica              itens_medicos.ind_genetica%type;
    v_ind_diretriz              itens_medicos.ind_diretriz%type;
    --
    begin
        --In�cio
        v_posicao := 0;
        v_parametro := '';
        v_parametro := v_parametro || CHR(13) || 'p_item_medico     = ' || p_item_medico;
        --
        v_posicao := 199;
        --------------------------------------------------------------------------------
        --- Obtem o item informado
        --------------------------------------------------------------------------------
        begin
            select /* RB_PREVIA_REEMBOLSO.RetornaProcedimento */
                   CAST(vp.item_medico AS VARCHAR2(8))
                 , vp.nome_item
                 , nvl(vp.ind_cirurgia,'N')
                 , nvl(im.ind_rol_procedimentos,'N')
                 , nvl(im.ind_genetica,'N')
                 , nvl(im.ind_diretriz,'N')
              into v_item_medico
				         , v_nome_item
                 , v_ind_cirurgia
                 , v_ind_rol_procedimentos
                 , v_ind_genetica
                 , v_ind_diretriz
            from vwm_procedimento vp
               , itens_medicos    im
           where vp.item_medico = im.item_medico(+)
             and vp.item_medico   = p_item_medico
             and rownum           < 2;
        exception
        when no_data_found then
            v_msg_retorno := 'Item n�o encontrado';
            goto trata_retorno_erro;
        when others then
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || 'Item:' || chr(13) || p_item_medico, 'RetornaProcedimento' );
            v_msg_retorno := 'Item n�o encontrado';
            goto trata_retorno_erro;
        end;
        --
        -- Retorna permiss�es do procedimento para fun��es auxiliares
        -- Obs: atualmente retorna todas as fun��es, no futuro haver� valida��es exibir apenas as fun��es do procedimento
        --
        --
        v_num_associado         := p_num_associado;
        v_dt_atendimento        := nvl(TO_DATE(p_dt_atendimento, 'DD/MM/YYYY'),trunc(sysdate));
        v_cod_ts_contrato       := p_cod_ts_contrato;
        v_num_contrato          := p_num_contrato;
        v_cod_plano             := p_cod_plano;
        v_dt_nascimento         := TO_DATE(p_dt_nascimento, 'dd/mm/yyyy');
        v_ind_sexo              := trim(p_ind_sexo);
        v_ind_tipo_reembolso    := p_ind_tipo_reembolso;
        v_ind_via               := p_ind_via;
        v_ind_principal         := p_ind_principal;
        v_cod_motivo_reembolso  := p_cod_motivo_reembolso;
        v_ind_dobra_calculo     := p_ind_dobra_calculo;
        v_ind_add_anestesista   := p_ind_add_anestesista;
        v_cod_inspetoria_ts     := p_cod_inspetoria_ts;
        v_cod_operadora         := p_cod_operadora;
        v_ind_doppler           := p_ind_doppler;
        v_cod_ts_tit            := p_cod_ts_tit;
        v_num_titular           := p_num_titular;
        v_ind_apenas_consulta   := p_ind_apenas_consulta;
        --
        if( nvl(p_qtd_informado,0) < 1 ) then
           v_qtd_informado      := 1;
        else
           v_qtd_informado      := p_qtd_informado;
        end if;
        --
        v_xml_funcao := '';
        for p in ( select fv.* --, fa.nome_funcao
                   from table (   ts.rb_calcula_previa_reembolso.retornafuncoesvalores( v_num_associado
                                                                                     ,  v_dt_atendimento
                                                                                     ,  v_cod_ts_contrato
                                                                                     ,  v_num_contrato
                                                                                     ,  v_cod_ts_tit
                                                                                     ,  v_num_titular
                                                                                     ,  v_item_medico
                                                                                     ,  v_cod_plano
                                                                                     ,  v_dt_nascimento
                                                                                     ,  v_ind_sexo
                                                                                     ,  v_ind_tipo_reembolso
                                                                                     ,  v_qtd_informado
                                                                                     ,  v_ind_via
                                                                                     ,  v_ind_doppler
                                                                                     ,  v_ind_principal
                                                                                     ,  v_cod_motivo_reembolso
                                                                                     ,  v_ind_dobra_calculo
                                                                                     ,  v_ind_add_anestesista
                                                                                     ,  v_cod_inspetoria_ts
                                                                                     ,  v_cod_operadora
                                                                                     ,  v_ind_apenas_consulta
                                                                                      )
                              )  fv --, tipo_funcao_auxiliar fa
                   order by to_number(fv.ind_funcao)
        ) loop
            v_cod_procedimento_para     := p.cod_procedimento;
            v_ind_dobra_calculo         := p.ind_dobra_calculo;
            v_ind_origem_anestesista    := p.ind_origem_anestesista;
            v_cod_grupo_estatistico     := p.cod_grupo_estatistico;
            --
            v_xml_funcao := v_xml_funcao || '<FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<COD_FUNCAO>'            || lpad(NVL(p.ind_funcao,'99'),2,'0') || '</COD_FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<NOME_FUNCAO><![CDATA['  || p.nome_funcao                      || ']]></NOME_FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<PERC_FUNCAO_LABEL>'     || case when nvl(p.perc_funcao,0) = 0 then '-' else to_char(p.perc_funcao) end || '</PERC_FUNCAO_LABEL>';
            v_xml_funcao := v_xml_funcao || '<PERC_FUNCAO>'           || p.perc_funcao                      || '</PERC_FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<COD_GRUPO_ESTATISTICO>' || p.cod_grupo_estatistico            || '</COD_GRUPO_ESTATISTICO>';
            v_xml_funcao := v_xml_funcao || '<PCT_CIRU_MULTIPLA>'     || p.pct_cirurgia_multipla            || '</PCT_CIRU_MULTIPLA>';
            v_xml_funcao := v_xml_funcao || '<VAL_CALCULADO>'         || to_char(p.val_calculado ,'FM999G999G999G990D00','nls_numeric_characters='',.') || '</VAL_CALCULADO>';
            v_xml_funcao := v_xml_funcao || '<IND_TIPO_COMPOSICAO>'   || p.ind_tipo_composicao              || '</IND_TIPO_COMPOSICAO>';
            v_xml_funcao := v_xml_funcao || '<COD_REEMBOLSO>'         || p.cod_reembolso                    || '</COD_REEMBOLSO>';
            v_xml_funcao := v_xml_funcao || '<VAL_COTACAO_RB>'        || to_char(p.val_cotacao_rb ,'999999999999D9999','nls_numeric_characters='',.') || '</VAL_COTACAO_RB>';
            v_xml_funcao := v_xml_funcao || '<COD_PORTE_RB>'          || p.cod_porte_rb                     || '</COD_PORTE_RB>';
            v_xml_funcao := v_xml_funcao || '<SIGLA_TABELA_RB>'       || p.sigla_tabela_rb                  || '</SIGLA_TABELA_RB>';
            v_xml_funcao := v_xml_funcao || '<SIGLA_TABELA_TAXAS>'    || p.sigla_tabela_taxas               || '</SIGLA_TABELA_TAXAS>';
            v_xml_funcao := v_xml_funcao || '<QTD_VEZES_TABELA>'      || p.qtd_vezes_tabela                 || '</QTD_VEZES_TABELA>';
            v_xml_funcao := v_xml_funcao || '<PCT_RECIBO>'            || p.pct_recibo                       || '</PCT_RECIBO>';
            v_xml_funcao := v_xml_funcao || '<VAL_COTACAO_TAXA>'      || to_char(p.val_cotacao_taxa,'999999999999D9999','nls_numeric_characters='',.') || '</VAL_COTACAO_TAXA>';
            v_xml_funcao := v_xml_funcao || '<VAL_LIMITE>'            || to_char(p.val_limite ,'FM999G999G999G990D00','nls_numeric_characters='',.')   || '</VAL_LIMITE>';
            v_xml_funcao := v_xml_funcao || '<VAL_FIXO>'              || to_char(p.val_fixo ,'FM999G999G999G990D00','nls_numeric_characters='',.')     || '</VAL_FIXO>';
            v_xml_funcao := v_xml_funcao || '<QTD_PRAZO_DIAS>'        || p.qtd_prazo_dias                   || '</QTD_PRAZO_DIAS>';
            v_xml_funcao := v_xml_funcao || '<SIGLA_MOEDA>'           || p.sigla_moeda                      || '</SIGLA_MOEDA>';
            v_xml_funcao := v_xml_funcao || '<IND_CIRURGIA>'          || nvl(p.ind_cirurgia,'N')            || '</IND_CIRURGIA>';
            v_xml_funcao := v_xml_funcao || '<COD_CONCESSAO>'         || p.cod_concessao                    || '</COD_CONCESSAO>';
            v_xml_funcao := v_xml_funcao || '<NOME_COBERTURA>'        || p.nome_cobertura                   || '</NOME_COBERTURA>';
            v_xml_funcao := v_xml_funcao || '<TXT_MEMORIA_CALCULO><![CDATA['   || p.txt_memoria_calculo              || ']]></TXT_MEMORIA_CALCULO>';
            v_xml_funcao := v_xml_funcao || '<XML_MEMORIA_CALCULO>'   || p.xml_memoria_calculo              || '</XML_MEMORIA_CALCULO>';
            v_xml_funcao := v_xml_funcao || '</FUNCAO>';
        end loop;
        --
        v_posicao := 8;
        --
        v_posicao := 44;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>0</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<CODIGO>' || p_item_medico || '</CODIGO>';
        p_xml_retorno := p_xml_retorno || '<CODIGO_PARA>' || nvl(v_cod_procedimento_para,p_item_medico) || '</CODIGO_PARA>';
        p_xml_retorno := p_xml_retorno || '<DESCRICAO><![CDATA[' || v_nome_item || ']]></DESCRICAO>';
        p_xml_retorno := p_xml_retorno || '<IND_CIRURGIA>' || v_ind_cirurgia || '</IND_CIRURGIA>';
        p_xml_retorno := p_xml_retorno || '<IND_DOBRA_CALCULO>' || v_ind_dobra_calculo || '</IND_DOBRA_CALCULO>';
        p_xml_retorno := p_xml_retorno || '<IND_ORIGEM_ANESTESISTA>' || v_ind_origem_anestesista || '</IND_ORIGEM_ANESTESISTA>';
        p_xml_retorno := p_xml_retorno || '<IND_ADD_ANESTESISTA>' || v_ind_add_anestesista || '</IND_ADD_ANESTESISTA>';
        p_xml_retorno := p_xml_retorno || '<IND_EXIBE_DOBRA_CALC>' || v_ind_exibe_dobra_calc || '</IND_EXIBE_DOBRA_CALC>';
        p_xml_retorno := p_xml_retorno || '<QTD_INFORMADO>' || v_qtd_informado  || '</QTD_INFORMADO>';
        p_xml_retorno := p_xml_retorno || '<GRUPO_BENEFICIO></GRUPO_BENEFICIO>';
        p_xml_retorno := p_xml_retorno || '<COD_GRUPO_ESTATISTICO>' || v_cod_grupo_estatistico     || '</COD_GRUPO_ESTATISTICO>';
        p_xml_retorno := p_xml_retorno || '<IND_ROL_PROCEDIMENTOS>' || v_ind_rol_procedimentos     || '</IND_ROL_PROCEDIMENTOS>';
        p_xml_retorno := p_xml_retorno || '<IND_DIRETRIZ>'          || v_ind_diretriz              || '</IND_DIRETRIZ>';
        p_xml_retorno := p_xml_retorno || '<IND_GENETICA>'          || v_ind_genetica             || '</IND_GENETICA>';

        p_xml_retorno := p_xml_retorno || v_xml_funcao;
        p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';
        --
        return;
        --
        <<trata_retorno_erro>>
        --
        v_cod_retorno := 9;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>' || v_cod_retorno || '</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO><![CDATA[' || v_msg_retorno || ']]></MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO_COMPLETO><![CDATA[RB_PREVIA_REEMBOLSO.RetornaProcedimento:: ' || v_msg_retorno || ' - ' || ts.top_utl_padrao.msgerro || ' (Posi��o: ' || v_posicao || ')]]></MSG_RETORNO_COMPLETO>';
        p_xml_retorno := p_xml_retorno || '<POSICAO>' || v_posicao || '</POSICAO>';
        p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';
        --
        return;
        --
    exception
        when others then
            --
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || 'Parametros:' || chr(13) || v_parametro, 'RetornaProcedimento' );
            --
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO><![CDATA[' || 'Erro:' || sqlerrm || '(' || ts.top_utl_padrao.msgerro || ')]]></MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<POSICAO>' || v_posicao || '</POSICAO>';
            p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';
            --
            return;
    end RetornaProcedimento;
    --
    --
    procedure RetornaProcedimentoCAM ( p_item_medico                in  varchar2
                                     , p_num_associado              in  varchar2
                                     , p_num_contrato               in  varchar2
                                     , p_num_titular                in  varchar2
                                     , p_cod_plano                  in  varchar2
                                     , p_dt_nascimento              in  varchar2
                                     , p_ind_sexo                   in  varchar2
                                     , p_dt_atendimento             in  varchar2
                                     , p_ind_tipo_reembolso         in  varchar2
                                     , p_qtd_informado              in  number default 1
                                     , p_ind_principal              in  varchar2 default 'N'
                                     , p_ind_via                    in  varchar2 default null
                                     , p_ind_doppler                in  varchar2 default null
                                     , p_cod_motivo_reembolso       in  varchar2 default null
                                     , p_ind_dobra_calculo          in  varchar2 default null
                                     , p_ind_add_anestesista        in  varchar2 default null
                                     , p_cod_inspetoria_ts          in  number   default 4
                                     , p_cod_operadora              in  number   default 1
                                     , p_ind_regulamentado          in  varchar2 default 'S'
                                     , p_xml_retorno                out clob
                                     )
    is
    --
    --
    --Declara��es:
    v_posicao                   number;
    v_parametro                 varchar2(4000);
    v_item_vazio                varchar2(3) := '���';
    --
    v_cod_retorno               number;
    v_msg_retorno               varchar2(4000);
    v_nome_item                 itens_medicos.nome_item%type;
    v_ind_cirurgia              itens_medicos.ind_cirurgia%type;
    v_item_medico               itens_medicos.item_medico%type;
    v_xml_funcao                clob;
    --
    v_num_associado             associado.num_associado%type;
    v_num_contrato              pedido_reembolso.num_contrato%type;
    v_num_titular               pedido_reembolso.num_titular%type;
    v_dt_atendimento            date;
    v_cod_plano                 associado.cod_plano%type;
    v_dt_nascimento             date;
    v_ind_sexo                  entidade_sistema.ind_sexo%type;
    v_ind_tipo_reembolso        pedido_reembolso_previa.ind_tipo_reembolso%type;
    v_cod_procedimento_para     itens_medicos.item_medico%type;
    v_memoria_calculo           varchar2(4000);
    v_qtd_informado             procedimento_reembolso_previa.qtd_informado%type;
    v_ind_via                   procedimento_reembolso_previa.ind_via%type;
    v_ind_principal             procedimento_reembolso_previa.ind_principal%type;
    v_cod_motivo_reembolso      motivo_reembolso.cod_motivo_reembolso%type;
    v_ind_dobra_calculo         procedimento_reembolso_previa.ind_dobra_calculo%type;
    v_ind_add_anestesista       procedimento_reembolso_previa.ind_add_anestesista%type;
    v_ind_origem_anestesista    procedimento_reembolso_previa.ind_origem_anestesista%type;
    v_ind_exibe_dobra_calc      procedimento_reembolso_previa.ind_exibe_dobra_calc%type;
    v_cod_inspetoria_ts         pedido_reembolso_previa.cod_inspetoria_ts_abertura%type;
    v_cod_padrao_origem         itens_medicos.cod_padrao%type;
    v_cod_padrao_destino        itens_medicos.cod_padrao%type;
    v_grupo_beneficio           procedimento_reembolso_previa.grupo_beneficio%type;
    v_ind_regulamentado         varchar2(2);
    v_cod_operadora             operadora.cod_operadora%type;
    v_ind_doppler               procedimento_reembolso_previa.ind_doppler%type;
    v_cod_grupo_estatistico     procedimento_reembolso_previa.cod_grupo_estatistico%type;
    --
    begin
        --In�cio
        v_posicao := 0;
        v_parametro := '';
        v_parametro := v_parametro || CHR(13) || 'p_item_medico     = ' || p_item_medico;
        --
        v_posicao := 199;
        --------------------------------------------------------------------------------
        --- Obtem o item informado
        --------------------------------------------------------------------------------
        begin
            select /* RB_PREVIA_REEMBOLSO.RetornaProcedimento */
                   vp.item_medico                  , vp.nome_item
                 , nvl(vp.ind_cirurgia,'N')
            into   v_item_medico                  , v_nome_item
                 , v_ind_cirurgia
            from   vwm_procedimento vp
            where  vp.item_medico   = p_item_medico
            and    rownum           < 2;
        exception
        when no_data_found then
            v_msg_retorno := 'Item n�o encontrado';
            goto trata_retorno_erro;
        end;
        --
        -- Retorna permiss�es do procedimento para fun��es auxiliares
        -- Obs: atualmente retorna todas as fun��es, no futuro haver� valida��es exibir apenas as fun��es do procedimento
        --
        --
        v_num_associado         := p_num_associado;
        v_num_contrato          := p_num_contrato;
        v_num_titular           := p_num_titular;
        v_dt_atendimento        := nvl(TO_DATE(p_dt_atendimento, 'DD/MM/YYYY'),sysdate);
        v_cod_plano             := p_cod_plano;
        v_dt_nascimento         := TO_DATE(p_dt_nascimento, 'dd/mm/yyyy');
        v_ind_sexo              := trim(p_ind_sexo);
        v_ind_tipo_reembolso    := p_ind_tipo_reembolso;
        v_ind_via               := p_ind_via;
        v_ind_principal         := p_ind_principal;
        v_cod_motivo_reembolso  := p_cod_motivo_reembolso;
        v_ind_dobra_calculo     := p_ind_dobra_calculo;
        v_ind_add_anestesista   := p_ind_add_anestesista;
        v_cod_inspetoria_ts     := p_cod_inspetoria_ts;
        v_cod_operadora         := p_cod_operadora;
        v_ind_doppler           := p_ind_doppler;
        --
        -- Busca de para do procedimento
        --
        begin
            select cod_padrao
              into v_cod_padrao_origem
              from itens_medicos
             where item_medico = p_item_medico;

            if v_cod_padrao_origem  = 'AMB' then
               v_cod_padrao_destino := 'CBHPM';
            else
               v_cod_padrao_destino := 'AMB';
            end if;

            v_cod_procedimento_para := rb_calcula_reembolso.cm_converte_amb_cbhpm_s(p_item_medico,to_date(p_dt_atendimento,'dd/mm/yyyy'),v_cod_padrao_destino);

        exception
            when others then
            v_msg_retorno := 'Item n�o encontrado';
            goto trata_retorno_erro;
        end;
        --
        -- Busca grupo beneficio
        --
        begin
            SELECT CASE WHEN p_ind_regulamentado = 'S'
                    THEN to_char (a.cod_grupo_benef_atual)
                    ELSE to_char (a.cod_grupo_benef_velho)
                    END  grupo_beneficio
            INTO    v_grupo_beneficio
            FROM    amil_item_grupo_benef a
            WHERE  item_medico = p_item_medico;
        exception
            when no_data_found then
                begin
                    SELECT CASE WHEN p_ind_regulamentado = 'S'
                            THEN to_char (a.cod_grupo_benef_atual)
                            ELSE to_char (a.cod_grupo_benef_velho)
                            END  grupo_beneficio
                    INTO    v_grupo_beneficio
                    FROM    amil_item_grupo_benef a
                    WHERE  item_medico = v_cod_procedimento_para;
                exception
                when no_data_found then
                    v_grupo_beneficio := null;
                end;
        end;
        --
        if v_grupo_beneficio is null then
            v_msg_retorno := 'Grupo de Benef�cio n�o encontrado';
            goto trata_retorno_erro;
        end if;
        --
        if ( nvl(p_qtd_informado,0) < 1 ) then
           v_qtd_informado      := 1;
        else
           v_qtd_informado      := p_qtd_informado;
        end if;
        --
        v_xml_funcao := '';
        for p in ( select fv.* --, fa.nome_funcao
                   from table (   ts.rb_calcula_previa_cam.retornafuncoesvalores(
                                       /*p_num_associado          =>*/ v_num_associado
                                     , /*p_num_contrato           =>*/ v_num_contrato
                                     , /*p_num_titular            =>*/ v_num_titular
                                     , /*p_dt_atendimento         =>*/ v_dt_atendimento
                                     , /*p_grupo_beneficio        =>*/ v_grupo_beneficio
                                     , /*p_item_medico            =>*/ v_item_medico
                                     , /*p_cod_plano              =>*/ v_cod_plano
                                     , /*p_dt_nascimento          =>*/ v_dt_nascimento
                                     , /*p_ind_sexo               =>*/ v_ind_sexo
                                     , /*p_ind_tipo_reembolso     =>*/ v_ind_tipo_reembolso
                                     , /*p_qtd_realizada          =>*/ v_qtd_informado
                                     , /*p_ind_via_acesso         =>*/ v_ind_via
                                     , /*p_ind_doppler            =>*/ v_ind_doppler
                                     , /*p_ind_principal          =>*/ v_ind_principal
                                     , /*p_cod_motivo_reembolso   =>*/ v_cod_motivo_reembolso
                                     , /*p_ind_dobra_calculo      =>*/ v_ind_dobra_calculo
                                     , /*p_ind_add_anestesista    =>*/ v_ind_add_anestesista
                                     , /*p_cod_inspetoria_ts      =>*/ v_cod_inspetoria_ts
                                     , /*p_cod_opearadora         =>*/ v_cod_operadora
                                  )
                              )  fv --, tipo_funcao_auxiliar fa
                   --where fa.cod_funcao = fv.ind_funcao
        ) loop
            v_cod_procedimento_para     := p.cod_procedimento;
            v_ind_dobra_calculo         := p.ind_dobra_calculo;
            v_ind_origem_anestesista    := p.ind_origem_anestesista;
            v_ind_exibe_dobra_calc      := p.ind_exibe_dobra_calc;
            v_cod_grupo_estatistico     := p.cod_grupo_estatistico;

            v_xml_funcao := v_xml_funcao || '<FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<COD_FUNCAO>'            || lpad(NVL(p.ind_funcao,'99'),2,'0') || '</COD_FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<NOME_FUNCAO><![CDATA['  || UPPER(p.nome_funcao)               || ']]></NOME_FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<PERC_FUNCAO_LABEL>'     || case when nvl(p.perc_funcao,0) = 0 then '-' else to_char(p.perc_funcao) end || '</PERC_FUNCAO_LABEL>';
            v_xml_funcao := v_xml_funcao || '<PERC_FUNCAO>'           || p.perc_funcao                      || '</PERC_FUNCAO>';
            v_xml_funcao := v_xml_funcao || '<COD_GRUPO_ESTATISTICO>' || p.cod_grupo_estatistico            || '</COD_GRUPO_ESTATISTICO>';
            v_xml_funcao := v_xml_funcao || '<PCT_CIRU_MULTIPLA>'     || p.pct_cirurgia_multipla            || '</PCT_CIRU_MULTIPLA>';
            v_xml_funcao := v_xml_funcao || '<VAL_CALCULADO>'         || to_char(p.val_calculado ,'FM999G999G999G990D00','nls_numeric_characters='',.') || '</VAL_CALCULADO>';
            v_xml_funcao := v_xml_funcao || '<IND_TIPO_COMPOSICAO>'   || p.ind_tipo_composicao              || '</IND_TIPO_COMPOSICAO>';
            v_xml_funcao := v_xml_funcao || '<COD_REEMBOLSO>'         || p.cod_reembolso                    || '</COD_REEMBOLSO>';
            v_xml_funcao := v_xml_funcao || '<VAL_COTACAO_RB>'        || to_char(p.val_cotacao_rb ,'999999999999D9999','nls_numeric_characters='',.') || '</VAL_COTACAO_RB>';
            v_xml_funcao := v_xml_funcao || '<COD_PORTE_RB>'          || p.cod_porte_rb                     || '</COD_PORTE_RB>';
            v_xml_funcao := v_xml_funcao || '<SIGLA_TABELA_RB>'       || p.sigla_tabela_rb                  || '</SIGLA_TABELA_RB>';
            v_xml_funcao := v_xml_funcao || '<SIGLA_TABELA_TAXAS>'    || p.sigla_tabela_taxas               || '</SIGLA_TABELA_TAXAS>';
            v_xml_funcao := v_xml_funcao || '<QTD_VEZES_TABELA>'      || p.qtd_vezes_tabela                 || '</QTD_VEZES_TABELA>';
            v_xml_funcao := v_xml_funcao || '<PCT_RECIBO>'            || p.pct_recibo                       || '</PCT_RECIBO>';
            v_xml_funcao := v_xml_funcao || '<VAL_COTACAO_TAXA>'      || to_char(p.val_cotacao_taxa,'999999999999D9999','nls_numeric_characters='',.') || '</VAL_COTACAO_TAXA>';
            v_xml_funcao := v_xml_funcao || '<VAL_LIMITE>'            || to_char(p.val_limite ,'FM999G999G999G990D00','nls_numeric_characters='',.')   || '</VAL_LIMITE>';
            v_xml_funcao := v_xml_funcao || '<VAL_FIXO>'              || to_char(p.val_fixo ,'FM999G999G999G990D00','nls_numeric_characters='',.')     || '</VAL_FIXO>';
            v_xml_funcao := v_xml_funcao || '<QTD_PRAZO_DIAS>'        || p.qtd_prazo_dias                   || '</QTD_PRAZO_DIAS>';
            v_xml_funcao := v_xml_funcao || '<SIGLA_MOEDA>'           || p.sigla_moeda                      || '</SIGLA_MOEDA>';
            v_xml_funcao := v_xml_funcao || '<IND_CIRURGIA>'          || nvl(p.ind_cirurgia,'N')            || '</IND_CIRURGIA>';
            v_xml_funcao := v_xml_funcao || '<COD_CONCESSAO>'         || p.cod_concessao                    || '</COD_CONCESSAO>';
            v_xml_funcao := v_xml_funcao || '<NOME_COBERTURA>'        || p.nome_cobertura                   || '</NOME_COBERTURA>';
            v_xml_funcao := v_xml_funcao || '<TXT_MEMORIA_CALCULO>'   || p.txt_memoria_calculo              || '</TXT_MEMORIA_CALCULO>';
            v_xml_funcao := v_xml_funcao || '<XML_MEMORIA_CALCULO>'   || p.xml_memoria_calculo              || '</XML_MEMORIA_CALCULO>';
            v_xml_funcao := v_xml_funcao || '</FUNCAO>';
        end loop;
        --
        v_posicao := 8;
        --
        v_posicao := 44;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>0</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<CODIGO>' || p_item_medico || '</CODIGO>';
        p_xml_retorno := p_xml_retorno || '<CODIGO_PARA>' || nvl(v_cod_procedimento_para,p_item_medico) || '</CODIGO_PARA>';
        p_xml_retorno := p_xml_retorno || '<DESCRICAO><![CDATA[' || v_nome_item || ']]></DESCRICAO>';
        p_xml_retorno := p_xml_retorno || '<IND_CIRURGIA>' || v_ind_cirurgia || '</IND_CIRURGIA>';
        p_xml_retorno := p_xml_retorno || '<IND_DOBRA_CALCULO>' || v_ind_dobra_calculo || '</IND_DOBRA_CALCULO>';
        p_xml_retorno := p_xml_retorno || '<IND_ORIGEM_ANESTESISTA>' || v_ind_origem_anestesista || '</IND_ORIGEM_ANESTESISTA>';
        p_xml_retorno := p_xml_retorno || '<IND_ADD_ANESTESISTA>' || v_ind_add_anestesista || '</IND_ADD_ANESTESISTA>';
        p_xml_retorno := p_xml_retorno || '<IND_EXIBE_DOBRA_CALC>' || v_ind_exibe_dobra_calc || '</IND_EXIBE_DOBRA_CALC>';
        p_xml_retorno := p_xml_retorno || '<QTD_INFORMADO>' || v_qtd_informado  || '</QTD_INFORMADO>';
        p_xml_retorno := p_xml_retorno || '<GRUPO_BENEFICIO>' || v_grupo_beneficio  || '</GRUPO_BENEFICIO>';
        p_xml_retorno := p_xml_retorno || '<COD_GRUPO_ESTATISTICO>' || v_cod_grupo_estatistico     || '</COD_GRUPO_ESTATISTICO>';
        --p_xml_retorno := p_xml_retorno || '<TXT_MEMORIA_CALCULO>' || v_memoria_calculo || '</TXT_MEMORIA_CALCULO>';
        p_xml_retorno := p_xml_retorno || v_xml_funcao;
        p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';
        --
        --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 00, 'Debug', p_xml_retorno, 'RetornaProcedimento - XML' );
        --
        return;
        --
        <<trata_retorno_erro>>
        --
        v_cod_retorno := 9;
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>' || v_cod_retorno || '</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO><![CDATA[' || v_msg_retorno || ']]></MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO_COMPLETO><![CDATA[RB_PREVIA_REEMBOLSO.RetornaProcedimento:: ' || v_msg_retorno || ' - ' || ts.top_utl_padrao.msgerro || ' (Posi��o: ' || v_posicao || ')]]></MSG_RETORNO_COMPLETO>';
        p_xml_retorno := p_xml_retorno || '<POSICAO>' || v_posicao || '</POSICAO>';

        p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';
        --
        --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 00, 'ERRO', p_xml_retorno, 'RetornaProcedimento - XML' );
        --
        return;
        --
    exception
        when others then
            --
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', v_posicao, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || 'Parametros:' || chr(13) || v_parametro, 'RetornaProcedimento' );
            --
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO><![CDATA[' || 'Erro:' || sqlerrm || '(' || ts.top_utl_padrao.msgerro || ')]]></MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<POSICAO>' || v_posicao || '</POSICAO>';
            p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';
            --
            return;
    end;
    --
    --
    function RetornaFuncoesProcedimento(  p_num_reembolso              in      number
                                        , p_cod_procedimento_cm        in      varchar2
                                        , p_cod_procedimento           in      varchar2
                                        , p_ind_via                    in      varchar2
                                        , p_num_seq_grupo_procedimento in      number        default 1
                                        )
    return clob is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    xmlRetorno              clob;
    v_qtd_glosa_analise     pls_integer;
    v_qtd_glosa_analisada   pls_integer;
    begin
        --In�cio
        v_posicao := 1;

        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            xmlRetorno := '<?xml version="1.0"?>';
            xmlRetorno := xmlRetorno || '<FUNCAO><DADOS>';
            xmlRetorno := xmlRetorno || '<COD_RETORNO>9</COD_RETORNO>';
            xmlRetorno := xmlRetorno || '<MSG_RETORNO>4 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
            xmlRetorno := xmlRetorno || '</DADOS></FUNCAO>';
            return xmlRetorno;
        END IF;

        v_posicao := 15;

        xmlRetorno := '';
        xmlRetorno := xmlRetorno || '<FUNCOES>';

         for p in ( select prp.num_seq_item
                         , prp.cod_grupo_estatistico
                         , prp.ind_funcao
                         , prp.ind_via
                         , prp.pct_cirurgia_multipla
                         , prp.qtd_informado
                         , to_char(prp.val_apresentado,'FM999G999G999G990D00','nls_numeric_characters='',.') val_apresentado
                         , to_char(prp.val_calculado,'FM999G999G999G990D00','nls_numeric_characters='',.') val_calculado
                         , to_char(prp.val_reembolsado,'FM999G999G999G990D00','nls_numeric_characters='',.') val_reembolsado
                         , prp.ind_tipo_composicao
                         , prp.cod_reembolso
                         , to_char(prp.val_cotacao_rb,'999999999999D9999','nls_numeric_characters='',.') val_cotacao_rb
                         , prp.cod_porte_rb
                         , prp.sigla_tabela_rb
                         , prp.sigla_tabela_taxas
                         , prp.qtd_vezes_tabela
                         , prp.pct_recibo
                         , to_char(prp.val_cotacao_taxa,'999999999999D9999','nls_numeric_characters='',.') val_cotacao_taxa
                         , to_char(prp.val_limite,'FM999G999G999G990D00','nls_numeric_characters='',.') val_limite
                         , to_char(prp.val_fixo,'FM999G999G999G990D00','nls_numeric_characters='',.') val_fixo
                         , prp.qtd_prazo_dias
                         , prp.sigla_moeda
                         , prp.cod_concessao
                         , prp.perc_funcao
                         , prp.ind_situacao_funcao
                         , mrp.txt_memoria txt_memoria_calculo
                         , fa.nome_funcao
                         ,to_char(prp.val_calculado_participacao,'FM999G999G999G990D00','nls_numeric_characters='',.') val_calculado_participacao
                     from  procedimento_reembolso_previa prp
                         , memoria_reembolso_previa mrp
                         , tipo_funcao_auxiliar fa
                    where prp.num_reembolso         = p_num_reembolso
                      and prp.cod_procedimento_cm   = p_cod_procedimento_cm
                      and nvl(prp.ind_via,'X')      = nvl(p_ind_via,'X')
                      and prp.num_reembolso         = mrp.num_reembolso (+)
                      and prp.num_seq_item          = mrp.num_seq_item  (+)
                      and prp.ind_funcao            = fa.cod_funcao (+)
                      and nvl(prp.num_seq_grupo_procedimento,1) = nvl(p_num_seq_grupo_procedimento,1)
                 order by to_number(prp.ind_funcao) asc
        ) loop

           begin
               select sum(qtd_analise)
                    , sum(qtd_analisada)
               into   v_qtd_glosa_analise
                    , v_qtd_glosa_analisada
               from   ( select case when nvl(a.ind_analisado,'N') = 'N' then 1 else 0 end qtd_analise
                             , case when nvl(a.ind_analisado,'N') = 'S' then 1 else 0 end qtd_analisada
                        from   reembolso_previa_glosa   a
                        where  a.num_reembolso          = p_num_reembolso
                          and  a.num_seq_item           = p.num_seq_item
                      );
           exception
           when others then
              v_qtd_glosa_analise   := 0;
              v_qtd_glosa_analisada := 0;
           end;

            xmlRetorno := xmlRetorno || '<FUNCAO>';
            xmlRetorno := xmlRetorno || '<NUM_SEQ_ITEM>'                      || p.num_seq_item               || '</NUM_SEQ_ITEM>';
            xmlRetorno := xmlRetorno || '<COD_GRUPO_ESTATISTICO>'     || p.cod_grupo_estatistico      || '</COD_GRUPO_ESTATISTICO>';
            xmlRetorno := xmlRetorno || '<IND_FUNCAO>'                || p.ind_funcao                     || '</IND_FUNCAO>';
            xmlRetorno := xmlRetorno || '<PCT_CIRURGIA_MULTIPLA>'         || p.pct_cirurgia_multipla          || '</PCT_CIRURGIA_MULTIPLA>';
            xmlRetorno := xmlRetorno || '<QTD_INFORMADO>'                         || p.qtd_informado                  || '</QTD_INFORMADO>';
            xmlRetorno := xmlRetorno || '<VAL_APRESENTADO>'                     || p.val_apresentado                  || '</VAL_APRESENTADO>';
            xmlRetorno := xmlRetorno || '<VAL_CALCULADO>'                     || p.val_calculado                  || '</VAL_CALCULADO>';
            xmlRetorno := xmlRetorno || '<VAL_REEMBOLSADO>'                     || p.val_reembolsado                  || '</VAL_REEMBOLSADO>';
            xmlRetorno := xmlRetorno || '<IND_TIPO_COMPOSICAO>'           || p.ind_tipo_composicao        || '</IND_TIPO_COMPOSICAO>';
            xmlRetorno := xmlRetorno || '<COD_REEMBOLSO>'                     || p.cod_reembolso                  || '</COD_REEMBOLSO>';
            xmlRetorno := xmlRetorno || '<VAL_COTACAO_RB>'                    || p.val_cotacao_rb                 || '</VAL_COTACAO_RB>';
            xmlRetorno := xmlRetorno || '<COD_PORTE_RB>'                      || p.cod_porte_rb                   || '</COD_PORTE_RB>';
            xmlRetorno := xmlRetorno || '<SIGLA_TABELA_RB>'                   || p.sigla_tabela_rb                || '</SIGLA_TABELA_RB>';
            xmlRetorno := xmlRetorno || '<SIGLA_TABELA_TAXAS>'                || p.sigla_tabela_taxas         || '</SIGLA_TABELA_TAXAS>';
            xmlRetorno := xmlRetorno || '<QTD_VEZES_TABELA>'                    || p.qtd_vezes_tabela               || '</QTD_VEZES_TABELA>';
            xmlRetorno := xmlRetorno || '<PCT_RECIBO>'                                || p.pct_recibo                         || '</PCT_RECIBO>';
            xmlRetorno := xmlRetorno || '<VAL_COTACAO_TAXA>'                || p.val_cotacao_taxa               || '</VAL_COTACAO_TAXA>';
            xmlRetorno := xmlRetorno || '<VAL_LIMITE>'                      || p.val_limite               || '</VAL_LIMITE>';
            xmlRetorno := xmlRetorno || '<VAL_FIXO>'                        || p.val_fixo               || '</VAL_FIXO>';
            xmlRetorno := xmlRetorno || '<QTD_PRAZO_DIAS>'                        || p.qtd_prazo_dias                 || '</QTD_PRAZO_DIAS>';
            xmlRetorno := xmlRetorno || '<SIGLA_MOEDA>'                           || p.sigla_moeda                    || '</SIGLA_MOEDA>';
            xmlRetorno := xmlRetorno || '<COD_CONCESSAO>'                 || p.cod_concessao              || '</COD_CONCESSAO>';
            xmlRetorno := xmlRetorno || '<PERC_FUNCAO_LABEL>'              || case when nvl(p.perc_funcao,0) = 0 then '-' else to_char(p.perc_funcao) end || '</PERC_FUNCAO_LABEL>';
            xmlRetorno := xmlRetorno || '<PERC_FUNCAO>'                   || p.perc_funcao                 || '</PERC_FUNCAO>';
            xmlRetorno := xmlRetorno || '<IND_SITUACAO_FUNCAO>'           || p.ind_situacao_funcao         || '</IND_SITUACAO_FUNCAO>';
            xmlRetorno := xmlRetorno || '<TXT_MEMORIA_CALCULO>'           || p.txt_memoria_calculo        || '</TXT_MEMORIA_CALCULO>';
            xmlRetorno := xmlRetorno || '<NOME_FUNCAO>'                       || p.nome_funcao                    || '</NOME_FUNCAO>';
            xmlRetorno := xmlRetorno || '<QTD_GLOSA_ANALISE>'               || to_char(v_qtd_glosa_analise)      || '</QTD_GLOSA_ANALISE>';
            xmlRetorno := xmlRetorno || '<QTD_GLOSA_ANALISADA>'             || to_char(v_qtd_glosa_analisada)    || '</QTD_GLOSA_ANALISADA>';
            xmlRetorno := xmlRetorno || '<VAL_COPART>'             || p.val_calculado_participacao    || '</VAL_COPART>';
            xmlRetorno := xmlRetorno || '</FUNCAO>';

        end loop;

        xmlRetorno := xmlRetorno || '</FUNCOES>';

        return xmlRetorno;
    EXCEPTION
        WHEN OTHERS THEN
            --
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 0, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || 'P_NUM_REEMBSOLO:' || chr(13) || p_num_reembolso, 'RetornaFuncoes' );
            --
            xmlRetorno := '<?xml version="1.0"?>';
            xmlRetorno := xmlRetorno || '<FUNCOES><DADOS>';
            xmlRetorno := xmlRetorno || '<COD_RETORNO>9</COD_RETORNO>';
            xmlRetorno := xmlRetorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            xmlRetorno := xmlRetorno || '<SQL>' || v_SQL || '</SQL>';
            xmlRetorno := xmlRetorno || '</DADOS></FUNCOES>';

            return xmlRetorno;
    END;
    --
    --
    procedure RetornaItensNova ( p_num_reembolso in  varchar2
                               , p_xml_retorno   out clob
                               )
    is
    --Declara��es:
    v_SQL                   varchar2(4000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    v_cod_procedimento      procedimento_reembolso_previa.cod_procedimento%type;
    v_nome_procedimento     itens_medicos.nome_item%type;
    v_txt_memoria_calculo   memoria_reembolso_previa.txt_memoria%type;
    v_qtd_informado         procedimento_reembolso_previa.qtd_informado%type;
    v_ind_situacao          procedimento_reembolso_previa.ind_situacao%type;
    v_ind_principal         procedimento_reembolso_previa.ind_principal%type;
    v_ind_via               procedimento_reembolso_previa.ind_via%type;
    v_ind_cirurgia          procedimento_reembolso_previa.ind_cirurgia%type;
    v_ind_dobra_calculo     procedimento_reembolso_previa.ind_dobra_calculo%type;
    v_ind_origem_anestesista procedimento_reembolso_previa.ind_origem_anestesista%type;
    v_ind_add_anestesista   procedimento_reembolso_previa.ind_add_anestesista%type;
    v_ind_exibe_dobra_calc  procedimento_reembolso_previa.ind_exibe_dobra_calc%type;
    v_grupo_beneficio       procedimento_reembolso_previa.grupo_beneficio%type;
    v_xml_funcao            clob;
    c_nome_cabecalho        varchar2(50) := 'FUNCOES';
    c_nome_coluna           varchar2(50) := 'DADOS';
    v_qtd_funcoes           number;
    v_qtd_glosa_analise     pls_integer;
    v_qtd_glosa_analisada   pls_integer;
    v_ind_doppler           varchar2(1);
    v_cod_grupo_estatistico procedimento_reembolso_previa.cod_grupo_estatistico%type;
    v_cod_especialidade     procedimento_reembolso_previa.cod_especialidade%type;
    v_val_apresentadado     procedimento_reembolso_previa.val_apresentado%type;
    --
    v_tamanho_reembolso     number;
    v_num_reembolso         varchar2(20);
    v_ind_rol_procedimentos     itens_medicos.ind_rol_procedimentos%type;
    v_ind_diretriz              itens_medicos.ind_diretriz%type;
    v_ind_genetica              itens_medicos.ind_genetica%type;
    --
    begin
        --In�cio
        --
        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>5 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</DADOS></PROCEDIMENTO>';
            return;
        END IF;

        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
        --
        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        v_num_reembolso := p_num_reembolso;
        --
        if v_tamanho_reembolso > 15 then
          select p.num_reembolso
            into v_num_reembolso
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        end if;
        --Montar XML dos procedimentos
         for p in ( select   prp.cod_procedimento_cm
                           ,to_char(sum(case when prp.ind_situacao_funcao = 'A' then prp.val_apresentado else 0 end),'FM999G999G999G990D00','nls_numeric_characters='',.') val_apresentado
                           , to_char(sum(prp.val_calculado),'FM999G999G999G990D00','nls_numeric_characters='',.') val_calculado
                           , to_char(sum(case when prp.ind_situacao_funcao = 'A' then prp.val_reembolsado else 0 end),'FM999G999G999G990D00','nls_numeric_characters='',.') val_reembolsado
                           , min(num_seq_item) num_seq_item
                           , ind_via
                           , nvl(cod_especialidade, '') cod_especialidade
                           ,to_char(sum(case when prp.ind_situacao_funcao = 'A' then prp.val_calculado_participacao else 0 end),'FM999G999G999G990D00','nls_numeric_characters='',.') val_calculado_participacao
                           , prp.num_seq_grupo_procedimento
                   from     procedimento_reembolso_previa prp
                  where     prp.num_reembolso = v_num_reembolso
--                    and     prp.val_reembolsado > 0
                  group by prp.cod_procedimento_cm, ind_via, nvl(cod_especialidade, ''), prp.num_seq_grupo_procedimento
                  order by num_seq_item
        ) loop
           begin
             select vp.nome_item
                  , mrp.txt_memoria
                  , prp.qtd_informado
                  , prp.ind_situacao
                  , prp.ind_principal
--                  , prp.ind_via
                  , prp.ind_doppler
                  , prp.ind_cirurgia
                  , prp.ind_dobra_calculo
                  , prp.ind_origem_anestesista
                  , prp.ind_add_anestesista
                  , prp.ind_exibe_dobra_calc
                  , prp.cod_procedimento
                  , prp.grupo_beneficio
                  , prp.cod_grupo_estatistico
                  , prp.cod_especialidade
                  , prp.val_apresentado
                  , NVL(im.ind_rol_procedimentos, 'N') AS ind_rol_procedimentos
                  , NVL(im.ind_diretriz, 'N') AS ind_diretriz
                  , NVL(im.ind_genetica, 'N') AS ind_genetica
             into   v_nome_procedimento
                  , v_txt_memoria_calculo
                  , v_qtd_informado
                  , v_ind_situacao
                  , v_ind_principal
--                  , v_ind_via
                  , v_ind_doppler
                  , v_ind_cirurgia
                  , v_ind_dobra_calculo
                  , v_ind_origem_anestesista
                  , v_ind_add_anestesista
                  , v_ind_exibe_dobra_calc
                  , v_cod_procedimento
                  , v_grupo_beneficio
                  , v_cod_grupo_estatistico
                  , v_cod_especialidade
				  , v_val_apresentadado
                  , v_ind_rol_procedimentos
                  , v_ind_diretriz
                  , v_ind_genetica
             from   procedimento_reembolso_previa  prp
                  , vwm_procedimento               vp
                  , memoria_reembolso_previa       mrp
                  , itens_medicos                  im

             where  prp.num_reembolso             = v_num_reembolso
             and    prp.cod_procedimento_cm        = p.cod_procedimento_cm
             and    nvl(prp.cod_especialidade,'X') = nvl(p.cod_especialidade,'X')
             and    nvl(prp.ind_via,'X')           = nvl(p.ind_via,'X')
             and    prp.num_reembolso              = mrp.num_reembolso (+)
             and    prp.num_seq_item               = mrp.num_seq_item (+)
             and    prp.cod_procedimento_cm        = vp.item_medico (+)
             and    nvl(prp.num_seq_grupo_procedimento,1) = nvl(p.num_seq_grupo_procedimento,1)
             and    prp.cod_procedimento_cm        = im.item_medico
             and    rownum                         = 1;
           exception
           when others then
               null;
           end;
           --
           begin
               select count(*)
               into   v_qtd_funcoes
               from   procedimento_reembolso_previa
               where  num_reembolso         = v_num_reembolso
                 and  cod_procedimento_cm        = p.cod_procedimento_cm
                 and  nvl(ind_via,'X')           = nvl(p.ind_via,'X')
                 and  nvl(cod_especialidade,'X') = nvl(p.cod_especialidade,'X')
                 and  nvl(num_seq_grupo_procedimento,1) = nvl(p.num_seq_grupo_procedimento,1)
               group by cod_procedimento_cm, ind_via;
           exception
           when others then
              v_qtd_funcoes := 0;
           end;
           --
           -- Caso tenha apenas 1 funcao colocar dados de glosas na primeira linha do procedimento
           --
           if v_qtd_funcoes = 1 then
           --
           -- GLOSAS ANALISADAS e ANALISE
           --
               begin
                   select sum(qtd_analise)
                        , sum(qtd_analisada)
                   into   v_qtd_glosa_analise
                        , v_qtd_glosa_analisada
                   from   ( select case when nvl(a.ind_analisado,'N') = 'N' then 1 else 0 end qtd_analise
                                 , case when nvl(a.ind_analisado,'N') = 'S' then 1 else 0 end qtd_analisada
                            from   reembolso_previa_glosa   a
                            where  a.num_reembolso         = v_num_reembolso
                              and  a.num_seq_item          = p.num_seq_item -- pega as glosas do procedimento a partir do item retornado no caso de ser apenas uma funcao
                          );
               exception
               when others then
                  v_qtd_glosa_analise   := 0;
                  v_qtd_glosa_analisada := 0;
               end;
           else
                v_qtd_glosa_analise   := 0;
                v_qtd_glosa_analisada := 0;
           end if;
           --
            p_xml_retorno := p_xml_retorno || '<DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_PROCEDIMENTO_CM>'       || p.cod_procedimento_cm        || '</COD_PROCEDIMENTO_CM>';
            p_xml_retorno := p_xml_retorno || '<COD_PROCEDIMENTO>'          || v_cod_procedimento           || '</COD_PROCEDIMENTO>';
            p_xml_retorno := p_xml_retorno || '<VAL_APRESENTADO>'           || p.val_apresentado            || '</VAL_APRESENTADO>';
            p_xml_retorno := p_xml_retorno || '<VAL_CALCULADO>'             || p.val_calculado              || '</VAL_CALCULADO>';
            p_xml_retorno := p_xml_retorno || '<VAL_REEMBOLSADO>'           || p.val_reembolsado            || '</VAL_REEMBOLSADO>';
            p_xml_retorno := p_xml_retorno || '<QTD_FUNCOES>'               || to_char(v_qtd_funcoes)       || '</QTD_FUNCOES>';
            p_xml_retorno := p_xml_retorno || '<NOME_PROCEDIMENTO>'         || v_nome_procedimento          || '</NOME_PROCEDIMENTO>';
            p_xml_retorno := p_xml_retorno || '<TXT_MEMORIA_CALCULO>'       || v_txt_memoria_calculo        || '</TXT_MEMORIA_CALCULO>';
            p_xml_retorno := p_xml_retorno || '<QTD_INFORMADO>'             || to_char(v_qtd_informado)     || '</QTD_INFORMADO>';
            p_xml_retorno := p_xml_retorno || '<IND_SITUACAO>'              || v_ind_situacao               || '</IND_SITUACAO>';
            p_xml_retorno := p_xml_retorno || '<IND_PRINCIPAL>'             || v_ind_principal              || '</IND_PRINCIPAL>';
            p_xml_retorno := p_xml_retorno || '<IND_VIA>'                   || p.ind_via                    || '</IND_VIA>';
            p_xml_retorno := p_xml_retorno || '<IND_CIRURGIA>'              || v_ind_cirurgia               || '</IND_CIRURGIA>';
            p_xml_retorno := p_xml_retorno || '<IND_DOBRA_CALCULO>'         || v_ind_dobra_calculo          || '</IND_DOBRA_CALCULO>';
            p_xml_retorno := p_xml_retorno || '<IND_ORIGEM_ANESTESISTA>'    || v_ind_origem_anestesista     || '</IND_ORIGEM_ANESTESISTA>';
            p_xml_retorno := p_xml_retorno || '<IND_ADD_ANESTESISTA>'       || v_ind_add_anestesista        || '</IND_ADD_ANESTESISTA>';
            p_xml_retorno := p_xml_retorno || '<IND_EXIBE_DOBRA_CALC>'      || v_ind_exibe_dobra_calc       || '</IND_EXIBE_DOBRA_CALC>';
            p_xml_retorno := p_xml_retorno || '<GRUPO_BENEFICIO>'           || v_grupo_beneficio            || '</GRUPO_BENEFICIO>';
            p_xml_retorno := p_xml_retorno || '<COD_GRUPO_ESTATISTICO>'     || v_cod_grupo_estatistico      || '</COD_GRUPO_ESTATISTICO>';
            p_xml_retorno := p_xml_retorno || '<IND_DOPPLER>'               || v_ind_doppler                || '</IND_DOPPLER>';
            p_xml_retorno := p_xml_retorno || '<IND_ROL_PROCEDIMENTOS>'     || v_ind_rol_procedimentos      || '</IND_ROL_PROCEDIMENTOS>';
            p_xml_retorno := p_xml_retorno || '<IND_DIRETRIZ>'              || v_ind_diretriz               || '</IND_DIRETRIZ>';
            p_xml_retorno := p_xml_retorno || '<IND_GENETICA>'              || v_ind_genetica               || '</IND_GENETICA>';
            -- adiciona fun��es do procedimento
            p_xml_retorno := p_xml_retorno || RetornaFuncoesProcedimento( v_num_reembolso, p.cod_procedimento_cm, v_cod_procedimento,p.ind_via, p.num_seq_grupo_procedimento);
            --
            p_xml_retorno := p_xml_retorno || '<QTD_GLOSA_ANALISE>'   || to_char(v_qtd_glosa_analise)    || '</QTD_GLOSA_ANALISE>';
            p_xml_retorno := p_xml_retorno || '<QTD_GLOSA_ANALISADA>' || to_char(v_qtd_glosa_analisada)  || '</QTD_GLOSA_ANALISADA>';
            p_xml_retorno := p_xml_retorno || '<COD_ESPECIALIDADE>'   || to_char(v_cod_especialidade)    || '</COD_ESPECIALIDADE>';
            p_xml_retorno := p_xml_retorno || '<VAL_COPART_TOTAL>'          || p.val_calculado_participacao        || '</VAL_COPART_TOTAL>';
            p_xml_retorno := p_xml_retorno || '</DADOS>';

        end loop;

        p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';

    EXCEPTION
        WHEN OTHERS THEN
            --
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 0, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || 'P_NUM_REEMBSOLO:' || chr(13) || p_num_reembolso, 'RetornaItens' );
            --
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS></PROCEDIMENTO>';
            return;
    END;
    --

    procedure RetornaItensReemb( p_num_reembolso in  varchar2
                               , p_xml_retorno   out clob
                               )
    is
    --Declara��es:
    v_SQL                   varchar2(4000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;
    v_cod_procedimento      procedimento_reembolso_previa.cod_procedimento%type;
    v_nome_procedimento     itens_medicos.nome_item%type;
    v_txt_memoria_calculo   memoria_reembolso_previa.txt_memoria%type;
    v_qtd_informado         procedimento_reembolso_previa.qtd_informado%type;
    v_ind_situacao          procedimento_reembolso_previa.ind_situacao%type;
    v_ind_principal         procedimento_reembolso_previa.ind_principal%type;
    v_ind_via               procedimento_reembolso_previa.ind_via%type;
    v_ind_cirurgia          procedimento_reembolso_previa.ind_cirurgia%type;
    v_ind_dobra_calculo     procedimento_reembolso_previa.ind_dobra_calculo%type;
    v_ind_origem_anestesista procedimento_reembolso_previa.ind_origem_anestesista%type;
    v_ind_add_anestesista   procedimento_reembolso_previa.ind_add_anestesista%type;
    v_ind_exibe_dobra_calc  procedimento_reembolso_previa.ind_exibe_dobra_calc%type;
    v_especialidade         procedimento_reembolso_previa.cod_especialidade%type;
    v_nome_especialidade    especialidade.nome_especialidade%type;
    v_cod_funcao            tipo_funcao_auxiliar.cod_funcao%type;
    v_nome_funcao           tipo_funcao_auxiliar.nome_funcao%type;
    v_xml_funcao            clob;
    c_nome_cabecalho        varchar2(50) := 'FUNCOES';
    c_nome_coluna           varchar2(50) := 'DADOS';
    v_qtd_funcoes           number;
    v_qtd_glosa_analise     pls_integer;
    v_qtd_glosa_analisada   pls_integer;
    v_tamanho_reembolso     number;
    v_num_reembolso         varchar(20);
    --
    begin
        --In�cio
        --
        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        if v_tamanho_reembolso > 15 then
          select p.num_reembolso
            into v_num_reembolso
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        else
          v_num_reembolso := p_num_reembolso;
        end if;

        --Valida��o
        IF NVL(v_num_reembolso,0) = 0 THEN
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>6 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</DADOS></PROCEDIMENTO>';
            return;
        END IF;

        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO>';
        --
        --Montar XML dos procedimentos
         for p in ( select   prp.cod_procedimento_cm
                           --, to_char(sum(prp.val_calculado),'FM999G999G999G990D00','nls_numeric_characters='',.') val_calculado
                           , to_char(prp.val_calculado,'FM999G999G999G990D00','nls_numeric_characters='',.') val_calculado
                           --, to_char(sum(prp.val_reembolsado),'FM999G999G999G990D00','nls_numeric_characters='',.') val_reembolsado
                           , to_char(prp.val_reembolsado,'FM999G999G999G990D00','nls_numeric_characters='',.') val_reembolsado
                           --, min(num_seq_item) num_seq_item
                           , num_seq_item
                           , ind_via
                           , prp.cod_especialidade
                   from     procedimento_reembolso_previa prp
                  where     prp.num_reembolso = v_num_reembolso
                    and     prp.ind_situacao_funcao = 'A'
                    --and     prp.val_reembolsado > 0
                  --group by prp.cod_procedimento_cm, ind_via, prp.cod_especialidade
                  order by num_seq_item
        ) loop
           begin
             select im.nome_item
                  , mrp.txt_memoria
                  , prp.qtd_informado
                  , prp.ind_situacao
                  , prp.ind_principal
--                  , prp.ind_via
                  , prp.ind_cirurgia
                  , prp.ind_dobra_calculo
                  , prp.ind_origem_anestesista
                  , prp.ind_add_anestesista
                  , prp.ind_exibe_dobra_calc
                  , prp.cod_procedimento
                  , prp.cod_especialidade
                  , e.nome_especialidade
                  , f.cod_funcao
                  , f.nome_funcao
             into   v_nome_procedimento
                  , v_txt_memoria_calculo
                  , v_qtd_informado
                  , v_ind_situacao
                  , v_ind_principal
--                  , v_ind_via
                  , v_ind_cirurgia
                  , v_ind_dobra_calculo
                  , v_ind_origem_anestesista
                  , v_ind_add_anestesista
                  , v_ind_exibe_dobra_calc
                  , v_cod_procedimento
                  , v_especialidade
                  , v_nome_especialidade
                  , v_cod_funcao
                  , v_nome_funcao
             from   procedimento_reembolso_previa prp
                  , itens_medicos                 im
                  , memoria_reembolso_previa      mrp
                  , especialidade                 e
                  , tipo_funcao_auxiliar          f
             where  prp.num_reembolso             = v_num_reembolso
             and    prp.num_seq_item              = p.num_seq_item
             and    prp.cod_procedimento_cm       = p.cod_procedimento_cm
             and    nvl(prp.ind_via,'X')          = nvl(p.ind_via,'X')
             and    nvl(prp.cod_especialidade,'X')= nvl(p.cod_especialidade,'X')
             and    prp.num_reembolso             = mrp.num_reembolso (+)
             and    prp.num_seq_item              = mrp.num_seq_item (+)
             and    prp.cod_procedimento_cm       = im.item_medico (+)
             and    prp.cod_especialidade         = e.cod_especialidade (+)
             and    prp.ind_funcao                = f.cod_funcao  (+)
       --      and    prp.val_reembolsado           > 0
             and    rownum                        = 1;
           exception
           when others then
               null;
           end;
           --
           begin
               select count(*)
               into   v_qtd_funcoes
               from   procedimento_reembolso_previa
               where  num_reembolso              = v_num_reembolso
                 and  cod_procedimento_cm        = p.cod_procedimento_cm
                 and  nvl(ind_via,'X')           = nvl(p.ind_via,'X')
                 and  nvl(cod_especialidade,'X') = nvl(p.cod_especialidade,'X')
              group by cod_procedimento_cm, ind_via, cod_especialidade;
           exception
           when others then
              v_qtd_funcoes := 0;
           end;
           --
           -- Caso tenha apenas 1 funcao colocar dados de glosas na primeira linha do procedimento
           --
           if v_qtd_funcoes = 1 then
           --
           -- GLOSAS ANALISADAS e ANALISE
           --
               begin
                   select sum(qtd_analise)
                        , sum(qtd_analisada)
                   into   v_qtd_glosa_analise
                        , v_qtd_glosa_analisada
                   from   ( select case when nvl(a.ind_analisado,'N') = 'N' then 1 else 0 end qtd_analise
                                 , case when nvl(a.ind_analisado,'N') = 'S' then 1 else 0 end qtd_analisada
                            from   reembolso_previa_glosa   a
                            where  a.num_reembolso         = v_num_reembolso
                              and  a.num_seq_item          = p.num_seq_item -- pega as glosas do procedimento a partir do item retornado no caso de ser apenas uma funcao
                          );
               exception
               when others then
                  v_qtd_glosa_analise   := 0;
                  v_qtd_glosa_analisada := 0;
               end;
           else
                v_qtd_glosa_analise   := 0;
                v_qtd_glosa_analisada := 0;
           end if;
           --

            p_xml_retorno := p_xml_retorno || '<DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_PROCEDIMENTO_CM>'       || p.cod_procedimento_cm        || '</COD_PROCEDIMENTO_CM>';
            p_xml_retorno := p_xml_retorno || '<COD_PROCEDIMENTO>'          || v_cod_procedimento           || '</COD_PROCEDIMENTO>';
            p_xml_retorno := p_xml_retorno || '<VAL_CALCULADO>'             || p.val_calculado              || '</VAL_CALCULADO>';
            p_xml_retorno := p_xml_retorno || '<VAL_REEMBOLSADO>'           || p.val_reembolsado            || '</VAL_REEMBOLSADO>';
            p_xml_retorno := p_xml_retorno || '<QTD_FUNCOES>'               || to_char(v_qtd_funcoes)       || '</QTD_FUNCOES>';
            p_xml_retorno := p_xml_retorno || '<NOME_PROCEDIMENTO>'         || v_nome_procedimento          || '</NOME_PROCEDIMENTO>';
            p_xml_retorno := p_xml_retorno || '<TXT_MEMORIA_CALCULO>'       || v_txt_memoria_calculo        || '</TXT_MEMORIA_CALCULO>';
            p_xml_retorno := p_xml_retorno || '<QTD_INFORMADO>'             || to_char(v_qtd_informado)     || '</QTD_INFORMADO>';
            p_xml_retorno := p_xml_retorno || '<IND_SITUACAO>'              || v_ind_situacao               || '</IND_SITUACAO>';
            p_xml_retorno := p_xml_retorno || '<IND_PRINCIPAL>'             || v_ind_principal              || '</IND_PRINCIPAL>';
            p_xml_retorno := p_xml_retorno || '<IND_VIA>'                   || p.ind_via                    || '</IND_VIA>';
            p_xml_retorno := p_xml_retorno || '<IND_CIRURGIA>'              || v_ind_cirurgia               || '</IND_CIRURGIA>';
            p_xml_retorno := p_xml_retorno || '<IND_DOBRA_CALCULO>'         || v_ind_dobra_calculo          || '</IND_DOBRA_CALCULO>';
            p_xml_retorno := p_xml_retorno || '<IND_ORIGEM_ANESTESISTA>'    || v_ind_origem_anestesista     || '</IND_ORIGEM_ANESTESISTA>';
            p_xml_retorno := p_xml_retorno || '<IND_ADD_ANESTESISTA>'       || v_ind_add_anestesista        || '</IND_ADD_ANESTESISTA>';
            p_xml_retorno := p_xml_retorno || '<IND_EXIBE_DOBRA_CALC>'      || v_ind_exibe_dobra_calc       || '</IND_EXIBE_DOBRA_CALC>';

            -- adiciona fun��es do procedimento
            p_xml_retorno := p_xml_retorno || RetornaFuncoesProcedimento( v_num_reembolso, p.cod_procedimento_cm, v_cod_procedimento,p.ind_via);
            --
            p_xml_retorno := p_xml_retorno || '<QTD_GLOSA_ANALISE>'   || to_char(v_qtd_glosa_analise)    || '</QTD_GLOSA_ANALISE>';
            p_xml_retorno := p_xml_retorno || '<QTD_GLOSA_ANALISADA>' || to_char(v_qtd_glosa_analisada)  || '</QTD_GLOSA_ANALISADA>';
            p_xml_retorno := p_xml_retorno || '<COD_ESPECIALIDADE>'   || v_especialidade                 || '</COD_ESPECIALIDADE>';
            p_xml_retorno := p_xml_retorno || '<NOME_ESPECIALIDADE>'  || v_nome_especialidade            || '</NOME_ESPECIALIDADE>';
            p_xml_retorno := p_xml_retorno || '<COD_FUNCAO>'          || to_char(v_cod_funcao)           || '</COD_FUNCAO>';
            p_xml_retorno := p_xml_retorno || '<NOME_FUNCAO>'         || v_nome_funcao                   || '</NOME_FUNCAO>';

            p_xml_retorno := p_xml_retorno || '</DADOS>';

        end loop;

        p_xml_retorno := p_xml_retorno || '</PROCEDIMENTO>';

        --TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 0, 'XML', p_xml_retorno, 'RetornaItens' );

    EXCEPTION
        WHEN OTHERS THEN
            --
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 0, 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm || chr(13) || ts.top_utl_padrao.msgerro || chr(13) || 'P_NUM_REEMBSOLO:' || chr(13) || v_num_reembolso, 'RetornaItens' );
            --
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<PROCEDIMENTO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS></PROCEDIMENTO>';
            return;
    END;
    --
    --
    procedure RetornaFuncoes(p_num_reembolso in number, p_xml_retorno out clob) is

    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    qryCtx                  DBMS_XMLGEN.ctxHandle;

    begin
        --In�cio
        v_posicao := 1;

        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<FUNCAO><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>7 - N�mero da pr�via de reembolso n�o informado</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</DADOS></FUNCAO>';
            return;
        END IF;

        v_posicao := 15;

        --Monta XML das participa��es
        v_SQL := '';
        v_SQL := v_SQL || 'SELECT  prp.*';
        v_SQL := v_SQL || ' , mrp.txt_memoria txt_memoria_calculo';
        v_SQL := v_SQL || ' , fa.nome_funcao  ';
        v_SQL := v_SQL || 'FROM  procedimento_reembolso_previa prp';
        v_SQL := v_SQL || ' , memoria_reembolso_previa mrp';
        v_SQL := v_SQL || ' , tipo_funcao_auxiliar fa  ';
        v_SQL := v_SQL || ' ( select sum(qtd_analise) qtd_glosa_analise';
        v_SQL := v_SQL || '      , sum(qtd_analisada) qtd_glosa_analisada';
        v_SQL := v_SQL || ' from   ( select case when nvl(a.ind_analisado,''N'') = ''N'' then 1 else 0 end qtd_analise';
        v_SQL := v_SQL || '              , case when nvl(a.ind_analisado,''N'') = ''S'' then 1 else 0 end qtd_analisada';
        v_SQL := v_SQL || '         from   reembolso_previa_glosa   a';
        v_SQL := v_SQL || '         where  a.num_reembolso          = prp.num_reembolso';
        v_SQL := v_SQL || '           and  a.num_seq_item          = prp.num_seq_item';
        v_SQL := v_SQL || '       ) ) glosas';
        v_SQL := v_SQL || 'WHERE prp.num_reembolso = :num_reembolso ';
        v_SQL := v_SQL || '  AND prp.num_reembolso = mrp.num_reembolso (+) ';
        v_SQL := v_SQL || '  AND prp.num_seq_item  = mrp.num_seq_item  (+) ';
        v_SQL := v_SQL || '  AND prp.ind_funcao    = fa.cod_funcao (+) ';
        v_SQL := v_SQL || 'ORDER BY prp.cod_procedimento_cm, prp.ind_funcao, prp.num_seq_item ';

        qryCtx := dbms_xmlgen.newContext(v_SQL);
        dbms_xmlgen.setBindValue(qryCtx, 'num_reembolso', p_num_reembolso);
        dbms_xmlgen.setCheckInvalidChars(qryCtx, TRUE);
        dbms_xmlgen.useNullAttributeIndicator(qryCtx, TRUE);
        dbms_xmlgen.setRowSetTag ( qryCtx, 'FUNCAO' );
        dbms_xmlgen.setRowTag (qryCtx, 'DADOS');
        p_xml_retorno := dbms_xmlgen.getXML(qryCtx);
        dbms_xmlgen.closeContext(qryCtx);

    EXCEPTION
        WHEN OTHERS THEN

            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<FUNCOES><DADOS>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || SQLERRM || '</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
            p_xml_retorno := p_xml_retorno || '</DADOS></FUNCOES>';

            return;
    END;
    --
    function get_rs_grupo_usuario ( p_cod_usuario   in varchar2 , p_num_pedido  in varchar2 default null) return sys_refcursor
    is
      result              sys_refcursor;
      str_sql             varchar2(32000);
      v_cod_tipo_usuario  pls_integer;
    begin
        --
        --
        str_sql :=  rtrim (' select  a.cod_grupo_previa          ')
                ||  rtrim ('       , a.nom_grupo_previa          ')
                ||  rtrim ('       , a.nom_grupo_previa          ')
                ||  rtrim (' from   reemb_previa_grupo    a      ')
                ||  rtrim (' where a.cod_grupo_previa not in ( select cod_grupo_previa from pedido_reembolso_previa_grupo where num_reembolso = :num_reembolso and ind_situacao = ''1'' ) ');


        open result
        for  str_sql
        using p_num_pedido;
        --
        return result;
        --
    exception
    when others then
        --
        ts_log_execucao ('RB_PREVIA_REEMBOLSO'
                        , NULL
                        , sqlerrm
                        , p_cod_usuario || chr(13) || str_sql
                        , 'Erro: get_rs_grupo_usuario'
                        ) ;
        raise;
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Retornar as unidades que o usu�rio tem acesso
    ----------------------------------------------------------------------------
    function get_in_unidade_usuario ( p_cod_usuario           in varchar2 )
    return varchar2
    is
        v_cod_tipo_usuario          usuario.cod_tipo_usuario%type;
        v_cod_identificacao_ts      usuario.cod_identificacao_ts%type;
        v_retorno                   varchar2(4000);
    begin
        begin
            select cod_tipo_usuario         , cod_identificacao_ts
            into   v_cod_tipo_usuario       , v_cod_identificacao_ts
            from   ts_sec.usuario
            where  cod_usuario              = p_cod_usuario;
        exception
        when no_data_found then
            return '0';
        end;
        --
        begin
            --
            if v_cod_tipo_usuario = 1 then
                -- Pegar do prestador
                select to_char(cod_inspetoria_ts)
                into   v_retorno
                from   prestador_servico
                where  cod_prestador_ts = v_cod_identificacao_ts;
            else
                --Pegar do perfil
                select ts_util.aggvarchar (cast (collect (to_char (val_permitido)) as lst_varchar_4k),',')
                into   v_retorno
                from ( select distinct apn.val_permitido
                       from   autorizacao_perfil_nivel apn
                       where  apn.cod_tipo_nivel       =  '12'
                       and  (    exists ( select null
                                          from   perfil_usuario      pu
                                          where  pu.cod_usuario      = p_cod_usuario
                                          and    pu.cod_perfil       = apn.cod_perfil
                                        )
                              or exists ( select null
                                          from   perfil_tipo_usuario pu
                                          where  pu.cod_tipo_usuario = v_cod_tipo_usuario
                                          and    pu.cod_perfil       = apn.cod_perfil
                                        )
                            )
                     );
                --
            end if;
        exception
        when no_data_found then
            return '0';
        end;
        --
        return nvl(v_retorno,'0');
        --
    end;
    --
    --
   ----------------------------------------------------------------------------
    -- Retornar as operadoras que o usu�rio tem acesso
    ----------------------------------------------------------------------------
    function get_in_operadora_usuario ( p_cod_usuario           in varchar2 )
    return varchar2
    is
        v_cod_tipo_usuario          usuario.cod_tipo_usuario%type;
        v_cod_identificacao_ts      usuario.cod_identificacao_ts%type;
        v_retorno                   varchar2(4000);
    begin
        begin
            select cod_tipo_usuario         , cod_identificacao_ts
            into   v_cod_tipo_usuario       , v_cod_identificacao_ts
            from   usuario
            where  cod_usuario              = p_cod_usuario;
        exception
        when no_data_found then
            return '0';
        end;
        --
        begin
            --
            if v_cod_tipo_usuario = 1 then
                --Usu�rio prestador
                select ts_util.aggvarchar (cast (collect (to_char (cod_operadora)) as lst_varchar_4k),',')
                into   v_retorno
                from   prestador_operadora
                where  cod_prestador_ts     = v_cod_identificacao_ts
                and    ind_situacao         in ('A','I');
            else
                -- Pegar do perfil
                select ts_util.aggvarchar (cast (collect (to_char (val_permitido)) as lst_varchar_4k),',')
                into   v_retorno
                from ( select distinct apn.val_permitido
                       from   autorizacao_perfil_nivel apn
                       where  apn.cod_tipo_nivel       =  '11'
                       and  (    exists ( select null
                                          from   perfil_usuario      pu
                                          where  pu.cod_usuario      = p_cod_usuario
                                          and    pu.cod_perfil       = apn.cod_perfil
                                        )
                              or exists ( select null
                                          from   perfil_tipo_usuario pu
                                          where  pu.cod_tipo_usuario = v_cod_tipo_usuario
                                          and    pu.cod_perfil       = apn.cod_perfil
                                        )
                            )
                     );
                --
            end if;
            --
        exception
        when no_data_found then
            return '0';
        end;
        --
        return nvl(v_retorno,'0');
        --
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Retornar um cursor com os pedidos em analise para o grupo informado
    ----------------------------------------------------------------------------
    function get_rs_pedido_por_grupo ( p_xml_parametros in clob )
    return sys_refcursor
    is
        cur             sys_refcursor;
        tab_parans      top_utl_xml.tbl_fields;
        str_sql         varchar2(32000);
    begin
        --
        tab_parans := top_utl_xml.toCollection( p_xml_parametros ); -- Retorna um MAP ( chave , valor ) com os n�s do XML;
        --
        if  not tab_parans.exists('cod_operadora'                 ) then tab_parans('cod_operadora').valor(1) := null;
        end if;
        --
        if  not tab_parans.exists('cod_inspetoria_ts'             ) then tab_parans('cod_inspetoria_ts').valor(1) := null;
        end if;
        --
        if  not tab_parans.exists('ind_origem'                    ) then tab_parans('ind_origem').valor(1) := null;
        end if;
        --
        if  not tab_parans.exists('ind_abrangencia'               ) then tab_parans('ind_abrangencia').valor(1) := 'N';
        end if;
        --
        str_sql := trim(' select distinct  a.num_reembolso                                      ' )
                ||rtrim('      , a.nome_associado                                               ' )
                ||rtrim('      , a.num_associado                                                ' )
                ||rtrim('      , to_char(a.dt_inclusao, ''dd/mm/yyyy'') data_solicitacao        ' )
                ||rtrim('      , a.dt_inclusao dt_ordem                                         ' )
                ||rtrim('      , o.nom_operadora                                                ' )
                ||rtrim(' from   pedido_reembolso_previa          a                             ' )
                ||rtrim('      , operadora                        o                             ' )
                ||rtrim('      , ts.pedido_reembolso_previa_grupo p_r_p_g                       ' )
                ||rtrim('      , ts.reemb_previa_grupo            r_p_g                         ' )
                ||rtrim(' where  a.ind_situacao               IN (5, 1)                         ' )
                ||rtrim(' and    p_r_p_g.num_reembolso        = a.num_reembolso                 ' )
                ||rtrim(' and    p_r_p_g.ind_situacao         = 1                               ' )
                ||rtrim(' and    a.cod_operadora_contrato     = o.cod_operadora(+)              ' )
                ||rtrim(' and    p_r_p_g.cod_grupo_previa     = :cod_grupo_encaminhamento       ' );
        --
        if nvl(tab_parans('cod_operadora').valor(1),'X') <> 'X' then
            str_sql := str_sql || rtrim(' and    a.cod_operadora_contrato      in  ( select /*+cardinality(x,10)*/ x.column_value            ');
            str_sql := str_sql || rtrim('                                      from table( top_utl_padrao.split(:cod_operadora,'','')) x )   ');
        else
            str_sql := str_sql || rtrim(' and    ''X''                         = :cod_operadora ' );
        end if;
        --
        if nvl(tab_parans('ind_abrangencia').valor(1),'N') <> 'N' then
            if nvl(tab_parans('cod_inspetoria_ts').valor(1),'X') <> 'X' then
                if nvl(tab_parans('ind_abrangencia').valor(1),'A') = 'A' then
                    str_sql := str_sql || rtrim(' and    a.cod_inspetoria_ts_contrato     in  ( select /*+cardinality(x,10)*/ x.column_value            ');
                    str_sql := str_sql || rtrim('                                  from table( top_utl_padrao.split(:cod_inspetoria_ts,'','')) x )      ');
                else
                    str_sql := str_sql || rtrim(' and    a.cod_inspetoria_ts_abertura     in  ( select /*+cardinality(x,10)*/ x.column_value            ');
                    str_sql := str_sql || rtrim('                                  from table( top_utl_padrao.split(:cod_inspetoria_ts,'','')) x )      ');
                end if;
            else
                str_sql := str_sql || rtrim(' and    ''X''                                =  :cod_inspetoria_ts' );
            end if;
        else
            str_sql := str_sql || rtrim(' and    ''X''                            =  :cod_inspetoria_ts ' );
            tab_parans('cod_inspetoria_ts').valor(1) := 'X';
        end if;
        str_sql := str_sql || ' order by a.dt_inclusao ';
        --
        --ts_log_execucao ( 'RB_PREVIA_REEMBOLSO', 999, 'debug', p_xml_parametros || chr(13) || str_sql, 'get_rs_pedido_por_grupo');
        --
        open   cur
        for    str_sql
        using  tab_parans('cod_grupo_encaminhamento').valor(1)
            ,  nvl(tab_parans('cod_operadora').valor(1),'X')
            ,  nvl(tab_parans('cod_inspetoria_ts').valor(1),'X');
        --
        tab_parans.delete;
        --
        return cur;
        --
    exception
    when others then
        tab_parans.delete;
        raise_application_error( -20001 , 'RB_PREVIA_REEMBOLSO.get_rs_pedido_por_grupo - Ocorreu o seguinte erro: ' || sqlerrm || ts.top_utl_padrao.msgerro);
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Retornar um XML com as fun��es de reembolso que o usuario possui permiss�o
    ----------------------------------------------------------------------------
    procedure get_xml_permissoes    ( p_xml_retorno      out clob
                                    , p_cod_retorno      out number
                                    , p_msg_retorno      out varchar2
                                    , p_cod_usuario      in varchar2
                                    , p_cod_tipo_usuario in varchar2
                                    )
    is
        str_Sql                         Varchar2(32000) := '';
        str_Xml                         clob;
        v_cod_mod_previa_reembolso      Varchar2(2) := '11';
    begin
        --
        p_cod_retorno := 0;
        p_msg_retorno := '';
        --
        --Montar SQL
        --
        str_Sql := '';
        str_Sql := str_Sql || ' select u.cod_usuario        , upper (ap.cod_funcao) cod_funcao';
        str_Sql := str_Sql || ' from   usuario              u';
        str_Sql := str_Sql || '      , perfil_tipo_usuario  ptu';
        str_Sql := str_Sql || '      , autorizacao_perfil   ap';
        str_Sql := str_Sql || ' where  u.cod_usuario        = :cod_usuario';
        str_Sql := str_Sql || ' and    u.cod_tipo_usuario   = ptu.cod_tipo_usuario';
        str_Sql := str_Sql || ' and    ptu.cod_perfil       = ap.cod_perfil';
        str_Sql := str_Sql || ' and    exists (select null';
        str_Sql := str_Sql || '                from   ts.funcao f';
        str_Sql := str_Sql || '                where  f.cod_funcao = ap.cod_funcao';
        str_Sql := str_Sql || '                and    f.cod_modulo = :cod_modulo)';
        str_Sql := str_Sql || ' union ';
        str_Sql := str_Sql || ' select u.cod_usuario        , upper (ap.cod_funcao) cod_funcao';
        str_Sql := str_Sql || ' from   usuario              u';
        str_Sql := str_Sql || '      , perfil_usuario       pu';
        str_Sql := str_Sql || '      , autorizacao_perfil   ap';
        str_Sql := str_Sql || ' where  u.cod_usuario        = :cod_usuario';
        str_Sql := str_Sql || ' and    u.cod_usuario        = pu.cod_usuario';
        str_Sql := str_Sql || ' and    pu.cod_perfil        = ap.cod_perfil';
        str_Sql := str_Sql || ' and    exists (select null';
        str_Sql := str_Sql || '                from   ts.funcao f';
        str_Sql := str_Sql || '                where  f.cod_funcao = ap.cod_funcao';
        str_Sql := str_Sql || '                and    f.cod_modulo = :cod_modulo)';
        --
        add_parametro_sql ( p_nome_parametro          => 'cod_usuario'
                              , p_valor_parametro     => p_cod_usuario
                              );
        --
        add_parametro_sql ( p_nome_parametro        => 'cod_modulo'
                              , p_valor_parametro   => v_cod_mod_previa_reembolso
                              );

        --
        --Montar XML com resultado da pesquisa
        str_Xml := gerar_xml ( p_sql            => str_Sql
                             , p_nome_cabecalho => 'USUARIO_FUNCAO'
                             , p_nome_coluna    => 'DADOS'
                             );
        --
        p_xml_retorno := str_Xml;
        --
        /* ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                        , 999
                        , 'Erro n�o previsto'
                        , str_Xml
                        , 'get_xml_funcao'
                        );*/

        return;
        --
    exception
    when others then
        --
        p_cod_retorno := 9;
        p_msg_retorno := 'Ocorreu o seguinte erro: ' || sqlerrm || ' ( ' || top_utl_padrao.msgerro  || ' )';
        --
        ts_log_execucao ( 'rb_previa_reembolso'
                        , 999
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || top_utl_padrao.msgerro
                        , 'get_xml_funcao'
                        );
        --
    end;
    --

  ----------------------------------------------------------------------------
  -- Encaminhar pedido de autoriza��o
  ----------------------------------------------------------------------------
  procedure processa_encaminhamento ( p_num_pedido in  varchar2
                                    , p_cod_grupo_encaminhamento in  varchar2
                                    , p_cod_usuario  in  varchar2
                                    , p_cod_retorno  out pls_integer
                                    , p_msg_retorno  out varchar2
                                    , p_ind_reencaminhamento     in varchar2 default 'N'
                                    )

  is
      v_num_pedido                    pedido_reembolso_previa_grupo.num_reembolso%type;
      v_num_seq_pedido                pls_integer;
      v_txt_obs_operadora             varchar2(4000);
      v_txt_obs_operadora_ant         varchar2(4000);
      v_txt_obs_emissao               varchar2(4000);
      v_nom_grupo_aut                 reemb_previa_grupo.nom_grupo_previa%type;
      v_cod_retorno                   pls_integer;
      v_msg_retorno                   varchar2(4000);
      v_qtd                           pls_integer;
      v_dt_liberacao_ant              pedido_reembolso_previa_grupo.dt_atu%type;
      v_cod_usuario_liberacao_ant     pedido_reembolso_previa_grupo.cod_usuario_atu%type;
      v_num_seq_liberacao             number;
  BEGIN
      --
      p_cod_retorno := 0;
      p_msg_retorno := '';
      --
      if p_cod_grupo_encaminhamento is null then
         p_cod_retorno := 1;
         p_msg_retorno := 'Grupo n�o foi informado.';
         return;
      end if;
      --
      v_num_pedido := null;
      begin
          select /*  rb_previa_reembolso.processa_encaminhamento */
                 txt_observacao_operadora
          into   v_txt_obs_operadora
          from   pedido_reembolso_previa
          where  num_reembolso            = p_num_pedido;
          --
      exception
      when no_data_found then
          p_cod_retorno := 1;
          P_msg_retorno := 'Encaminhamento n�o realizado. Pedido n�o encontrado.';
          return;
      end;
      --
      if v_cod_retorno != 0 then
          p_cod_retorno :=  v_cod_retorno;
          p_msg_retorno := 'Encaminhamento do pedido ' || p_num_pedido || ' n�o pode ser realizado, veja lista de ocorr�ncias';
      else
          --
          select nvl(max(num_seq_liberacao),0)+1
          into   v_num_seq_liberacao
          from   pedido_reembolso_previa_grupo
          where  num_reembolso            = p_num_pedido
          and    cod_grupo_previa         = p_cod_grupo_encaminhamento;
          --
          -- Tudo OK, continua encaminhamento
          begin
              --
              insert
              into /* rb_previa_reembolso.processa_encaminhamento */
                  pedido_reembolso_previa_grupo
                ( num_reembolso,                cod_grupo_previa,
                  ind_tipo,                     ind_encaminhamento,
                  dt_atu,                       cod_usuario_atu,
                  cod_usuario_encam,            ind_situacao,
                  dt_encaminhamento,            num_seq_liberacao)
              values
                ( p_num_pedido,                 p_cod_grupo_encaminhamento,
                  1,                            'S',
                  sysdate,                      p_cod_usuario,
                  p_cod_usuario,                '1',
                  sysdate,                      v_num_seq_liberacao);
              --
              begin
                  select  nom_grupo_previa
                  into    v_nom_grupo_aut
                  from    reemb_previa_grupo
                  where   cod_grupo_previa   = p_cod_grupo_encaminhamento;
              exception
              when no_data_found then
                  v_nom_grupo_aut := null;
              end;
               --
              -- n�o gera ocorrencia em caso de reencaminhamento
              if p_ind_reencaminhamento <> 'S' then
                  GeraOcorrencia ( p_num_pedido,null,35,null,v_txt_obs_operadora,p_cod_usuario,p_cod_retorno,p_msg_retorno);
              end if;
              --
              if p_cod_retorno != 0 then
                 rollback;
                 return;
              end if;
              --
          exception
          when dup_val_on_index then
              --
              select /*  rb_previa_reembolso.processa_encaminhamento */
                     dt_atu             , cod_usuario_atu
              into   v_dt_liberacao_ant       , v_cod_usuario_liberacao_ant
              from   pedido_reembolso_previa_grupo
              where  num_reembolso            = p_num_pedido
              and    cod_grupo_previa         = p_cod_grupo_encaminhamento
              and    rownum                   < 2;
              --
              update /*  rb_previa_reembolso.processa_encaminhamento */
                     pedido_reembolso_previa_grupo
              set   ind_tipo                   = 1
                   , ind_encaminhamento        = 'S'
                   , cod_usuario_atu               = p_cod_usuario
                   , dt_atu                    = sysdate
              where  num_reembolso             = p_num_pedido
              and    cod_grupo_previa  = p_cod_grupo_encaminhamento;
              --
              -- gera ocorrencia de Cancelamento encaminhamento ----------------
              if nvl(v_cod_usuario_liberacao_ant,'X') != 'X' then
                  begin
                      v_txt_obs_operadora_ant := nvl(v_txt_obs_operadora, ' ') || '-> Anteriormente aprovado por ' || v_cod_usuario_liberacao_ant || ' em ' || v_dt_liberacao_ant || '. ' ;
                  exception
                  when others then
                      null;
                  end;
                  --
                  GeraOcorrencia ( p_num_pedido,null,36,null,v_txt_obs_operadora,p_cod_usuario,p_cod_retorno,p_msg_retorno);

                  if p_cod_retorno != 0 then
                      rollback;
                      return;
                  end if;
              end if;
          end;
          --
          p_cod_retorno :=  0;
          p_msg_retorno := 'Encaminhamento do pedido ' || p_num_pedido || ' realizado.';
          --
      end if;
      --
      commit;
      --
  exception
  when others then
      p_cod_retorno := 9;
      p_msg_retorno := 'processa_encaminhamento - '||p_num_pedido|| '-' || ': ' || sqlerrm;
      rollback;
  end;
  --
  --
  ----------------------------------------------------------------------------
  -- Executar reencaminhamento dos pedidos para grupos de analise
  ----------------------------------------------------------------------------
  procedure processa_reencaminhamento ( p_cod_retorno    out number
                                      , p_msg_retorno    out varchar2
                                      , p_xml_parametros in   clob
                                      )
  is
      v_cod_grupo_encaminhamento      reemb_previa_grupo.cod_grupo_previa%type;
      v_qtd                           pls_integer := 0;
      --
      cursor cur_rcItens              (pXML           in sys.XMLType) is
      select extractValue( VALUE(T) , '//NUM_PEDIDO'                      ) NUM_PEDIDO
           , extractValue( VALUE(T) , '//COD_USUARIO'                     ) COD_USUARIO
           , extractValue( VALUE(T) , '//COD_GRUPO_ENCAMINHAMENTO_DE'     ) COD_GRUPO_ENCAMINHAMENTO_DE
           , extractValue( VALUE(T) , '//COD_GRUPO_ENCAMINHAMENTO_PARA'   ) COD_GRUPO_ENCAMINHAMENTO_PARA
           , extractValue( VALUE(T) , '//COD_GRUPO_ENCAMINHAMENTO_ITEM'   ) COD_GRUPO_ENCAMINHAMENTO_ITEM
      from   table ( xmlsequence ( extract(pXML,'/REENCAMINHAMENTO/ITEM') ) ) T;
      --
      V_XML                           sys.XMLType;
      --
      v_ind_encaminha                 number;
  BEGIN
      --
      p_cod_retorno := 0;
      p_msg_retorno := null;
      --
      -- Verificar lista informada
      V_XML := sys.XMLType.createXML( p_xml_parametros );
      --
      for rcItens in cur_rcItens (V_XML) loop
          --
          if rcItens.COD_GRUPO_ENCAMINHAMENTO_DE is null then
              p_cod_retorno := 1;
              p_msg_retorno := 'Grupo An�lise a ser encaminhado � obrigat�rio.';
              return;
          end if;
          --
          --
          v_cod_grupo_encaminhamento := case when rcItens.COD_GRUPO_ENCAMINHAMENTO_ITEM is not null then rcItens.COD_GRUPO_ENCAMINHAMENTO_ITEM -- Encaminhar o pedido para o grupo selecionado no grid
                                             when rcItens.COD_GRUPO_ENCAMINHAMENTO_PARA is not null then rcItens.COD_GRUPO_ENCAMINHAMENTO_PARA -- Encaminhar o pedido para o grupo selecionado na cabe�a
                                             else                                                        null
                                        end;
          --
          --
          if v_cod_grupo_encaminhamento is not null then
              --
              v_qtd := v_qtd + 1;
              -- Colocar o pedido como encerrado e encerrar
               update /* rb_previa_reembolso.processa_reencaminhamento */
                      pedido_reembolso_previa_grupo
               set    cod_usuario_liberacao     = rcItens.COD_USUARIO
                    , dt_liberacao              = sysdate
                    , txt_obs                   = 'Encerramento autom�tico executado pelo reencaminhamento'
                    , ind_situacao              = 2
                    , ind_encaminhamento        = 'S'
              where   num_reembolso             = rcItens.NUM_PEDIDO
                and   cod_grupo_previa          = rcItens.COD_GRUPO_ENCAMINHAMENTO_DE;
              --
              -- Gerar ocorrencia
              GeraOcorrencia ( rcItens.NUM_PEDIDO,null,38,null,'Pedido reencaminhado do grupo ' || rcItens.COD_GRUPO_ENCAMINHAMENTO_DE || ' para o grupo ' || v_cod_grupo_encaminhamento,rcItens.COD_USUARIO,p_cod_retorno,p_msg_retorno);
              --
              if p_cod_retorno != 0 then
                  rollback;
                  return;
              end if;
              --
              -- Verifica se a Pr�via de Reembolso j� esta pendente no grupo a ser encaminhado, se sim, ignora.
              begin
                select count(*)
                  into v_ind_encaminha
                  from pedido_reembolso_previa_grupo
                 where num_reembolso     = rcItens.NUM_PEDIDO
                   and cod_grupo_previa  = v_cod_grupo_encaminhamento
                   and ind_situacao      = 1;
              exception
                when others then
                   v_ind_encaminha := 0;
              end;
              --
              if v_ind_encaminha = 0 then
                  -- Encaminhar o pedido para o grupo selecionado na cabe�a
                  processa_encaminhamento ( p_num_pedido               => rcItens.NUM_PEDIDO
                                          , p_cod_grupo_encaminhamento => v_cod_grupo_encaminhamento
                                          , p_cod_usuario              => rcItens.COD_USUARIO
                                          , p_cod_retorno              => p_cod_retorno
                                          , p_msg_retorno              => p_msg_retorno
                                          , p_ind_reencaminhamento     => 'S'
                                          );

                  --
                  if p_cod_retorno != 0 then
                      rollback;
                      return;
                  end if;
                  --
              end if;
              --
          end if;
          --
      end loop;
      --
      if v_qtd = 0 then
          p_cod_retorno :=  9;
          p_msg_retorno := 'Nenhum grupo de destino foi selecionado.';
          rollback;
      else
          p_cod_retorno :=  0;
          p_msg_retorno := 'Reencaminhamento realizado.';
          commit;
      end if;
      --
  exception
  when others then
      p_cod_retorno := 9;
      p_msg_retorno := 'processa_reencaminhamento : ' ||sqlerrm || '( ' || ts.top_utl_padrao.msgerro || ')';
      rollback;
  end;
  --
  --
  ---------------------------------------------------------------------------
    -- Encaminhar pedido de autoriza��o para um ou mais grupos de analise
    ----------------------------------------------------------------------------
    procedure processa_encaminhamento_mult ( p_cod_retorno        out number
                                           , p_msg_retorno        out varchar2
                                           , p_xml_dados          in  clob
                                           , p_num_pedido         in  varchar2
                                           , p_txt_obs_operadora  in  varchar2
                                           , p_cod_usuario        in  varchar2
                                           )
    is
        --
        V_XML                           sys.XMLType;
        --
        cursor cur_Item                (pXML           in sys.XMLType) is
        select extractValue( VALUE(T) , '//COD_GRUPO_ENCAMINHAMENTO') COD_GRUPO_ENCAMINHAMENTO
        from   table ( xmlsequence ( extract(pXML,'/ROOT/ENCAMINHAMENTO/ITEM') ) ) T;
        --
        v_ind_tipo_encaminhamento       varchar2(1);
        v_num_seq_encaminhamento        pls_integer;
        v_cod_grupo_tecnico             reemb_previa_grupo.cod_grupo_previa%type;
        v_xml_dados                     clob;
        v_txt_obs_operadora             pedido_reembolso_previa.txt_observacao_operadora%type;
        v_qtd                           pls_integer;
        v_cod_usuario_analise_tec       varchar2(20);
        v_cod_tipo_usuario              pls_integer;
        --
    begin
        --
        p_cod_retorno := 0;
        p_msg_retorno := null;
        --
        if p_num_pedido is null then
            p_cod_retorno := 1;
            p_msg_retorno := 'N�mero da pr�via do Reembolso � obrigat�rio.';
            return;
        end if;
        --
        ----------------------
        -- Trata XML informado
        ----------------------
        v_xml_dados := p_xml_dados;
        --
        TOP_XML_VALIDAR ( p_cod_retorno, p_msg_retorno, v_xml_dados );
        --
        if p_cod_retorno != 0 then
            TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO', 99, 'Erro ao validar XML', p_xml_dados || chr(13) || p_msg_retorno, 'trata_xml_dados' );
            p_cod_retorno := 9;
            p_msg_retorno := 'Erro ao validar XML de entrada: ' || p_msg_retorno;
            return;
        end if;
        --
        --
        update /*  rb_previa_reembolso.processa_encaminhamento_mult */
               pedido_reembolso_previa
        set    txt_observacao_operadora = p_txt_obs_operadora
        where  num_reembolso            = p_num_pedido;
        --
        begin
            select extractValue( VALUE(T) , '//IND_TIPO_ENCAMINHAMENTO'     ) IND_TIPO_ENCAMINHAMENTO
            into   v_ind_tipo_encaminhamento
            from   table ( xmlsequence ( extract(sys.XMLType.createXML(v_xml_dados),'/ROOT/ENCAMINHAMENTO') ) ) T;
        exception
        when no_data_found then
            v_ind_tipo_encaminhamento := 'E';
        end;
        --
        select /* rb_previa_reembolso.processa_encaminhamento_mult */
               txt_observacao_operadora
        into   v_txt_obs_operadora
        from   pedido_reembolso_previa
        where  num_reembolso           = p_num_pedido;
        --
         begin
            select cod_tipo_usuario
            into   v_cod_tipo_usuario
            from   usuario
            where  cod_usuario = p_cod_usuario;
        exception
        when no_data_found then
            v_cod_tipo_usuario := 0;
        end;
        --
        if nvl(v_ind_tipo_encaminhamento,'E') = 'T' then --E-Encaminhamento ou T-Transfer�ncia
            --
           for C_item in ( select /* rb_reembolso.processa_encaminhamento */
                                   cod_grupo_previa
                                 , num_seq_liberacao
                                 , rowid
                            from   pedido_reembolso_previa_grupo
                            where  ind_situacao     = '1'
                            and    num_reembolso    = p_num_pedido
                            and    cod_grupo_previa in ( select pgp.cod_grupo_previa
                                                         from   reemb_previa_grupo_perfil pgp
                                                         where  exists        ( select cod_perfil
                                                                                from   ( select pu.cod_perfil
                                                                                         from   perfil_usuario pu
                                                                                         where  pu.cod_usuario = p_cod_usuario
                                                                                         union
                                                                                         select ptu.cod_perfil
                                                                                         from   perfil_tipo_usuario ptu
                                                                                         where  ptu.cod_tipo_usuario = v_cod_tipo_usuario
                                                                                       ) x
                                                                                where  x.cod_perfil = pgp.cod_perfil
                                                                              )
                                                      )
                          )
            loop
                --Caso exista, colocar como analisado, por�m colocando uma obs
                  update /* rb_previa_reembolso.processa_encaminhamento_mult */
                         pedido_reembolso_previa_grupo
                  set    cod_usuario_liberacao     = p_cod_usuario
                       , dt_liberacao              = sysdate
                       , txt_obs                   = substr(txt_obs || ' -- Transferido',1,500)
                       , ind_situacao              = 2
                       , ind_encaminhamento        = 'S'
                  where  rowid                     = C_item.rowid;
                  --
                  --Gerar ocorr�ncia de Transfer�ncia
                  GeraOcorrencia ( p_num_pedido,null,45,null,v_txt_obs_operadora,p_cod_usuario,p_cod_retorno,p_msg_retorno);
                  --
                if p_cod_retorno != 0 then
                   rollback;
                   return;
                end if;
                --
            end loop;
            --
        end if;
        --
        V_XML := sys.XMLType.createXML( v_xml_dados );
        --
       for rcItem in cur_Item (V_XML) loop
            --
            -- ENCAMINHAR O PEDIDO PARA O GRUPO SELECIONADO
            processa_encaminhamento ( p_num_pedido               => p_num_pedido
                                    , p_cod_grupo_encaminhamento => rcItem.COD_GRUPO_ENCAMINHAMENTO
                                    , p_cod_usuario              => p_cod_usuario
                                    , p_cod_retorno              => p_cod_retorno
                                    , p_msg_retorno              => p_msg_retorno
                                    );
            --
            if p_cod_retorno != 0 then
                rollback;
                return;
            end if;
            --
        end loop;
        --
        --
        p_cod_retorno :=  0;
        if nvl(v_ind_tipo_encaminhamento,'E') = 'T' then --E-Encaminhamento ou T-Transfer�ncia
            p_msg_retorno := 'Transfer�ncia realizada com sucesso';
        else
            p_msg_retorno := 'Encaminhamento realizado com sucesso';
        end if;
        --
        commit;
        --
    /* if  p_cod_retorno = 0 then
          p_cod_retorno :=  0;
          p_msg_retorno := 'Encaminhamento realizado com sucesso.';
          commit;
     end if;*/
    exception
    when others then
        --
        p_cod_retorno := 9;
        p_msg_retorno := 'rb_previa_reembolso.processa_encaminhamento_mult::' || ': ' || sqlerrm || '(' || ts.top_utl_padrao.msgerro || ')';
        rollback;
        TS_LOG_EXECUCAO ( 'rb_previa_reembolso', 9, sqlerrm, p_xml_dados, 'processa_encaminhamento_mult' );
    end;
    --
    --
    ----------------------------------------------------------------------------
    -- Retorna xml com as informa��es das Ocorr�ncias
    ----------------------------------------------------------------------------
    function RetornaGrupoAnalisePrevia(p_num_reembolso in  number) return sys_refcursor
    is
    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    c                       sys_refcursor;
    begin
        --In�cio
        v_posicao := 1;
        ---
        --Valida��o
        IF NVL(p_num_reembolso,0) = 0 THEN
            --return get_cursor_vazio;
            raise_application_error( -20001 , 'rb_previa_reembolso.RetornaGrupoAnalisePrevia - N� da pr�via n�o informado');
        END IF;
        v_posicao := 15;
        --Monta XML dos grupos de an�lise
        v_SQL := '';
        v_SQL := v_SQL || ' select b.nom_grupo_previa              , b.cod_grupo_previa';
        v_SQL := v_SQL || '      , a.cod_usuario_liberacao         , a.txt_obs';
        v_SQL := v_SQL || '      , a.ind_situacao                  , a.num_seq_liberacao';
        v_SQL := v_SQL || '      , a.cod_usuario_encam';
        v_SQL := v_SQL || '      , to_char(a.dt_liberacao,''DD/MM/YYYY'')      dt_liberacao';
        v_SQL := v_SQL || '      , to_char(a.dt_encaminhamento,''DD/MM/YYYY'') dt_encaminhamento';
        v_SQL := v_SQL || ' from   pedido_reembolso_previa_grupo   a';
        v_SQL := v_SQL || '      , reemb_previa_grupo              b';
        v_SQL := v_SQL || ' where  a.num_reembolso                 = :num_reembolso';
        v_SQL := v_SQL || ' and    a.cod_grupo_previa              = b.cod_grupo_previa';
        v_SQL := v_SQL || ' order by b.nom_grupo_previa desc';
        --
        v_posicao := 16;
        --
       /* ts_log_execucao ( 'rb_previa_reembolso'
                        , 10
                        , 'p_num_reembolso=' || p_num_reembolso
                        , v_SQL
                        , 'get_cursor_vazio' );*/

        open  c
        for   v_SQL
        using p_num_reembolso;
        --
        return c;
    EXCEPTION
    WHEN OTHERS THEN
        ts_log_execucao ( 'rb_previa_reembolso'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Erro-ORA:'               || sqlerrm
                                  || chr(13) || v_SQL
                        , 'RetornaGrupoAnalisePrevia' );
        --
        raise_application_error( -20001 , 'rb_previa_reembolso.RetornaGrupoAnalisePrevia - Ocorreu o seguinte erro: ' || ts.top_utl_padrao.msgerro);

        --return get_cursor_vazio;
    END;
    --
    -----------------------------------------
    -- Listar autoriza��es do benefici�rio
    -----------------------------------------
    procedure RetornaAutorizacao ( p_num_associado in  varchar2
                                 , p_cod_tratamento in  number default 0
                                 , p_ind_internado  in  varchar2 default 'N'
                                 , p_xml_retorno   out clob
                                 , p_dt_inicio     in  date
                                 , p_dt_fim        in  date
                                 , p_cod_situacao  in  number default 0
                                 )
    IS
    -- Declara��es:
    v_SQL VARCHAR2(4000);
BEGIN
    -- Montar XML do pedidos
    v_SQL := TRIM('SELECT 0 COD_RETORNO')
          || RTRIM('      , a.num_pedido')
          || RTRIM('      , tt.nome_tratamento')
          || RTRIM('      , a.cod_cid_principal')
          || RTRIM('      , TO_CHAR(a.data_solicitacao, ''dd/mm/yyyy'') data_solicitacao')
          || RTRIM('      , TO_CHAR(a.data_autorizacao, ''dd/mm/yyyy'') data_autorizacao')
          || RTRIM('      , TO_CHAR(a.data_internacao, ''dd/mm/yyyy'') data_internacao')
          || RTRIM('      , TO_CHAR(a.data_provavel_internacao, ''dd/mm/yyyy'') data_provavel_internacao')
          || RTRIM('      , NVL(ps.nome_prestador, a.nome_prestador) nome_executante')
          || RTRIM('      , a.cod_situacao')
          || RTRIM('      , asi.nom_situacao')
          || RTRIM(' FROM autorizacao a')
          || RTRIM('      , tipo_tratamento tt')
          || RTRIM('      , prestador_servico ps')
          || RTRIM('      , ts.autorizacao_situacao asi')
          || RTRIM(' WHERE a.cod_situacao IN (1, 2, 6, 7, 9,15, 16, 22, 23)')
          || RTRIM(' AND a.num_associado = :num_associado')
          || RTRIM(' AND asi.cod_situacao = a.cod_situacao');

    IF p_dt_inicio  is not null and p_dt_fim is not null THEN
      v_SQL := v_SQL || RTRIM(' AND a.data_solicitacao between :dt_inicio AND :dt_fim');
    END IF;

     IF p_cod_tratamento != 0 THEN
        v_SQL := v_SQL || RTRIM(' AND a.cod_tratamento = :cod_tratamento');
    END IF;

    IF  p_cod_situacao != 0 THEN
        v_SQL := v_SQL || RTRIM(' AND asi.cod_situacao = :cod_situacao ');
    END IF;


    IF NVL(p_ind_internado, 'N') = 'S' THEN
        v_SQL := v_SQL || RTRIM(' AND tt.ind_internado = ''S''');
    END IF;

    v_SQL := v_SQL
          || RTRIM(' AND a.cod_tratamento = tt.cod_tratamento')
          || RTRIM(' AND a.cod_prestador_exec = ps.cod_prestador_ts (+)')
          || RTRIM(' ORDER BY a.data_autorizacao');

    -- Adicionar par�metros
    add_parametro_sql(p_nome_parametro => 'num_associado', p_valor_parametro => p_num_associado);

    IF p_cod_tratamento != 0 THEN
        add_parametro_sql(p_nome_parametro => 'cod_tratamento', p_valor_parametro => p_cod_tratamento);
    END IF;

    IF p_cod_situacao != 0 THEN
        add_parametro_sql(p_nome_parametro => 'cod_situacao', p_valor_parametro => p_cod_situacao);
    END IF;

    IF p_dt_inicio  is not null and p_dt_fim is not null THEN
       add_parametro_sql(p_nome_parametro => 'dt_inicio', p_valor_parametro => p_dt_inicio);
        add_parametro_sql(p_nome_parametro => 'dt_fim', p_valor_parametro => p_dt_fim);
    END IF;
    -- Montar XML com resultado da pesquisa
    p_xml_retorno := gerar_xml(p_sql => v_SQL, p_nome_cabecalho => 'AUTORIZACAO', p_nome_coluna => 'DADOS');

EXCEPTION
    WHEN OTHERS THEN
        TS_LOG_EXECUCAO('RB_REEMBOLSO', 0, 'Erro n�o previsto', 'Erro:' || CHR(13) || SQLERRM || CHR(13) || ts.top_utl_padrao.msgerro || CHR(13) || 'v_SQL:' || CHR(13) || v_SQL, 'RetornaAutorizacao');
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<AUTORIZACAO><DADOS>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>' || sqlerrm || '(' || ts.ts_util.msgerro || ')' || '</MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<SQL>' || v_SQL || '</SQL>';
        p_xml_retorno := p_xml_retorno || '</DADOS></AUTORIZACAO>';
        return;
    END;
    --
    --
    procedure RetornaNumAutorizacao                 ( p_num_associado       in  varchar2
                                                    , p_num_pedido          in  varchar2
                                                    , p_msg_retorno         out varchar2)

    is
        --Declara��es:
        v_possui                   number(1);
    begin
        --Montar XML do pedidos
         select 1
              into v_possui
              from   autorizacao          a
              where  a.num_pedido       = p_num_pedido
              and    a.num_associado      = p_num_associado;
         if v_possui = 1 then
            p_msg_retorno := '1';
         else
            p_msg_retorno := '0';
         end if;
        --
    exception
    when no_data_found then
           p_msg_retorno := '0';
    end;


    --
    --
        --  RETORNA OCORR�NCIA POR CONTRATO E BENEFICI�RIO
    --
    function  retornaocorrenciaAssCon (p_cod_ts_contrato   in number, p_cod_ts in number)
    return sys_refcursor
    is
       v_ind_ocorrencia number;
       v_Sql            varchar2(3200);
       c                       sys_refcursor;
    begin

        begin

           v_Sql :=  rtrim(' select   ''Contrato'' tipo                          ')
                    || rtrim('  ,     oa.dt_ocorrencia                                          ')
                    || rtrim('  , toc.nome_ocorrencia                                           ')
                    || rtrim('  , oa.txt_obs                                                    ')
                    || rtrim('  , oa.cod_ocorrencia                                             ')
                    || rtrim('  , TO_NUMBER(oa.cod_ts_contrato)                                 ')
                    || rtrim('  , oa.cod_usuario_atu                                            ')
                    || rtrim('  from ocorrencia_contrato oa                                     ')
                    || rtrim('    , tipo_ocorrencia_contrato toc                                ')
                    || rtrim('  where oa.cod_ocorrencia = toc.cod_ocorrencia                    ')
                    || rtrim('   and oa.cod_ocorrencia = 60                                     ')
                    || rtrim('   and cod_ts_contrato = to_char(:p_cod_ts_contrato)              ')
                    || rtrim(' union all                                                        ')
                    || rtrim(' select ''Benefici�rio'' tipo,                                    ')
                    || rtrim('     oa.dt_ocorrencia,                                            ')
                    || rtrim('   toa.nome_ocorrencia,                                           ')
                    || rtrim('   oa.txt_obs,                                                    ')
                    || rtrim('   oa.cod_ocorrencia,                                             ')
                    || rtrim('   oa.cod_ts ,                                                    ')
                    || rtrim('   oa.cod_usuario_atu                                             ')
                    || rtrim(' from ocorrencia_associado oa, tipo_ocorrencia_associado toa      ')
                    || rtrim(' where oa.cod_ocorrencia = toa.cod_ocorrencia                     ')
                    || rtrim(' and oa.cod_ocorrencia = 202                                      ')
                    || rtrim(' and oa.cod_ts = :p_cod_ts                                        ');

        open  c
        for   v_Sql
        using p_cod_ts_contrato
            , p_cod_ts;
        --
        return c;

        exception
            when no_data_found then
                TS_LOG_EXECUCAO ( 'RB_PREVIA_REEMBOLSO.RetornaOcorrenciaAssCon', '', 'Erro n�o previsto', 'Erro:' || chr(13) || sqlerrm , 'RetornaGlosa' );
                return get_cursor_vazio;
        end;

    end;
    --
    ----------------------------------------------------------------------------
    -- Retorna dados de contato cadastrado na pr�via
    ----------------------------------------------------------------------------
    function RetornaContatoPrevia(p_num_reembolso in  varchar2) return sys_refcursor
    is
    --Declara��es:
    v_posicao               number;
    v_SQL                   varchar2(4000);
    c                       sys_refcursor;
    v_tamanho_reembolso     number;
    v_num_reembolso         varchar(20);
    begin
        --In�cio
        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        if v_tamanho_reembolso > 15 then
          select p.num_reembolso
            into v_num_reembolso
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        else
          v_num_reembolso := p_num_reembolso;
        end if;
        --
        v_posicao := 1;
        --Monta XML dos grupos de an�lise
        v_SQL := '';
        v_SQL := v_SQL || ' select nvl(ind_tipo_emissao,''I'') ind_tipo_emissao';
        v_SQL := v_SQL || '      , txt_num_fax';
        v_SQL := v_SQL || '      , txt_ddd_fax';
        v_SQL := v_SQL || '      , txt_ramal_fax';
        v_SQL := v_SQL || '      , txt_email';
        v_SQL := v_SQL || '      , num_reembolso';
         v_SQL := v_SQL || '     , ind_situacao';
        v_SQL := v_SQL || ' from   pedido_reembolso_previa';
        v_SQL := v_SQL || ' where  num_reembolso                 = :num_reembolso';
        --
        open  c
        for   v_SQL
        using v_num_reembolso;
        --
        return c;
    EXCEPTION
    WHEN OTHERS THEN
        ts_log_execucao ( 'rb_previa_reembolso'
                        , 10
                        , 'Erro n�o previsto'
                        , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                                  || chr(13) || 'Erro-ORA:'               || sqlerrm
                                  || chr(13) || v_SQL
                        , 'RetornaGrupoAnalisePrevia' );
        --
        raise_application_error( -20001 , 'rb_previa_reembolso.RetornaContatoPrevia - Ocorreu o seguinte erro: ' || ts.top_utl_padrao.msgerro);

        --return get_cursor_vazio;
    END;


    ----------------------------------------------------------------------------
    -- Retorna dias prazo por inspetoria
    ----------------------------------------------------------------------------
    PROCEDURE RetornaDiasPrazo ( p_cod_inspetoria_ts      in number,
                                 p_cod_plano              in number,
                                 p_dias_prazo             out varchar2,
                                 p_dias_uteis             out varchar2,
                                 p_data_prazo             out varchar2,
                                 p_cod_retorno            out number,
                                 p_msg_retorno            out varchar2,
                                 p_data_solicitacao       in varchar2 default null)   is

    -----  retorno
    v_posicao         number;
    v_item_vazio      varchar2(3) := '���';
    v_dias_prazo      varchar2(5);
    v_dias_uteis      varchar2(5);
    v_prox_dia_util   varchar2(10);
    v_cod_municipio   number;
    v_cod_marca       varchar2(6);
    v_cod_sucursal    number;
    v_data_solicitacao varchar2(10);
    v_date            date;
    v_aux             number;
    --------------------------------------------------------------------------------
    v_valor_default   varchar(5);
    BEGIN

    v_posicao := 0;
    --
    --
    begin
        select cod_marca
          into v_cod_marca
          from plano_medico
         where cod_plano = p_cod_plano;
    exception
     when others then
        v_cod_marca := null;
    end;
    --
    --
    begin
        select cod_sucursal
          into v_cod_sucursal
          from inspetoria
         where cod_inspetoria_ts = p_cod_inspetoria_ts;
    exception
     when others then
        v_cod_sucursal := null;
    end;
    --
    -- Buscar o valor default
    --
    --------------------------------------------------------------------------------
    begin
        select b.val_parametro
        into p_dias_uteis
        from parametro_sistema_valor b
        where b.cod_parametro = 'PRAZO_PREVIA_REEMBOLSO'
        and b.cod_sucursal  = v_cod_sucursal
        and b.cod_marca = v_cod_marca;
    EXCEPTION
        when others then
            begin
               select b.val_parametro
               into p_dias_uteis
               from parametro_sistema_valor b
               where b.cod_parametro = 'PRAZO_PREVIA_REEMBOLSO'
               and b.cod_sucursal  = v_cod_sucursal;
             exception
                when others then
                   p_dias_uteis := null;
            end;
    end;
    --
    if p_dias_uteis is null or p_dias_uteis < 1 then
        begin
            select a.val_parametro_default
              into p_dias_uteis
              from parametro_sistema a
             where a.cod_parametro = 'PRAZO_PREVIA_REEMBOLSO';
        exception
            when others then
               p_cod_retorno := 9;
               p_msg_retorno := 'Erro ao recuperar prazo -' || v_posicao || ': ' || sqlerrm;
               return;
        end;
    end if;
    --
    select cod_municipio
      into v_cod_municipio
      from sucursal
     where cod_sucursal in ( select cod_sucursal from inspetoria where cod_inspetoria_ts = p_cod_inspetoria_ts );
    --
    if p_data_solicitacao is null then
        v_data_solicitacao :=    to_char(sysdate,'dd/mm/yyyy');
    else
        v_data_solicitacao :=    p_data_solicitacao;
    end if;
    --
    /*sur_obtem_prox_dia_util ( v_data_solicitacao,
                              v_cod_municipio,
                              p_dias_uteis,
                              p_dias_prazo,
                              p_data_prazo
                              );*/
    --calculo para dias uteis
     v_date    := to_date(ts.sur_obtem_prox_dia_util ( v_data_solicitacao, v_cod_municipio),'dd/mm/yyyy');
     p_dias_prazo := 0;

     FOR dd IN 1.. p_dias_uteis
     LOOP
         v_date         := v_date + 1;
         p_data_prazo   := ts.sur_obtem_prox_dia_util (to_char(v_date,'dd/mm/yyyy'), v_cod_municipio) ;
         v_aux          := to_date(p_data_prazo,'dd/mm/yyyy') - v_date;
         p_dias_prazo   := p_dias_prazo + 1 + v_aux; -- soma o valor anterior, mais 1 pela passagem no loop, mais o valor de diferen�a de datas, que � diferente de 0 quando encontra feriado ou fim de semana
         v_date         := to_date(p_data_prazo,'dd/mm/yyyy');
     END LOOP;
    --
    exception
        when others then

           p_cod_retorno := 9;
           p_msg_retorno := 'Erro ao recuperar prazo -' || v_posicao || ': ' || sqlerrm;
           rollback;
    end;
    --
    ----------------------------------------------------------------------------
    -- Retornar N� pedido no SisAmil
    ----------------------------------------------------------------------------
    procedure get_num_pedido_cam    ( p_num_pedido                out    varchar2
                                    , p_cod_situacao              out    pls_integer
                                    , p_num_reembolso_operadora   in     varchar2
                                    , p_num_associado             in     varchar2
                                    )
    is
    begin
        --
        begin
            --
            select p.num_reembolso      , p.ind_situacao
            into   p_num_pedido         , p_cod_situacao
            from   pedido_reembolso_previa      p
            where  p.num_reembolso         = p_num_reembolso_operadora
            and    p.num_associado         = p_num_associado;
            --
        exception
        when no_data_found then
            p_num_pedido  := '';
        end;
        --
        return;
        --
    exception
    when others then
    ts_log_execucao ( 'RB_PREVIA_REEMBOLSO'
                    , 10
                    , 'Erro n�o previsto'
                    , 'Erro:' || chr(13) || ts.top_utl_padrao.msgerro
                              || chr(13) || sqlerrm
                              || chr(13) || 'p_num_reembolso_operadora:'    || p_num_reembolso_operadora
                    , 'get_num_pedido_cam' );
    end;
    ------------------------------------------------------------------------------
    -- Trata os dados do XML retornado pelo WS e busca outras informa��es necess�rias para retornar o associado do CAM
    ------------------------------------------------------------------------------
    procedure RetornaDadosAssociadoWS ( p_xml_dados_associado      in     clob
                                      , p_num_associado            in     varchar2
                                      , p_xml_retorno                 out clob
                                      )
    is
        v_xml_retorno                   clob;
        v_xml_cam                       clob;
        v_ind_situacao                  varchar2(2);
        v_nome_situacao_associado       situacao_associado.nom_situacao%type;
        prm_forma_busca                 varchar2(100);
        p_cod_retorno                   number;
        p_msg_retorno                   varchar2(100);
        p_msg_retorno_clob              clob;
        v_xml_param                     clob;
        v_xml_pgto_beneficiario         clob;
        v_cod_erro_ws                   number;
        v_msg_erro_ws                   clob;
        v_ind_erro_ws                   varchar2(1);
        --
        v_num_cpf_associado             pedido_reembolso.num_cpf_associado%type;
        v_num_titular                   pedido_reembolso.num_titular%type;
        v_nome_titular                  pedido_reembolso.nome_titular%type;
        v_num_cpf_titular               pedido_reembolso.num_cpf_titular%type;
        v_cod_banco                     pedido_reembolso.cod_banco%type;
        v_nome_banco                    banco.nome_banco%type;
        v_cod_agencia                   pedido_reembolso.cod_agencia%type;
        v_num_dv_agencia                pedido_reembolso.num_dv_agencia%type;
        v_num_conta_corrente            pedido_reembolso.num_conta_corrente%type;
        v_num_dv_cc                     pedido_reembolso.num_dv_cc%type;
        v_num_ddd                       varchar2(3);
        v_num_telefone                  varchar2(15);
        v_txt_email                     pedido_reembolso.txt_email%type;
        v_centro_custo                  pedido_reembolso.num_centro_custo%type;
        v_ind_courrier                  varchar2(2);
        v_ind_cobertura_internacional   varchar2(2);
        v_cod_plano                     plano_medico.cod_plano%type;
        v_nome_plano                    varchar2(150);
        v_grupo_beneficio               varchar2(10);
        v_ind_plano_com_reembolso       plano_medico.ind_reembolso%type;
        v_cod_marca                     plano_medico.cod_marca%type;
        v_lista_favorecidos             clob;
        v_num_favorecidos               number;
        v_ind_responsavel_legal         varchar2(1);
        v_ind_favorecido_titular        varchar2(1);
        v_ind_dependente                varchar2(1);
        v_num_responsavel               varchar2(15);
        v_nome_responsavel              varchar2(100);
        v_cpf_responsavel               varchar2(11);
        v_cep_courrier                  varchar2(8);
        v_uf_courrier                   varchar2(30);
        v_cidade_courrier               varchar2(30);
        v_endereco_courrier             varchar2(80);
        v_cod_municipio                 number;
        v_bairro_courrier               pedido_reembolso.nome_bairro%type;
        v_cod_bairro                    number;
        v_complemento_courrier          pedido_reembolso.txt_complemento%type;
        v_cep_courrier_cb               varchar2(8);
        v_uf_courrier_cb                varchar2(30);
        v_cidade_courrier_cb            varchar2(30);
        v_endereco_courrier_cb          varchar2(80);
        v_cod_municipio_cb              number;
        v_bairro_courrier_cb            pedido_reembolso.nome_bairro%type;
        v_cod_bairro_cb                 number;
        v_complemento_courrier_cb       pedido_reembolso.txt_complemento%type;
        v_num_ddd_comercial             pedido_reembolso.ddd_comercial%type;
        v_num_telefone_comercial        pedido_reembolso.tel_comercial%type;
    begin
    --
        -- buscar informa��es do benefici�rio adicionais no cam
        RetornaParametro ('AT_FORMA_BUSCA_CAM_AUT', prm_forma_busca, 'WS');
        --
        AUT_CTX_BENEFICIARIO.carga_por_uk ( p_cod_retorno       => p_cod_retorno
                                          , p_msg_retorno       => p_msg_retorno
                                          , p_num_associado     => p_num_associado
                                          , p_frm_pesquisa      => prm_forma_busca
                                          );
        if p_cod_retorno > 0 then
          return;
        end if;
        --
        v_ind_erro_ws := 'N';
        v_ind_responsavel_legal := 'N';
        v_ind_favorecido_titular := 'N';
        v_ind_dependente := 'N';
        --
        begin
            v_xml_param :=  '<parametros>'
                        || '<numBeneficiario>'|| p_num_associado    ||'</numBeneficiario>'
                        || '<datAtendimento>' || to_char(sysdate, 'DD/MM/YYYY')     ||'</datAtendimento>'
                        || '</parametros>';


            v_xml_pgto_beneficiario :=  rbm_fch_integracao.get_xml_pgto_beneficiario  ( v_cod_erro_ws
                                                                                      , v_msg_erro_ws
                                                                                      , v_xml_param
                                                                                      );
            --
            --dbms_output.put_line(p_xml_dados_associado);
            --dbms_output.put_line(v_xml_pgto_beneficiario);
            --
            v_num_cpf_associado             :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cpfBeneficiario');
            v_num_titular                   :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numTitular');
            v_num_cpf_titular               :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cpfTitular');
            v_nome_titular                  :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'nomTitular');
            v_cod_banco                     :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'codBanco');
            v_cod_agencia                   :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'codAgencia');
            v_num_dv_agencia                :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numDvAgencia');
            v_num_conta_corrente            :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numContaCorrente');
            v_num_dv_cc                     :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numDvContaCorrente');
            v_num_ddd                       :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numDDD');
            v_num_telefone                  :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numTelefone');
            v_num_ddd_comercial             :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numDDDComercial');
            v_num_telefone_comercial        :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numTelefoneComercial');
            v_txt_email                     :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'txtEmail');
            v_centro_custo                  :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numCentroCusto');
            v_ind_courrier                  :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indCourrier');
            v_ind_cobertura_internacional   :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indCoberturaInternacional');
            v_ind_plano_com_reembolso       :=  get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indCoberturaReembolso');
            v_num_favorecidos               :=  to_number(get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numFavorecidos'));
            --
            v_lista_favorecidos             := '<LISTA_FAVORECIDOS><![CDATA[';
            for i in 1..v_num_favorecidos loop
                /*
                v_lista_favorecidos := v_lista_favorecidos || '<IND_TIPO_FAVORECIDO_'||i||'>'   || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indTipoFavorecido'||i) || '</IND_TIPO_FAVORECIDO_'||i||'>'
                                                           || '<NUM_FAVORECIDO_'||i||'>'        || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numFavorecido'||i)     || '</NUM_FAVORECIDO_'||i||'>'
                                                           || '<NOME_FAVORECIDO_'||i||'>'       || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'nomeFavorecido'||i)    || '</NOME_FAVORECIDO_'||i||'>'
                                                           || '<CPF_FAVORECIDO_'||i||'>'        || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cpfFavorecido'||i)     || '</CPF_FAVORECIDO_'||i||'>';
                */

                if get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indTipoFavorecido'||i) in ('T') then
                    v_ind_favorecido_titular := 'S';
                end if;

                if get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indTipoFavorecido'||i) in ('R','B')
                and nvl(get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numFavorecido'||i),0) != p_num_associado then

                    v_lista_favorecidos := v_lista_favorecidos || '<FAVORECIDO>'       || '<IND_TIPO_FAVORECIDO>'  || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'indTipoFavorecido'||i) || '</IND_TIPO_FAVORECIDO>'
                                                               || '<NUM_FAVORECIDO>'   || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numFavorecido'||i)     || '</NUM_FAVORECIDO>'
                                                               || '<NOME_FAVORECIDO>'  || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'nomeFavorecido'||i)    || '</NOME_FAVORECIDO>'
                                                               || '<CPF_FAVORECIDO>'   || get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cpfFavorecido'||i)     || '</CPF_FAVORECIDO>'
                                                               || '</FAVORECIDO>';


                    v_num_responsavel       := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'numFavorecido'||i);
                    v_nome_responsavel      := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'nomeFavorecido'||i);
                    v_cpf_responsavel       := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cpfFavorecido'||i);
                    v_ind_responsavel_legal := 'S';

                end if;
            end loop;
            v_lista_favorecidos             := v_lista_favorecidos || ']]></LISTA_FAVORECIDOS>';
            --
            -- carregar endere�o de courrier
            --
            if nvl(get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cepCorrespondencia'),'X') != 'X' then
                v_cep_courrier          := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cepCorrespondencia');
                v_uf_courrier           := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'estadoCorrespondencia');
                v_cidade_courrier       := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cidadeCorrespondencia');
                v_endereco_courrier     := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'enderecoCorrespondencia');
                v_bairro_courrier       := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'bairroCorrespondencia');
                v_complemento_courrier  := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'complemCorrespondencia');
            end if;
            --
            if nvl(get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cepCobranca'),'X') != 'X' then
                v_cep_courrier_cb          := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cepCobranca');
                v_uf_courrier_cb           := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'estadoCobranca');
                v_cidade_courrier_cb       := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'cidadeCobranca');
                v_endereco_courrier_cb     := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'enderecoCobranca');
                v_bairro_courrier_cb       := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'bairroCobranca');
                v_complemento_courrier_cb  := get_valor_xml( v_xml_pgto_beneficiario, 'pgtoBeneficiario' , 'complemCobranca');
            end if;
            --
            if v_cidade_courrier is not null then
                begin
                    select cod_municipio
                      into v_cod_municipio
                      from municipio
                     where ( nom_municipio = v_cidade_courrier
                        or nom_municipio_abrev = v_cidade_courrier )
                       and rownum = 1 ;
                exception
                    when others then
                        v_cod_municipio := null;
                end;
            end if;
            --
            if v_cidade_courrier_cb is not null then
                begin
                    select cod_municipio
                      into v_cod_municipio_cb
                      from municipio
                     where ( nom_municipio = v_cidade_courrier_cb
                        or nom_municipio_abrev = v_cidade_courrier_cb )
                       and rownum = 1 ;
                exception
                    when others then
                        v_cod_municipio_cb := null;
                end;
            end if;
            --
            if v_bairro_courrier is not null then
                begin
                    select cod_bairro
                      into v_cod_bairro
                      from bairro
                     where ( nom_bairro = v_bairro_courrier
                       or  nom_bairro_abrev = v_bairro_courrier )
                       and rownum = 1;
                exception
                    when others then
                        v_cod_bairro := null;
                end;
            end if;
            --
            if v_bairro_courrier_cb is not null then
                begin
                    select cod_bairro
                      into v_cod_bairro_cb
                      from bairro
                     where ( nom_bairro = v_bairro_courrier_cb
                       or  nom_bairro_abrev = v_bairro_courrier_cb )
                       and rownum = 1;
                exception
                    when others then
                        v_cod_bairro_cb := null;
                end;
            end if;
            --
            begin
                select nome_banco
                  into v_nome_banco
                  from banco
                 where cod_banco = v_cod_banco
                   and rownum = 1;
            exception
            when others then
                v_nome_banco := null;
            end;
            --
        exception
            when others then
            v_lista_favorecidos   := '<LISTA_FAVORECIDOS></LISTA_FAVORECIDOS>';
            v_ind_erro_ws         := 'S';
            v_msg_erro_ws         := ts.top_utl_padrao.msgerro;
        end;
        --
        v_cod_plano     :=  get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'codPlano');
        v_nome_plano    :=  replace(get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomePlano'), v_cod_plano || ' - ');
        --busca se o plano tem cobertura reembolso
        begin
            select cod_marca
              into v_cod_marca
              from plano_medico
             where cod_plano = v_cod_plano;
        exception
            when others then
            v_cod_marca := null;
        end;

        -- busca situa��o do benefici�rio
        v_ind_situacao := get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'indSituacao');
        --
        if v_ind_situacao is not null then
            begin
                select nom_situacao
                  into v_nome_situacao_associado
                  from situacao_associado
                 where ind_situacao = v_ind_situacao;
            exception
                when others then
                    v_nome_situacao_associado := null;
            end;
        end if;
        --
        --monta xml com os dados encontrados
        v_xml_retorno := '<?xml version="1.0"?>';
        v_xml_retorno := v_xml_retorno || '<ROWSET xmlns:xsi = "http://www.w3.org/2001/XMLSchema-instance"><ROW>';
        v_xml_retorno := v_xml_retorno || '<COD_RETORNO>0</COD_RETORNO>';
        v_xml_retorno := v_xml_retorno || '<DATA_ATUAL>'                     || trunc(sysdate)                                                                   || '</DATA_ATUAL>';
        v_xml_retorno := v_xml_retorno || '<IND_ORIGEM_ASSOCIADO>WS</IND_ORIGEM_ASSOCIADO>';
        v_xml_retorno := v_xml_retorno || '<COD_TS></COD_TS>';
        v_xml_retorno := v_xml_retorno || '<NUM_ASSOCIADO>'                  || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'numAssociado')     || '</NUM_ASSOCIADO>';
        v_xml_retorno := v_xml_retorno || '<NOME_ASSOCIADO><![CDATA['        || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomeAssociado')    || ']]></NOME_ASSOCIADO>';
        v_xml_retorno := v_xml_retorno || '<DATA_NASCIMENTO>'                || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'dataNascimento')   || '</DATA_NASCIMENTO>';
        v_xml_retorno := v_xml_retorno || '<IDADE_ASSOCIADO>'                || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'qtdIdade')         || '</IDADE_ASSOCIADO>';
        v_xml_retorno := v_xml_retorno || '<IND_SEXO>'                       || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'indSexo')          || '</IND_SEXO>';
        v_xml_retorno := v_xml_retorno || '<NUM_CPF>'                        || nvl(v_num_cpf_associado,AUT_CTX_BENEFICIARIO.get_numCpf)                         || '</NUM_CPF>';
        v_xml_retorno := v_xml_retorno || '<TIPO_ASSOCIADO>'                 || AUT_CTX_BENEFICIARIO.get_tipoAssociado                                           || '</TIPO_ASSOCIADO>';
        v_xml_retorno := v_xml_retorno || '<IND_SITUACAO>'                   || v_ind_situacao                                                                   || '</IND_SITUACAO>';
        v_xml_retorno := v_xml_retorno || '<NOM_SITUACAO_ASSOCIADO><![CDATA['|| v_nome_situacao_associado                                                        || ']]></NOM_SITUACAO_ASSOCIADO>';
        v_xml_retorno := v_xml_retorno || '<COD_LOTACAO_TS></COD_LOTACAO_TS>';
        v_xml_retorno := v_xml_retorno || '<DATA_INCLUSAO>'                  || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'dataInclusao')     || '</DATA_INCLUSAO>';
        v_xml_retorno := v_xml_retorno || '<DATA_EXCLUSAO>'                  || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'dataExclusao')     || '</DATA_EXCLUSAO>';
        v_xml_retorno := v_xml_retorno || '<DATA_ADMISSAO></DATA_ADMISSAO>';
        v_xml_retorno := v_xml_retorno || '<NUM_ASSOCIADO_TIT>'              || v_num_titular                                                                    || '</NUM_ASSOCIADO_TIT>';
        v_xml_retorno := v_xml_retorno || '<NOME_ASSOCIADO_TIT><![CDATA['    || v_nome_titular                                                                   || ']]></NOME_ASSOCIADO_TIT>';
        v_xml_retorno := v_xml_retorno || '<NUM_CPF_TIT>'                    || v_num_cpf_titular                                                                || '</NUM_CPF_TIT>';
        v_xml_retorno := v_xml_retorno || '<COD_TS_TIT></COD_TS_TIT>';
        v_xml_retorno := v_xml_retorno || '<COD_ENTIDADE_TS_TIT></COD_ENTIDADE_TS_TIT>';
        v_xml_retorno := v_xml_retorno || '<DATA_NASCIMENTO_TIT></DATA_NASCIMENTO_TIT>';
        v_xml_retorno := v_xml_retorno || '<IDADE_TITULAR></IDADE_TITULAR>';
        v_xml_retorno := v_xml_retorno || '<COD_PLANO>'                      || v_cod_plano                                                                      || '</COD_PLANO>';
        v_xml_retorno := v_xml_retorno || '<NOME_PLANO><![CDATA['            || v_nome_plano                                                                     || ']]></NOME_PLANO>';
        v_xml_retorno := v_xml_retorno || '<IND_COBERTURA_INTERNACIONAL>'    || v_ind_cobertura_internacional                                                    || '</IND_COBERTURA_INTERNACIONAL>';
        v_xml_retorno := v_xml_retorno || '<IND_ACOMODACAO>'                 || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'indAcomodacao')    || '</IND_ACOMODACAO>';
        v_xml_retorno := v_xml_retorno || '<COD_MARCA>'                      || v_cod_marca                                                                      || '</COD_MARCA>';
        v_xml_retorno := v_xml_retorno || '<IND_REGULAMENTADO>'              || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'indRegulamentado') || '</IND_REGULAMENTADO>';
        v_xml_retorno := v_xml_retorno || '<IND_PLANO_COM_REEMBOLSO>'        || nvl(v_ind_plano_com_reembolso,'N')                                               || '</IND_PLANO_COM_REEMBOLSO>';
        v_xml_retorno := v_xml_retorno || '<NOME_BANCO><![CDATA['            || null                                                                             || ']]></NOME_BANCO>';
        v_xml_retorno := v_xml_retorno || '<COD_BANCO>'                      || null                                                                             || '</COD_BANCO>';
        v_xml_retorno := v_xml_retorno || '<COD_AGENCIA_BANCARIA>'           || null                                                                             || '</COD_AGENCIA_BANCARIA>';
        v_xml_retorno := v_xml_retorno || '<NUM_DV_AGENCIA>'                 || null                                                                             || '</NUM_DV_AGENCIA>';
        v_xml_retorno := v_xml_retorno || '<NUM_CCO>'                        || null                                                                             || '</NUM_CCO>';
        v_xml_retorno := v_xml_retorno || '<NUM_CCO_DV>'                     || null                                                                             || '</NUM_CCO_DV>';
        v_xml_retorno := v_xml_retorno || '<NOME_BANCO_TIT>'                 || v_nome_banco                                                                     || '</NOME_BANCO_TIT>';
        v_xml_retorno := v_xml_retorno || '<COD_BANCO_TIT>'                  || v_cod_banco                                                                      || '</COD_BANCO_TIT>';
        v_xml_retorno := v_xml_retorno || '<COD_AGENCIA_BANCARIA_TIT>'       || v_cod_agencia                                                                    || '</COD_AGENCIA_BANCARIA_TIT>';
        v_xml_retorno := v_xml_retorno || '<NUM_DV_AGENCIA_TIT>'             || v_num_dv_agencia                                                                 || '</NUM_DV_AGENCIA_TIT>';
        v_xml_retorno := v_xml_retorno || '<NUM_CCO_TIT>'                    || v_num_conta_corrente                                                             || '</NUM_CCO_TIT>';
        v_xml_retorno := v_xml_retorno || '<NUM_CCO_DV_TIT>'                 || v_num_dv_cc                                                                      || '</NUM_CCO_DV_TIT>';
        v_xml_retorno := v_xml_retorno || '<COD_DEPENDENCIA></COD_DEPENDENCIA>';
        v_xml_retorno := v_xml_retorno || '<COD_SITUACAO_ESP>'               || null                                                                             || '</COD_SITUACAO_ESP>';
        v_xml_retorno := v_xml_retorno || '<NOME_SITUACAO_ESP>'              || null                                                                             || '</NOME_SITUACAO_ESP>';
        v_xml_retorno := v_xml_retorno || '<NOM_IMAGEM>'                     || null                                                                             || '</NOM_IMAGEM>';
        v_xml_retorno := v_xml_retorno || '<NOM_REDE><![CDATA['              || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomeRede')         || ']]></NOM_REDE>';
        v_xml_retorno := v_xml_retorno || '<COD_REDE>'                       || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'codRede')          || '</COD_REDE>';
        v_xml_retorno := v_xml_retorno || '<COD_OPERADORA>'                  || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'codOperadora')     || '</COD_OPERADORA>';
        v_xml_retorno := v_xml_retorno || '<NOM_OPERADORA><![CDATA['         || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomeOperadora')    || ']]></NOM_OPERADORA>';
        v_xml_retorno := v_xml_retorno || '<COD_INSPETORIA_TS>'              || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'codUnidade')       || '</COD_INSPETORIA_TS>';
        v_xml_retorno := v_xml_retorno || '<NOME_INSPETORIA><![CDATA['       || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomeUnidade')      || ']]></NOME_INSPETORIA>';
        v_xml_retorno := v_xml_retorno || '<NOME_SUCURSAL><![CDATA['         || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomeFilial')       || ']]></NOME_SUCURSAL>';
        v_xml_retorno := v_xml_retorno || '<TIPO_PESSOA_CONTRATO>'           || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'tipoPessoa')       || '</TIPO_PESSOA_CONTRATO>';
        v_xml_retorno := v_xml_retorno || '<COD_TS_CONTRATO></COD_TS_CONTRATO>';
        v_xml_retorno := v_xml_retorno || '<NUM_CONTRATO>'                   || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'numContrato')      || '</NUM_CONTRATO>';
        v_xml_retorno := v_xml_retorno || '<COD_EMPRESA>'                    || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'codEmpresa')       || ' </COD_EMPRESA>';
        v_xml_retorno := v_xml_retorno || '<IND_TIPO_PRECO>'                 || AUT_CTX_BENEFICIARIO.get_indTipoPreco                                            || '</IND_TIPO_PRECO>';
        v_xml_retorno := v_xml_retorno || '<COD_TIPO_CONTRATO>'              || AUT_CTX_BENEFICIARIO.get_codTipoContrato                                         || '</COD_TIPO_CONTRATO>';
        v_xml_retorno := v_xml_retorno || '<NOME_GRUPO_EMPRESA></NOME_GRUPO_EMPRESA>';
        v_xml_retorno := v_xml_retorno || '<NOME_CONTRATO><![CDATA['         || get_valor_xml( p_xml_dados_associado, 'beneficiario/dados' , 'nomeContrato')     || ']]></NOME_CONTRATO>';
        v_xml_retorno := v_xml_retorno || '<NUM_CENTRO_CUSTO>'               || v_centro_custo                                                                   || '</NUM_CENTRO_CUSTO>';
        v_xml_retorno := v_xml_retorno || '<IND_RETROATIVIDADE></IND_RETROATIVIDADE>';
        v_xml_retorno := v_xml_retorno || '<COD_TITULAR_CONTRATO></COD_TITULAR_CONTRATO>';
        v_xml_retorno := v_xml_retorno || '<DATA_INICIO_VIGENCIA></DATA_INICIO_VIGENCIA>';
        v_xml_retorno := v_xml_retorno || '<IND_CLASSIFICACAO></IND_CLASSIFICACAO>';
        v_xml_retorno := v_xml_retorno || '<NOME_TIPO_EMPRESA></NOME_TIPO_EMPRESA>';
        v_xml_retorno := v_xml_retorno || '<COD_ACAO_TS></COD_ACAO_TS>';
        v_xml_retorno := v_xml_retorno || '<DT_INI_VIGENCIA></DT_INI_VIGENCIA>';
        v_xml_retorno := v_xml_retorno || '<NUM_PROCESSO></NUM_PROCESSO>';
        v_xml_retorno := v_xml_retorno || '<TXT_OBS_JUDICIAL></TXT_OBS_JUDICIAL>';
        v_xml_retorno := v_xml_retorno || '<IND_PODE_COURRIER>'              || v_ind_courrier                                                                   || '</IND_PODE_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<IND_ERRO_WS>'                    || v_ind_erro_ws                                                                    || '</IND_ERRO_WS>';
        v_xml_retorno := v_xml_retorno || '<MSG_ERRO_WS><![CDATA['           || v_msg_erro_ws                                                                    || ']]></MSG_ERRO_WS>';
        --
        v_xml_retorno := v_xml_retorno || '<CONTATO>';
        v_xml_retorno := v_xml_retorno || '<END_EMAIL>'                      || v_txt_email                                                                      || '</END_EMAIL>';
        v_xml_retorno := v_xml_retorno || '<END_EMAIL_TIT></END_EMAIL_TIT>';
        v_xml_retorno := v_xml_retorno || '<NUM_FAX>'                        || null                                                                             || '</NUM_FAX>';
        v_xml_retorno := v_xml_retorno || '<NUM_DDD_FAX>'                    || null                                                                             || '</NUM_DDD_FAX>';
        v_xml_retorno := v_xml_retorno || '<NUM_TELEFONE>'                   || v_num_telefone                                                                   || '</NUM_TELEFONE>';
        v_xml_retorno := v_xml_retorno || '<NUM_DDD_TELEFONE>'               || v_num_ddd                                                                        || '</NUM_DDD_TELEFONE>';
        v_xml_retorno := v_xml_retorno || '<TEL_COMERCIAL>'                  || v_num_telefone_comercial                                                         || '</TEL_COMERCIAL>';
        v_xml_retorno := v_xml_retorno || '<DDD_COMERCIAL>'                  || v_num_ddd_comercial                                                              || '</DDD_COMERCIAL>';
        v_xml_retorno := v_xml_retorno || '<NUM_CELULAR>'                    || null                                                                             || '</NUM_CELULAR>';
        v_xml_retorno := v_xml_retorno || '<NUM_DDD_CELULAR>'                || null                                                                             || '</NUM_DDD_CELULAR>';
        v_xml_retorno := v_xml_retorno || '</CONTATO>';
        --
        --RETORNA DADOS DOS FAVORECIDOS
        v_xml_retorno := v_xml_retorno || v_lista_favorecidos;
        v_xml_retorno := v_xml_retorno || '<IND_RESPONSAVEL_LEGAL>'          || v_ind_responsavel_legal                                                          || '</IND_RESPONSAVEL_LEGAL>';
        v_xml_retorno := v_xml_retorno || '<IND_FAVORECIDO_TITULAR>'               || v_ind_favorecido_titular                                                   || '</IND_FAVORECIDO_TITULAR>';
--        v_xml_retorno := v_xml_retorno || '<IND_DEPENDENTE>'                 || v_ind_dependente                                                                 || '</IND_DEPENDENTE>';
        --
        -- dados do responsavel legao
        v_xml_retorno := v_xml_retorno || '<NUM_RESPONSAVEL>'                || v_num_responsavel                                                                || '</NUM_RESPONSAVEL>';
        v_xml_retorno := v_xml_retorno || '<NOME_RESPONSAVEL>'               || v_nome_responsavel                                                               || '</NOME_RESPONSAVEL>';
        v_xml_retorno := v_xml_retorno || '<CPF_RESPONSAVEL>'                || v_cpf_responsavel                                                                || '</CPF_RESPONSAVEL>';
        --
        -- dados courrier ( endere�o de correspondencia )
        v_xml_retorno := v_xml_retorno || '<CEP_COURRIER>'                   || v_cep_courrier                                                                   || '</CEP_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<UF_COURRIER>'                    || v_uf_courrier                                                                    || '</UF_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<ENDERECO_COURRIER>'              || v_endereco_courrier                                                              || '</ENDERECO_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<CIDADE_COURRIER>'                || v_cidade_courrier                                                                || '</CIDADE_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<COD_CIDADE_COURRIER>'            || v_cod_municipio                                                                  || '</COD_CIDADE_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<BAIRRO_COURRIER>'                || v_bairro_courrier                                                                || '</BAIRRO_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<COD_BAIRRO_COURRIER>'            || v_cod_bairro                                                                     || '</COD_BAIRRO_COURRIER>';
        v_xml_retorno := v_xml_retorno || '<COMPLEMENTO_COURRIER>'           || v_complemento_courrier                                                           || '</COMPLEMENTO_COURRIER>';
        --
        -- dados courrier ( endere�o de cobranca )
        v_xml_retorno := v_xml_retorno || '<CEP_COURRIER_CB>'                || v_cep_courrier_cb                                                                || '</CEP_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<UF_COURRIER_CB>'                 || v_uf_courrier_cb                                                                 || '</UF_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<ENDERECO_COURRIER_CB>'           || v_endereco_courrier_cb                                                           || '</ENDERECO_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<CIDADE_COURRIER_CB>'             || v_cidade_courrier_cb                                                             || '</CIDADE_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<COD_CIDADE_COURRIER_CB>'         || v_cod_municipio_cb                                                               || '</COD_CIDADE_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<BAIRRO_COURRIER_CB>'             || v_bairro_courrier_cb                                                             || '</BAIRRO_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<COD_BAIRRO_COURRIER_CB>'         || v_cod_bairro_cb                                                                  || '</COD_BAIRRO_COURRIER_CB>';
        v_xml_retorno := v_xml_retorno || '<COMPLEMENTO_COURRIER_CB>'        || v_complemento_courrier_cb                                                        || '</COMPLEMENTO_COURRIER_CB>';
        --
        v_xml_retorno := v_xml_retorno || '</ROW></ROWSET>';
        TS_LOG_EXECUCAO ( 'DADOS_ASSOCIADO', 00, 'debug' , 'p_xml_param ' || v_xml_retorno, '');
        p_xml_retorno := v_xml_retorno;
    --
    exception
        when others then
        raise;
    end;
    --
    ------------------------------------------------------------------------------
    -- Retornar as informa��es do associado (CTX)
    ------------------------------------------------------------------------------
    procedure get_xml_associado ( p_xml_retorno             out clob
                                , p_num_associado        in     varchar2
                                , p_cod_ts               in     pls_integer  default 0
                                , p_ctr_logs             in     varchar2     default 'N'
                                )
    is
        --
        v_cod_retorno               pls_integer;
        v_msg_retorno               clob;
        vXMLAdd                     clob;
        v_ind_origem                varchar2(2);
        v_cod_operadora             operadora.cod_operadora%type;
        v_qtd                       pls_integer;
        v_ind_situacao              varchar2(1);
        v_data_exclusao             date;
        v_num_associado             number;
        v_dados_associado           clob;
        v_ind_acao_judicial         varchar2(1);
        --
    begin
        --
        aut_ctx_beneficiario.get_xml_associado ( p_xml_retorno        => v_dados_associado
                                               , p_num_associado      => p_num_associado
                                               , p_cod_ts             => p_cod_ts
                                               , p_ctr_logs           => p_ctr_logs
                                               );
        --
        --dbms_output.put_line(v_dados_associado);
        --
        v_cod_retorno       := TO_NUMBER(nvl(get_valor_xml ( v_dados_associado, 'beneficiario/dados' , 'codRetorno'),0));
        v_ind_origem        := get_valor_xml ( v_dados_associado, 'beneficiario/dados' , 'indOrigem');
        v_ind_acao_judicial := get_valor_xml ( v_dados_associado, 'beneficiario/dados' , 'indAcaoJudicialCliente');

        --
        if v_cod_retorno = 0 then
            --
            --  Verifica se o associado vem do BD ou do WS
            --
            if v_ind_origem = 'DB' then
                --
                RetornaDadosAssociado( p_num_associado, sysdate, v_ind_acao_judicial, p_xml_retorno );
                --
            elsif v_ind_origem = 'WS' then
                --
                RetornaDadosAssociadoWS( v_dados_associado, p_num_associado, p_xml_retorno);
                --
            end if;
            --
        elsif v_cod_retorno = 3 and nvl(v_dados_associado,'X') != 'X' then
            --
            --N�o achou banco e WebService, ent�o verificar eventual
            --
            p_xml_retorno := '<?xml version="1.0"?>';
            p_xml_retorno := p_xml_retorno || '<ROWSET>';
            p_xml_retorno := p_xml_retorno || '<ROW>';
            p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
            p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Benefici�rio n�o encontrado.</MSG_RETORNO>';
            p_xml_retorno := p_xml_retorno || '</ROW>';
            p_xml_retorno := p_xml_retorno || '</ROWSET>';
            --
        end if;
        --
        return;
        --
    exception
    when others then
        --
        p_xml_retorno := '<?xml version="1.0"?>';
        p_xml_retorno := p_xml_retorno || '<ROWSET>';
        p_xml_retorno := p_xml_retorno || '<ROW>';
        p_xml_retorno := p_xml_retorno || '<COD_RETORNO>9</COD_RETORNO>';
        p_xml_retorno := p_xml_retorno || '<MSG_RETORNO>Erro na pesquisa do benefici�rio.</MSG_RETORNO>';
        p_xml_retorno := p_xml_retorno || '</ROW>';
        p_xml_retorno := p_xml_retorno || '</ROWSET>';
        --
        if  nvl(p_ctr_logs,'N') = 'S' then
            --
            ts_log_execucao ('RB_PREVIA_REEMBOLSO'
                            , NULL
                            , ts.top_utl_padrao.msgerro
                            , 'Erro:' || sqlerrm
                            , 'get_xml_associado'
                            ) ;
        end if;
        --
    end;
    --
    function get_rs_memoria ( p_num_reembolso in  varchar2
                            , p_num_seq_item  in  number
                            )
    return sys_refcursor
    is
        v_cod_retorno          number;
        v_msg_retorno          varchar2(200);
        c                       sys_refcursor;
        v_tamanho_reembolso    number;
        v_num_reembolso_ant    pedido_reembolso_previa.num_reembolso%type;
    begin

        --
        select length(p_num_reembolso)
          into v_tamanho_reembolso
          from dual;
        --
        if v_tamanho_reembolso > 15 then
          select p.num_reembolso
            into v_num_reembolso_ant
            from ts.pedido_reembolso_previa p
           where p.num_reembolso_ans = p_num_reembolso;
        else
          v_num_reembolso_ant := p_num_reembolso;
        end if;

          open c
          for select nvl(pr.num_reembolso_ans,pr.num_reembolso) num_reembolso
                  , pr.ind_situacao
                  , s.nome_situacao
                  , pr.num_associado
                  , pr.nome_associado
                  , pr.num_contrato
                  , pr.nome_contrato
                  , mrd.cod_procedimento_calc cod_procedimento
                  , p.nome_item nome_procedimento
                  , mrd.sigla_tabela_calc sigla_tabela
                  , pr.ind_tipo_reembolso
                  , t.nome_tipo_reembolso
                  , mrd.tipo_composicao_reembolso
                  , pr.cod_acomodacao
                  , a.nome_acomodacao
                  , pr.nome_prestador
                  , pr.num_insc_fiscal
				  , pro.val_apresentado val_informado
                  , to_char(pr.dt_inclusao,'DD/MM/YYYY') data_atendimento
                  , DECODE(mrd.ind_calc_urgencia,'S','SIM','N�O') urgencia
                  --, mrd.pct_urgencia
                  , DECODE(mrd.ind_via,'M','Mesma Via','D','Diferentes Via','Via �nica') via
                  --, mrd.pct_cirurgia_multipla pct_via
                  , DECODE(pro.ind_doppler,'P','Pulsado e Continuo','C','Colorido',null) doppler
                  , null qtd_diarias
                  , null qtd_sessoes
                  , null val_beneficio
                  , to_char(mrd.val_beneficio_anual,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_beneficio_anual
                  , to_char(mrd.val_utilizado_beneficio,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_utilizado_beneficio
                  , to_char(mrd.saldo_beneficio_anual,'FM999G999G999G990D00','nls_numeric_characters='',.')   saldo_beneficio_anual
                  , to_char(mrd.val_limite,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_limite
                  , to_char(mrd.val_fixo,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_fixo
                  , mrd.qtd_honorario
                  , pro.ind_funcao
                  , f.nome_funcao
                  , mrd.pct_funcao
                  , mrd.cod_porte
                  , mrd.val_porte
                  --, mrd.pct_porte_auxiliar
                  , mrd.sigla_moeda
                  , mrd.cotacao_reembolso
                  --, to_char( mrd.qtd_honorario * mrd.cotacao_reembolso,'FM999G999G999G990D00','nls_numeric_characters='',.') val_honorario
                  --, mrd.pct_recibo
                  , mrd.pct_copart
                  , to_char(mrd.val_copart,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_copart
                  --, mrd.qtd_co
                  , mrd.qtd_filme
                  , mrd.cotacao_filme
                  , to_char(mrd.val_filme,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_filme
                  , mrd.qtd_vezes_tabela
                  , pro.qtd_informado
                  , to_char(mrd.val_tax_materiais,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_tax_materiais
                  , to_char(pro.val_calculado,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_calculado
                  , to_char(pro.val_reembolsado,'FM999G999G999G990D00','nls_numeric_characters='',.')   val_reembolsado
             from pedido_reembolso_previa pr
                , procedimento_reembolso_previa pro
                , reembolso_previa_situacao s
                , tipo_reembolso t
                , vwm_procedimento p
                , memoria_previa_detalhe mrd
                , tipo_funcao_auxiliar f
                , tipo_acomodacao a
             where pr.num_reembolso          = mrd.num_reembolso
               and pro.num_reembolso         = mrd.num_reembolso
               and pro.num_seq_item          = mrd.num_seq_item
               and pr.ind_situacao           = s.ind_situacao
               and pr.ind_tipo_reembolso     = t.ind_tipo_reembolso
               and mrd.cod_procedimento_calc = p.item_medico
               and pro.ind_funcao            = f.cod_funcao  (+)
               and pr.cod_acomodacao         = a.cod_acomodacao (+)
               and mrd.num_reembolso         = v_num_reembolso_ant
               and mrd.num_seq_item          = p_num_seq_item
               ;

          return c;

    exception
        when others then
            open c
            for select * from dual where 1 = 2;

            return c;

    end;
--
--
    procedure getProtocolo( p_cod_operadora_atd in  varchar2
                          , p_cod_usuario_atd in  varchar2
                          , p_cod_ts_atd      in  varchar2
                          , p_cod_retorno    out varchar2
                          , p_msg_retorno    out varchar2
                          , p_num_protocolo  out varchar2
                          )
    is
        tab_parans                       top_utl_xml.tbl_fields;
        --
        v_dt_ultima_geracao              date;
        v_seq_protocolo                  number;
        v_num_registro_ans               varchar2(6);
        --
        v_cod_operadora                  atd_protocolo_geral.cod_operadora%type;
        v_cod_usuario                    usuario.cod_usuario%type;
        v_cod_ts                         pedido_reembolso_previa.cod_ts%type;
        v_nome_associado                 beneficiario.nome_associado%type;
        --
        v_cod_retorno                    varchar2(3) := 0;
        v_msg_retorno                    varchar2(100);
        --
    begin
        --
        p_cod_retorno      := '0';
        p_msg_retorno      := 'Protocolo gerado com sucesso.';
        v_num_registro_ans := '326305';
        --
        begin
          select distinct(cod_ts_tit), nome_associado
            into v_cod_ts, v_nome_associado
            from ts.beneficiario
           where cod_ts = p_cod_ts_atd
             and rownum = 1;
        exception
          when others then
            v_cod_ts := p_cod_ts_atd;
        end;
        --
        ts.get_protocolo(p_num_protocolo, v_num_registro_ans);
        --
        begin
            --
            gravaProtocolo( p_num_protocolo                => p_num_protocolo
                          , p_cod_ts                       => v_cod_ts
                          , p_cod_operadora                => p_cod_operadora_atd
                          , p_nome_associado               => v_nome_associado
                          , p_dt_geracao                   => sysdate
                          , p_cod_usuario      => p_cod_usuario_atd
                          ) ;
            --
        exception
            when others then
                 p_num_protocolo := null;
                p_cod_retorno := '1';
                p_msg_retorno := 'N�o foi poss�vel gerar um n�mero de protocolo.' || v_msg_retorno;
                rollback;
        end;
        --
        commit;
        --
    exception
        when others then
            p_num_protocolo := null;
            p_cod_retorno := '1';
            p_msg_retorno := 'N�o foi poss�vel gerar um n�mero de protocolo.' || sqlerrm;
            rollback;
    end;
    --
    --
    --
  procedure gravaProtocolo ( p_num_protocolo                in  atd_protocolo_geral.num_protocolo%type
                           , p_cod_ts                       in  pedido_reembolso_previa.cod_ts%type
                           , p_cod_operadora                in  atd_protocolo_geral.cod_operadora%type
                           , p_nome_associado               in  beneficiario.nome_associado%type
                           , p_dt_geracao                   in  atd_protocolo_geral.dt_geracao%type
                           , p_cod_usuario      in  usuario.cod_usuario%type
                           )
  is
  begin
      begin
         --
         insert
         into   atd_protocolo_geral
              ( num_protocolo
              , id_solicitante
              , cod_operadora
              , cod_qualificacao_solicitante
              , cod_sistema_solicitante
              , dt_geracao
              , COD_USUARIO_SOLICITANTE
              )
         values
              ( p_num_protocolo
              , null
              , p_cod_operadora
              , null
              , null
              , p_dt_geracao
              , p_cod_usuario
              ) ;
         --
      end;
      --
      begin
          insert into ts.atd_controle
            (num_atendimento_ts,
             data_inicio_atendimento,
             data_fim_atendimento,
             ind_situacao,
             ind_origem_atendimento,
             ind_forma_retorno,
             sgl_area,
             cod_usuario,
             cod_operadora,
             cod_sucursal,
             cod_inspetoria_ts,
             cod_ts,
             cod_ts_contrato,
             cod_prestador_ts,
             cod_corretor_ts,
             num_cpf_benef_event,
             nom_benef_event,
             txt_obs,
             cod_sucursal_atd,
             cod_atendimento,
             historico_navegacao,
             num_seq_proposta_ts,
             cod_praca,
             cod_celula)
          values
            (p_num_protocolo,
             sysdate,
             null,
             '2',
             '9',
             null,
             'REEMBOLSO',
             p_cod_usuario,
             p_cod_operadora,
             null,
             null,
             p_cod_ts,
             null,
             null,
             null,
             null,
             null,
             'Pr�via cadastrada no sistema. Benefici�rio(a): ' || p_nome_associado,
             null,
             null,
             'Inclus�o de Pr�via Reembolso',
             null,
             null,
             null);
       exception
         when others then
              rollback;
       end;
       --
       begin
          insert into ts.atd_motivo
            (num_atendimento_ts, cod_motivo, dt_atu, cod_usuario_atu)
          values
            (p_num_protocolo, '301', sysdate, p_cod_usuario);
       exception
         when others then
              rollback;
       end;
  --
  exception
        when others then
            rollback;
   end;
--
end RB_PREVIA_REEMBOLSO;
/
