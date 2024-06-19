package application;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import model.Funcionario;
import service.FuncionarioService;

public class AppProthera {

    private static final double SALARIO_MINIMO = 1212.00;

    public static void main(String[] args) {
        List<Funcionario> funcionarios = criarFuncionarios();

        FuncionarioService funcionarioService = new FuncionarioService(funcionarios);

        funcionarioService.imprimirMensagem("LISTA INICIAL DE FUNCIONÁRIOS");
        funcionarioService.imprimirFuncionarios();

        funcionarioService.removerFuncionario("João");

        funcionarioService.imprimirMensagem("LISTA DOS FUNCIONÁRIOS");
        funcionarioService.imprimirFuncionarios();

        funcionarioService.aumentarSalarios(10);
        funcionarioService.imprimirMensagem("10% DE AUMENTO NO SALÁRIO DOS FUNCIONÁRIOS\nLISTA DE FUNCIONÁRIOS COM NOVOS SALÁRIOS");
        funcionarioService.imprimirFuncionarios();

        funcionarioService.imprimirMensagem("FUNCIONÁRIO POR FUNÇÃO");
        funcionarioService.imprimirPorFuncao();

        funcionarioService.imprimirMensagem("FUNCIONÁRIOS ANIVERSARIANTES");
        funcionarioService.imprimirAniversariantes(Arrays.asList(10, 12));

        funcionarioService.imprimirFuncionarioMaisVelho();

        funcionarioService.imprimirMensagem("FUNCIONÁRIOS POR ORDEM ALFABÉTICA");
        funcionarioService.imprimirFuncionariosPorOrdemAlfabetica();

        funcionarioService.imprimirTotalSalarios();

        funcionarioService.imprimirMensagem("SALÁRIOS MÍNIMOS POR FUNCIONÁRIO");
        funcionarioService.imprimirSalariosMinimos(SALARIO_MINIMO);
    }

    private static List<Funcionario> criarFuncionarios() {
        return Arrays.asList(
            new Funcionario("Maria", LocalDate.of(2000, 10, 18), new BigDecimal("2009.44"), "Operador"),
            new Funcionario("João", LocalDate.of(1990, 5, 12), new BigDecimal("2284.38"), "Operador"),
            new Funcionario("Caio", LocalDate.of(1961, 5, 2), new BigDecimal("9836.14"), "Coordenador"),
            new Funcionario("Miguel", LocalDate.of(1988, 10, 14), new BigDecimal("19119.88"), "Diretor"),
            new Funcionario("Alice", LocalDate.of(1995, 1, 5), new BigDecimal("2234.68"), "Recepcionista"),
            new Funcionario("Heitor", LocalDate.of(1999, 11, 19), new BigDecimal("1582.72"), "Operador"),
            new Funcionario("Arthur", LocalDate.of(1993, 3, 31), new BigDecimal("4071.84"), "Contador"),
            new Funcionario("Laura", LocalDate.of(1994, 7, 8), new BigDecimal("3017.45"), "Gerente"),
            new Funcionario("Heloísa", LocalDate.of(2003, 5, 24), new BigDecimal("1606.85"), "Eletricista"),
            new Funcionario("Helena", LocalDate.of(1996, 9, 2), new BigDecimal("2799.93"), "Gerente")
        );
    }
}