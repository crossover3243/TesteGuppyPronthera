package service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import model.Funcionario;

public class FuncionarioService {

    private final List<Funcionario> funcionarios;
    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    public FuncionarioService(List<Funcionario> funcionarios) {
        this.funcionarios = new ArrayList<>(funcionarios);
    }

    public void imprimirMensagem(String mensagem) {
        System.out.println("-- " + mensagem + ":");
    }

    public void imprimirFuncionarios() {
        funcionarios.forEach(f -> System.out.println(formatarFuncionario(f)));
        System.out.println();
    }

    private String formatarFuncionario(Funcionario f) {
        String dataFormatada = f.getDataNascimento().format(DATE_FORMATTER);
        String salarioFormatado = String.format("%,.2f", f.getSalario());
        return String.format("Nome: %s, Data de Nascimento: %s, Salário: %s, Função: %s", 
                f.getNome(), dataFormatada, salarioFormatado, f.getFuncao());
    }

    public void aumentarSalarios(int percentual) {
        for (Funcionario f : funcionarios) {
            BigDecimal aumento = f.getSalario().multiply(BigDecimal.valueOf(percentual)).divide(BigDecimal.valueOf(100));
            f.setSalario(f.getSalario().add(aumento));
        }
    }

    public void removerFuncionario(String nome) {
        funcionarios.removeIf(f -> f.getNome().equalsIgnoreCase(nome));
        imprimirMensagem("FUNCIONÁRIO JOÃO EXCLUÍDO");
        System.out.println();
    }

    public void imprimirPorFuncao() {
        Map<String, List<Funcionario>> funcionariosPorFuncao = agruparPorFuncao();
        funcionariosPorFuncao.forEach((funcao, lista) -> {
            System.out.println("Função: " + funcao);
            lista.forEach(f -> System.out.println("\t" + formatarFuncionario(f)));
        });
        System.out.println();
    }

    private Map<String, List<Funcionario>> agruparPorFuncao() {
        return funcionarios.stream().collect(Collectors.groupingBy(Funcionario::getFuncao));
    }

    public void imprimirAniversariantes(List<Integer> meses) {
        funcionarios.stream()
            .filter(f -> meses.contains(f.getDataNascimento().getMonthValue()))
            .forEach(f -> System.out.println(formatarFuncionario(f)));
        System.out.println();
    }

    public void imprimirFuncionarioMaisVelho() {
        Funcionario maisVelho = Collections.min(funcionarios, Comparator.comparing(Funcionario::getDataNascimento));
        int idade = LocalDate.now().getYear() - maisVelho.getDataNascimento().getYear();
        System.out.println("Funcionário mais velho: " + maisVelho.getNome() + ", Idade: " + idade);
        System.out.println();
    }

    public void imprimirFuncionariosPorOrdemAlfabetica() {
        funcionarios.stream()
                .sorted(Comparator.comparing(Funcionario::getNome))
                .forEach(f -> System.out.println(formatarFuncionario(f)));
        System.out.println();
    }

    public void imprimirTotalSalarios() {
        BigDecimal total = funcionarios.stream().map(Funcionario::getSalario).reduce(BigDecimal.ZERO, BigDecimal::add);
        System.out.println("Total dos salários: " + String.format("%,.2f", total));
        System.out.println();
    }

    public void imprimirSalariosMinimos(double salarioMinimo) {
        funcionarios.forEach(f -> {
            double salariosMinimos = f.getSalario().doubleValue() / salarioMinimo;
            System.out.println(f.getNome() + " ganha " + String.format("%.2f", salariosMinimos) + " salários mínimos.");
        });
    }
}
