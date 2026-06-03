# Restaurante App

## Formatar o código Java

Para garantir que todo o código Java segue o mesmo padrão de formatação, executa o seguinte comando na raiz do projeto:

```bash
./gradlew spotlessApply
```

Este comando utiliza o plugin [Spotless](https://github.com/diffplug/spotless) com o Google Java Format para formatar automaticamente todos os ficheiros `.java` do projeto.
