import config.ConfigManager
import vault.VaultManager
import scala.io.StdIn
import scala.util.{Success, Failure, Try}

trait Command {
  def name: String
  def description: String
  def requiresAuth: Boolean = true
  def execute(vault: VaultManager, arg: String): Unit
}

object CreateCommand extends Command {
  val name = "create"
  val description = "Создать новое хранилище"
  override val requiresAuth = false

  def execute(vault: VaultManager, arg: String): Unit = {
    if (vault.vaultExists()) {
      print("Файл хранилища уже существует. Перезаписать? [да/нет]: ")
      val ans = StdIn.readLine().trim.toLowerCase
      if (ans != "да" && ans != "yes") {
        println("Создание отменено.")
        return
      }
    }
    print("Введите мастер-пароль: ")
    val pass1 = StdIn.readLine()
    print("Повторите мастер-пароль: ")
    val pass2 = StdIn.readLine()

    if (pass1 != pass2) {
      println("Ошибка: мастер-пароли не совпадают.")
    } else if (pass1.isEmpty) {
      println("Ошибка: мастер-пароль не может быть пустым.")
    } else {
      vault.createVault(pass1)
    }
  }
}

object OpenCommand extends Command {
  val name = "open"
  val description = "Открыть хранилище"
  override val requiresAuth = false

  def execute(vault: VaultManager, arg: String): Unit = {
    print("Введите мастер-пароль: ")
    val pass = StdIn.readLine()
    vault.openVault(pass)
  }
}

object ListCommand extends Command {
  val name = "list"
  val description = "Показать список записей"
  def execute(vault: VaultManager, arg: String): Unit = vault.listEntries()
}

object ShowCommand extends Command {
  val name = "show"
  val description = "Показать выбранную запись [id]"
  def execute(vault: VaultManager, arg: String): Unit = {
    Try(arg.toInt) match {
      case Success(id) => vault.showEntry(id)
      case Failure(_)  => println("Ошибка: неверный формат ID.")
    }
  }
}

object PassCommand extends Command {
  val name = "pass"
  val description = "Показать пароль записи [id]"
  def execute(vault: VaultManager, arg: String): Unit = {
    Try(arg.toInt) match {
      case Success(id) => vault.showPassword(id)
      case Failure(_)  => println("Ошибка: неверный формат ID.")
    }
  }
}

object AddCommand extends Command {
  val name = "add"
  val description = "Добавить новую запись"
  def execute(vault: VaultManager, arg: String): Unit = {
    print("Сервис: ")
    val s = StdIn.readLine()
    print("Имя пользователя: ")
    val u = StdIn.readLine()
    print("Пароль: ")
    val p = StdIn.readLine()
    print("Email: ")
    val em = StdIn.readLine()
    print("URL: ")
    val url = StdIn.readLine()
    print("Заметка: ")
    val n = StdIn.readLine()
    vault.addEntry(s, u, p, em, url, n)
  }
}

object EditCommand extends Command {
  val name = "edit"
  val description = "Изменить запись [id]"
  def execute(vault: VaultManager, arg: String): Unit = {
    Try(arg.toInt) match {
      case Success(id) =>
        println("Оставьте поле пустым, чтобы сохранить текущее значение.")
        print("Новый сервис: ")
        val s = StdIn.readLine()
        print("Новое имя пользователя: ")
        val u = StdIn.readLine()

        print("Изменить пароль? [да/нет]: ")
        val changePassAns = StdIn.readLine().trim.toLowerCase
        val p = if (changePassAns == "да" || changePassAns == "yes") {
          print("Новый пароль: ")
          StdIn.readLine()
        } else ""

        print("Новый Email: ")
        val em = StdIn.readLine()
        print("Новый URL: ")
        val url = StdIn.readLine()
        print("Новая заметка: ")
        val n = StdIn.readLine()
        vault.updateEntry(id, s, u, p, em, url, n)
      case Failure(_) =>
        println("Ошибка: неверный формат ID.")
    }
  }
}

object SearchCommand extends Command {
  val name = "search"
  val description = "Найти записи по запросу [запрос]"
  def execute(vault: VaultManager, arg: String): Unit = vault.searchEntries(arg)
}

object RemoveCommand extends Command {
  val name = "remove"
  val description = "Удалить запись [id]"
  def execute(vault: VaultManager, arg: String): Unit = {
    Try(arg.toInt) match {
      case Success(id) => vault.deleteEntry(id)
      case Failure(_)  => println("Ошибка: неверный формат ID.")
    }
  }
}

object LockCommand extends Command {
  val name = "lock"
  val description = "Заблокировать хранилище"
  def execute(vault: VaultManager, arg: String): Unit = vault.lockVault()
}

object ReloadCommand extends Command {
  val name = "reload"
  val description = "Перезагрузить конфигурацию"
  override val requiresAuth = false

  def execute(vault: VaultManager, arg: String): Unit = {
    if (vault.isVaultUnlocked) {
      println("Ошибка: нельзя перезагрузить конфигурацию при открытом хранилище.")
    } else {
      ConfigManager.loadAndValidate() match {
        case Left(err) => println(s"Ошибка перезагрузки конфигурации:\n$err")
        case Right(newConf) => vault.reloadConfig(newConf)
      }
    }
  }
}

object ExitCommand extends Command {
  val name = "exit"
  val description = "Выход из приложения"
  override val requiresAuth = false

  def execute(vault: VaultManager, arg: String): Unit = {
  }
}

//Main

object Main {
  val commands: Map[String, Command] = Map(
    CreateCommand.name -> CreateCommand,
    OpenCommand.name -> OpenCommand,
    ListCommand.name -> ListCommand,
    ShowCommand.name -> ShowCommand,
    PassCommand.name -> PassCommand,
    AddCommand.name -> AddCommand,
    EditCommand.name -> EditCommand,
    SearchCommand.name -> SearchCommand,
    RemoveCommand.name -> RemoveCommand,
    LockCommand.name -> LockCommand,
    ReloadCommand.name -> ReloadCommand,
    ExitCommand.name -> ExitCommand
  )

  def main(args: Array[String]): Unit = {
    println("Запуск приложения локального хранилища паролей...")

    ConfigManager.loadAndValidate() match {
      case Left(error) =>
        println(s"Критическая ошибка конфигурации:\n$error")
      case Right(config) =>
        println("Конфигурация успешно загружена.")
        val vault = new VaultManager(config)
        vault.log("INFO", "Приложение запущено и конфигурация успешно загружена.")
        runCli(vault)
    }
  }

  def runCli(vault: VaultManager): Unit = {
    var running = true

    println("\nДоступные команды:")
    commands.values.foreach(cmd => println(f"  ${cmd.name}%-16s - ${cmd.description}"))

    while (running) {
      print("\n> ")
      val input = StdIn.readLine()
      if (input != null) {
        val parts = input.trim.split("\\s+", 2)
        val cmdName = parts(0).toLowerCase
        val arg = if (parts.length > 1) parts(1) else ""

        if (cmdName == "exit") {
          running = false
          vault.log("INFO", "Приложение завершено.")
          println("Завершение работы приложения.")
        } else {
          commands.get(cmdName) match {
            case Some(command) =>
              if (command.requiresAuth && !vault.isVaultUnlocked) {
                println("Команда недоступна: хранилище заблокировано. Сначала откройте его командой 'open'.")
              } else {
                command.execute(vault, arg)
              }
            case None =>
              vault.log("WARN", s"Введена неизвестная команда: '$cmdName'")
              println(s"Неизвестная команда: '$cmdName'.")
          }
        }
      }
    }
  }
}