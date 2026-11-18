import config.ConfigManager
import vault.VaultManager
import model.{Command, AppError}
import scala.io.StdIn
import scala.util.{Success, Failure, Try}

object Main {
  val commands: List[Command] = List(
    Command.Create,
    Command.Open,
    Command.ListEntries,
    Command.Show(0),
    Command.ShowPassword(0),
    Command.Add,
    Command.Edit(0),
    Command.Search(""),
    Command.Remove(0),
    Command.Lock,
    Command.Reload,
    Command.Exit
  )

  def parseCommand(input: String): Command = {
    val parts = input.trim.split("\\s+", 2)
    val cmdName = parts.headOption.map(_.toLowerCase).getOrElse("")
    val arg = parts.lift(1).getOrElse("")

    cmdName match {
      case "create" => Command.Create
      case "open" => Command.Open
      case "list" => Command.ListEntries
      case "show" => Try(arg.toInt).map(Command.Show).getOrElse(Command.Unknown(cmdName))
      case "pass" => Try(arg.toInt).map(Command.ShowPassword).getOrElse(Command.Unknown(cmdName))
      case "add" => Command.Add
      case "edit" => Try(arg.toInt).map(Command.Edit).getOrElse(Command.Unknown(cmdName))
      case "search" => Command.Search(arg)
      case "remove" => Try(arg.toInt).map(Command.Remove).getOrElse(Command.Unknown(cmdName))
      case "lock" => Command.Lock
      case "reload" => Command.Reload
      case "exit" => Command.Exit
      case other => Command.Unknown(other)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Запуск приложения локального хранилища паролей...")

    ConfigManager.loadAndValidate() match {
      case Left(error) =>
        println(s"Критическая ошибка конфигурации:\n${error.message}")
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
    commands.foreach(cmd => println(f"  ${cmd.name}%-16s - ${cmd.description}"))

    while (running) {
      print("\n> ")
      val input = StdIn.readLine()
      if (input != null) {
        val command = parseCommand(input)

        command match {
          case Command.Exit =>
            running = false
            vault.log("INFO", "Приложение завершено.")
            println("Завершение работы приложения.")

          case Command.Unknown(name) =>
            vault.log("WARN", s"Введена неизвестная команда: '$name'")
            println(s"Неизвестная команда: '$name'.")

          case cmd =>
            if (cmd.requiresAuth && !vault.currentState.isUnlocked) {
              println("Команда недоступна: хранилище заблокировано. Сначала откройте его командой 'open'.")
            } else {
              executeCommand(vault, cmd)
            }
        }
      }
    }
  }

  def executeCommand(vault: VaultManager, cmd: Command): Unit = {
    cmd match {
      case Command.Create =>
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
          vault.createVault(pass1) match {
            case Right(_) => println("Хранилище успешно создано и открыто.")
            case Left(err) => println(err.message)
          }
        }

      case Command.Open =>
        print("Введите мастер-пароль: ")
        val pass = StdIn.readLine()
        vault.openVault(pass) match {
          case Right(_) => println("Хранилище успешно открыто.")
          case Left(err) => println(err.message)
        }

      case Command.ListEntries =>
        val sorted = vault.getSortedEntries()
        println(f"${"ID"}%-4s ${"Сервис"}%-15s ${"Имя пользователя"}%-25s ${"Изменена"}%s")
        println("-" * 65)
        sorted.foreach { e =>
          println(f"${e.id}%-4d ${e.service}%-15s ${e.username}%-25s ${e.updatedAt}%s")
        }

      case Command.Show(id) =>
        vault.findEntry(id) match {
          case Right(e) =>
            println(s"Сервис: ${e.service}")
            println(s"Адрес: ${e.url}")
            println(s"Имя пользователя: ${e.username}")
            println(s"Пароль: ********")
            println(s"Заметка: ${e.note}")
            println(s"Создана: ${e.createdAt}")
            println(s"Изменена: ${e.updatedAt}")
          case Left(err) => println(err.message)
        }

      case Command.ShowPassword(id) =>
        vault.findEntry(id) match {
          case Right(e) =>
            vault.log("INFO", s"Отображён пароль записи: id=$id")
            println(s"Пароль для ID $id: ${e.password}")
          case Left(err) => println(err.message)
        }

      case Command.Add =>
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

        vault.addEntry(s, u, p, em, url, n) match {
          case Right(newId) => println(s"Добавлена запись с ID: $newId")
          case Left(err) => println(err.message)
        }

      case Command.Edit(id) =>
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

        vault.updateEntry(id, s, u, p, em, url, n) match {
          case Right(_) => println("Запись успешно обновлена.")
          case Left(err) => println(err.message)
        }

      case Command.Search(query) =>
        vault.searchEntries(query) match {
          case Right(found) =>
            println(f"${"ID"}%-4s ${"Сервис"}%-15s ${"Имя пользователя"}%s")
            println("-" * 45)
            found.foreach(e => println(f"${e.id}%-4d ${e.service}%-15s ${e.username}%s"))
          case Left(err) => println(err.message)
        }

      case Command.Remove(id) =>
        vault.findEntry(id) match {
          case Right(e) =>
            val confirm = vault.config.confirmBeforeDelete
            val proceed = if (confirm) {
              println(s"Будет удалена запись:\nСервис: ${e.service}\nИмя пользователя: ${e.username}")
              print("Удалить запись? [да/нет]: ")
              val answer = StdIn.readLine().trim.toLowerCase
              answer == "да" || answer == "yes"
            } else true

            if (proceed) {
              vault.deleteEntry(id) match {
                case Right(_) => println("Запись успешно удалена.")
                case Left(err) => println(err.message)
              }
            } else {
              println("Удаление отменено.")
            }
          case Left(err) => println(err.message)
        }

      case Command.Lock =>
        vault.lockVault()
        println("Хранилище заблокировано.")

      case Command.Reload =>
        if (vault.currentState.isUnlocked) {
          println("Ошибка: нельзя перезагрузить конфигурацию при открытом хранилище.")
        } else {
          ConfigManager.loadAndValidate() match {
            case Left(err) => println(s"Ошибка перезагрузки конфигурации:\n${err.message}")
            case Right(newConf) =>
              vault.reloadConfig(newConf)
              println("Конфигурация успешно перезагружена.")
          }
        }

      case _ =>
        println("Неизвестная команда.")
    }
  }
}