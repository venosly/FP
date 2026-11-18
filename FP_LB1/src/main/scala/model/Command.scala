package model

sealed trait Command {
  def name: String
  def description: String
  def requiresAuth: Boolean = true
}

object Command {
  case object Create extends Command {
    val name = "create"
    val description = "Создать новое хранилище"
    override val requiresAuth = false
  }

  case object Open extends Command {
    val name = "open"
    val description = "Открыть хранилище"
    override val requiresAuth = false
  }

  case object ListEntries extends Command {
    val name = "list"
    val description = "Показать список записей"
  }

  case class Show(id: Int) extends Command {
    val name = "show"
    val description = "Показать выбранную запись [id]"
  }

  case class ShowPassword(id: Int) extends Command {
    val name = "pass"
    val description = "Показать пароль записи [id]"
  }

  case object Add extends Command {
    val name = "add"
    val description = "Добавить новую запись"
  }

  case class Edit(id: Int) extends Command {
    val name = "edit"
    val description = "Изменить запись [id]"
  }

  case class Search(query: String) extends Command {
    val name = "search"
    val description = "Найти записи по запросу [запрос]"
  }

  case class Remove(id: Int) extends Command {
    val name = "remove"
    val description = "Удалить запись [id]"
  }

  case object Lock extends Command {
    val name = "lock"
    val description = "Заблокировать хранилище"
  }

  case object Reload extends Command {
    val name = "reload"
    val description = "Перезагрузить конфигурацию"
    override val requiresAuth = false
  }

  case object Exit extends Command {
    val name = "exit"
    val description = "Выход из приложения"
    override val requiresAuth = false
  }

  case class Unknown(rawName: String) extends Command {
    val name = rawName
    val description = "Неизвестная команда"
    override val requiresAuth = false
  }
}