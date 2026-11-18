package model

sealed trait AppError {
  def message: String
}

object AppError {
  case class ConfigError(msg: String) extends AppError { def message: String = s"Ошибка конфигурации:\n$msg" }
  case class VaultFileNotFound(path: String) extends AppError { def message: String = s"Файл хранилища не существует: '$path'." }
  case class VaultAlreadyExists(path: String) extends AppError { def message: String = s"Файл хранилища уже существует: '$path'." }
  case class VaultCorrupted(details: String) extends AppError { def message: String = s"Ошибка: файл хранилища поврежден ($details)." }
  case class InvalidMasterPassword(attemptsLeft: Int) extends AppError { def message: String = s"Неправильный мастер-пароль. Осталось попыток: $attemptsLeft." }
  case class MaxAttemptsExceeded() extends AppError { def message: String = "Превышено максимальное количество попыток открытия. Доступ заблокирован." }
  case class VaultLocked() extends AppError { def message: String = "Хранилище заблокировано. Сначала откройте его." }
  case class EntryNotFound(id: Int) extends AppError { def message: String = s"Запись с ID $id не существует." }
  case class InvalidInput(details: String) extends AppError { def message: String = s"Ошибка ввода: $details" }
  case class OperationCancelled() extends AppError { def message: String = "Операция отменена." }
}