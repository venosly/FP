package config

import model.{AppConfig, AppError}
import java.io.File
import scala.io.Source
import scala.util.Using

object ConfigManager {
  private val ConfigFileName = "config.json"

  def loadAndValidate(): Either[AppError, AppConfig] = {
    val file = new File(ConfigFileName)
    if (!file.exists()) {
      return Left(AppError.ConfigError(s"Конфигурационный файл '$ConfigFileName' отсутствует."))
    }

    val contentTry = Using(Source.fromFile(file))(_.mkString)
    if (contentTry.isFailure) {
      return Left(AppError.ConfigError("Не удалось прочитать конфигурационный файл."))
    }

    val content = contentTry.get
    val map = parseSimpleJson(content)
    if (map.isEmpty) {
      return Left(AppError.ConfigError("Конфигурационный файл имеет неправильный формат или пуст."))
    }

    val storageFile = map.getOrElse("storageFile", "")
    val logFile = map.getOrElse("logFile", "")

    val autoLockTry = scala.util.Try(map.getOrElse("automaticLockMinutes", "5").toInt)
    val maxAttemptsTry = scala.util.Try(map.getOrElse("maximumUnlockAttempts", "3").toInt)
    val confirmTry = scala.util.Try(map.getOrElse("confirmBeforeDelete", "true").toBoolean)

    if (storageFile.isEmpty) {
      Left(AppError.ConfigError("Параметр: storageFile\nПолучено: пустая строка\nОжидается: путь к файлу хранилища."))
    } else if (logFile.isEmpty) {
      Left(AppError.ConfigError("Параметр: logFile\nПолучено: пустая строка\nОжидается: путь к файлу журнала."))
    } else if (storageFile == logFile) {
      Left(AppError.ConfigError("Параметр: storageFile / logFile\nПолучено совпадающие пути\nОжидается: Для хранилища и журнала должны быть указаны разные файлы."))
    } else if (autoLockTry.isFailure || autoLockTry.get < 1) {
      val raw = map.getOrElse("automaticLockMinutes", "неизвестно")
      Left(AppError.ConfigError(s"Параметр: automaticLockMinutes\nПолучено: $raw\nОжидается: целое число не меньше 1."))
    } else if (maxAttemptsTry.isFailure || maxAttemptsTry.get < 1) {
      val raw = map.getOrElse("maximumUnlockAttempts", "неизвестно")
      Left(AppError.ConfigError(s"Параметр: maximumUnlockAttempts\nПолучено: $raw\nОжидается: целое число не меньше 1."))
    } else if (!Seq("service", "date").contains(map.getOrElse("defaultSort", "service"))) {
      val raw = map.getOrElse("defaultSort", "")
      Left(AppError.ConfigError(s"Параметр: defaultSort\nПолучено: $raw\nОжидается: 'service' или 'date'."))
    } else if (confirmTry.isFailure) {
      val raw = map.getOrElse("confirmBeforeDelete", "")
      Left(AppError.ConfigError(s"Параметр: confirmBeforeDelete\nПолучено: $raw\nОжидается: логическое значение (true/false)."))
    } else {
      Right(AppConfig(
        storageFile = storageFile,
        logFile = logFile,
        automaticLockMinutes = autoLockTry.get,
        maximumUnlockAttempts = maxAttemptsTry.get,
        defaultSort = map.getOrElse("defaultSort", "service"),
        confirmBeforeDelete = confirmTry.get
      ))
    }
  }

  private def parseSimpleJson(json: String): Map[String, String] = {
    var map = Map[String, String]()
    val cleaned = json.replaceAll("[\\{\\}\"]", "")
    for (line <- cleaned.split(",")) {
      val parts = line.split(":", 2)
      if (parts.length == 2) {
        map += (parts(0).trim -> parts(1).trim)
      }
    }
    map
  }
}