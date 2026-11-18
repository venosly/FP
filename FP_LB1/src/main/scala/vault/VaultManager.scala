package vault

import model.{AppConfig, AppError, PasswordEntry, VaultState}
import java.io.{File, PrintWriter, FileWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import java.security.MessageDigest
import scala.io.Source
import scala.util.Using

class VaultManager(var config: AppConfig) {
  private var state: VaultState = VaultState()

  val dateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def currentState: VaultState = {
    checkAutoLock()
    state
  }

  def updateActivity(): Unit = {
    state = state.copy(lastActivityTime = System.currentTimeMillis())
  }

  private def checkAutoLock(): Unit = {
    if (state.isUnlocked) {
      val diffMinutes = (System.currentTimeMillis() - state.lastActivityTime) / (1000 * 60)
      if (diffMinutes >= config.automaticLockMinutes) {
        lockVault()
      }
    }
  }

  def reloadConfig(newConfig: AppConfig): Unit = {
    config = newConfig
    log("INFO", "Конфигурация успешно перезагружена.")
  }

  def vaultExists(): Boolean = {
    new File(config.storageFile).exists()
  }

  def createVault(password: String): Either[AppError, Unit] = {
    val hash = hashPassword(password)
    state = state.copy(
      isUnlocked = true,
      masterPasswordHash = Some(hash),
      currentMasterPassword = Some(password),
      entries = List.empty,
      failedAttempts = 0,
      lastActivityTime = System.currentTimeMillis()
    )
    saveToFile().map(_ => log("INFO", "Хранилище успешно создано и открыто."))
  }

  def openVault(password: String): Either[AppError, Unit] = {
    checkAutoLock()
    if (state.failedAttempts >= config.maximumUnlockAttempts) {
      log("WARN", "Попытка доступа к заблокированному хранилищу после исчерпания попыток.")
      return Left(AppError.MaxAttemptsExceeded())
    }

    val file = new File(config.storageFile)
    if (!file.exists()) {
      return Left(AppError.VaultFileNotFound(config.storageFile))
    }

    val lines = Using(Source.fromFile(file))(_.getLines().toList).getOrElse(List.empty)
    if (lines.isEmpty) {
      log("ERROR", "Обнаружен поврежденный файл хранилища (пустой).")
      return Left(AppError.VaultCorrupted("пустой файл"))
    }

    val savedHash = lines.head
    val inputHash = hashPassword(password)

    if (savedHash == inputHash) {
      loadFromFile(password) match {
        case Right(loadedEntries) =>
          state = state.copy(
            isUnlocked = true,
            failedAttempts = 0,
            masterPasswordHash = Some(savedHash),
            currentMasterPassword = Some(password),
            entries = loadedEntries,
            lastActivityTime = System.currentTimeMillis()
          )
          log("INFO", "Хранилище успешно открыто.")
          Right(())
        case Left(err) =>
          Left(err)
      }
    } else {
      val newAttempts = state.failedAttempts + 1
      state = state.copy(failedAttempts = newAttempts)
      val attemptsLeft = config.maximumUnlockAttempts - newAttempts
      log("WARN", s"Неудачная попытка открытия хранилища. Осталось попыток: $attemptsLeft")
      Left(AppError.InvalidMasterPassword(attemptsLeft))
    }
  }

  def lockVault(): Unit = {
    state = state.copy(isUnlocked = false, currentMasterPassword = None)
    log("INFO", "Хранилище заблокировано.")
  }

  def addEntry(service: String, username: String, pass: String, email: String, url: String, note: String): Either[AppError, Int] = {
    updateActivity()
    if (!state.isUnlocked) return Left(AppError.VaultLocked())
    if (service.isEmpty || username.isEmpty || pass.isEmpty) {
      return Left(AppError.InvalidInput("Обязательные поля (сервис, имя пользователя, пароль) не должны быть пустыми."))
    }

    val now = LocalDateTime.now().format(dateFormatter)
    val newId = if (state.entries.isEmpty) 1 else state.entries.map(_.id).max + 1
    val entry = PasswordEntry(newId, service, username, pass, email, url, note, now, now)

    val updatedEntries = state.entries :+ entry
    state = state.copy(entries = updatedEntries)

    saveToFile().map { _ =>
      log("INFO", s"Добавлена запись: id=$newId")
      newId
    }
  }

  def updateEntry(id: Int, service: String, username: String, pass: String, email: String, url: String, note: String): Either[AppError, Unit] = {
    updateActivity()
    if (!state.isUnlocked) return Left(AppError.VaultLocked())

    state.entries.find(_.id == id) match {
      case Some(old) =>
        val now = LocalDateTime.now().format(dateFormatter)
        val updated = PasswordEntry(
          id = id,
          service = if (service.isEmpty) old.service else service,
          username = if (username.isEmpty) old.username else username,
          password = if (pass.isEmpty) old.password else pass,
          email = if (email.isEmpty) old.email else email,
          url = if (url.isEmpty) old.url else url,
          note = if (note.isEmpty) old.note else note,
          createdAt = old.createdAt,
          updatedAt = now
        )
        state = state.copy(entries = state.entries.map(e => if (e.id == id) updated else e))
        saveToFile().map(_ => log("INFO", s"Изменена запись: id=$id"))
      case None =>
        Left(AppError.EntryNotFound(id))
    }
  }

  def getSortedEntries(): List[PasswordEntry] = {
    updateActivity()
    config.defaultSort match {
      case "service" => state.entries.sortBy(_.service.toLowerCase)
      case "date"    => state.entries.sortBy(_.updatedAt).reverse
      case _         => state.entries
    }
  }

  def findEntry(id: Int): Either[AppError, PasswordEntry] = {
    updateActivity()
    if (!state.isUnlocked) return Left(AppError.VaultLocked())
    state.entries.find(_.id == id).toRight(AppError.EntryNotFound(id))
  }

  def searchEntries(query: String): Either[AppError, List[PasswordEntry]] = {
    updateActivity()
    if (!state.isUnlocked) return Left(AppError.VaultLocked())
    if (query.trim.isEmpty) {
      return Left(AppError.InvalidInput("Поисковый запрос не задан."))
    }
    val q = query.toLowerCase
    val found = state.entries.filter(e =>
      e.service.toLowerCase.contains(q) ||
        e.username.toLowerCase.contains(q) ||
        e.url.toLowerCase.contains(q)
    )
    log("INFO", s"Выполнен поиск: найдено записей=${found.size}")
    Right(found)
  }

  def deleteEntry(id: Int): Either[AppError, Unit] = {
    updateActivity()
    if (!state.isUnlocked) return Left(AppError.VaultLocked())

    state.entries.find(_.id == id) match {
      case Some(_) =>
        state = state.copy(entries = state.entries.filterNot(_.id == id))
        saveToFile().map(_ => log("INFO", s"Удалена запись: id=$id"))
      case None =>
        Left(AppError.EntryNotFound(id))
    }
  }

  private def saveToFile(): Either[AppError, Unit] = {
    try {
      val writer = new PrintWriter(new FileWriter(config.storageFile))
      try {
        state.masterPasswordHash.foreach(writer.println)
        state.currentMasterPassword match {
          case Some(pass) =>
            state.entries.foreach { e =>
              val line = s"${e.id}|${e.service}|${e.username}|${e.password}|${e.email}|${e.url}|${e.note}|${e.createdAt}|${e.updatedAt}"
              writer.println(encrypt(line, pass))
            }
          case None => return Left(AppError.VaultLocked())
        }
      } finally {
        writer.close()
      }
      Right(())
    } catch {
      case e: Exception =>
        log("ERROR", s"Ошибка сохранения файла хранилища: ${e.getMessage}")
        Left(AppError.VaultCorrupted(e.getMessage))
    }
  }

  private def loadFromFile(password: String): Either[AppError, List[PasswordEntry]] = {
    try {
      val file = new File(config.storageFile)
      val lines = Using(Source.fromFile(file))(_.getLines().toList).getOrElse(List.empty)
      if (lines.isEmpty) {
        return Left(AppError.VaultCorrupted("пустой файл"))
      }

      var loadedEntries = List[PasswordEntry]()
      for (encryptedLine <- lines.tail) {
        val decrypted = decrypt(encryptedLine, password)
        val parts = decrypted.split("\\|", -1)
        if (parts.length == 9) {
          loadedEntries = loadedEntries :+ PasswordEntry(
            parts(0).toInt, parts(1), parts(2), parts(3), parts(4), parts(5), parts(6), parts(7), parts(8)
          )
        }
      }
      Right(loadedEntries)
    } catch {
      case _: Exception =>
        log("ERROR", "Обнаружен поврежденный файл хранилища или неверный мастер-ключ.")
        Left(AppError.VaultCorrupted("неверный ключ или повреждение"))
    }
  }

  private def getSecretKey(password: String): SecretKeySpec = {
    val sha = MessageDigest.getInstance("SHA-256")
    val keyBytes = sha.digest(password.getBytes("UTF-8"))
    new SecretKeySpec(keyBytes.take(16), "AES")
  }

  private def encrypt(data: String, password: String): String = {
    val keySpec = getSecretKey(password)
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, keySpec)
    val encryptedBytes = cipher.doFinal(data.getBytes("UTF-8"))
    Base64.getEncoder.encodeToString(encryptedBytes)
  }

  private def decrypt(encryptedData: String, password: String): String = {
    val keySpec = getSecretKey(password)
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.DECRYPT_MODE, keySpec)
    val decryptedBytes = cipher.doFinal(Base64.getDecoder.decode(encryptedData))
    new String(decryptedBytes, "UTF-8")
  }

  private def hashPassword(pass: String): String = {
    val md = java.security.MessageDigest.getInstance("SHA-256")
    md.digest(pass.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  def log(level: String, message: String): Unit = {
    try {
      val fw = new FileWriter(config.logFile, true)
      val time = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      fw.write(s"$time $level $message\n")
      fw.close()
    } catch {
      case _: Exception =>
    }
  }
}