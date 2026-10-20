package vault

import model.{AppConfig, PasswordEntry}
import java.io.{File, PrintWriter, FileWriter}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import java.security.MessageDigest
import scala.io.{Source, StdIn}
import scala.util.Using

class VaultManager(var config: AppConfig) {
  private var isUnlocked: Boolean = false
  private var masterPasswordHash: Option[String] = None
  private var currentMasterPassword: Option[String] = None
  private var entries: List[PasswordEntry] = List.empty
  private var failedAttempts: Int = 0
  private var lastActivityTime: Long = System.currentTimeMillis()

  val dateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def isVaultUnlocked: Boolean = {
    checkAutoLock()
    isUnlocked
  }

  def updateActivity(): Unit = {
    lastActivityTime = System.currentTimeMillis()
  }

  private def checkAutoLock(): Unit = {
    if (isUnlocked) {
      val diffMinutes = (System.currentTimeMillis() - lastActivityTime) / (1000 * 60)
      if (diffMinutes >= config.automaticLockMinutes) {
        lockVault()
        println("\n[Внимание] Хранилище автоматически заблокировано из-за неактивности.")
      }
    }
  }

  def reloadConfig(newConfig: AppConfig): Unit = {
    config = newConfig
    log("INFO", "Конфигурация успешно перезагружена.")
    println("Конфигурация успешно перезагружена.")
  }

  def createVault(password: String): Unit = {
    masterPasswordHash = Some(hashPassword(password))
    currentMasterPassword = Some(password)
    entries = List.empty
    saveToFile()
    isUnlocked = true
    failedAttempts = 0
    updateActivity()
    log("INFO", "Хранилище успешно создано и открыто.")
    println("Хранилище успешно создано и открыто.")
  }

  def vaultExists(): Boolean = {
    new File(config.storageFile).exists()
  }

  def openVault(password: String): Boolean = {
    checkAutoLock()
    if (failedAttempts >= config.maximumUnlockAttempts) {
      log("WARN", "Попытка доступа к заблокированному хранилищу после исчерпания попыток.")
      println("Превышено максимальное количество попыток открытия. Доступ заблокирован.")
      return false
    }

    val file = new File(config.storageFile)
    if (!file.exists()) {
      println("Файл хранилища не существует. Сначала создайте хранилище командой 'create'.")
      return false
    }

    val lines = Using(Source.fromFile(file))(_.getLines().toList).getOrElse(List.empty)
    if (lines.isEmpty) {
      println("Ошибка: файл хранилища поврежден.")
      log("ERROR", "Обнаружен поврежденный файл хранилища (пустой).")
      return false
    }

    val savedHash = lines.head
    val inputHash = hashPassword(password)

    if (savedHash == inputHash) {
      currentMasterPassword = Some(password)
      if (loadFromFile()) {
        isUnlocked = true
        failedAttempts = 0
        masterPasswordHash = Some(savedHash)
        updateActivity()
        log("INFO", "Хранилище успешно открыто.")
        println("Хранилище успешно открыто.")
        true
      } else {
        currentMasterPassword = None
        false
      }
    } else {
      failedAttempts += 1
      log("WARN", s"Неудачная попытка открытия хранилища. Осталось попыток: ${config.maximumUnlockAttempts - failedAttempts}")
      println(s"Неправильный мастер-пароль. Осталось попыток: ${config.maximumUnlockAttempts - failedAttempts}")
      false
    }
  }

  def lockVault(): Unit = {
    isUnlocked = false
    currentMasterPassword = None // Очищаем мастер-пароль из оперативной памяти при блокировке
    log("INFO", "Хранилище заблокировано.")
    println("Хранилище заблокировано.")
  }

  def addEntry(service: String, username: String, pass: String, email: String, url: String, note: String): Unit = {
    updateActivity()
    if (service.isEmpty || username.isEmpty || pass.isEmpty) {
      println("Ошибка: обязательные поля (сервис, имя пользователя, пароль) не должны быть пустыми.")
      return
    }
    val now = LocalDateTime.now().format(dateFormatter)
    val newId = if (entries.isEmpty) 1 else entries.map(_.id).max + 1
    val entry = PasswordEntry(newId, service, username, pass, email, url, note, now, now)
    entries = entries :+ entry
    if (saveToFile()) {
      log("INFO", s"Добавлена запись: id=$newId")
      println(s"Добавлена запись с ID: $newId")
    }
  }

  def updateEntry(id: Int, service: String, username: String, pass: String, email: String, url: String, note: String): Unit = {
    updateActivity()
    entries.find(_.id == id) match {
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
        entries = entries.map(e => if (e.id == id) updated else e)
        if (saveToFile()) {
          log("INFO", s"Изменена запись: id=$id")
          println("Запись успешно обновлена.")
        }
      case None =>
        println(s"Запись с ID $id не существует.")
    }
  }

  def listEntries(): Unit = {
    updateActivity()
    val sorted = config.defaultSort match {
      case "service" => entries.sortBy(_.service.toLowerCase)
      case "date"    => entries.sortBy(_.updatedAt).reverse
      case _         => entries
    }

    println(f"${"ID"}%-4s ${"Сервис"}%-15s ${"Имя пользователя"}%-25s ${"Изменена"}%s")
    println("-" * 65)
    sorted.foreach { e =>
      println(f"${e.id}%-4d ${e.service}%-15s ${e.username}%-25s ${e.updatedAt}%s")
    }
  }

  def showEntry(id: Int): Unit = {
    updateActivity()
    entries.find(_.id == id) match {
      case Some(e) =>
        println(s"Сервис: ${e.service}")
        println(s"Адрес: ${e.url}")
        println(s"Имя пользователя: ${e.username}")
        println(s"Пароль: ********")
        println(s"Заметка: ${e.note}")
        println(s"Создана: ${e.createdAt}")
        println(s"Изменена: ${e.updatedAt}")
      case None =>
        println(s"Запись с ID $id не существует.")
    }
  }

  def showPassword(id: Int): Unit = {
    updateActivity()
    entries.find(_.id == id) match {
      case Some(e) =>
        log("INFO", s"Отображён пароль записи: id=$id")
        println(s"Пароль для ID $id: ${e.password}")
      case None    =>
        println(s"Запись с ID $id не существует.")
    }
  }

  def searchEntries(query: String): Unit = {
    updateActivity()
    if (query.trim.isEmpty) {
      println("Ошибка: поисковый запрос не задан.")
      return
    }
    val q = query.toLowerCase
    val found = entries.filter(e =>
      e.service.toLowerCase.contains(q) ||
        e.username.toLowerCase.contains(q) ||
        e.url.toLowerCase.contains(q)
    )

    log("INFO", s"Выполнен поиск: найдено записей=${found.size}")
    println(f"${"ID"}%-4s ${"Сервис"}%-15s ${"Имя пользователя"}%s")
    println("-" * 45)
    found.foreach(e => println(f"${e.id}%-4d ${e.service}%-15s ${e.username}%s"))
  }

  def deleteEntry(id: Int): Unit = {
    updateActivity()
    entries.find(_.id == id) match {
      case Some(e) =>
        if (config.confirmBeforeDelete) {
          println(s"Будет удалена запись:\nСервис: ${e.service}\nИмя пользователя: ${e.username}")
          print("Удалить запись? [да/нет]: ")
          val answer = StdIn.readLine().trim.toLowerCase
          if (answer == "да" || answer == "yes") {
            entries = entries.filterNot(_.id == id)
            if (saveToFile()) {
              log("INFO", s"Удалена запись: id=$id")
              println("Запись успешно удалена.")
            }
          } else {
            println("Удаление отменено.")
          }
        } else {
          entries = entries.filterNot(_.id == id)
          if (saveToFile()) {
            log("INFO", s"Удалена запись: id=$id")
            println("Запись успешно удалена.")
          }
        }
      case None =>
        println(s"Запись с ID $id не найдена.")
    }
  }

  private def saveToFile(): Boolean = {
    try {
      val writer = new PrintWriter(new FileWriter(config.storageFile))
      try {
        masterPasswordHash.foreach(writer.println)
        currentMasterPassword match {
          case Some(pass) =>
            entries.foreach { e =>
              val line = s"${e.id}|${e.service}|${e.username}|${e.password}|${e.email}|${e.url}|${e.note}|${e.createdAt}|${e.updatedAt}"
              writer.println(encrypt(line, pass)) // Шифруем AES с мастер-ключом
            }
          case None => return false
        }
      } finally {
        writer.close()
      }
      true
    } catch {
      case e: Exception =>
        log("ERROR", s"Ошибка сохранения файла хранилища: ${e.getMessage}")
        println("Ошибка: не удалось сохранить хранилище на диск.")
        false
    }
  }

  private def loadFromFile(): Boolean = {
    try {
      val file = new File(config.storageFile)
      val lines = Using(Source.fromFile(file))(_.getLines().toList).getOrElse(List.empty)
      if (lines.isEmpty) {
        log("ERROR", "Обнаружен поврежденный файл хранилища.")
        println("Ошибка: файл хранилища поврежден или пуст.")
        return false
      }

      currentMasterPassword match {
        case Some(pass) =>
          var loadedEntries = List[PasswordEntry]()
          for (encryptedLine <- lines.tail) {
            val decrypted = decrypt(encryptedLine, pass)
            val parts = decrypted.split("\\|", -1)
            if (parts.length == 9) {
              loadedEntries = loadedEntries :+ PasswordEntry(
                parts(0).toInt, parts(1), parts(2), parts(3), parts(4), parts(5), parts(6), parts(7), parts(8)
              )
            }
          }
          entries = loadedEntries
          true
        case None => false
      }
    } catch {
      case _: Exception =>
        log("ERROR", "Обнаружен поврежденный файл хранилища или неверный мастер-ключ.")
        println("Ошибка: файл хранилища поврежден или неверный мастер-пароль.")
        false
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