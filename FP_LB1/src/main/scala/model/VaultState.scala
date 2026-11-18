package model

case class VaultState(
                       isUnlocked: Boolean = false,
                       masterPasswordHash: Option[String] = None,
                       currentMasterPassword: Option[String] = None,
                       entries: List[PasswordEntry] = List.empty,
                       failedAttempts: Int = 0,
                       lastActivityTime: Long = System.currentTimeMillis()
                     )