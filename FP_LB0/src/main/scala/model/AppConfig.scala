package model

case class AppConfig(
                      storageFile: String,
                      logFile: String,
                      automaticLockMinutes: Int,
                      maximumUnlockAttempts: Int,
                      defaultSort: String,
                      confirmBeforeDelete: Boolean
                    )