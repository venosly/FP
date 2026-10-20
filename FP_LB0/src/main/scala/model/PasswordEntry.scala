package model

case class PasswordEntry(
                          id: Int,
                          service: String,
                          username: String,
                          password: String,
                          email: String,
                          url: String,
                          note: String,
                          createdAt: String,
                          updatedAt: String
                        )