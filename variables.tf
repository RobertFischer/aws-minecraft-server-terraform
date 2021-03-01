variable "instance_type" {
  type        = string
  description = "The EC2 instance type on which to run the server"
  default     = "c6g.medium"
}

variable "slug" {
  type        = string
  description = "The prefix slug for names: should be only alphanumerics, dashes, and/or underscores"
  default     = "minecraft"
}

variable "human_name" {
  type        = string
  description = "The human-readable name"
}

variable "timezone" {
  type        = string
  description = "Linux time zone"
  default     = "America/New_York"
}

variable "server_jar_url" {
  type        = string
  description = "The URL where we can find server.jar.  See https://www.minecraft.net/en-us/download/server"
  default     = "https://launcher.mojang.com/v1/objects/1b557e7b033b583cd9f66746b7a9ab1ec1673ced/server.jar"
}

variable "primary_op" {
  type = object({
    uuid = string
    name = string
  })
  description = "the uuid/username of the primary operator"
}

variable "other_ops" {
  type = list(object({
    uuid  = string
    name  = string
    level = number
  }))
  description = "the uuid/username/level triplets for the operators"
  default     = []
}

variable "whitelist" {
  type = list(object({
    uuid = string
    name = string
  }))
  description = "the uuid/username pairs for whitelisted people"
  default     = []
}

variable "aws_profile" {
  type        = string
  description = "The AWS profile to use for credentials and configuration"
  default     = "default"
}

variable "aws_region" {
  type        = string
  description = "The AWS region to use"
  default     = "us-east-1"
}

variable "motd" {
  type        = string
  description = "The brief message to display on the server list screen"
  default     = "Enjoy!"
}

variable "start_at" {
  type        = string
  description = "Cron expression that specifies when to start the server"
  default     = "cron(50 11 * * ? *)" // 6:50AM EST
}

variable "stop_at" {
  type        = string
  description = "Cron expression that specifies when to start the server"
  default     = "cron(0 3 * * ? *)" // 10PM EST
}
