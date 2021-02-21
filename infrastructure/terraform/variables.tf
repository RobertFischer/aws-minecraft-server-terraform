variable "instance_type" {
  type        = string
  description = "The EC2 instance type on which to run the server"
  default     = "t4g.small"
}

variable "slug" {
  type        = string
  description = "The prefix slug for names: should be only alphanumerics, dashes, and/or underscores"
  default     = "minecraft"
}

variable "human_name" {
  type        = string
  description = "The human-readable name"
  default     = "Minecraft Server"
}

variable "timezone" {
  type        = string
  description = "Linux time zone"
  default     = "America/New_York"
}

variable "server_jar_url" {
  type        = string
  description = "The URL where we can find server.jar.  See https://www.minecraft.net/en-us/download/server"
  default     = "https://launcher.mojang.com/v1/objects/35139deedbd5182953cf1caa23835da59ca3d7cd/server.jar"
}

variable "aws_profile" {
  type        = string
  description = "The name of the pre-configured AWS identity profile to use"
  default     = "default"
}

variable "aws_region" {
  type        = string
  description = "The AWS region to connect to"
  default     = "us-east-1"
}

variable op {
  type = object({
    uuid = string
    username = string
  })
  description = "the uuid and username of the initial operator"
}
