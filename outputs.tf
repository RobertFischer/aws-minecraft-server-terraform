output "game_data" {
  description = "The storage for game data as an 'aws_ebs_volume' resource"
  value       = aws_efs_file_system.rundir
}

output "host_address" {
  description = "The address where people should connect to"
  value       = aws_eip.main.public_ip
}

output "aws" {
  description = "Info about the executing AWS environment"
  value = {
    region  = var.aws_region
    profile = var.aws_profile
  }
}

