locals {
  node_modules_pkgs = length(fileset("${path.module}/lambda/node_modules", "**/package.json"))
  lambda_name = "${var.slug}startstop"
  keydir      = "${path.module}/keys"
  name_prefix = "${var.slug}_"
  az          = data.aws_availability_zones.available.names[0]
  tags = {
    Name = "${var.slug} - ${random_pet.deployment_name.id}"
    Slug = var.slug
  }
  slug      = var.slug
  whitelist = var.whitelist
  ops = concat(
    [merge(var.primary_op, { level : 4, bypassesPlayerLimit : true })],
    var.other_ops
  )
  homedir  = "/opt/${var.slug}"
  mountdir = local.homedir
  basic_interps = {
    keysdir        = local.keydir
    timezone       = var.timezone
    homedir        = local.homedir
    mountdir       = local.mountdir
    heredir        = path.module
    rcon_password  = random_password.rcon.result
    slug           = local.slug
    server_jar_url = var.server_jar_url
    human_name     = var.human_name
    motd           = var.motd
    efs_id         = aws_efs_mount_target.rundir.dns_name
  }
  private_key = file("${path.module}/keys/ec2-user")
}


data "aws_availability_zones" "available" {
  state = "available"
  filter {
    name   = "opt-in-status"
    values = ["opt-in-not-required"]
  }
}

data "aws_ami" "main" {
  owners      = ["amazon"]
  most_recent = true
  filter {
    name   = "root-device-type"
    values = ["ebs"]
  }
  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
  filter {
    name   = "architecture"
    values = ["arm64"]
  }
  filter {
    name   = "state"
    values = ["available"]
  }
  filter {
    name   = "image-type"
    values = ["machine"]
  }
  filter {
    name   = "name"
    values = ["amzn2-ami-*-gp2"]
  }
}

resource "aws_key_pair" "main" {
  key_name_prefix = local.name_prefix
  public_key      = file("${local.keydir}/ec2-user.pub")
  lifecycle {
    prevent_destroy = true
  }
}



resource "aws_vpc" "main" {
  cidr_block                       = "10.0.0.0/16"
  assign_generated_ipv6_cidr_block = true
  enable_dns_hostnames             = true
  enable_dns_support               = true
  tags                             = local.tags
}

resource "aws_subnet" "main" {
  vpc_id            = aws_vpc.main.id
  cidr_block        = "10.0.1.0/24"
  tags              = local.tags
  availability_zone = local.az
}

resource "aws_security_group" "efs" {
  name_prefix            = "${local.name_prefix}efs_"
  description            = "${var.human_name} EFS"
  vpc_id                 = aws_vpc.main.id
  revoke_rules_on_delete = true
}

resource "aws_security_group" "main" {
  name_prefix            = "${local.name_prefix}main_"
  description            = "${var.human_name} main"
  vpc_id                 = aws_vpc.main.id
  revoke_rules_on_delete = true
  ingress {
    protocol        = -1
    from_port       = 0
    to_port         = 0
    security_groups = [aws_security_group.efs.id]
    self            = true
  }
  egress {
    protocol        = -1
    from_port       = 0
    to_port         = 0
    security_groups = [aws_security_group.efs.id]
    self            = true
  }
  ingress {
    from_port   = 25575
    to_port     = 25575
    cidr_blocks = ["0.0.0.0/0"]
    protocol    = "tcp"
    description = "Minecraft rcon"
  }
  ingress {
    from_port   = 25565
    to_port     = 25565
    cidr_blocks = ["0.0.0.0/0"]
    protocol    = "tcp"
    description = "Minecraft"
  }
  ingress {
    from_port   = 22
    to_port     = 22
    cidr_blocks = ["0.0.0.0/0"]
    protocol    = "tcp"
    description = "SSH"
  }
  egress {
    from_port   = 80
    to_port     = 80
    cidr_blocks = ["0.0.0.0/0"]
    protocol    = "tcp"
    description = "HTTP"
  }
  egress {
    from_port   = 443
    to_port     = 443
    cidr_blocks = ["0.0.0.0/0"]
    protocol    = "tcp"
    description = "TLS / HTTPS"
  }
  tags = local.tags
}

resource "aws_default_network_acl" "main" {
  default_network_acl_id = aws_vpc.main.default_network_acl_id

  ingress { // ephemeral tcp
    protocol   = "tcp"
    rule_no    = 100
    action     = "allow"
    from_port  = 1024
    to_port    = 65535
    cidr_block = "0.0.0.0/0"
  }

  ingress { // ssh
    protocol   = "tcp"
    rule_no    = 110
    action     = "allow"
    from_port  = 22
    to_port    = 22
    cidr_block = "0.0.0.0/0"
  }

  ingress { // minecraft
    protocol   = "tcp"
    rule_no    = 120
    action     = "allow"
    from_port  = 25575
    to_port    = 25575
    cidr_block = "0.0.0.0/0"
  }

  ingress { // ephemeral udp (ntp)
    protocol   = "udp"
    rule_no    = 130
    action     = "allow"
    from_port  = 1024
    to_port    = 65535
    cidr_block = "0.0.0.0/0"
  }

  egress { // ephemeral tcp
    protocol   = "tcp"
    rule_no    = 100
    action     = "allow"
    from_port  = 1024
    to_port    = 65535
    cidr_block = "0.0.0.0/0"
  }

  egress { // http
    protocol   = "tcp"
    rule_no    = 110
    action     = "allow"
    from_port  = 80
    to_port    = 80
    cidr_block = "0.0.0.0/0"
  }

  egress { // tls/https
    protocol   = "tcp"
    rule_no    = 120
    action     = "allow"
    from_port  = 443
    to_port    = 443
    cidr_block = "0.0.0.0/0"
  }

  egress { // ntp
    protocol   = "udp"
    rule_no    = 130
    action     = "allow"
    from_port  = 123
    to_port    = 123
    cidr_block = "0.0.0.0/0"
  }

  egress { // dns udp
    protocol   = "udp"
    rule_no    = 140
    action     = "allow"
    from_port  = 52
    to_port    = 52
    cidr_block = "0.0.0.0/0"
  }

  egress { // dns tcp
    protocol   = "tcp"
    rule_no    = 150
    action     = "allow"
    from_port  = 52
    to_port    = 52
    cidr_block = "0.0.0.0/0"
  }

}

resource "aws_efs_file_system" "rundir" {
  encrypted = false
  lifecycle_policy {
    transition_to_ia = "AFTER_7_DAYS"
  }
  tags = local.tags
  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_efs_mount_target" "rundir" {
  file_system_id = aws_efs_file_system.rundir.id
  subnet_id      = aws_subnet.main.id
  security_groups = [
    aws_security_group.main.id,
    aws_security_group.efs.id,
  ]
  # AWS says to make sure to wait after this thing is created before using it
  provisioner "local-exec" {
    on_failure  = continue
    interpreter = ["/bin/bash", "-c"]
    command     = "sleep 90000"
  }
  lifecycle {
    create_before_destroy = true
  }
}

data "aws_caller_identity" "current" {}

data "aws_iam_policy_document" "efs_user_assume_role" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com", "backup.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "efs_user" {
  name_prefix        = "${var.slug}_efs_user_"
  assume_role_policy = data.aws_iam_policy_document.efs_user_assume_role.json
  tags               = local.tags
}


data "aws_iam_policy_document" "efs_fs_policy" {
  statement {
    actions = [
      "elasticfilesystem:ClientMount",
      "elasticfilesystem:ClientRootAccess",
      "elasticfilesystem:ClientWrite",
    ]
    principals {
      type = "AWS"
      identifiers = [
        "arn:aws:iam::${data.aws_caller_identity.current.account_id}:root",
        aws_iam_role.efs_user.arn,
      ]
    }
    resources = [
      aws_efs_file_system.rundir.arn
    ]
    condition {
      test     = "Bool"
      variable = "aws:SecureTransport"
      values   = ["true"]
    }
  }
}

resource "aws_efs_file_system_policy" "efs_user" {
  file_system_id = aws_efs_file_system.rundir.id
  policy         = data.aws_iam_policy_document.efs_fs_policy.json
}

resource "aws_iam_instance_profile" "main_efs_user" {
  name_prefix = "${var.slug}_efs_user_"
  role        = aws_iam_role.efs_user.name
}

data archive_file config {

  type = "zip"
  output_path = "${path.module}/config.zip"

  source {
    content = templatefile("${path.module}/config/install.sh", local.basic_interps)
    filename = "config/install.sh"
  }

  source {
    content = templatefile("${path.module}/config/log4j2.xml", local.basic_interps)
    filename = "config/server/log4j2.xml"
  }

  source {
    content = templatefile("${path.module}/config/server.properties", local.basic_interps)
    filename = "config/server/server.properties"
  }

  source {
    content = "eula=true\n"
    filename = "config/server/eula.txt"
  }

  source {
    content = jsonencode(local.ops)
    filename = "config/server/ops.json"
  }

  source {
    content = templatefile("${path.module}/config/minecraft.service", local.basic_interps)
    filename = "config/minecraft.service"
  }

}

resource "aws_instance" "main" {
  ami                                  = data.aws_ami.main.image_id
  key_name                             = aws_key_pair.main.key_name
  availability_zone                    = local.az
  ebs_optimized                        = true
  instance_initiated_shutdown_behavior = "stop"
  instance_type                        = var.instance_type
  vpc_security_group_ids               = [aws_security_group.main.id]
  subnet_id                            = aws_subnet.main.id
  associate_public_ip_address          = true
  iam_instance_profile                 = aws_iam_instance_profile.main_efs_user.name
  cpu_threads_per_core                 = 1
  credit_specification {
    cpu_credits = "standard"
  }
  user_data_base64 = base64gzip(templatefile("${path.module}/config/cloud-config.yaml",
    merge(
      local.basic_interps,
      { config_zip_sha256 = data.archive_file.config.output_base64sha256 }
    )
  ))
  volume_tags      = local.tags
  root_block_device {
    volume_size = 16
  }
  tags = local.tags
  depends_on = [
    // Make sure that things are mostly put together before creating the instance
    // TODO Move these things into a module and then use outputs from the module
    aws_eip.main,
    aws_default_network_acl.main,
    aws_internet_gateway.main,
    aws_main_route_table_association.main,
    aws_efs_mount_target.rundir,
    aws_efs_file_system_policy.efs_user,
    aws_iam_role_policy_attachment.efs_utils,
    aws_iam_role_policy_attachment.backup_policy,
    data.archive_file.config,
  ]

  provisioner "file" {
    source      = data.archive_file.config.output_path
    destination = "./config.zip"
    connection {
      type        = "ssh"
      user        = "ec2-user"
      private_key = local.private_key
      host        = self.public_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "unzip ./config.zip",
      "chmod +x ./config/install.sh",
      "sudo ./config/install.sh",
      "sudo df -T"
    ]
    connection {
      type        = "ssh"
      user        = "ec2-user"
      private_key = local.private_key
      host        = self.public_ip
    }
  }
}

resource "aws_eip" "main" {
  vpc = true
  tags = merge(
    local.tags,
    { AssignedTo : "server" }
  )
  lifecycle {
    prevent_destroy = true
  }
}

resource "aws_eip_association" "main" {
  instance_id   = aws_instance.main.id
  allocation_id = aws_eip.main.id
  provisioner "local-exec" {
    on_failure  = continue
    interpreter = ["/bin/bash", "-c"]
    command     = "ssh-keygen -R ${aws_eip.main.public_ip}"
  }
}

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id
  tags   = local.tags
}

resource "aws_route_table" "main" {
  vpc_id = aws_vpc.main.id
  tags   = local.tags
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }
}

resource "aws_main_route_table_association" "main" {
  vpc_id         = aws_vpc.main.id
  route_table_id = aws_route_table.main.id
}

resource "time_rotating" "rcon_password_update" {
  rotation_days = 1
}

resource "random_password" "rcon" {
  length = 16
  keepers = {
    date = time_rotating.rcon_password_update.id
  }
}

resource "aws_cloudwatch_event_rule" "good_morning" {
  name_prefix         = "${var.slug}_start_"
  description         = "Turns on the ${var.slug} server on the mornings"
  schedule_expression = var.start_at
}

resource "aws_cloudwatch_event_rule" "good_night" {
  name_prefix         = "${var.slug}_stop_"
  description         = "Turns off the ${var.slug} server"
  schedule_expression = var.stop_at
}

resource "aws_iam_role_policy_attachment" "backup_policy" {
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSBackupServiceRolePolicyForBackup"
  role       = aws_iam_role.efs_user.name
}

resource "aws_iam_role_policy_attachment" "efs_utils" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonElasticFileSystemsUtils"
  role       = aws_iam_role.efs_user.name
}

resource "aws_backup_selection" "rundir" {
  name         = "${var.slug}_efs_backup"
  iam_role_arn = aws_iam_role.efs_user.arn
  plan_id      = aws_backup_plan.rundir.id
  resources = [
    aws_efs_file_system.rundir.arn
  ]
}

resource "aws_backup_vault" "rundir" {
  name = "${var.slug}_efs_backup"
  tags = local.tags
}

resource "aws_backup_plan" "rundir" {
  name = "${var.slug}_efs_backup"
  rule {
    rule_name         = "Run_At_Stop_And_Clean_Up"
    schedule          = var.stop_at
    target_vault_name = aws_backup_vault.rundir.name
    lifecycle {
      cold_storage_after = 10
      delete_after       = 100
    }
  }
  tags = local.tags
}

resource "random_pet" "deployment_name" {
  length    = 2
  separator = "_"
}

data "aws_iam_policy_document" "lambda" {
  statement {
    actions   = ["ec2:StartInstances", "ec2:StopInstances"]
    resources = [aws_instance.main.arn]
  }
}

resource "aws_iam_role_policy" "lambda" {
  name_prefix = "instance_start_stop_"
  role        = aws_iam_role.lambda.id
  policy      = data.aws_iam_policy_document.lambda.json
}

resource "aws_iam_role_policy_attachment" "lambda_basics" {
  role       = aws_iam_role.lambda.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

resource "aws_iam_role_policy_attachment" "lambda_vpc" {
  role       = aws_iam_role.lambda.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaVPCAccessExecutionRole"
}

resource "aws_iam_role_policy_attachment" "lambda_default" {
  role       = aws_iam_role.lambda.id
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaRole"
}

resource null_resource build_lambda_code {
  triggers = {
    yarn_lock = filebase64sha256("${path.module}/lambda/yarn.lock")
    index_js = filebase64sha256("${path.module}/lambda/index.js")
    node_modules = local.node_modules_pkgs == 0 ? timestamp() : local.node_modules_pkgs
  }
  provisioner "local-exec" {
    working_dir = "${path.module}/lambda"
    command     = "rm -rf ./node_modules"
  }
  provisioner "local-exec" {
    working_dir = "${path.module}/lambda"
    command     = "yarn install --production --frozen-lockfile --no-link-bins"
  }

}

data archive_file lambda_code {
  type = "zip"
  output_path = "${path.module}/lambda.zip"
  source_dir = "${path.module}/lambda"
  depends_on = [
    null_resource.build_lambda_code
  ]
}

data aws_iam_policy_document lambda_assume_role {
  statement {
    actions = [ "sts:AssumeRole" ]
    principals {
      type = "Service"
      identifiers = [ "lambda.amazonaws.com" ]
    }
  }
}

resource aws_iam_role lambda {
  name_prefix = "start-stop-instance-lambda-"
  assume_role_policy = data.aws_iam_policy_document.lambda_assume_role.json
}


resource aws_cloudwatch_log_group lambda {
  name = "/aws/lambda/${local.lambda_name}"
  retention_in_days = 1
}

data aws_iam_policy_document cw_logs {
  statement {
    actions = [
        "logs:CreateLogStream",
        "logs:PutLogEvents",
    ]
    resources = [
      aws_cloudwatch_log_group.lambda.arn,
      "${aws_cloudwatch_log_group.lambda.arn}/*",
    ]
  }
}

resource aws_iam_role_policy cw_logs {
  name_prefix = "cloudwatch_logs_"
  role = aws_iam_role.lambda.id
  policy = data.aws_iam_policy_document.cw_logs.json
}

resource aws_lambda_function start_stop_instance {
  function_name = local.lambda_name
  handler = "index.handler"
  role = aws_iam_role.lambda.arn
  runtime = "nodejs14.x"
  filename = data.archive_file.lambda_code.output_path
  source_code_hash = data.archive_file.lambda_code.output_base64sha256
  depends_on = [
    aws_iam_role_policy.cw_logs,
  ]
}

resource "aws_lambda_permission" "allow_cloudwatch" {
  for_each = toset([
    aws_cloudwatch_event_rule.good_morning.arn,
    aws_cloudwatch_event_rule.good_night.arn,
  ])
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.start_stop_instance.function_name
  principal     = "events.amazonaws.com"
  source_arn    = each.value
}

resource "aws_cloudwatch_event_target" "start_stop_server" {
  for_each = {
    "start": aws_cloudwatch_event_rule.good_morning.name,
    "stop": aws_cloudwatch_event_rule.good_night.name,
  }
  arn  = aws_lambda_function.start_stop_instance.arn
  rule = each.value
  input_transformer {
    input_template = jsonencode({
      command = each.key
      arn     = aws_instance.main.arn
      instanceId = aws_instance.main.id
    })
  }
  depends_on = [
    aws_lambda_permission.allow_cloudwatch
  ]
}


