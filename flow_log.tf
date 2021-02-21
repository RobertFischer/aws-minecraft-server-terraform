resource "aws_flow_log" "example" {
  iam_role_arn    = aws_iam_role.flow_log.arn
  log_destination = aws_cloudwatch_log_group.flow_log.arn
  traffic_type    = "REJECT"
  vpc_id          = aws_vpc.main.id
  tags            = local.tags
}

resource "aws_cloudwatch_log_group" "flow_log" {
  name_prefix       = "/flow/vpc/${aws_vpc.main.id}_"
  retention_in_days = 1
  tags              = local.tags
}

resource "aws_iam_role" "flow_log" {
  name_prefix = "flow_log_${aws_vpc.main.id}_"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "",
      "Effect": "Allow",
      "Principal": {
        "Service": "vpc-flow-logs.amazonaws.com"
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
EOF
}

resource "aws_iam_role_policy" "flow_log" {
  name_prefix = "flow_log_"
  role        = aws_iam_role.flow_log.id

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents",
        "logs:DescribeLogGroups",
        "logs:DescribeLogStreams"
      ],
      "Effect": "Allow",
      "Resource": "*"
    }
  ]
}
EOF
}
