
module "lambda" {
  source      = "./lambda"
  lambda_name = var.lambda_name
  repo_url    = var.ecr_repo_url
}
