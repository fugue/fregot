provider "aws" {
  region = "us-east-1"
}

resource "aws_instance" "foo" {
  ami = "ami-04b9e92b5572fa0d1"
  instance_type = "t2.micro"
}

resource "aws_instance" "bar" {
  ami = "ami-0b69ea66ff7391e80"
  instance_type = "t2.micro"
}