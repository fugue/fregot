package fregot.examples.ami_id.inputs

valid_single_ami = {
   "resource_changes": [
      {
         "change": {
            "after": {
               "ami": "ami-0b69ea66ff7391e80"
               }
         }
      }
   ]
}

invalid_single_ami = {
   "resource_changes": [
      {
         "change": {
            "after": {
               "ami": "ami-atotallyfakeamiid"
               }
         }
      }
   ]
}  

valid_two_different_amis = {
   "resource_changes": [
      {
         "change": {
            "after": {
               "ami": "ami-04b9e92b5572fa0d1"
               }
         }
      },
      {
         "change": {
            "after": {
               "ami": "ami-0b69ea66ff7391e80"
               }
         }
      }
   ]
} 

invalid_two_different_amis = {
   "resource_changes": [
      {
         "change": {
            "after": {
               "ami": "ami-12345678901234567"
               }
         }
      },
      {
         "change": {
            "after": {
               "ami": "ami-0c322300a1dd5dc79"
               }
         }
      }
   ]
} 

valid_two_same_amis = {
   "resource_changes": [
      {
         "change": {
            "after": {
               "ami": "ami-04b9e92b5572fa0d1"
               }
         }
      },
      {
         "change": {
            "after": {
               "ami": "ami-04b9e92b5572fa0d1"
               }
         }
      }
   ]
} 

invalid_two_same_amis = {
   "resource_changes": [
      {
         "change": {
            "after": {
               "ami": "ami-0c322300a1dd5dc79"
               }
         }
      },
      {
         "change": {
            "after": {
               "ami": "ami-0c322300a1dd5dc79"
               }
         }
      }
   ]
} 

valid_single_ami_from_tf_plan = {
   "format_version": "0.1",
   "terraform_version": "0.12.12",
   "planned_values": {
      "root_module": {
         "resources": [
            {
               "address": "aws_instance.foo",
               "mode": "managed",
               "type": "aws_instance",
               "name": "foo",
               "provider_name": "aws",
               "schema_version": 1,
               "values": {
                  "ami": "ami-04b9e92b5572fa0d1",
                  "credit_specification": [],
                  "disable_api_termination": null,
                  "ebs_optimized": null,
                  "get_password_data": false,
                  "iam_instance_profile": null,
                  "instance_initiated_shutdown_behavior": null,
                  "instance_type": "t2.micro",
                  "monitoring": null,
                  "source_dest_check": true,
                  "tags": null,
                  "timeouts": null,
                  "user_data": null,
                  "user_data_base64": null
               }
            }
         ]
      }
   },
   "resource_changes": [
      {
         "address": "aws_instance.foo",
         "mode": "managed",
         "type": "aws_instance",
         "name": "foo",
         "provider_name": "aws",
         "change": {
            "actions": [
               "create"
            ],
            "before": null,
            "after": {
               "ami": "ami-04b9e92b5572fa0d1",
               "credit_specification": [],
               "disable_api_termination": null,
               "ebs_optimized": null,
               "get_password_data": false,
               "iam_instance_profile": null,
               "instance_initiated_shutdown_behavior": null,
               "instance_type": "t2.micro",
               "monitoring": null,
               "source_dest_check": true,
               "tags": null,
               "timeouts": null,
               "user_data": null,
               "user_data_base64": null
            },
            "after_unknown": {
               "arn": true,
               "associate_public_ip_address": true,
               "availability_zone": true,
               "cpu_core_count": true,
               "cpu_threads_per_core": true,
               "credit_specification": [],
               "ebs_block_device": true,
               "ephemeral_block_device": true,
               "host_id": true,
               "id": true,
               "instance_state": true,
               "ipv6_address_count": true,
               "ipv6_addresses": true,
               "key_name": true,
               "network_interface": true,
               "network_interface_id": true,
               "password_data": true,
               "placement_group": true,
               "primary_network_interface_id": true,
               "private_dns": true,
               "private_ip": true,
               "public_dns": true,
               "public_ip": true,
               "root_block_device": true,
               "security_groups": true,
               "subnet_id": true,
               "tenancy": true,
               "volume_tags": true,
               "vpc_security_group_ids": true
            }
         }
      }
   ],
   "configuration": {
      "provider_config": {
         "aws": {
            "name": "aws",
            "expressions": {
               "region": {
                  "constant_value": "us-east-1"
               }
            }
         }
      },
      "root_module": {
         "resources": [
            {
               "address": "aws_instance.foo",
               "mode": "managed",
               "type": "aws_instance",
               "name": "foo",
               "provider_config_key": "aws",
               "expressions": {
                  "ami": {
                     "constant_value": "ami-04b9e92b5572fa0d1"
                  },
                  "instance_type": {
                     "constant_value": "t2.micro"
                  }
               },
               "schema_version": 1
            }
         ]
      }
   }
}

invalid_single_ami_from_tf_plan = {
   "format_version": "0.1",
   "terraform_version": "0.12.12",
   "planned_values": {
      "root_module": {
         "resources": [
            {
               "address": "aws_instance.foo",
               "mode": "managed",
               "type": "aws_instance",
               "name": "foo",
               "provider_name": "aws",
               "schema_version": 1,
               "values": {
                  "ami": "ami-0547b1fd62b28a111",
                  "credit_specification": [],
                  "disable_api_termination": null,
                  "ebs_optimized": null,
                  "get_password_data": false,
                  "iam_instance_profile": null,
                  "instance_initiated_shutdown_behavior": null,
                  "instance_type": "t2.micro",
                  "monitoring": null,
                  "source_dest_check": true,
                  "tags": null,
                  "timeouts": null,
                  "user_data": null,
                  "user_data_base64": null
               }
            }
         ]
      }
   },
   "resource_changes": [
      {
         "address": "aws_instance.foo",
         "mode": "managed",
         "type": "aws_instance",
         "name": "foo",
         "provider_name": "aws",
         "change": {
            "actions": [
               "create"
            ],
            "before": null,
            "after": {
               "ami": "ami-0547b1fd62b28a111",
               "credit_specification": [],
               "disable_api_termination": null,
               "ebs_optimized": null,
               "get_password_data": false,
               "iam_instance_profile": null,
               "instance_initiated_shutdown_behavior": null,
               "instance_type": "t2.micro",
               "monitoring": null,
               "source_dest_check": true,
               "tags": null,
               "timeouts": null,
               "user_data": null,
               "user_data_base64": null
            },
            "after_unknown": {
               "arn": true,
               "associate_public_ip_address": true,
               "availability_zone": true,
               "cpu_core_count": true,
               "cpu_threads_per_core": true,
               "credit_specification": [],
               "ebs_block_device": true,
               "ephemeral_block_device": true,
               "host_id": true,
               "id": true,
               "instance_state": true,
               "ipv6_address_count": true,
               "ipv6_addresses": true,
               "key_name": true,
               "network_interface": true,
               "network_interface_id": true,
               "password_data": true,
               "placement_group": true,
               "primary_network_interface_id": true,
               "private_dns": true,
               "private_ip": true,
               "public_dns": true,
               "public_ip": true,
               "root_block_device": true,
               "security_groups": true,
               "subnet_id": true,
               "tenancy": true,
               "volume_tags": true,
               "vpc_security_group_ids": true
            }
         }
      }
   ],
   "configuration": {
      "provider_config": {
         "aws": {
            "name": "aws",
            "expressions": {
               "region": {
                  "constant_value": "us-east-1"
               }
            }
         }
      },
      "root_module": {
         "resources": [
            {
               "address": "aws_instance.foo",
               "mode": "managed",
               "type": "aws_instance",
               "name": "foo",
               "provider_config_key": "aws",
               "expressions": {
                  "ami": {
                     "constant_value": "ami-0547b1fd62b28a111"
                  },
                  "instance_type": {
                     "constant_value": "t2.micro"
                  }
               },
               "schema_version": 1
            }
         ]
      }
   }
}