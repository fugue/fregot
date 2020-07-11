package comprehension_index

mock_input = {
  "exposed": [
    {
      "interface": "eth0",
      "port": 8080,
    },
    {
      "interface": "eth0",
      "port": 8081,
    },
    {
      "interface": "eth1",
      "port": 443,
    },
    {
      "interface": "lo1",
      "port": 5000,
    }
  ]
}

deny[msg] {
  some i
  count(exposed_ports_by_interface[i]) > 1
  msg := sprintf("interface '%v' exposes too many ports", [i])
}

exposed_ports_by_interface[intf] = ports {
  some i
  intf := mock_input.exposed[i].interface
  ports := [port |
    some j
    mock_input.exposed[j].interface = intf
    port := mock_input.exposed[j].port
  ]
}

test_deny {
  deny["interface 'eth0' exposes too many ports"] with input as mock_input
}
