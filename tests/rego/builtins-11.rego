# Base64 related tests.
package fregot.tests.builtins_11

test_01 {
  string = "subjects?_d=1"

  base64.encode(string) == "c3ViamVjdHM/X2Q9MQ=="
  base64.decode("c3ViamVjdHM/X2Q9MQ==") == string

  base64url.encode(string) == "c3ViamVjdHM_X2Q9MQ=="
  base64url.decode("c3ViamVjdHM_X2Q9MQ==") == string
}
